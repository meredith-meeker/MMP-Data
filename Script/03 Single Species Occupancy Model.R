##Load all the packages

library(tidyverse)
library(ggplot2)
library(devtools)
library(rethinking)
library(dagitty)
library(ggdag)
library(dplyr)
library(spOccupancy)
library(MCMCvis)
library(stars)
library(pals)
library(cowplot)
library(tibble)
library(plyr)
library(unmarked)


##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)
n_distinct(data$Species.Code)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Survey Details.csv")
head(survey_details)

load("General Covariates.RData")
load("Species.Detection.COYE.RData")


## Join species and covariate data for the model 

m1_data <- list(y, Cov.gen$occ.covs, Cov.gen$coords, Cov.gen$det.covs)

names(m1_data)
names(m1_data)[1] <- "y"
names(m1_data)[2] <- "occ.covs"
names(m1_data)[3] <- "coords"
names(m1_data)[4] <- "det.covs"

str(m1_data)

# Model fitting --------------------------------------------------------USGS
# Fit a non-spatial, single-species occupancy model
occ_COYE <- PGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) + (Habitat.Type), 
            det.formula = ~ scale(date + I(scale(date^2))) + noise, 
             data = m1_data, 
             n.samples = 5000, 
             n.thin = 4, 
             n.burn = 3000, 
             n.chains = 3,
             n.report = 500)
summary(occ_COYE)

## Plot summary

# Concise summary of main parameter estimates
summary(occ_SWSP)
# Take a look at objects in resulting object
names(occ_SWSP)
str(occ_SWSP$beta.samples)
# Create simple plot summaries using MCMCvis package.
# Occupancy covariate effects ---------
occ_cov <- MCMCplot(occ_SWSP$beta.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(occ_SWSP$alpha.samples, ref_ovl = TRUE, ci = c(50, 95))

##relationship graph 



#Difference in Habitat Type
coef(occ_SWSP)
diff.HT <- extract.samples(occ_SWSP, n=10000)

## Spatial Model

plot(m1_data$coords, pch = 19)

COYE.sp <- spPGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) +(Habitat.Type), 
                  det.formula = ~ scale(date) + I(scale(date^2)) + noise, 
                  data = m1_data, 
                  n.batch = 400, 
                  batch.length = 25,
                  NNGP = TRUE, 
                  n.neighbors = 5, 
                  n.thin = 10, 
                  n.burn = 5000, 
                  n.chains = 3,
                  n.report = 100)
summary(COYE.sp)

# Occupancy covariate effects ---------
MCMCplot(COYE.sp$beta.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(COYE.sp$alpha.samples, ref_ovl = TRUE, ci = c(50, 95))

# 3. Model validation -----------------------------------------------------
# Perform a posterior predictive check to assess model fit. 
ppc.out <- ppcOcc(occ_COYE, fit.stat = 'freeman-tukey', group = 1)
ppc.out.sp <- ppcOcc(COYE.sp, fit.stat = 'freeman-tukey', group = 1)
# Calculate a Bayesian p-value as a simple measure of Goodness of Fit.
# Bayesian p-values between 0.1 and 0.9 indicate adequate model fit. 
summary(ppc.out)
summary(ppc.out.sp)

# 4. Model comparison -----------------------------------------------------
# Compute Widely Applicable Information Criterion (WAIC)
# Lower values indicate better model fit. 
# Non-spatial
waicOcc(occ_COYE)
waicOcc(COYE.sp)
# Spatial

##Single Species Prediction
load("C:/Users/mmeek/Downloads/switzerlandPredData.rda")

pred.vals.land <- seq(min(m1_data$occ.covs$Landscape), 
                        max(m1_data$occ.covs$Landscape), 
                        length.out = 100)

land.0 <- pred.vals.land.scale <- (pred.vals - mean(m1_data$occ.covs$Landscape)) / 
  sd(m1_data$occ.covs$Landscape)

veg.0 <- pred.vals.veg.scale <- (pred.vals - mean(m1_data$occ.covs$Veg)) / 
  sd(m1_data$occ.covs$Veg)

area.0 <- pred.vals.area.scale <- (pred.vals - mean(m1_data$occ.covs$Area)) / 
  sd(m1_data$occ.covs$Area)

pred.df <- as.matrix(data.frame(intercept = 1, Landscape = pred.vals.land.scale, 
                                Veg = 0, Area = 0,  
                                slope = 0))

out.pred <- predict(occ_COYE, pred.df)

str(out.pred)

psi.0.quants <- apply(out.pred$psi.0.samples, 2, quantile, 
                      prob = c(0.025, 0.5, 0.975))
psi.plot.dat <- data.frame(psi.med = psi.0.quants[2, ], 
                           psi.low = psi.0.quants[1, ], 
                           psi.high = psi.0.quants[3, ], 
                           Landscape = pred.vals.land)
ggplot(psi.plot.dat, aes(x = Landscape, y = psi.med)) + 
  geom_ribbon(aes(ymin = psi.low, ymax = psi.high), fill = 'grey70') +
  geom_line() + 
  theme_bw() + 
  scale_y_continuous(limits = c(0, 1)) + 
  labs(x = 'Landscape (% cover)', y = 'Occupancy Probability') 

# Create prediction design matrix
X.0 <- cbind(1, land.0, land.0^2, veg.0, area.0)
# Predict at new locations
out.pred <- predict(COYE.sp, X.0, coords.0)
# Occupancy probability means
psi.0.mean <- apply(out.pred$psi.0.samples, 2, mean)
# Occupancy probability standard deviations
psi.0.sd <- apply(out.pred$psi.0.samples, 2, sd)
# Spatial process mean and sd
w.0.mean <- apply(out.pred$w.0.samples, 2, mean)
w.0.sd <- apply(out.pred$w.0.samples, 2, sd)


##Single species map
plot.df <- data.frame(psi.mean = psi.0.mean,
                      psi.sd = psi.0.sd,
                      w.mean = w.0.mean, 
                      w.sd = w.0.sd,
                      x = coords.0[, 1],
                      y = coords.0[, 2])
pred.stars <- st_as_stars(plot.df, dims = c('x', 'y'))
psi.mean.plot <- ggplot() +
  geom_stars(data = pred.stars, aes(x = x, y = y, fill = psi.mean),interpolate = TRUE) +
  scale_fill_gradientn("", colors = ocean.tempo(1000), limits = c(0, 1),
                       na.value = NA) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  labs(x = "Easting", y = "Northing", title = 'Occupancy Mean')
psi.sd.plot <- ggplot() +
  geom_stars(data = pred.stars, aes(x = x, y = y, fill = psi.sd),interpolate = TRUE) +
  scale_fill_gradientn("", colors = ocean.tempo(1000), limits = c(0, 1),
                       na.value = NA) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  labs(x = "Easting", y = "Northing", title = 'Occupancy SD')
w.mean.plot <- ggplot() +
  geom_stars(data = pred.stars, aes(x = x, y = y, fill = w.mean),interpolate = TRUE) +
  scale_fill_gradientn("", colors = ocean.tempo(1000),
                       na.value = NA) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  labs(x = "Easting", y = "Northing", title = 'Spatial Effect Mean')
w.sd.plot <- ggplot() +
  geom_stars(data = pred.stars, aes(x = x, y = y, fill = w.sd),interpolate = TRUE) +
  scale_fill_gradientn("", colors = ocean.tempo(1000),
                       na.value = NA) +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  labs(x = "Easting", y = "Northing", title = 'Spatial Effect SD') 
plot_grid(psi.mean.plot, w.mean.plot, 
          psi.sd.plot, w.sd.plot, nrow = 2, ncol = 2)
