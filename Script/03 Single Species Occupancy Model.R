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
load("Species.Detection.SWSP.RData")


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
occ_SWSP <- PGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) +(Habitat.Type), 
             det.formula = ~ scale(date + I(scale(date^2))) + noise, 
             data = m1_data, 
             n.samples = 5000, 
             n.thin = 4, 
             n.burn = 3000, 
             n.chains = 3,
             n.report = 500)
summary(occ_SWSP)

## Plot summary

# Concise summary of main parameter estimates
summary(occ_SWSP)
# Take a look at objects in resulting object
names(occ_SWSP)
str(occ_SWSP$beta.samples)
# Create simple plot summaries using MCMCvis package.
# Occupancy covariate effects ---------
MCMCplot(occ_SWSP$beta.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(occ_SWSP$alpha.samples, ref_ovl = TRUE, ci = c(50, 95))

## Spatial Model

plot(m1_data$coords, pch = 19)

SWSP.sp <- spPGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) +(Habitat.Type), 
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
summary(SWSP.sp)

# Occupancy covariate effects ---------
MCMCplot(SWSP.sp$beta.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(SWSP.sp$alpha.samples, ref_ovl = TRUE, ci = c(50, 95))

# Fit a spatially-explicit joint species distribution model with 
# imperfect detection. 
load("Focal.Species.Detection.RData")
m2_data <- list(y_focal, Cov.gen$occ.covs, Cov.gen$coords, Cov.gen$det.covs)

names(m2_data)
names(m2_data)[1] <- "y"
names(m2_data)[2] <- "occ.covs"
names(m2_data)[3] <- "coords"
names(m2_data)[4] <- "det.covs"

focal.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) +(Habitat.Type), 
                        det.formula = ~ scale(date) + I(scale(date^2)) + noise, 
                 data = m2_data, 
                 n.batch = 400, 
                 batch.length = 25,
                 n.thin = 10, 
                 n.burn = 5000, 
                 n.chains = 1,
                 NNGP = TRUE,
                 n.factors = 4,
                 n.neighbors = 5,
                 n.omp.threads = 1,
                 cov.model = 'exponential',
                 n.report = 10)
summary(out, level = 'community')
