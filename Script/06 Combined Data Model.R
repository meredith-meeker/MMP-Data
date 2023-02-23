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
library(purrr)

##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)
n_distinct(data$Species.Code)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Survey Details.csv")
head(survey_details)

save(y,file = "Species.Detection.Multi.SORA.RData")

##Create Guilds
##focal
load("Outputs/Species.Detection.SORA.RData")
SORA <- y
load("Outputs/Species.Detection.COGA.RData")
COGA <- y
load("Outputs/Species.Detection.VIRA.RData")
VIRA <- y
load("Outputs/Species.Detection.LEBI.RData")
LEBI <- y
load("Outputs/Species.Detection.PBGR.RData")
PBGR <- y

species.data <- list(SORA, VIRA, COGA, LEBI, PBGR)
names(species.data)[1] <- "SORA"
names(species.data)[2] <- "VIRA"
names(species.data)[3] <- "COGA"
names(species.data)[4] <- "LEBI"
names(species.data)[5] <- "PBGR"

focal.species <- c("COGA", "LEBI", "SORA", "VIRA", "PBGR")
s <- list("COGA", "LEBI", "SORA", "VIRA", "PBGR")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

##Create data Array

tmp <- array(NA,dim = c(5,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(focal.species)){ 
  i = focal.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

## Join data

load("Emergent Covariates.RData")
load("General Covariates.RData")
load("Combined Covariates.RData")
m1_data <- list(tmp, covariates_v, Cov.gen$coords, det.covs.combined)

names(m1_data)
names(m1_data)[1] <- "y"
names(m1_data)[2] <- "occ.covs"
names(m1_data)[3] <- "coords"
names(m1_data)[4] <- "det.covs"


##Multi Species Model
out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                 det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                 data = m1_data, 
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

# Occupancy community-level effects 
MCMCplot(out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##common wetland
load("Outputs/Species.Detection.RWBL.RData")
RWBL <- y
load("Outputs/Species.Detection.COYE.RData")
COYE <- y
load("Outputs/Species.Detection.SWSP.RData")
SWSP <- y
load("Outputs/Species.Detection.GCFL.RData")
GCFL <- y
load("Outputs/Species.Detection.YEWA.RData")
YEWA <- y
load("Outputs/Species.Detection.SOSP.RData")
SOSP <- y
load("Outputs/Species.Detection.EAKI.RData")
EAKI <- y
load("Outputs/Species.Detection.MAWR.RData")
MAWR <- y
load("Outputs/Species.Detection.WIFL.RData")
WIFL <- y
load("Outputs/Species.Detection.GRHE.RData")
GRHE <- y



species.data <- list(RWBL, COYE, SWSP, GCFL, YEWA, SOSP, EAKI, MAWR, WIFL, GRHE)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "GCFL"
names(species.data)[5] <- "YEWA"
names(species.data)[6] <- "SOSP"
names(species.data)[7] <- "EAKI"
names(species.data)[8] <- "MAWR"
names(species.data)[9] <- "WIFL"
names(species.data)[10] <- "GRHE"

common.species <- c("RWBL", "COYE", "SWSP", "GCFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE")
s <- list("RWBL", "COYE", "SWSP", "GCFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

##Create data Array

tmp <- array(NA,dim = c(10,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(common.species)){ 
  i = common.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

## Join data

load("General Covariates.RData")
m2_data <- list(tmp, covariates_v, Cov.gen$coords, det.covs.combined)

names(m2_data)
names(m2_data)[1] <- "y"
names(m2_data)[2] <- "occ.covs"
names(m2_data)[3] <- "coords"
names(m2_data)[4] <- "det.covs"


##Multi Species Model common
common.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                        det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
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
summary(common.out, level = 'community')

# Occupancy community-level effects 
MCMCplot(common.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(common.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(common.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))


##Multi Species Model waterfowl
load("Outputs/Species.Detection.MALL.RData")
MALL <- y
load("Outputs/Species.Detection.CANG.RData")
CANG <- y
load("Outputs/Species.Detection.WODU.RData")
WODU <- y
load("Outputs/Species.Detection.BWTE.RData")
BWTE <- y
load("Outputs/Species.Detection.DCCO.RData")
DCCO <- y
load("Outputs/Species.Detection.HOME.RData")
HOME <- y

species.data <- list(MALL, CANG, WODU, BWTE, DCCO, HOME)
names(species.data)[1] <- "MALL"
names(species.data)[2] <- "CANG"
names(species.data)[3] <- "WODU"
names(species.data)[4] <- "BWTE"
names(species.data)[5] <- "DCCO"
names(species.data)[6] <- "HOME"

focal.species <- c("MALL", "CANG", "WODU", "BWTE", "DCCO", "HOME")
s <- list("MALL", "CANG", "WODU", "BWTE", "DCCO", "HOME")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

tmp <- array(NA,dim = c(6,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(focal.species)){ 
  i = focal.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

m3_data <- list(tmp, covariates_v, Cov.gen$coords, det.covs.combined)

names(m3_data)
names(m3_data)[1] <- "y"
names(m3_data)[2] <- "occ.covs"
names(m3_data)[3] <- "coords"
names(m3_data)[4] <- "det.covs"

water.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                       det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                       data = m3_data, 
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
summary(water.out, level = 'community')

# Occupancy community-level effects 
MCMCplot(water.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(water.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(water.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Wetland All
load("Outputs/Species.Detection.AMBI.RData")
AMBI <- y
load("Outputs/Species.Detection.AMWO.RData")
AMWO <- y
load("Outputs/Species.Detection.BEKI.RData")
BEKI <- y
load("Outputs/Species.Detection.GREG.RData")
GREG <- y
load("Outputs/Species.Detection.KIRA.RData")
KIRA <- y
load("Outputs/Species.Detection.NOWA.RData")
NOWA <- y
load("Outputs/Species.Detection.WISN.RData")
WISN <- y


species.data <- list(RWBL, COYE, SWSP, GCFL, YEWA, SOSP, EAKI, MAWR, WIFL, GRHE, SORA, VIRA, COGA, LEBI, AMBI, AMWO, BEKI, GREG, KIRA, NOWA, WISN)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "GCFL"
names(species.data)[5] <- "YEWA"
names(species.data)[6] <- "SOSP"
names(species.data)[7] <- "EAKI"
names(species.data)[8] <- "MAWR"
names(species.data)[9] <- "WIFL"
names(species.data)[10] <- "GRHE"
names(species.data)[11] <- "SORA"
names(species.data)[12] <- "VIRA"
names(species.data)[13] <- "COGA"
names(species.data)[14] <- "LEBI"
names(species.data)[15] <- "AMBI"
names(species.data)[16] <- "AMWO"
names(species.data)[17] <- "BEKI"
names(species.data)[18] <- "GREG"
names(species.data)[19] <- "KIRA"
names(species.data)[20] <- "NOWA"
names(species.data)[21] <- "WISN"

All.wet.species <- c("RWBL", "COYE", "SWSP", "GCFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "KIRA", "NOWA", "WISN")
s <- list("RWBL", "COYE", "SWSP", "GCFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "KIRA", "NOWA", "WISN")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

tmp <- array(NA,dim = c(21,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(All.wet.species)){ 
  i = All.wet.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

m4_data <- list(tmp, covariates_v, Cov.gen$coords, det.covs.combined)

names(m4_data)
names(m4_data)[1] <- "y"
names(m4_data)[2] <- "occ.covs"
names(m4_data)[3] <- "coords"
names(m4_data)[4] <- "det.covs"

wet.all.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                         det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                         data = m4_data, 
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
summary(wet.all.out, level = 'community')

# Occupancy community-level effects 
MCMCplot(wet.all.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(wet.all.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(wet.all.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))


