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
load("Species.Detection.SORA.RData")
SORA <- y
load("Species.Detection.COGA.RData")
COGA <- y
load("Species.Detection.VIRA.RData")
VIRA <- y
load("Species.Detection.LEBI.RData")
LEBI <- y

species.data <- list(SORA, VIRA, COGA, LEBI)
names(species.data)[1] <- "SORA"
names(species.data)[2] <- "VIRA"
names(species.data)[3] <- "COGA"
names(species.data)[4] <- "LEBI"

focal.species <- c("COGA", "LEBI", "SORA", "VIRA")
s <- list("COGA", "LEBI", "SORA", "VIRA")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2")

##Create data Array

tmp <- array(NA,dim = c(4,61,2))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(focal.species)){ 
  i = focal.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
  }

## Join data

load("General Covariates.RData")
m3_data <- list(tmp, Cov.gen$occ.covs, Cov.gen$coords, Cov.gen$det.covs)

names(m3_data)
names(m3_data)[1] <- "y"
names(m3_data)[2] <- "occ.covs"
names(m3_data)[3] <- "coords"
names(m3_data)[4] <- "det.covs"


##Multi Species Model
out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(Veg) + scale(Area) + (Habitat.Type),
                 det.formula = ~ scale(date + I(scale(date^2))) + noise, 
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
summary(out, level = 'community')

# Occupancy community-level effects 
MCMCplot(out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Multi Species Model - Emergent Cover

load("Emergent Covariates.RData")
m4_data <- list(tmp, covariates_v, Cov.gen$coords, Cov.gen$det.covs)

names(m4_data)
names(m4_data)[1] <- "y"
names(m4_data)[2] <- "occ.covs"
names(m4_data)[3] <- "coords"
names(m4_data)[4] <- "det.covs"

out.f.v. <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                 det.formula = ~ scale(date + I(scale(date^2))) + noise, 
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
summary(out.f.v., level = 'community')
# Occupancy community-level effects 
MCMCplot(out.f.v.$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(out.f.v.$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(out.f.v.$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##common wetland
load("Species.Detection.RWBL.RData")
RWBL <- y
load("Species.Detection.COYE.RData")
COYE <- y
load("Species.Detection.SWSP.RData")
SWSP <- y
load("Species.Detection.GCFL.RData")
GCFL <- y
load("Species.Detection.YEWA.RData")
YEWA <- y
load("Species.Detection.SOSP.RData")
SOSP <- y
load("Species.Detection.EAKI.RData")
EAKI <- y
load("Species.Detection.MAWR.RData")
MAWR <- y
load("Species.Detection.WIFL.RData")
WIFL <- y
load("Species.Detection.GRHE.RData")
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
Visit <- list("Visit 1", "Visit 2")

##Create data Array

tmp <- array(NA,dim = c(10,61,2))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(common.species)){ 
  i = common.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

## Join data

load("General Covariates.RData")
m4_data <- list(tmp, covariates_v, Cov.gen$coords, Cov.gen$det.covs)

names(m4_data)
names(m4_data)[1] <- "y"
names(m4_data)[2] <- "occ.covs"
names(m4_data)[3] <- "coords"
names(m4_data)[4] <- "det.covs"


##Multi Species Model common
common.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                 det.formula = ~ scale(date + I(scale(date^2))) + noise, 
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
summary(common.out, level = 'community')

# Occupancy community-level effects 
MCMCplot(common.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(common.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(common.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))


##Multi Species Model waterfowl
load("Species.Detection.MALL.RData")
MALL <- y
load("Species.Detection.CAGO.RData")
CAGO <- y
load("Species.Detection.WODU.RData")
WODU <- y
load("Species.Detection.BWTE.RData")
BWTE <- y
load("Species.Detection.DCCO.RData")
DCCO <- y
load("Species.Detection.HOME.RData")
HOME <- y

species.data <- list(MALL, CAGO, WODU, BWTE, DCCO, HOME)
names(species.data)[1] <- "MALL"
names(species.data)[2] <- "CAGO"
names(species.data)[3] <- "WODU"
names(species.data)[4] <- "BWTE"
names(species.data)[5] <- "DCCO"
names(species.data)[6] <- "HOME"

focal.species <- c("MALL", "CAGO", "WODU", "BWTE", "DCCO", "HOME")
s <- list("MALL", "CAGO", "WODU", "BWTE", "DCCO", "HOME")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2")

tmp <- array(NA,dim = c(6,61,2))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(focal.species)){ 
  i = focal.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

m5_data <- list(tmp, covariates_v, Cov.gen$coords, Cov.gen$det.covs)

names(m5_data)
names(m5_data)[1] <- "y"
names(m5_data)[2] <- "occ.covs"
names(m5_data)[3] <- "coords"
names(m5_data)[4] <- "det.covs"

water.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                        det.formula = ~ scale(date + I(scale(date^2))) + noise, 
                        data = m5_data, 
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
load("Species.Detection.AMBI.RData")
AMBI <- y
load("Species.Detection.AMWO.RData")
AMWO <- y
load("Species.Detection.BEKI.RData")
BEKI <- y
load("Species.Detection.GREG.RData")
GREG <- y
load("Species.Detection.KIRA.RData")
KIRA <- y
load("Species.Detection.NOWA.RData")
NOWA <- y
load("Species.Detection.WISN.RData")
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
Visit <- list("Visit 1", "Visit 2")

tmp <- array(NA,dim = c(21,61,2))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(All.wet.species)){ 
  i = All.wet.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

m6_data <- list(tmp, covariates_v, Cov.gen$coords, Cov.gen$det.covs)

names(m6_data)
names(m6_data)[1] <- "y"
names(m6_data)[2] <- "occ.covs"
names(m6_data)[3] <- "coords"
names(m6_data)[4] <- "det.covs"

wet.all.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(total) + scale(Area) + (Habitat.Type),
                       det.formula = ~ scale(date + I(scale(date^2))) + noise, 
                       data = m6_data, 
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


