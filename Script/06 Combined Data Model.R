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
library(posterior)
library(GGally)

##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)
n_distinct(data$Species.Code)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Survey Details.csv")
head(survey_details)

##Testing correlation of covariates_V
ggpairs(data = covariates_v)
ggpairs(data = covariates_sd)
pairs(covariates_v)


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
load("Outputs/Species.Detection.AMBI.RData")
AMBI <- y

species.data <- list(SORA, VIRA, COGA, LEBI, PBGR, AMBI)
names(species.data)[1] <- "SORA"
names(species.data)[2] <- "VIRA"
names(species.data)[3] <- "COGA"
names(species.data)[4] <- "LEBI"
names(species.data)[5] <- "PBGR"
names(species.data)[6] <- "AMBI"

obligate.species <- c("COGA", "LEBI", "SORA", "VIRA", "PBGR", "AMBI")
s <- list("COGA", "LEBI", "SORA", "VIRA", "PBGR", "AMBI")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

##Create data Array

tmp <- array(NA,dim = c(6,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(obligate.species)){ 
  i = obligate.species[sn]
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
                 n.batch = 1000, 
                 batch.length = 25,
                 n.thin = 10, 
                 n.burn = 5000, 
                 n.chains = 4,
                 NNGP = TRUE,
                 n.factors = 4,
                 n.neighbors = 5,
                 n.omp.threads = 1,
                 cov.model = 'exponential',
                 n.report = 10)
       
summary(out, level = 'community')
summary(out, level = 'species')
# Occupancy community-level effects 
MCMCplot(out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Community Level difference
m1_beta_output <-as_draws_df(out$beta.comm.samples)
m1_beta_output <- rename_variables(m1_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m1_beta_output <-mutate_variables(m1_beta_output, diff = (SWMP-Wetland))
m1_summary_com <- (summarise_draws(m1_beta_output))
saveRDS(m1_summary_com, file = "Outputs/m1_summary_com.RDS")

##Species Level Difference 
m1_beta_sp_output <-as_draws_df(out$beta.samples)

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.COGA= '(Intercept)-COGA', Wetland.COGA= 'Habitat.TypeWetland-COGA')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.COGA= (SWMP.COGA-Wetland.COGA))

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.LEBI= '(Intercept)-LEBI', Wetland.LEBI= 'Habitat.TypeWetland-LEBI')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.LEBI= (SWMP.LEBI-Wetland.LEBI))

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.AMBI= '(Intercept)-AMBI', Wetland.AMBI= 'Habitat.TypeWetland-AMBI')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.AMBI= (SWMP.AMBI-Wetland.AMBI))

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.SORA= '(Intercept)-SORA', Wetland.SORA= 'Habitat.TypeWetland-SORA')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.SORA= (SWMP.SORA-Wetland.SORA))

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.VIRA= '(Intercept)-VIRA', Wetland.VIRA= 'Habitat.TypeWetland-VIRA')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.VIRA= (SWMP.VIRA-Wetland.VIRA))

m1_beta_sp_output <- rename_variables(m1_beta_sp_output, SWMP.PBGR= '(Intercept)-PBGR', Wetland.PBGR= 'Habitat.TypeWetland-PBGR')
m1_beta_sp_output <-mutate_variables(m1_beta_sp_output, diff.PBGR= (SWMP.PBGR-Wetland.PBGR))

m1_summary_sp <- (summarise_draws(m1_beta_sp_output))
saveRDS(m1_summary_sp, file = "Outputs/m1_summary_sp.RDS")

##Detection

##Community 
m1_alpha_output_det <-as_draws_df(out$alpha.comm.samples)
m1_alpha_output_det <- rename_variables(m1_alpha_output_det, ARU = '(Intercept)', PC = methodPC)
m1_alpha_output_det <-mutate_variables(m1_alpha_output_det, diff = (ARU-PC))
m1_summary_com_det <- (summarise_draws(m1_alpha_output_det))
saveRDS(m1_summary_com_det, file = "Outputs/m1_summary_com_det.RDS")
##Species 
m1_alpha_sp_output_det <-as_draws_df(out$alpha.samples)

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.COGA= '(Intercept)-COGA', PC.COGA= 'methodPC-COGA')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.COGA= (ARU.COGA-PC.COGA))

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.LEBI= '(Intercept)-LEBI', PC.LEBI= 'methodPC-LEBI')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.LEBI= (ARU.LEBI-PC.LEBI))

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.AMBI= '(Intercept)-AMBI', PC.AMBI= 'methodPC-AMBI')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.AMBI= (ARU.AMBI-PC.AMBI))

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.SORA= '(Intercept)-SORA', PC.SORA= 'methodPC-SORA')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.SORA= (ARU.SORA-PC.SORA))

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.VIRA= '(Intercept)-VIRA', PC.VIRA= 'methodPC-VIRA')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.VIRA= (ARU.VIRA-PC.VIRA))

m1_alpha_sp_output_det <- rename_variables(m1_alpha_sp_output_det, ARU.PBGR= '(Intercept)-PBGR', PC.PBGR= 'methodPC-PBGR')
m1_alpha_sp_output_det <-mutate_variables(m1_alpha_sp_output_det, diff.PBGR= (ARU.PBGR-PC.PBGR))


m1_summary_sp_det <- (summarise_draws(m1_alpha_sp_output_det))
saveRDS(m1_summary_sp_det, file = "Outputs/m1_summary_sp_det.RDS")



##Simpson's index

load("Diversity Index Covariates")
m8_data <- list(tmp, covariates_sd, Cov.gen$coords, det.covs.combined)

names(m8_data)
names(m8_data)[1] <- "y"
names(m8_data)[2] <- "occ.covs"
names(m8_data)[3] <- "coords"
names(m8_data)[4] <- "det.covs"


##Multi Species Model
out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(D.1) + scale(Area) + (Habitat.Type),
                 det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                 data = m8_data, 
                 n.batch = 1000, 
                 batch.length = 25,
                 n.thin = 10, 
                 n.burn = 5000, 
                 n.chains = 4,
                 NNGP = TRUE,
                 n.factors = 4,
                 n.neighbors = 5,
                 n.omp.threads = 1,
                 cov.model = 'exponential',
                 n.report = 10)

summary(out, level = 'community')
summary(out, level = 'species')

# Occupancy community-level effects 
MCMCplot(out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))


##Community Level difference
m8_beta_output <-as_draws_df(out$beta.comm.samples)
m8_beta_output <- rename_variables(m8_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m8_beta_output <-mutate_variables(m8_beta_output, diff = (SWMP-Wetland))
m8_summary_com <- (summarise_draws(m8_beta_output))
saveRDS(m8_summary_com, file = "Outputs/m8_summary_com.RDS")

##Species Level Difference 
m8_beta_sp_output <-as_draws_df(out$beta.samples)

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.COGA= '(Intercept)-COGA', Wetland.COGA= 'Habitat.TypeWetland-COGA')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.COGA= (SWMP.COGA-Wetland.COGA))

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.LEBI= '(Intercept)-LEBI', Wetland.LEBI= 'Habitat.TypeWetland-LEBI')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.LEBI= (SWMP.LEBI-Wetland.LEBI))

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.AMBI= '(Intercept)-AMBI', Wetland.AMBI= 'Habitat.TypeWetland-AMBI')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.AMBI= (SWMP.AMBI-Wetland.AMBI))

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.SORA= '(Intercept)-SORA', Wetland.SORA= 'Habitat.TypeWetland-SORA')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.SORA= (SWMP.SORA-Wetland.SORA))

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.VIRA= '(Intercept)-VIRA', Wetland.VIRA= 'Habitat.TypeWetland-VIRA')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.VIRA= (SWMP.VIRA-Wetland.VIRA))

m8_beta_sp_output <- rename_variables(m8_beta_sp_output, SWMP.PBGR= '(Intercept)-PBGR', Wetland.PBGR= 'Habitat.TypeWetland-PBGR')
m8_beta_sp_output <-mutate_variables(m8_beta_sp_output, diff.PBGR= (SWMP.PBGR-Wetland.PBGR))


m8_summary_sp <- (summarise_draws(m8_beta_sp_output))
saveRDS(m8_summary_sp, file = "Outputs/m8_summary_sp.RDS")



##common wetland
load("Outputs/Species.Detection.RWBL.RData")
RWBL <- y
load("Outputs/Species.Detection.COYE.RData")
COYE <- y
load("Outputs/Species.Detection.SWSP.RData")
SWSP <- y
load("Outputs/Species.Detection.ALFL.RData")
ALFL <- y
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
load("Outputs/Species.Detection.COGR.RData")
COGR <- y



species.data <- list(RWBL, COYE, SWSP, ALFL, YEWA, SOSP, EAKI, MAWR, WIFL, COGR)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "ALFL"
names(species.data)[5] <- "YEWA"
names(species.data)[6] <- "SOSP"
names(species.data)[7] <- "EAKI"
names(species.data)[8] <- "MAWR"
names(species.data)[9] <- "WIFL"
names(species.data)[10] <- "COGR"

common.species <- c("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "COGR")
s <- list("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "COGR")
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
                        n.batch = 1000, 
                        batch.length = 25,
                        n.thin = 10, 
                        n.burn = 5000, 
                        n.chains = 4,
                        NNGP = TRUE,
                        n.factors = 4,
                        n.neighbors = 5,
                        n.omp.threads = 1,
                        cov.model = 'exponential',
                        n.report = 10)
summary(common.out, level = 'community')
summary(common.out, level = 'species')

# Occupancy community-level effects 
MCMCplot(common.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(common.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(common.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Community Level difference
m2_beta_output <-as_draws_df(common.out$beta.comm.samples)
m2_beta_output <- rename_variables(m2_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m2_beta_output <-mutate_variables(m2_beta_output, diff = (SWMP-Wetland))
m2_summary_com <- (summarise_draws(m2_beta_output))
saveRDS(m2_summary_com, file = "Outputs/m2_summary_com.RDS")

##Species Level Difference 
m2_beta_sp_output <-as_draws_df(common.out$beta.samples)

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.RWBL= '(Intercept)-RWBL', Wetland.RWBL= 'Habitat.TypeWetland-RWBL')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.RWBL= (SWMP.RWBL-Wetland.RWBL))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.YEWA= '(Intercept)-YEWA', Wetland.YEWA= 'Habitat.TypeWetland-YEWA')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.YEWA= (SWMP.YEWA-Wetland.YEWA))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.COYE= '(Intercept)-COYE', Wetland.COYE= 'Habitat.TypeWetland-COYE')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.COYE= (SWMP.COYE-Wetland.COYE))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.SWSP= '(Intercept)-SWSP', Wetland.SWSP= 'Habitat.TypeWetland-SWSP')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.SWSP= (SWMP.SWSP-Wetland.SWSP))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.SOSP= '(Intercept)-SOSP', Wetland.SOSP= 'Habitat.TypeWetland-SOSP')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.SOSP= (SWMP.SOSP-Wetland.SOSP))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.EAKI= '(Intercept)-EAKI', Wetland.EAKI= 'Habitat.TypeWetland-EAKI')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.EAKI= (SWMP.EAKI-Wetland.EAKI))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.WIFL= '(Intercept)-WIFL', Wetland.WIFL= 'Habitat.TypeWetland-WIFL')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.WIFL= (SWMP.WIFL-Wetland.WIFL))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.ALFL= '(Intercept)-ALFL', Wetland.ALFL= 'Habitat.TypeWetland-ALFL')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.ALFL= (SWMP.ALFL-Wetland.ALFL))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.COGR= '(Intercept)-COGR', Wetland.COGR= 'Habitat.TypeWetland-COGR')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.COGR= (SWMP.COGR-Wetland.COGR))

m2_beta_sp_output <- rename_variables(m2_beta_sp_output, SWMP.MAWR= '(Intercept)-MAWR', Wetland.MAWR= 'Habitat.TypeWetland-MAWR')
m2_beta_sp_output <-mutate_variables(m2_beta_sp_output, diff.MAWR= (SWMP.MAWR-Wetland.MAWR))


m2_summary_sp <- (summarise_draws(m2_beta_sp_output))
saveRDS(m2_summary_sp, file = "Outputs/m2_summary_sp.RDS")

##Detection Variables 
##Community 
m2_alpha_output_det <-as_draws_df(common.out$alpha.comm.samples)
m2_alpha_output_det <- rename_variables(m2_alpha_output_det, ARU = '(Intercept)', PC = methodPC)
m2_alpha_output_det <-mutate_variables(m2_alpha_output_det, diff = (ARU-PC))
m2_summary_com_det <- (summarise_draws(m2_alpha_output_det))
saveRDS(m2_summary_com_det, file = "Outputs/m2_summary_com_det.RDS")
##Species 
m2_alpha_sp_output_det <-as_draws_df(common.out$alpha.samples)

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.RWBL= '(Intercept)-RWBL', PC.RWBL= 'methodPC-RWBL')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.RWBL= (ARU.RWBL-PC.RWBL))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.YEWA= '(Intercept)-YEWA', PC.YEWA= 'methodPC-YEWA')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.YEWA= (ARU.YEWA-PC.YEWA))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.COYE= '(Intercept)-COYE', PC.COYE= 'methodPC-COYE')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.COYE= (ARU.COYE-PC.COYE))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.SWSP= '(Intercept)-SWSP', PC.SWSP= 'methodPC-SWSP')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.SWSP= (ARU.SWSP-PC.SWSP))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.SOSP= '(Intercept)-SOSP', PC.SOSP= 'methodPC-SOSP')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.SOSP= (ARU.SOSP-PC.SOSP))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.EAKI= '(Intercept)-EAKI', PC.EAKI= 'methodPC-EAKI')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.EAKI= (ARU.EAKI-PC.EAKI))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.WIFL= '(Intercept)-WIFL', PC.WIFL= 'methodPC-WIFL')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.WIFL= (ARU.WIFL-PC.WIFL))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.ALFL= '(Intercept)-ALFL', PC.ALFL= 'methodPC-ALFL')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.ALFL= (ARU.ALFL-PC.ALFL))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.COGR= '(Intercept)-COGR', PC.COGR= 'methodPC-COGR')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.COGR= (ARU.COGR-PC.COGR))

m2_alpha_sp_output_det <- rename_variables(m2_alpha_sp_output_det, ARU.MAWR= '(Intercept)-MAWR', PC.MAWR= 'methodPC-MAWR')
m2_alpha_sp_output_det <-mutate_variables(m2_alpha_sp_output_det, diff.MAWR= (ARU.MAWR-PC.MAWR))


m2_summary_sp_det <- (summarise_draws(m2_alpha_sp_output_det))
saveRDS(m2_summary_sp_det, file = "Outputs/m2_summary_sp_det.RDS")

##Wetland All
load("Outputs/Species.Detection.AMBI.RData")
AMBI <- y
load("Outputs/Species.Detection.AMWO.RData")
AMWO <- y
load("Outputs/Species.Detection.BEKI.RData")
BEKI <- y
load("Outputs/Species.Detection.GREG.RData")
GREG <- y
load("Outputs/Species.Detection.BCNH.RData")
BCNH <- y
load("Outputs/Species.Detection.WISN.RData")
WISN <- y
load("Outputs/Species.Detection.PBGR.RData")
PBGR <- y
load("Outputs/Species.Detection.GRHE.RData")
GRHE <- y



species.data <- list(RWBL, COYE, SWSP, ALFL, YEWA, SOSP, EAKI, MAWR, WIFL, GRHE,
                     SORA, VIRA, COGA, LEBI, AMBI, AMWO, BEKI, GREG, BCNH, COGR, 
                     WISN, PBGR)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "ALFL"
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
names(species.data)[19] <- "BCNH"
names(species.data)[20] <- "COGR"
names(species.data)[21] <- "WISN"
names(species.data)[22] <- "PBGR"

All.wet.species <- c("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "BCNH", "COGR", "WISN", "PBGR")
s <- list("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "BCNH", "COGR", "WISN", "PBGR")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

tmp <- array(NA,dim = c(22,61,5))
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
                         n.batch = 1000, 
                         batch.length = 25,
                         n.thin = 10, 
                         n.burn = 5000, 
                         n.chains = 4,
                         NNGP = TRUE,
                         n.factors = 4,
                         n.neighbors = 5,
                         n.omp.threads = 1,
                         cov.model = 'exponential',
                         n.report = 10)
summary(wet.all.out, level = 'community')
summary(wet.all.out, level = 'species')

# Occupancy community-level effects 
MCMCplot(wet.all.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(wet.all.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(wet.all.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Community Level difference
m4_beta_output <-as_draws_df(wet.all.out$beta.comm.samples)
m4_beta_output <- rename_variables(m4_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m4_beta_output <-mutate_variables(m4_beta_output, diff = (SWMP-Wetland))
m4_summary_com <- (summarise_draws(m4_beta_output))
saveRDS(m4_summary_com, file = "Outputs/m4_summary_com.RDS")

##Species Level Difference 
m4_beta_sp_output <-as_draws_df(wet.all.out$beta.samples)

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.RWBL= '(Intercept)-RWBL', Wetland.RWBL= 'Habitat.TypeWetland-RWBL')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.RWBL= (SWMP.RWBL-Wetland.RWBL))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.YEWA= '(Intercept)-YEWA', Wetland.YEWA= 'Habitat.TypeWetland-YEWA')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.YEWA= (SWMP.YEWA-Wetland.YEWA))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.COYE= '(Intercept)-COYE', Wetland.COYE= 'Habitat.TypeWetland-COYE')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.COYE= (SWMP.COYE-Wetland.COYE))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.SWSP= '(Intercept)-SWSP', Wetland.SWSP= 'Habitat.TypeWetland-SWSP')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.SWSP= (SWMP.SWSP-Wetland.SWSP))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.SOSP= '(Intercept)-SOSP', Wetland.SOSP= 'Habitat.TypeWetland-SOSP')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.SOSP= (SWMP.SOSP-Wetland.SOSP))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.EAKI= '(Intercept)-EAKI', Wetland.EAKI= 'Habitat.TypeWetland-EAKI')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.EAKI= (SWMP.EAKI-Wetland.EAKI))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.WIFL= '(Intercept)-WIFL', Wetland.WIFL= 'Habitat.TypeWetland-WIFL')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.WIFL= (SWMP.WIFL-Wetland.WIFL))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.ALFL= '(Intercept)-ALFL', Wetland.ALFL= 'Habitat.TypeWetland-ALFL')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.ALFL= (SWMP.ALFL-Wetland.ALFL))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.GRHE= '(Intercept)-GRHE', Wetland.GRHE= 'Habitat.TypeWetland-GRHE')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.GRHE= (SWMP.GRHE-Wetland.GRHE))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.MAWR= '(Intercept)-MAWR', Wetland.MAWR= 'Habitat.TypeWetland-MAWR')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.MAWR= (SWMP.MAWR-Wetland.MAWR))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.COGA= '(Intercept)-COGA', Wetland.COGA= 'Habitat.TypeWetland-COGA')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.COGA= (SWMP.COGA-Wetland.COGA))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.LEBI= '(Intercept)-LEBI', Wetland.LEBI= 'Habitat.TypeWetland-LEBI')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.LEBI= (SWMP.LEBI-Wetland.LEBI))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.AMBI= '(Intercept)-AMBI', Wetland.AMBI= 'Habitat.TypeWetland-AMBI')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.AMBI= (SWMP.AMBI-Wetland.AMBI))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.SORA= '(Intercept)-SORA', Wetland.SORA= 'Habitat.TypeWetland-SORA')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.SORA= (SWMP.SORA-Wetland.SORA))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.VIRA= '(Intercept)-VIRA', Wetland.VIRA= 'Habitat.TypeWetland-VIRA')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.VIRA= (SWMP.VIRA-Wetland.VIRA))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.PBGR= '(Intercept)-PBGR', Wetland.PBGR= 'Habitat.TypeWetland-PBGR')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.PBGR= (SWMP.PBGR-Wetland.PBGR))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.WISN= '(Intercept)-WISN', Wetland.WISN= 'Habitat.TypeWetland-WISN')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.WISN= (SWMP.WISN-Wetland.WISN))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.BEKI= '(Intercept)-BEKI', Wetland.BEKI= 'Habitat.TypeWetland-BEKI')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.BEKI= (SWMP.BEKI-Wetland.BEKI))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.AMWO= '(Intercept)-AMWO', Wetland.AMWO= 'Habitat.TypeWetland-AMWO')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.AMWO= (SWMP.AMWO-Wetland.AMWO))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.GREG= '(Intercept)-GREG', Wetland.GREG= 'Habitat.TypeWetland-GREG')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.GREG= (SWMP.GREG-Wetland.GREG))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.COGR= '(Intercept)-COGR', Wetland.COGR= 'Habitat.TypeWetland-COGR')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.COGR= (SWMP.COGR-Wetland.COGR))

m4_beta_sp_output <- rename_variables(m4_beta_sp_output, SWMP.BCNH= '(Intercept)-BCNH', Wetland.BCNH= 'Habitat.TypeWetland-BCNH')
m4_beta_sp_output <-mutate_variables(m4_beta_sp_output, diff.BCNH= (SWMP.BCNH-Wetland.BCNH))


m4_summary_sp <- (summarise_draws(m4_beta_sp_output))
saveRDS(m4_summary_sp, file = "Outputs/m4_summary_sp.RDS")

##Detection Variables 
##Community 
m4_alpha_output_det <-as_draws_df(wet.all.out$alpha.comm.samples)
m4_alpha_output_det <- rename_variables(m4_alpha_output_det, ARU = '(Intercept)', PC = methodPC)
m4_alpha_output_det <-mutate_variables(m4_alpha_output_det, diff = (ARU-PC))
m4_summary_com_det <- (summarise_draws(m4_alpha_output_det))
saveRDS(m4_summary_com_det, file = "Outputs/m4_summary_com_det.RDS")
##Species 
m4_alpha_sp_output_det <-as_draws_df(wet.all.out$alpha.samples)

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.RWBL= '(Intercept)-RWBL', PC.RWBL= 'methodPC-RWBL')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.RWBL= (ARU.RWBL-PC.RWBL))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.YEWA= '(Intercept)-YEWA', PC.YEWA= 'methodPC-YEWA')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.YEWA= (ARU.YEWA-PC.YEWA))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.COYE= '(Intercept)-COYE', PC.COYE= 'methodPC-COYE')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.COYE= (ARU.COYE-PC.COYE))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.SWSP= '(Intercept)-SWSP', PC.SWSP= 'methodPC-SWSP')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.SWSP= (ARU.SWSP-PC.SWSP))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.SOSP= '(Intercept)-SOSP', PC.SOSP= 'methodPC-SOSP')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.SOSP= (ARU.SOSP-PC.SOSP))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.EAKI= '(Intercept)-EAKI', PC.EAKI= 'methodPC-EAKI')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.EAKI= (ARU.EAKI-PC.EAKI))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.WIFL= '(Intercept)-WIFL', PC.WIFL= 'methodPC-WIFL')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.WIFL= (ARU.WIFL-PC.WIFL))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.ALFL= '(Intercept)-ALFL', PC.ALFL= 'methodPC-ALFL')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.ALFL= (ARU.ALFL-PC.ALFL))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.GRHE= '(Intercept)-GRHE', PC.GRHE= 'methodPC-GRHE')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.GRHE= (ARU.GRHE-PC.GRHE))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.MAWR= '(Intercept)-MAWR', PC.MAWR= 'methodPC-MAWR')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.MAWR= (ARU.MAWR-PC.MAWR))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.COGA= '(Intercept)-COGA', PC.COGA= 'methodPC-COGA')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.COGA= (ARU.COGA-PC.COGA))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.LEBI= '(Intercept)-LEBI', PC.LEBI= 'methodPC-LEBI')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.LEBI= (ARU.LEBI-PC.LEBI))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.AMBI= '(Intercept)-AMBI', PC.AMBI= 'methodPC-AMBI')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.AMBI= (ARU.AMBI-PC.AMBI))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.SORA= '(Intercept)-SORA', PC.SORA= 'methodPC-SORA')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.SORA= (ARU.SORA-PC.SORA))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.VIRA= '(Intercept)-VIRA', PC.VIRA= 'methodPC-VIRA')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.VIRA= (ARU.VIRA-PC.VIRA))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.PBGR= '(Intercept)-PBGR', PC.PBGR= 'methodPC-PBGR')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.PBGR= (ARU.PBGR-PC.PBGR))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.WISN= '(Intercept)-WISN', PC.WISN= 'methodPC-WISN')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.WISN= (ARU.WISN-PC.WISN))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.BEKI= '(Intercept)-BEKI', PC.BEKI= 'methodPC-BEKI')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.BEKI= (ARU.BEKI-PC.BEKI))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.AMWO= '(Intercept)-AMWO', PC.AMWO= 'methodPC-AMWO')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.AMWO= (ARU.AMWO-PC.AMWO))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.GREG= '(Intercept)-GREG', PC.GREG= 'methodPC-GREG')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.GREG= (ARU.GREG-PC.GREG))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.COGR= '(Intercept)-COGR', PC.COGR= 'methodPC-COGR')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.COGR= (ARU.COGR-PC.COGR))

m4_alpha_sp_output_det <- rename_variables(m4_alpha_sp_output_det, ARU.BCNH= '(Intercept)-BCNH', PC.BCNH= 'methodPC-BCNH')
m4_alpha_sp_output_det <-mutate_variables(m4_alpha_sp_output_det, diff.BCNH= (ARU.BCNH-PC.BCNH))

m4_summary_sp_det <- (summarise_draws(m4_alpha_sp_output_det))
saveRDS(m4_summary_sp_det, file = "Outputs/m4_summary_sp_det.RDS")

## Veg Hetero Index Model

species.data <- list(RWBL, COYE, SWSP, ALFL, YEWA, SOSP, EAKI, MAWR, WIFL, COGR)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "ALFL"
names(species.data)[5] <- "YEWA"
names(species.data)[6] <- "SOSP"
names(species.data)[7] <- "EAKI"
names(species.data)[8] <- "MAWR"
names(species.data)[9] <- "WIFL"
names(species.data)[10] <- "COGR"

common.species <- c("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "COGR")
s <- list("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "COGR")
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

load("Diversity Index Covariates")
m5_data <- list(tmp, covariates_sd, Cov.gen$coords, det.covs.combined)

names(m5_data)
names(m5_data)[1] <- "y"
names(m5_data)[2] <- "occ.covs"
names(m5_data)[3] <- "coords"
names(m5_data)[4] <- "det.covs"


##Multi Species Model common
common.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(D.1) + scale(Area) + (Habitat.Type),
                        det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                        data = m5_data, 
                        n.batch = 1000, 
                        batch.length = 25,
                        n.thin = 10, 
                        n.burn = 5000, 
                        n.chains = 4,
                        NNGP = TRUE,
                        n.factors = 4,
                        n.neighbors = 5,
                        n.omp.threads = 1,
                        cov.model = 'exponential',
                        n.report = 10)
summary(common.out, level = 'community')
summary(common.out, level = 'species')

# Occupancy community-level effects 
MCMCplot(common.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(common.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(common.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Community Level difference
m5_beta_output <-as_draws_df(common.out$beta.comm.samples)
m5_beta_output <- rename_variables(m5_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m5_beta_output <-mutate_variables(m5_beta_output, diff = (SWMP-Wetland))
m5_summary_com <- (summarise_draws(m5_beta_output))
saveRDS(m5_summary_com, file = "Outputs/m5_summary_com.RDS")

##Species Level Difference 
m5_beta_sp_output <-as_draws_df(common.out$beta.samples)

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.RWBL= '(Intercept)-RWBL', Wetland.RWBL= 'Habitat.TypeWetland-RWBL')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.RWBL= (SWMP.RWBL-Wetland.RWBL))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.YEWA= '(Intercept)-YEWA', Wetland.YEWA= 'Habitat.TypeWetland-YEWA')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.YEWA= (SWMP.YEWA-Wetland.YEWA))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.COYE= '(Intercept)-COYE', Wetland.COYE= 'Habitat.TypeWetland-COYE')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.COYE= (SWMP.COYE-Wetland.COYE))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.SWSP= '(Intercept)-SWSP', Wetland.SWSP= 'Habitat.TypeWetland-SWSP')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.SWSP= (SWMP.SWSP-Wetland.SWSP))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.SOSP= '(Intercept)-SOSP', Wetland.SOSP= 'Habitat.TypeWetland-SOSP')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.SOSP= (SWMP.SOSP-Wetland.SOSP))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.EAKI= '(Intercept)-EAKI', Wetland.EAKI= 'Habitat.TypeWetland-EAKI')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.EAKI= (SWMP.EAKI-Wetland.EAKI))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.WIFL= '(Intercept)-WIFL', Wetland.WIFL= 'Habitat.TypeWetland-WIFL')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.WIFL= (SWMP.WIFL-Wetland.WIFL))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.ALFL= '(Intercept)-ALFL', Wetland.ALFL= 'Habitat.TypeWetland-ALFL')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.ALFL= (SWMP.ALFL-Wetland.ALFL))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.COGR= '(Intercept)-COGR', Wetland.COGR= 'Habitat.TypeWetland-COGR')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.COGR= (SWMP.COGR-Wetland.COGR))

m5_beta_sp_output <- rename_variables(m5_beta_sp_output, SWMP.MAWR= '(Intercept)-MAWR', Wetland.MAWR= 'Habitat.TypeWetland-MAWR')
m5_beta_sp_output <-mutate_variables(m5_beta_sp_output, diff.MAWR= (SWMP.MAWR-Wetland.MAWR))


m5_summary_sp <- (summarise_draws(m5_beta_sp_output))
saveRDS(m5_summary_sp, file = "Outputs/m5_summary_sp.RDS")


## All Species Veg Index 
species.data <- list(RWBL, COYE, SWSP, ALFL, YEWA, SOSP, EAKI, MAWR, WIFL, GRHE, SORA, VIRA, COGA, LEBI, AMBI, AMWO, BEKI, GREG, BCNH, COGR, WISN, PBGR)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "ALFL"
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
names(species.data)[19] <- "BCNH"
names(species.data)[20] <- "COGR"
names(species.data)[21] <- "WISN"
names(species.data)[22] <- "PBGR"

All.wet.species <- c("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "BCNH", "COGR", "WISN", "PBGR")
s <- list("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE", "COGA", "LEBI", "SORA", "VIRA", "AMBI", "AMWO", "BEKI", "GREG", "BCNH", "COGR", "WISN", "PBGR")
Site <- list(1:61)
Visit <- list("Visit 1", "Visit 2","Visit 3", "Visit 4", "Visit 5")

tmp <- array(NA,dim = c(22,61,5))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (sn in 1:length(All.wet.species)){ 
  i = All.wet.species[sn]
  tmp[sn,,] <- as.matrix(species.data[[i]])
  print(i)
}

load("Diversity Index Covariates")
m7_data <- list(tmp, covariates_sd, Cov.gen$coords, det.covs.combined)

names(m7_data)
names(m7_data)[1] <- "y"
names(m7_data)[2] <- "occ.covs"
names(m7_data)[3] <- "coords"
names(m7_data)[4] <- "det.covs"

##Multi Species Model all
all.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(D.1) + scale(Area) + (Habitat.Type),
                        det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                        data = m7_data, 
                        n.batch = 1000, 
                        batch.length = 25,
                        n.thin = 10, 
                        n.burn = 5000, 
                        n.chains = 4,
                        NNGP = TRUE,
                        n.factors = 4,
                        n.neighbors = 5,
                        n.omp.threads = 1,
                        cov.model = 'exponential',
                        n.report = 10)
summary(all.out, level = 'community')
summary(all.out, level = 'species')

# Occupancy community-level effects 
MCMCplot(all.out$beta.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Detection covariate effects --------- 
MCMCplot(all.out$alpha.comm.samples, ref_ovl = TRUE, ci = c(50, 95))
# Occupancy species-specific effects
MCMCplot(all.out$beta.samples, ref_ovl = TRUE, ci = c(50, 95))

##Community Level difference
m7_beta_output <-as_draws_df(all.out$beta.comm.samples)
m7_beta_output <- rename_variables(m7_beta_output, SWMP = '(Intercept)', Wetland = Habitat.TypeWetland)
m7_beta_output <-mutate_variables(m7_beta_output, diff = (SWMP-Wetland))
m7_summary_com <- (summarise_draws(m7_beta_output))
saveRDS(m7_summary_com, file = "Outputs/m7_summary_com.RDS")

##Species Level Difference 
m7_beta_sp_output <-as_draws_df(all.out$beta.samples)

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.RWBL= '(Intercept)-RWBL', Wetland.RWBL= 'Habitat.TypeWetland-RWBL')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.RWBL= (SWMP.RWBL-Wetland.RWBL))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.YEWA= '(Intercept)-YEWA', Wetland.YEWA= 'Habitat.TypeWetland-YEWA')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.YEWA= (SWMP.YEWA-Wetland.YEWA))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.COYE= '(Intercept)-COYE', Wetland.COYE= 'Habitat.TypeWetland-COYE')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.COYE= (SWMP.COYE-Wetland.COYE))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.SWSP= '(Intercept)-SWSP', Wetland.SWSP= 'Habitat.TypeWetland-SWSP')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.SWSP= (SWMP.SWSP-Wetland.SWSP))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.SOSP= '(Intercept)-SOSP', Wetland.SOSP= 'Habitat.TypeWetland-SOSP')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.SOSP= (SWMP.SOSP-Wetland.SOSP))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.EAKI= '(Intercept)-EAKI', Wetland.EAKI= 'Habitat.TypeWetland-EAKI')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.EAKI= (SWMP.EAKI-Wetland.EAKI))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.WIFL= '(Intercept)-WIFL', Wetland.WIFL= 'Habitat.TypeWetland-WIFL')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.WIFL= (SWMP.WIFL-Wetland.WIFL))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.ALFL= '(Intercept)-ALFL', Wetland.ALFL= 'Habitat.TypeWetland-ALFL')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.ALFL= (SWMP.ALFL-Wetland.ALFL))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.GRHE= '(Intercept)-GRHE', Wetland.GRHE= 'Habitat.TypeWetland-GRHE')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.GRHE= (SWMP.GRHE-Wetland.GRHE))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.MAWR= '(Intercept)-MAWR', Wetland.MAWR= 'Habitat.TypeWetland-MAWR')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.MAWR= (SWMP.MAWR-Wetland.MAWR))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.COGA= '(Intercept)-COGA', Wetland.COGA= 'Habitat.TypeWetland-COGA')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.COGA= (SWMP.COGA-Wetland.COGA))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.LEBI= '(Intercept)-LEBI', Wetland.LEBI= 'Habitat.TypeWetland-LEBI')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.LEBI= (SWMP.LEBI-Wetland.LEBI))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.AMBI= '(Intercept)-AMBI', Wetland.AMBI= 'Habitat.TypeWetland-AMBI')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.AMBI= (SWMP.AMBI-Wetland.AMBI))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.SORA= '(Intercept)-SORA', Wetland.SORA= 'Habitat.TypeWetland-SORA')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.SORA= (SWMP.SORA-Wetland.SORA))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.VIRA= '(Intercept)-VIRA', Wetland.VIRA= 'Habitat.TypeWetland-VIRA')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.VIRA= (SWMP.VIRA-Wetland.VIRA))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.PBGR= '(Intercept)-PBGR', Wetland.PBGR= 'Habitat.TypeWetland-PBGR')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.PBGR= (SWMP.PBGR-Wetland.PBGR))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.WISN= '(Intercept)-WISN', Wetland.WISN= 'Habitat.TypeWetland-WISN')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.WISN= (SWMP.WISN-Wetland.WISN))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.BEKI= '(Intercept)-BEKI', Wetland.BEKI= 'Habitat.TypeWetland-BEKI')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.BEKI= (SWMP.BEKI-Wetland.BEKI))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.AMWO= '(Intercept)-AMWO', Wetland.AMWO= 'Habitat.TypeWetland-AMWO')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.AMWO= (SWMP.AMWO-Wetland.AMWO))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.GREG= '(Intercept)-GREG', Wetland.GREG= 'Habitat.TypeWetland-GREG')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.GREG= (SWMP.GREG-Wetland.GREG))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.COGR= '(Intercept)-COGR', Wetland.COGR= 'Habitat.TypeWetland-COGR')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.COGR= (SWMP.COGR-Wetland.COGR))

m7_beta_sp_output <- rename_variables(m7_beta_sp_output, SWMP.BCNH= '(Intercept)-BCNH', Wetland.BCNH= 'Habitat.TypeWetland-BCNH')
m7_beta_sp_output <-mutate_variables(m7_beta_sp_output, diff.BCNH= (SWMP.BCNH-Wetland.BCNH))


m7_summary_sp <- (summarise_draws(m7_beta_sp_output))
saveRDS(m7_summary_sp, file = "Outputs/m7_summary_sp.RDS")


## Impervious Index Model

species.data <- list(RWBL, COYE, SWSP, ALFL, YEWA, SOSP, EAKI, MAWR, WIFL, GRHE)
names(species.data)[1] <- "RWBL"
names(species.data)[2] <- "COYE"
names(species.data)[3] <- "SWSP"
names(species.data)[4] <- "ALFL"
names(species.data)[5] <- "YEWA"
names(species.data)[6] <- "SOSP"
names(species.data)[7] <- "EAKI"
names(species.data)[8] <- "MAWR"
names(species.data)[9] <- "WIFL"
names(species.data)[10] <- "GRHE"

common.species <- c("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE")
s <- list("RWBL", "COYE", "SWSP", "ALFL", "YEWA", "SOSP", "EAKI", "MAWR", "WIFL", "GRHE")
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

load("Impervious Covariates")
m6_data <- list(tmp, covariates_imp, Cov.gen$coords, det.covs.combined)

names(m6_data)
names(m6_data)[1] <- "y"
names(m6_data)[2] <- "occ.covs"
names(m6_data)[3] <- "coords"
names(m6_data)[4] <- "det.covs"


##Multi Species Model common
common.out <- sfMsPGOcc(occ.formula = ~ scale(Landscape) + scale(Impervious) + scale(Area) + (Habitat.Type),
                        det.formula = ~ scale(date + I(scale(date^2))) + noise + method, 
                        data = m6_data, 
                        n.batch = 1000, 
                        batch.length = 25,
                        n.thin = 10, 
                        n.burn = 5000, 
                        n.chains = 4,
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
m6_summary <- (summarise_draws(common.out$beta.samples))
saveRDS(m6_summary, file = "Outputs/m6_summary")

