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
load("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/GitHub/MMP-Data/Species.Detection.SORA.RData")
SORA <- y
load("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/GitHub/MMP-Data/Species.Detection.COGA.RData")
COGA <- y
load("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/GitHub/MMP-Data/Species.Detection.VIRA.RData")
VIRA <- y
load("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/GitHub/MMP-Data/Species.Detection.LEBI.RData")
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

tmp <- array(NA,dim = c(4,61,2))
dimnames(tmp)[[1]] <- s
dimnames(tmp)[[3]] <- Visit

for (i in focal.species){ 
  tmp[s,,] <- as.matrix(species.data[["i"]])
  print(i)}


