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
library(lubridate)

##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)
n_distinct(data$Species.Code)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Survey Details.csv")
head(survey_details)

##add landscape, area, and veg data

covariates <- subset(selection_data, select = c(Area, Landscape, Veg, Habitat.Type))

##create dummy variable

covariates$Habitat.Type <- ifelse(covariates$Habitat.Type=="SWMP", 1, 0)
covariates$Habitat.Type <- ifelse(covariates$Habitat.Type=="1", 2, 1)


##Extract Coords
Coords <- subset(selection_data, select = c(Study.Number, Lat, Long))

coord_mat <- Coords %>% 
  select(Lat,Long) %>% 
  as.matrix()
##Extract Date by Visit

Date <- subset(survey_details, select = c(Study.ID, Visit.Number, Date))
Date.1 <-subset(Date, Visit.Number=="1")
Date.2 <-subset(Date, Visit.Number=="2")
Date <- merge(Date.1, Date.2, by=c('Study.ID'), all=TRUE)
Date.x <- lubridate::dmy(Date$Date.x)
Date.y <- lubridate::dmy(Date$Date.y)
Date.y <- as.integer(gsub("-", "", Date.y))
Date.x <- as.integer(gsub("-", "", Date.x))
Date <- data.frame(Date.x, Date.y)

Date_mat <- Date %>% 
  select(Date.x,Date.y) %>% 
  as.matrix()

## Extract Noise Code
Noise <- subset(survey_details, select = c(Study.ID, Visit.Number, Noise))
Noise.1 <-subset(Noise, Visit.Number=="1")
Noise.2 <-subset(Noise, Visit.Number=="2")
Noise <- merge(Noise.1, Noise.2, by=c('Study.ID'), all=TRUE)
Noise<-subset(Noise, select = c(Study.ID, Noise.x, Noise.y))
Noise_mat <- Noise %>% 
  select(Noise.x,Noise.y) %>% 
  as.matrix()


##Extract Wind
Wind <- subset(survey_details, select = c(Study.ID, Visit.Number, Wind))
Wind.1 <-subset(Wind, Visit.Number=="1")
Wind.2 <-subset(Wind, Visit.Number=="2")
Wind <- merge(Wind.1, Wind.2, by=c('Study.ID'), all=TRUE)
Wind <-subset(Wind, select = c(Study.ID, Wind.x, Wind.y))
Wind_mat <- Wind %>% 
  select(Wind.x, Wind.y) %>% 
  as.matrix()


det.covs <- list(Date_mat, Wind_mat, Noise_mat)
names(det.covs)
names(det.covs)[1] <- "date"
names(det.covs)[2] <- "wind"
names(det.covs)[3] <- "noise"

##Format df as list
Cov.gen <-list(covariates,coord_mat, det.covs)
Cov.gen

names(Cov.gen)
names(Cov.gen)[1] <- "occ.covs"
names(Cov.gen)[2] <- "coords"
names(Cov.gen)[3] <- "det.covs"

save(Cov.gen, file = "General Covariates.RData")
