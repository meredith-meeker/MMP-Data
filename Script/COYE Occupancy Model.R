##Load all the packages

library(tidyverse)
library(ggplot2)
library(devtools)
library(rethinking)
library(dagitty)
library(ggdag)
library(dplyr)

install.packages("spOccupancy")
install.packages("MCMCvis")
install.packages(c("sf", "stars", "pals", "cowplot"))
install.packages("plyr")

library(spOccupancy)
library(MCMCvis)
library(stars)
library(pals)
library(cowplot)
library(tibble)
library(plyr)


##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)
n_distinct(data$Species.Code)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)


##Create Detection dataset for COYE 
d_COYE <- data %>%
  distinct_Study.ID
  
head(d_COYE)
d_COYE_all <- subset(data, Species.Code=="COYE")

d_COYE <- subset(d_COYE_all, select = -c(Route, Habitat.Unit))
d_COYE <- subset(d_COYE, select = c(Study.ID, Visit.Number, Species.Code))


first_column <- data.frame(unique(data$Study.ID))
first_column$Study.ID <- first_column$unique.data.Study.ID.
first_column <- subset(first_column, select = c(Study.ID))

First_Visit <- subset(data, Visit.Number == "1")
First_Visit <- subset(First_Visit, Species.Code == "COYE")
First_Visit <- subset(First_Visit, select = c(Study.ID, Species.Code))

Second_Visit <-  subset(data, Visit.Number == "2")
Second_Visit <- subset(Second_Visit, Species.Code == "COYE")
Second_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))


d_COYE <- merge(first_column, First_Visit, by=c('Study.ID'), all=TRUE)
d_COYE <- merge(d_COYE, Second_Visit, by=c('Study.ID'), all=TRUE)

##create dummy variable

d_COYE$Species.Code.x <- ifelse(d_COYE$Species.Code.x=="COYE", 1, 0)
d_COYE$Species.Code.y <- ifelse(d_COYE$Species.Code.y=="COYE", 1, 0)
d_COYE[is.na(d_COYE)] = 0

##Rename Columns
colnames(d_COYE)
colnames(d_COYE)[2] <- "Visit 1"
colnames(d_COYE)[3] <- "Visit 2"

##add landscape, area, and veg data

covariates <- subset(selection_data, select = c(Study.Number, Area, Landscape, Veg))
colnames(covariates)[1] <- "Study.ID"

d_COYE <- merge(d_COYE, covariates, by=c('Study.ID'), all=TRUE)

str(d_COYE)

# Model fitting --------------------------------------------------------
# Fit a non-spatial, single-species occupancy model
occ_COYE <- PGOcc(occ.formula = ~ scale(selection_data$Landscape) + scale(selection_data$Veg), 
             det.formula = ~ scale(date) + I(scale(date^2)) + scale(dur), 
             data = data.goldfinch, 
             n.samples = 5000, 
             n.thin = 4, 
             n.burn = 3000, 
             n.chains = 3,
             n.report = 500)
summary(out)