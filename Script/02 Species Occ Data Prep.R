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

##Create species list

species.list <- list(unique(data$Species.Code))
species.list

##Create Detection dataset for Species loop

sp <- "LEBI"

d_sp <- data 

head(d_sp)
d_sp_all <- subset(data, Species.Code==sp)

d_sp <- subset(d_sp_all, select = -c(Route, Habitat.Unit))
d_sp <- subset(d_sp, select = c(Study.ID, Visit.Number, Species.Code))


first_column <- data.frame(unique(data$Study.ID))
first_column$Study.ID <- first_column$unique.data.Study.ID.
first_column <- subset(first_column, select = c(Study.ID))

First_Visit <- subset(data, Visit.Number == 1)
First_Visit <- subset(First_Visit, Species.Code ==sp)
First_Visit <- subset(First_Visit, select = c(Study.ID, Species.Code))

Second_Visit <-  subset(data, Visit.Number == 2)
Second_Visit <- subset(Second_Visit, Species.Code ==sp)
Second_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))


d_sp <- merge(first_column, First_Visit, by=c('Study.ID'), all=TRUE)
d_sp <- merge(d_sp, Second_Visit, by=c('Study.ID'), all=TRUE)

##create dummy variable

d_sp$Species.Code.x <- ifelse(d_sp$Species.Code.x==sp, 1, 0)
d_sp$Species.Code.y <- ifelse(d_sp$Species.Code.y==sp, 1, 0)
d_sp[is.na(d_sp)] = 0

##Rename Columns
colnames(d_sp)[1] <- "Site"
colnames(d_sp)[2] <- "Visit 1"
colnames(d_sp)[3] <- "Visit 2"

Visit_1 <- as.integer(d_sp$`Visit 1`)
Visit_2 <- as.integer(d_sp$`Visit 2`)
y <- data.frame(Visit_1, Visit_2)
y <- y %>% 
  select(Visit_1, Visit_2) %>% 
  as.matrix()


save(y,file = "Species.Detection.LEBI.RData")

