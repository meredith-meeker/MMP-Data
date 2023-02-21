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
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined Species Detections .csv")
head(data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined survey details.csv")

##Create species list

species.list <- list(unique(data$Species.Code))
species.list

##Create Detection dataset for Species loop

sp <- "WISN"

d_sp <- data 

head(d_sp)
d_sp_all <- subset(data, Species.Code==sp)

d_sp <- subset(d_sp_all, select = -c(Method))



first_column <- data.frame(unique(data$Study.ID))
first_column$Study.ID <- first_column$unique.data.Study.ID.
first_column <- subset(first_column, select = c(Study.ID))

First_Visit <- subset(data, Visit.Number == 1)
First_Visit <- subset(First_Visit, Species.Code ==sp)
First_Visit <- subset(First_Visit, select = c(Study.ID, Species.Code))

Second_Visit <-  subset(data, Visit.Number == 2)
Second_Visit <- subset(Second_Visit, Species.Code ==sp)
Second_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))

Third_Visit <- subset(data, Visit.Number == 3)
Third_Visit <- subset(Second_Visit, Species.Code ==sp)
Third_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))

Fourth_Visit <- subset(data, Visit.Number == 4)
Fourth_Visit <- subset(Second_Visit, Species.Code ==sp)
Fourth_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))

Fifth_Visit <- subset(data, Visit.Number == 5)
Fifth_Visit <- subset(Second_Visit, Species.Code ==sp)
Fifth_Visit <- subset(Second_Visit, select = c(Study.ID, Species.Code))



d_sp <- merge(first_column, First_Visit, by=c('Study.ID'), all=TRUE)
colnames(d_sp)[2] <- "Visit 1"
d_sp <- merge(d_sp, Second_Visit, by=c('Study.ID'), all=TRUE)
colnames(d_sp)[3] <- "Visit 2"
d_sp <- merge(d_sp, Third_Visit, by=c('Study.ID'), all=TRUE)
colnames(d_sp)[4] <- "Visit 3"
d_sp <- merge(d_sp, Fourth_Visit, by=c('Study.ID'), all=TRUE)
colnames(d_sp)[5] <- "Visit 4"
d_sp <- merge(d_sp, Fifth_Visit, by=c('Study.ID'), all=TRUE)
colnames(d_sp)[6] <- "Visit 5"

##create dummy variable

d_sp$`Visit 1` <- ifelse(d_sp$`Visit 1`==sp, 1, 0)
d_sp$`Visit 2` <- ifelse(d_sp$`Visit 2`==sp, 1, 0)
d_sp$`Visit 3` <- ifelse(d_sp$`Visit 3`==sp, 1, 0)
d_sp$`Visit 4` <- ifelse(d_sp$`Visit 4`==sp, 1, 0)
d_sp$`Visit 5` <- ifelse(d_sp$`Visit 5`==sp, 1, 0)
d_sp[is.na(d_sp)] = 0

##Rename Columns
colnames(d_sp)[1] <- "Site"


Visit_1 <- as.integer(d_sp$`Visit 1`)
Visit_2 <- as.integer(d_sp$`Visit 2`)
Visit_3 <- as.integer(d_sp$`Visit 3`)
Visit_4 <- as.integer(d_sp$`Visit 4`)
Visit_5 <- as.integer(d_sp$`Visit 5`)

y <- data.frame(Visit_1, Visit_2, Visit_3, Visit_4, Visit_5)
y <- y %>% 
  select(Visit_1, Visit_2, Visit_3, Visit_4, Visit_5) %>% 
  as.matrix()


save(y,file = "Outputs/Species.Detection.WISN.RData")
