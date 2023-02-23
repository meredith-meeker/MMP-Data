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
install.packages("DataEditR")
library(DataEditR)

##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined Species Detections .csv")
head(data)

survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined survey details.csv")

##Create species list

species.list <- list(unique(data$Species.Code))
species.list

##Create Detection dataset for Species loop

sp <- "YEWA"

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


save(y,file = "Outputs/Species.Detection.YEWA.RData")


## Combined detection covariates 

first_column <- data.frame(unique(data$Study.ID))
first_column$Study.ID <- first_column$unique.data.Study.ID.
first_column <- subset(first_column, select = c(Study.ID))
colnames(first_column)[1] <- "Site.ID"

Date <- subset(survey_details, select = c(Site.ID, Visit, Date))
Date.1 <-subset(Date, Visit=="1")
Date.2 <-subset(Date, Visit=="2")
Date.3 <-subset(Date, Visit=="3")
Date.4 <-subset(Date, Visit=="4")
Date.5 <-subset(Date, Visit=="5")

Date <- merge(first_column, Date.1, by=c ('Site.ID'), all=TRUE)
Date <- merge(Date.1, Date.2, by=c ('Site.ID'), all=TRUE)
colnames(Date)[2] <- "Visit 1"
colnames(Date)[3] <- "Date 1"
colnames(Date)[4] <- "Visit 2"
colnames(Date)[5] <- "Date 2"
Date <- merge(Date, Date.3, by=c('Site.ID'), all=TRUE)
colnames(Date)[6] <- "Visit 3"
colnames(Date)[7] <- "Date 3"
Date <- merge(Date, Date.4, by=c('Site.ID'), all=TRUE)
colnames(Date)[8] <- "Visit 4"
colnames(Date)[9] <- "Date 4"
Date <- merge(Date, Date.5, by=c('Site.ID'), all=TRUE)
colnames(Date)[10] <- "Visit 5"
colnames(Date)[11] <- "Date 5"

Date.1 <- lubridate::dmy(Date$`Date 1`)
Date.2 <- lubridate::dmy(Date$`Date 2`)
Date.3 <- lubridate::dmy(Date$`Date 3`)
Date.4 <- lubridate::dmy(Date$`Date 4`)
Date.5 <- lubridate::dmy(Date$`Date 5`)

Date.1 <- as.integer(gsub("-", "", Date.1))
Date.2 <- as.integer(gsub("-", "", Date.2))
Date.3 <- as.integer(gsub("-", "", Date.3))
Date.4 <- as.integer(gsub("-", "", Date.4))
Date.5 <- as.integer(gsub("-", "", Date.5))

Date <- data.frame(Date.1,Date.2, Date.3, Date.4, Date.5)
Date_mat <- Date %>% 
  select(Date.1,Date.2, Date.3, Date.4, Date.5) %>% 
  as.matrix()

##Extract Noise

Noise <- subset(survey_details, select = c(Site.ID, Visit, Noise))
Noise.1 <-subset(Noise, Visit=="1")
Noise.2 <-subset(Noise, Visit=="2")
Noise.3 <-subset(Noise, Visit=="3")
Noise.4 <-subset(Noise, Visit=="4")
Noise.5 <-subset(Noise, Visit=="5")

Noise <- merge(first_column, Noise.1, by=c ('Site.ID'), all=TRUE)
Noise <- merge(Noise.1, Noise.2, ('Site.ID'), all=TRUE)
colnames(Noise)[2] <- "Visit 1"
colnames(Noise)[3] <- "Noise 1"
colnames(Noise)[4] <- "Visit 2"
colnames(Noise)[5] <- "Noise 2"
Noise <- merge(Noise, Noise.3, by=c('Site.ID'), all=TRUE)
colnames(Noise)[6] <- "Visit 3"
colnames(Noise)[7] <- "Noise 3"
Noise <- merge(Noise, Noise.4, by=c('Site.ID'), all=TRUE)
colnames(Noise)[8] <- "Visit 4"
colnames(Noise)[9] <- "Noise 4"
Noise <- merge(Noise, Noise.5, by=c('Site.ID'), all=TRUE)
colnames(Noise)[10] <- "Visit 5"
colnames(Noise)[11] <- "Noise 5"
Noise <- data.frame(Noise)
Noise <- subset(Noise, select = c (Site.ID, Noise.1,Noise.2, Noise.3, Noise.4, Noise.5))

Noise_mat <- Noise %>% 
  select(Noise.1,Noise.2, Noise.3, Noise.4, Noise.5) %>% 
  as.matrix()

##Extract Survey Type

Method <- subset(survey_details, select = c(Site.ID, Visit, Method))
Method.1 <-subset(Method, Visit=="1")
Method.2 <-subset(Method, Visit=="2")
Method.3 <-subset(Method, Visit=="3")
Method.4 <-subset(Method, Visit=="4")
Method.5 <-subset(Method, Visit=="5")

Method <- merge(Method.1, Method.2, ('Site.ID'), all=TRUE)
colnames(Method)[2] <- "Visit 1"
colnames(Method)[3] <- "Method 1"
colnames(Method)[4] <- "Visit 2"
colnames(Method)[5] <- "Method 2"
Method <- merge(Method, Method.3, by=c('Site.ID'), all=TRUE)
colnames(Method)[6] <- "Visit 3"
colnames(Method)[7] <- "Method 3"
Method <- merge(Method, Method.4, by=c('Site.ID'), all=TRUE)
colnames(Method)[8] <- "Visit 4"
colnames(Method)[9] <- "Method 4"
Method <- merge(Method, Method.5, by=c('Site.ID'), all=TRUE)
colnames(Method)[10] <- "Visit 5"
colnames(Method)[11] <- "Method 5"
Method <- data.frame(Method)
Method <- subset(Method, select = c (Site.ID, Method.1,Method.2, Method.3, Method.4, Method.5))

Method_mat <- Method %>% 
  select(Method.1,Method.2, Method.3, Method.4, Method.5) %>% 
  as.matrix()

det.covs.combined <- list(Date_mat, Method_mat, Noise_mat)
names(det.covs.combined)
names(det.covs.combined)[1] <- "date"
names(det.covs.combined)[2] <- "method"
names(det.covs.combined)[3] <- "noise"

save(det.covs.combined, file = "Combined Covariates.RData")

## Fix NA Values
y <-as.data.frame(y)
y[14, 5] = NA
y[15, 5] = NA
y[18, 5] = NA
y[24, 5] = NA
y[27, 5] = NA
y[28, 5] = NA
y[31, 5] = NA
y[37, 5] = NA
y[38, 5] = NA
y[38, 4] = NA
y[38, 3] = NA
y[40, 5] = NA
y[42, 5] = NA
y[43, 5] = NA
y[44, 5] = NA
y[44, 4] = NA
y[44, 3] = NA
y[50, 5] = NA
y[57, 5] = NA
y[58, 5] = NA
y[61, 5] = NA
y <-as.matrix(y)

save(y,file = "Outputs/Species.Detection.YEWA.RData")
