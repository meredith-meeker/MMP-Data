##Load all the packages

library(tidyverse)
library(ggplot2)
library(devtools)
library(rethinking)
library(dagitty)
library(ggdag)
library(dplyr)

##Load and check the data
data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/MMP Data.csv")
head(data)
n_distinct(data$Habitat.Unit)

selection_data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Site Selection/Final Sites.csv")
head(selection_data)

##Count number of species per site
Site_Count <-data %>%
  group_by(Study.ID) %>%
  summarize(distinct_Species.Code = n_distinct(Species.Code))
head(Site_Count)

##create Habitat Type Column and Index
Site_Count$Type <- (str_split_fixed(Site_Count$Study.ID, "[:digit:]", n=2)[,1])


Site_Count$Type <- ifelse(Site_Count$Type=="W", 1, 0)
Site_Count$Type <- ifelse(Site_Count$Type=="1", 2, 1)

##Plot data

#Box Plot

Number_of_Species <- Site_Count$distinct_Species.Code
Habitat_Index <- Site_Count$Type


boxplot(Number_of_Species~Habitat_Index,
        data=Site_Count,
        main="Species Counts of SWMP and Wetlands",
        xlab="Habitat Type",
        ylab="Number of Species",
        col="blue",
        border="black")

#Linear Regression 
Site_Count$Veg <- selection_data$Veg
Site_Count$Landscape <- selection_data$Landscape
Site_Count$Area <- selection_data$Area

plot(x=Site_Count$Landscape, y=Site_Count$distinct_Species.Code)
plot(x=Site_Count$Area, y=Site_Count$distinct_Species.Code)
plot(x=Site_Count$Veg, y=Site_Count$distinct_Species.Code)

d <- Site_Count
d <- na.omit(d)


d$SL<- standardize(d$Landscape)
d$SA<- standardize(d$Area)
d$SC<- d$distinct_Species.Code

Veg_Area_Landscape <- quap(
  alist(
    SC ~ dnorm( mu , sigma ) ,
    mu <- a[Veg] + bSL*SL + bSA*SA,
    a[Veg] ~ dnorm( 0 , 1 ) ,
    bSL ~ dnorm( 0 , 1 ),
    bSA ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ) , data = d )

precis(HGboth, depth = 2)
plot(precis(HGboth, depth = 2))