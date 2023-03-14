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

##Combined Summary
com.data <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined Species Detections .csv")
head(com.data)

com.survey_details <- read.csv("C:/Users/mmeek/OneDrive/Documents/Master's Thesis/Fieldwork/Combined survey details.csv")

Com.Site_Count <-com.data %>%
  group_by(Study.ID) %>%
  summarize(distinct_Species.Code = n_distinct(Species.Code))
head(Com.Site_Count)

##create Habitat Type Column and Index
Com.Site_Count$Type <- (str_split_fixed(Com.Site_Count$Study.ID, "[:digit:]", n=2)[,1])


Com.Site_Count$Type <- ifelse(Com.Site_Count$Type=="W", 1, 0)
Com.Site_Count$Type <- ifelse(Com.Site_Count$Type=="1", 2, 1)

sp_sum <- com.data %>%
   group_by(Species.Code) %>%
  summarise(number_sites = length(unique(Study.ID)), # number of BBS routes where observed
           ) # number of years where observed

sp_means <- data %>% # including zero values
  group_by(Species.Code) %>%
  summarise(mean_count = mean(Number.of.Individuals)) %>% #mean counts of each species over the dataset
  inner_join(.,sp_sum,
             by = c("Species.Code")) %>%
  arrange(mean_count)

save(sp_sum, file = "Species Occurance.RData")

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
d$distinct_species <- d$distinct_Species.Code
d<- subset(d, select = -c(distinct_Species.Code))

d$SL<- standardize(d$Landscape)
d$SA<- standardize(d$Area)
d$SC<- d$distinct_species

Area_Landscape <- quap(
  alist(
    SC ~ dnorm( mu , sigma ) ,
    mu <- a + bSL*SL + bSA*SA,
    a ~ dnorm( 0 , 1 ) ,
    bSL ~ dnorm( 0 , 1 ),
    bSA ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ) , data = d )

ALprior <- extract.prior( Area_Landscape )
mu <- link( Area_Landscape, post=ALprior , data=list( SL=c(-2,2) ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2), xlab = "Standardized Landscape", ylab = "Species Count" )
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("black",0.4)) 


Area_Landscape_Type <- ulam(
  alist(
    SC ~ dnorm( mu , sigma ) ,
    mu <- a[Type] + bSL*SL + bSA*SA,
    a[Type] ~ dnorm( 0 , 1 ) ,
    bSL ~ dnorm( 0 , 1 ),
    bSA ~ dnorm( 0 , 1 ),
    sigma ~ dexp( 1 )
  ) , data = d )


precis(Area_Landscape_Type, depth = 2)
plot(precis(Area_Landscape_Type, depth = 2))

post <- extract.samples(Area_Landscape_Type)
diffHT <- post$a[,1] - post$a[,2]
dens(diffHT, show.HPDI = 0.95)
