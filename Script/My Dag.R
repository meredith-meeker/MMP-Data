##Load Packages

library(tidyverse)
library(ggplot2)
library(devtools)
library(rethinking)
library(dagitty)
library(ggdag)
library(dplyr)

## Create my first dag 

dag1 <- dagitty("dag{HA -> SR; HT -> SR; HT -> VH -> SR; LC -> SR; LC -> HA -> SR}")
coordinates(dag1) <- list(x=c(G=0,P=1,H=2), y=c(G=0, P=1, H=0))
plot(dag1)

## Create my secondary dag 

dag2 <- dagitty("dag{HA -> SR; HT -> SR; HT -> VH -> SR; LC -> SR; LC -> HA -> SR; LC -> VH -> SR}")
coordinates(dag2) <- list(x=c(G=0,P=1,H=2), y=c(G=0, P=1, H=0))
plot(dag2)

