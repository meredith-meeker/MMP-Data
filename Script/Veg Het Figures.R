## Load Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(dplyr)
library(stringr)

#Load Data
readRDS("Outputs/m5_summary_com.RDS")
readRDS("Outputs/m5_summary_sp.RDS")
readRDS("Outputs/m7_summary_com.RDS")
readRDS("Outputs/m7_summary_sp.RDS")
readRDS("Outputs/m8_summary_com.RDS")
readRDS("Outputs/m8_summary_sp.RDS")

##change colour scheme

cbPalette <- c(  "#56B4E9", "#000000",  "#D55E00")

##set point offset

dodge <- position_dodge(width=0.25) 
## Subset Data

diff_sp_1 <- tail(m8_summary_sp, 6)
diff_sp_1 <- as.data.frame(diff_sp_1)
##Plot
focal_diff <- ggplot(data = diff_sp_1) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(focal_diff)


## Subset Data
diff_sp_2 <- tail(m5_summary_sp, 10)
diff_sp_2 <- as.data.frame(diff_sp_2)
##Plot
common_diff <- ggplot(data = diff_sp_2) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(common_diff)

## Subset Data

diff_sp_4 <- tail(m7_summary_sp, 22)
diff_sp_4 <- as.data.frame(diff_sp_4)

##Plot
all_diff <- ggplot(data = diff_sp_4) + 
  geom_pointrange(aes(y= reorder(variable, -mean), x=mean, xmin=q5, xmax=q95))
plot(all_diff)

##merge into one plot

diff_sp_1$model <- "focal"
diff_sp_2$model <- "common"
diff_sp_4$model <- "all"
diff_all <- rbind(diff_sp_1, diff_sp_2, diff_sp_4)
##change species names

comb_Diff_All <- diff_all %>%
  mutate(species_name = str_split_fixed(variable, "f", n = 2)[,2],
         species_name = str_split_fixed(species_name, "/.", n = 2)[,2])

comb_diff <- ggplot(data = comb_Diff_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95
                      , group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette) +
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()

plot(comb_diff)

##Landscape Variable 
##Community plot models 5, 7, 8
Land_com8 <- m8_summary_com[2,]
Land_com5 <- m5_summary_com[2,]
Land_com7 <- m7_summary_com[2,]
Land_com <- bind_rows(Land_com8, Land_com5, Land_com7)
Com <- c("Focal", "Common", "All")
Land_com$Com <- Com

Com_Land <- ggplot(data = Land_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Land)
##Species Model
Land_sp1 <- m8_summary_sp[7:12,]
Land_sp2 <- m5_summary_sp[11:20,]
Land_sp4 <- m7_summary_sp[23:44,]

Land_sp1$model <- "focal"
Land_sp2$model <- "common"
Land_sp4$model <- "all"

land_all <- rbind(Land_sp1, Land_sp2, Land_sp4)
##change species names

comb_Land_All <- land_all %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])

comb_land <- ggplot(data = comb_Land_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)

plot(comb_land)


##Vegetation Heterogeneity 
##Community plot models 5, 7, 8
Veg_com5 <- m5_summary_com[3,]
Veg_com7 <- m7_summary_com[3,]
Veg_com8 <- m8_summary_com[3,]
Veg_com_het <- bind_rows(Veg_com5, Veg_com7, Veg_com8)
Com <- c("Focal", "Common", "All")
Veg_com_het$Com <- Com

Com_Veg_het <- ggplot(data = Veg_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Veg)

##Species Model
Veg_sp5 <- m8_summary_sp[13:18,]
Veg_sp7 <- m5_summary_sp[21:30,]
Veg_sp8 <- m7_summary_sp[45:66,]

Veg_sp5$model <- "focal"
Veg_sp7$model <- "common"
Veg_sp8$model <- "all"

Veg_all_het <- rbind(Veg_sp5, Veg_sp7, Veg_sp8)
##change species names

comb_Veg_All_het <- Veg_all_het %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])

comb_Veg_het <- ggplot(data = comb_Veg_All_het) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3)  +
  scale_colour_manual(values = cbPalette)

plot(comb_Veg_het)

##Area 
Area_com8 <- m8_summary_com[4,]
Area_com5 <- m5_summary_com[4,]
Area_com7 <- m7_summary_com[4,]
Area_com <- bind_rows(Area_com8, Area_com5, Area_com7)
Com <- c("Focal", "Common", "All")
Area_com$Com <- Com

Com_Area <- ggplot(data = Area_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Area)

##Species Model
Area_sp1 <- m8_summary_sp[19:24,]
Area_sp2 <- m5_summary_sp[31:40,]
Area_sp4 <- m7_summary_sp[67:88,]

Area_sp1$model <- "focal"
Area_sp2$model <- "common"
Area_sp4$model <- "all"

Area_all <- rbind(Area_sp1, Area_sp2, Area_sp4)
##change species names

comb_Area_All <- Area_all %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])


comb_Area <- ggplot(data = comb_Area_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3)  +
  scale_colour_manual(values = cbPalette) +
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")

plot(comb_Area)

##Plot Land, Veg, Area together

comb_Land_All$type <- "Landscape"
comb_Area_All$type <- "Area"
comb_Veg_All$type <- "Vegetation Heterogeneity"
joined_data <- rbind(comb_Land_All, comb_Area_All, comb_Veg_All)

joined_plot <- ggplot(data = joined_data) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95, 
                      group = model, col=model), position=dodge)+
  facet_wrap(~type) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)+
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()

plot(joined_plot)
