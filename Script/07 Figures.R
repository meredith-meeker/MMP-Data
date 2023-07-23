## Load Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(patchwork)
library(dplyr)
library(stringr)

#Load Data
readRDS("Outputs/m1_summary_com.RDS")
readRDS("Outputs/m1_summary_sp.RDS")
readRDS("Outputs/m2_summary_com.RDS")
readRDS("Outputs/m2_summary_sp.RDS")
readRDS("Outputs/m4_summary_com.RDS")
readRDS("Outputs/m4_summary_sp.RDS")
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
##Pond Type Difference
##Community plot models 1, 2, 4
Diff_com1 <- m1_summary_com[6,]
Diff_com2 <- m2_summary_com[6,]
Diff_com4 <- m4_summary_com[6,]
Diff_com <- bind_rows(Diff_com1, Diff_com2, Diff_com4)
Com <- c("Focal", "Common", "All")
Diff_com$Com <- Com

Com_Diff <- ggplot(data = Diff_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95, col=Com)) + 
  scale_colour_manual(values = cbPalette)+
  labs(y= "Model Guild", x = "Posterior Estimate of the Mean Difference between the Occupancy of SWMP - Occupancy of Natural Wetlands")+
  theme_bw()

plot(Com_Diff)

##Emergent Vegetation
## Subset Data

diff_sp_1 <- tail(m1_summary_sp, 6)
diff_sp_1 <- as.data.frame(diff_sp_1)
##Plot
focal_diff <- ggplot(data = diff_sp_1) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(focal_diff)


## Subset Data
diff_sp_2 <- tail(m2_summary_sp, 10)
diff_sp_2 <- as.data.frame(diff_sp_2)
##Plot
common_diff <- ggplot(data = diff_sp_2) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(common_diff)

## Subset Data

diff_sp_4 <- tail(m4_summary_sp, 22)
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
  mutate(species_name = str_split_fixed(variable, "\\.", n = 2)[,2],
         species_name = str_split_(species_name, "\\.", n = 2)[,2])

comb_diff <- ggplot(data = comb_Diff_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95
                      , group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette) +
  labs(y= "Bird Species code", x = "Posterior Estimate of the Mean Difference between the Occupancy of SWMP - Occupancy of Natural Wetlands")+
  theme_bw()

plot(comb_diff)

##Landscape Variable 
##Community plot models 1, 2, 4
Land_com1 <- m1_summary_com[2,]
Land_com2 <- m2_summary_com[2,]
Land_com4 <- m4_summary_com[2,]
Land_com <- bind_rows(Land_com1, Land_com2, Land_com4)
Com <- c("Focal", "Common", "All")
Land_com$Com <- Com

Com_Land <- ggplot(data = Land_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Land)
##Species Model
Land_sp1 <- m1_summary_sp[7:12,]
Land_sp2 <- m2_summary_sp[11:20,]
Land_sp4 <- m4_summary_sp[23:44,]

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
##Community plot models 1, 2, 4
Veg_com1 <- m1_summary_com[3,]
Veg_com2 <- m2_summary_com[3,]
Veg_com4 <- m4_summary_com[3,]
Veg_com <- bind_rows(Veg_com1, Veg_com2, Veg_com4)
Com <- c("Focal", "Common", "All")
Veg_com$Com <- Com

Com_Veg <- ggplot(data = Veg_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Veg)

##Species Model
Veg_sp1 <- m1_summary_sp[13:18,]
Veg_sp2 <- m2_summary_sp[21:30,]
Veg_sp4 <- m4_summary_sp[45:66,]

Veg_sp1$model <- "focal"
Veg_sp2$model <- "common"
Veg_sp4$model <- "all"

Veg_all <- rbind(Veg_sp1, Veg_sp2, Veg_sp4)
##change species names

comb_Veg_All <- Veg_all %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])

comb_Veg <- ggplot(data = comb_Veg_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3)  +
  scale_colour_manual(values = cbPalette)

plot(comb_Veg)

##Vegetation Heterogeneity 
##Community plot models 5, 7, 8
Veg_com5 <- m5_summary_com[3,]
Veg_com7 <- m7_summary_com[3,]
Veg_com8 <- m8_summary_com[3,]
Veg_com_het <- bind_rows(Veg_com5, Veg_com7, Veg_com8)
Com <- c("Focal", "Common", "All")
Veg_com_het$Com <- Com

Com_Veg_het <- ggplot(data = Veg_com_het) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Veg_het)

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
Area_com1 <- m1_summary_com[4,]
Area_com2 <- m2_summary_com[4,]
Area_com4 <- m4_summary_com[4,]
Area_com <- bind_rows(Area_com1, Area_com2, Area_com4)
Com <- c("Focal", "Common", "All")
Area_com$Com <- Com

Com_Area <- ggplot(data = Area_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Area)

##Species Model
Area_sp1 <- m1_summary_sp[19:24,]
Area_sp2 <- m2_summary_sp[31:40,]
Area_sp4 <- m4_summary_sp[67:88,]

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

comb_Land_All$type <- "Landscape Composition"
comb_Area_All$type <- "Area"
comb_Veg_All$type <- "Emergent Vegetation"
comb_Veg_All_het$type <- "Vegetation Heterogeneity**"
joined_data <- rbind(comb_Land_All, comb_Area_All, comb_Veg_All, comb_Veg_All_het)

joined_plot <- ggplot(data = joined_data) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95, 
                      group = model, col=model), position=dodge)+
                    facet_grid(~factor(type, levels=c('Landscape Composition', 'Vegetation Heterogeneity**', 'Emergent Vegetation', 'Area'))) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)+
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()

plot(joined_plot)
##Patchwork
comb_Area+comb_land+comb_Veg+plot_layout(guides = "collect")

##Plot Hyper parameters together
Land_com$type <- "Landscape Composition"
Area_com$type <- "Area"
Veg_com$type <- "Emergent Vegetation"
Veg_com_het$type <- "Vegetation Heterogeneity**"
joined_data_com <- rbind(Land_com, Area_com, Veg_com, Veg_com_het)

joined_plot_com <- ggplot(data = joined_data_com) + 
  geom_pointrange(aes(y= reorder(Com, -mean), x=mean, xmin=q5, xmax=q95, 
 col= Com), position=dodge)+
  facet_grid(~factor(type, levels=c('Landscape Composition', 'Vegetation Heterogeneity**', 'Emergent Vegetation', 'Area'))) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)+
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()

plot(joined_plot_com)

##Detection Variables 
## Subset Data

diff_sp_1 <- tail(m1_summary_sp_det, 6)
diff_sp_1 <- as.data.frame(diff_sp_1)

##Plot
focal_diff <- ggplot(data = diff_sp_1) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(focal_diff)


## Subset Data
diff_sp_2 <- tail(m2_summary_sp_det, 10)
diff_sp_2 <- as.data.frame(diff_sp_2)
##Plot
common_diff <- ggplot(data = diff_sp_2) + 
  geom_pointrange(aes(y=variable, x=mean, xmin=q5, xmax=q95))
plot(common_diff)

## Subset Data

diff_sp_4 <- tail(m4_summary_sp_det, 22)
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
  mutate(species_name = str_split_fixed(variable, "\\.", n = 2)[,2],
         species_name = str_split_fixed(species_name, "\\.", n = 2)[,2])

comb_diff <- ggplot(data = comb_Diff_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95
                      , group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette) +
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()

plot(comb_diff)

##Noise Variable 
##Community plot models 1, 2, 4
Noise_com1 <- m1_summary_com_det[2,]
Noise_com2 <- m2_summary_com_det[2,]
Noise_com4 <- m4_summary_com_det[2,]
Noise_com <- bind_rows(Noise_com1, Noise_com2, Noise_com4)
Com <- c("Focal", "Common", "All")
Noise_com$Com <- Com

Com_Noise <- ggplot(data = Noise_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Noise)

##Species Model
Noise_sp1 <- m1_summary_sp_det[13:18,]
Noise_sp2 <- m2_summary_sp_det[21:30,]
Noise_sp4 <- m4_summary_sp_det[45:66,]

Noise_sp1$model <- "focal"
Noise_sp2$model <- "common"
Noise_sp4$model <- "all"

Noise_all <- rbind(Noise_sp1, Noise_sp2, Noise_sp4)
##change species names

comb_Noise_All <- Noise_all %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])

comb_Noise <- ggplot(data = comb_Noise_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)

plot(comb_Noise)

##Date Variable 
##Community plot models 1, 2, 4
Date_com1 <- m1_summary_com_det[2,]
Date_com2 <- m2_summary_com_det[2,]
Date_com4 <- m4_summary_com_det[2,]
Date_com <- bind_rows(Date_com1, Date_com2, Date_com4)
Com <- c("Focal", "Common", "All")
Date_com$Com <- Com

Com_Date <- ggplot(data = Date_com) +
  geom_pointrange(aes(reorder(Com, -mean), x=mean, xmin=q5, xmax=q95))
plot(Com_Date)

##Species Model
Date_sp1 <- m1_summary_sp_det[7:12,]
Date_sp2 <- m2_summary_sp_det[11:20,]
Date_sp4 <- m4_summary_sp_det[23:44,]

Date_sp1$model <- "focal"
Date_sp2$model <- "common"
Date_sp4$model <- "all"

Date_all <- rbind(Date_sp1, Date_sp2, Date_sp4)
##change species names

comb_Date_All <- Date_all %>%
  mutate(species_name = str_split_fixed(variable, "-", n = 2)[,2])

comb_Date <- ggplot(data = comb_Date_All) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95,
                      group = model, col=model), position=dodge) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)

plot(comb_Date)

##Plot Detection together

comb_Diff_All$type <- "Survey Method"
comb_Date_All$type <- "Date"
comb_Noise_All$type <- "Noise"
joined_data <- rbind(comb_Diff_All, comb_Date_All, comb_Noise_All)

joined_plot <- ggplot(data = joined_data) + 
  geom_pointrange(aes(y= reorder(species_name, -mean), x=mean, xmin=q5, xmax=q95, 
                      group = model, col=model), position=dodge)+
  facet_wrap(~type) +
  geom_vline(xintercept=0,linetype=3) +
  scale_colour_manual(values = cbPalette)+
  labs(y= "Bird Species code", x = "Posterior Mean Estimate")+
  theme_bw()
plot(joined_plot)