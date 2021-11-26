

# 1. Load libraries -------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(adehabitatHR) # for RSFs
library(TMB)
library(glmmTMB) # RSFs according to Muff et al. (2019)
library(jtools) # for effect_plot() (section 3)

theme_nuwcru <- function(){
  theme_bw() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 14),
          axis.text.x = element_text(angle = -45, hjust = -0.05),
          axis.line.x = element_line(color = "black"),
          axis.line.y = element_line(color = "black"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 15, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 10),
          #legend.title = element_blank(),
          #legend.position = c(0.9, 0.9),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 4, linetype = "blank"))
}

setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/")

# 2. Import data and format -------

# bear data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# create squared conc column then scale
used_avail_RSF_freezeup_FINAL$CONC_2 = '^'(used_avail_RSF_freezeup_FINAL$CONC,2)
used_avail_RSF_freezeup_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_freezeup_FINAL$CONC_2, scale=TRUE, center=TRUE)

used_avail_RSF_breakup_FINAL$CONC_2 = '^'(used_avail_RSF_breakup_FINAL$CONC,2)
used_avail_RSF_breakup_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_breakup_FINAL$CONC_2, scale=TRUE, center=TRUE)

used_avail_RSF_winter_FINAL$CONC_2 = '^'(used_avail_RSF_winter_FINAL$CONC,2)
used_avail_RSF_winter_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_winter_FINAL$CONC_2, scale=TRUE, center=TRUE)


# seal data
breakup_RSF <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/DS_harpseals/data/breakup_RSF.csv")
freezeup_RSF <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/DS_harpseals/data/freezeup_RSF.csv")
      # add weight column
breakup_RSF$W <- ifelse(breakup_RSF$USE == "used", 1, 1000)
freezeup_RSF$W <- ifelse(freezeup_RSF$USE == "used", 1, 1000)



# 3. Run top RSF models -------

# freeze-up, break-up, and winter for bears
# freeze-up and break-up only for seal

###

# bears
      # break-up: Model 2a (CONC + CONC_2)
bears_breakup_m2a <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m2a
summary(bears_breakup_m2a)

      # freeze-up: Model 5a (BATH + CONC + CONC_2)
bears_freezeup_m5a <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5a
summary(bears_freezeup_m5a)

      # winter: Model 5a (BATH + CONC + CONC_2)
bears_winter_m5a <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m5a
summary(bears_winter_m5a)

###

# seals
      # freeze-up: Model 12 (BATH + CONC + DIST_WATER)
seals_freezeup_m12 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=freezeup_RSF)
seals_freezeup_m12
summary(seals_freezeup_m12)

      # break-up: Model 14 (BATH + CONC + DIST_LAND + DIST_WATER)
seals_breakup_m14 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=breakup_RSF)
seals_breakup_m14
summary(seals_breakup_m14)

###

# 4. Create predictive plots (bears) -------
# 4a.      Winter ---------

# winter: Model 5a (BATH + CONC + CONC_2)


# BATH
median_CONC = median(used_avail_RSF_winter_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_winter_model5 = coef(bears_winter_m5a)
coefs_winter_model5 = coefs_winter_model5$cond$ID
rest_of_prediction = median_CONC * coefs_winter_model5$CONC_SCALED[1] + median_CONC_2 * coefs_winter_model5$CONC_2_SCALED[1]+ coefs_winter_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$BATH)) / sd(used_avail_RSF_winter_FINAL$BATH) * 
                       coefs_winter_model5$BATH_SCALED[1] + rest_of_prediction))), 
      xlim = range(used_avail_RSF_winter_FINAL$BATH), ylim = c(0,1), 
      xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


# CONC
median_BATH = median(used_avail_RSF_winter_FINAL$BATH_SCALED)
coefs_winter_model5 = coef(bears_winter_m5a)
coefs_winter_model5 = coefs_winter_model5$cond$ID
rest_of_prediction = median_BATH * coefs_winter_model5$BATH_SCALED[1] + coefs_winter_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$CONC)) / sd(used_avail_RSF_winter_FINAL$CONC) * 
                       coefs_winter_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_winter_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_winter_FINAL$CONC_2) * coefs_winter_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction))), xlim = range(used_avail_RSF_winter_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")




# 4b.      Freeze-up --------

# top model for freeze-up: Model 5a (BATH + CONC + CONC_2)

###

# BATH
median_CONC = median(used_avail_RSF_freezeup_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_freezeup_model5a = coef(bears_freezeup_m5a)
coefs_freezeup_model5a = coefs_freezeup_model5a$cond$ID
rest_of_prediction = median_CONC * coefs_freezeup_model5a$CONC_SCALED[1] + median_CONC_2 * coefs_freezeup_model5a$CONC_2_SCALED[1] + coefs_freezeup_model5a$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction))), xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


# CONC
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model5 = coef(bears_freezeup_m5a)
coefs_freezeup_model5 = coefs_freezeup_model5$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model5$BATH_SCALED[1] + coefs_freezeup_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * 
                       coefs_freezeup_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_freezeup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_freezeup_FINAL$CONC_2) * coefs_freezeup_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction))), xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")


###

# 4c.      Break-up --------

# break-up: Model 2a (CONC + CONC_2)

###

# CONC
#median_BATH = median(used_avail_RSF_breakup_FINAL$BATH_SCALED)
coefs_breakup_model2a = coef(bears_breakup_m2a)
coefs_breakup_model2a = coefs_breakup_model2a$cond$ID
#rest_of_prediction = median_BATH * used_avail_RSF_breakup_FINAL$BATH_SCALED[1] + used_avail_RSF_breakup_FINAL$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_breakup_FINAL$CONC)) / sd(used_avail_RSF_breakup_FINAL$CONC) * 
                       coefs_breakup_model2a$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_breakup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_breakup_FINAL$CONC_2) * coefs_breakup_model2a$CONC_2_SCALED[1]))), 
      xlim = range(used_avail_RSF_breakup_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")


###

# 5. Create predictive plots (seals) ---------





# 5a.      Freeze-up --------

# freeze-up: Model 12 (BATH + CONC + DIST_WATER)

freezeup_RSF

# BATH
median_CONC = median(freezeup_RSF$CONC_SCALED)
median_WATER = median(freezeup_RSF$DIST_WATER_SCALED)
coefs_seals_freeze_model12 = coef(seals_freezeup_m12)
coefs_seals_freeze_model12 = coefs_seals_freeze_model12$cond$ID
rest_of_prediction = median_CONC * coefs_seals_freeze_model12$CONC_SCALED[1] + median_WATER * coefs_seals_freeze_model12$DIST_WATER_SCALED[1] + coefs_seals_freeze_model12$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(freezeup_RSF$BATH)) / sd(freezeup_RSF$BATH) * 
                       coefs_seals_freeze_model12$BATH_SCALED[1] + rest_of_prediction))), xlim = range(freezeup_RSF$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


summary(freezeup_RSF)




###

# 5b.      Break-up --------

# break-up: Model 14 (BATH + CONC + DIST_LAND + DIST_WATER)

breakup_RSF

#


# 6. Create predictive maps (bears) -------

bears_model5_freeze_5
bear_freeze_predict <- predict(bears_model5_freeze_5, used_avail_RSF_freezeup_FINAL)
plot(used_avail_RSF_freezeup_FINAL$BATH, bear_freeze_predict, type="l")

used_avail_RSF_freezeup_FINAL$BATH[which.max(bear_freeze_predict)]






###