
# 1. Load libraries ------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggpubr) # for ggqqplots
library(lme4) # for RSFs
library(adehabitatHR) # for RSFs
library(adehabitatHS) # for RSFs
library(TMB)
library(glmmTMB) # RSFs according to Muff et al. (2019)
library(data.table) # required for section 10
library(gridExtra) # for grid.arrange() in section 10

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
          legend.title = element_blank(),
          #legend.position = c(0.9, 0.9),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 4, linetype = "blank"))
}

setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/")


# 2. Import final dataframes ---------

# import data
used_avail_RSF_pooled_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# Note: all top models were determined in script 08_RSFs


# 3. Analyze top pooled RSF model results --------

# pooled: Model 11 (BATH + CONC + DIST_LAND)
# run model 

model11_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model11_tmp$parameters$theta[1] = log(1e3)
model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
model11 <- glmmTMB:::fitTMB(model11_tmp) 
summary(model11)
fixef(model11)

### I NEED TO FIGURE OUT HOW TO UNSCALE THE COEFFICIENTS
# Try just running the model again on the original values

unscale_model11_tmp <- glmmTMB(USED_AVAIL~BATH+CONC+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
unscale_model11_tmp$parameters$theta[1] = log(1e3)
unscale_model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
unscale_model11 <- glmmTMB:::fitTMB(unscale_model11_tmp) 
summary(unscale_model11)
fixef(unscale_model11)

# get mean values
mean(used_avail_RSF_pooled_FINAL$DIST_WATER_M)

##


# 4. Analyze top seasonal RSF model (freeze-up) results--------

# freeze-up: Model 7 (BATH + DIST_WATER) - note that we couldn't set the variance here
model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
summary(model7_freeze_free_var)

# re-run model with unscaled values
unscale_model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH+DIST_WATER+(1|ID)+(0+BATH|ID)+(0+DIST_WATER|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
summary(unscale_model7_freeze_free_var)

# get mean values
mean(used_avail_RSF_freezeup_FINAL$CONC)

# 5. Analyze top seasonal RSF model (winter) results--------

# winter: Model 8 (CONT + DIST_LAND) - same issue as above
model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model8_winter_free_var)

# re-run model with unscaled values
unscale_model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC+DIST_LAND+(1|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(unscale_model8_winter_free_var)

# get mean values
mean(used_avail_RSF_winter_FINAL$DIST_WATER_M)

# 6. Analyze top seasonal RSF model (break-up) results--------

# break-up: Model 6 (BATH + DIST_LAND)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)

# re-run model with unscaled values
unscale_model6_tmp_break <- glmmTMB(USED_AVAIL~BATH+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+DIST_LAND|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
unscale_model6_tmp_break$parameters$theta[1] = log(1e3)
unscale_model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
unscale_model6_break <- glmmTMB:::fitTMB(unscale_model6_tmp_break) 
summary(unscale_model6_break)

# get mean values
mean(used_avail_RSF_breakup_FINAL$CONC)



# 7. figures? ---
