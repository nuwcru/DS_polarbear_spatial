

# 1. Load libraries -------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
#library(lme4) # for RSFs
library(adehabitatHR) # for RSFs
#library(adehabitatHS) # for RSFs
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
#used_avail_RSF_pooled_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
#used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# seal data
breakup_RSF <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/DS_harpseals/data/breakup_RSF.csv")
freezeup_RSF <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/DS_harpseals/data/freezeup_RSF.csv")
      # add weight column
breakup_RSF$W <- ifelse(breakup_RSF$USE == "used", 1, 1000)
freezeup_RSF$W <- ifelse(freezeup_RSF$USE == "used", 1, 1000)


# 3. Run top RSF models -------

# freeze-up and break-up only!

# bears
      # freeze-up: Model 5 (BATH + CONC)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
bears_model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(bears_model5_freeze)

# freezeup attempt 2: Nov 1
bears_model5_freeze_2 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, weights=W)

# freezeup attempt 3: (no weights) Nov 1
bears_model5_freeze_3 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)

# freezeup attempt 4 (no weights and random intercept only, not slope): Nov 1
bears_model5_freeze_4 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)

# freezeup attempt 5 (weights added and random intercept only, not slope): Nov 1
bears_model5_freeze_5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, weights=W)

summary(bears_model5_freeze_4)
summary(bears_model5_freeze_5)


      # break-up: Model 6 (BATH + DIST_LAND)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
bears_model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(bears_model6_break)

# seals
      # freeze-up: Model 14 (BATH + CONC + DIST_LAND + DIST_WATER)
model14_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=freezeup_RSF, doFit=F, weights=W)
model14_tmp_freeze$parameters$theta[1] = log(1e3)
model14_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:4)))
seals_model14_freeze <- glmmTMB:::fitTMB(model14_tmp_freeze) 
summary(seals_model14_freeze) 
      # break-up: Model 14 (BATH + CONC + DIST_LAND + DIST_WATER)
model14_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=breakup_RSF, doFit=F, weights=W)
model14_tmp_break$parameters$theta[1] = log(1e3)
model14_tmp_break$mapArg = list(theta = factor(c(NA, 1:4)))
seals_model14_break <- glmmTMB:::fitTMB(model14_tmp_break) 
summary(seals_model14_break) 


# 3. Create predictive maps (bears) ---------

# https://terpconnect.umd.edu/~egurarie/research/NWT/Step09_RSF_PartIV.html

bears_model5_freeze
bears_model6_break

bear_freeze_predict <- predict(bears_model5_freeze, used_avail_RSF_freezeup_FINAL)
plot(used_avail_RSF_freezeup_FINAL$BATH, bear_freeze_predict)

BATH_SCALED <- used_avail_RSF_freezeup_FINAL$BATH_SCALED
CONC_SCALED <- used_avail_RSF_freezeup_FINAL$CONC_SCALED
ID <- used_avail_RSF_freezeup_FINAL$ID
W <- used_avail_RSF_freezeup_FINAL$W
bearfreeze_newdata <- data.frame(ID, CONC_SCALED, BATH_SCALED, W)

bear_freeze_predict <- predict(bears_model5_freeze, bearfreeze_newdata, type="response")
summary(bear_freeze_predict) # all the same values (0.5)
plot(used_avail_RSF_freezeup_FINAL$BATH_SCALED, bear_freeze_predict) # straight line regardless of if original or scaled values are used
plot(used_avail_RSF_freezeup_FINAL$CONC_SCALED, bear_freeze_predict) # same as above
plot(used_avail_RSF_freezeup_FINAL$BATH_SCALED, exp(bear_freeze_predict)/max(exp(bear_freeze_predict)), type="l") # as above
plot(used_avail_RSF_freezeup_FINAL$CONC_SCALED, exp(bear_freeze_predict)/max(exp(bear_freeze_predict)), type="l") # as above

# Peter Here
# Let's do concentration first
# 4
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model4 = coef(bears_model5_freeze_4)
coefs_freezeup_model4 = coefs_freezeup_model4$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model4$BATH_SCALED[1] + coefs_freezeup_model4$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * coefs_freezeup_model4$CONC_SCALED[1] + rest_of_prediction))), 
      xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Model 4", ylab = "Relative probability of selection")
summary(bears_model5_freeze_4)
summary(coefs_freezeup_model4)

# 5
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model5 = coef(bears_model5_freeze_5)
coefs_freezeup_model5 = coefs_freezeup_model5$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model4$BATH_SCALED[1] + coefs_freezeup_model4$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * coefs_freezeup_model5$CONC_SCALED[1] + rest_of_prediction))), 
      xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Model 5", ylab = "Relative probability of selection")
summary(bears_model5_freeze_5)
summary(coefs_freezeup_model5)

###



# 4. Create predictive maps (seals) ---------

# https://terpconnect.umd.edu/~egurarie/research/NWT/Step09_RSF_PartIV.html

seals_model14_freeze
seals_model14_break

seals_freeze_predict <- predict(seals_model14_freeze, freezeup_RSF)
plot(freezeup_RSF$BATH, seals_freeze_predict)

BATH_SCALED <- freezeup_RSF$BATH_SCALED
CONC_SCALED <- freezeup_RSF$CONC_SCALED
DIST_SCALED <- freezeup_RSF$DIST_LAND
DIST_WATER_SCALED <- freezeup_RSF$DIST_WATER_SCALED
ID <- freezeup_RSF$ID
W <- freezeup_RSF$W
sealfreeze_newdata <- data.frame(ID, CONC_SCALED, BATH_SCALED, DIST_SCALED, DIST_WATER_SCALED, W)

seal_freeze_predict <- predict(seals_model14_freeze, sealfreeze_newdata, type="response")
summary(bear_freeze_predict) # all the same values (0.5)
plot(freezeup_RSF$BATH_SCALED, seal_freeze_predict) # straight line regardless of if original or scaled values are used
plot(freezeup_RSF$CONC_SCALED, seal_freeze_predict) # same as above
plot(freezeup_RSF$DIST_SCALED, seal_freeze_predict) # same as above
plot(freezeup_RSF$DIST_WATER_SCALED, seal_freeze_predict) # same as above

seal_freeze_predict2 <- predict(seals_model14_freeze, newdata=sealfreeze_newdata, allow.new.levels=TRUE)
summary(bear_freeze_predict2) # all zeros





