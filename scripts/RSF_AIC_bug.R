
# 1. Load libraries ------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggpubr) 
library(lme4) 
library(adehabitatHR) 
library(adehabitatHS) 
library(TMB)
library(glmmTMB) 
library(data.table) 
library(gridExtra) 
library(AICcmodavg)
library(MuMIn)

# 2. Import final dataframes ---------

# import data
used_avail_RSF_winter_FINAL <- read.csv("used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("used_avail_RSF_freezeup_FINAL_Apr2021.csv")


# 3. Freeze-up top model --------

# Run model: BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)
AIC(model5_freeze) # 3960.28

# get AICc with AICcmodavg
AICc(model5_freeze) # 3960.287

# get AICc with MuMln
AICc(model5_freeze) # same as above

# 4. Winter top model --------

# Run model: DIST_WATER + DIST_LAND (Model 10)
model10_winter_free_var <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model10_winter_free_var)
AIC(model10_winter_free_var) # 5142.002

# get AICc with AICcmodavg
AICc(model10_winter_free_var) # 5142.009

# get AICc with MuMln
AICc(model10_winter_free_var) # same as above

# 5. Break-up top model ------

# Run model: BATH + DIST_LAND (Model 6)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)
AIC(model6_break) # 7259.641

# get AICc with AICcmodavg
AICc(model6_break) # 7259.645

# get AICc with MuMln
AICc(model6_break) # same as above


