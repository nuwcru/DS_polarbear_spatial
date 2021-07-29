
# 1. Load libraries and import data --------

library(glmmTMB) # for section 2
library(effects) # for section 3: effects()
library(performance) # for section 3: binned_residuals()

used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")


# 2. Run top model for freeze-up season ----------

# BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), 
                             family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)
confint(model5_freeze)

saveRDS(model5_freeze, file="model5_freeze.RDS")


# 3. Plot model results -------

binned_residuals(model5_freeze) # "warning: probably bad model fit. only about 0% of the residuals are inside the error bounds"

bath_effect <- effect("BATH_SCALED", model5_freeze) 
summary(bath_effect)
plot(effect("BATH_SCALED", model5_freeze)) # looks very weird

conc_effect <- effect("CONC_SCALED", model5_freeze)
summary(conc_effect)
plot(effect("CONC_SCALED", model5_freeze)) # also looks weird


