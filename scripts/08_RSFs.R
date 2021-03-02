
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
install.packages("glmmTMB", type="source")
install.packages("TMB")
library(glmmTMB) # RSFs according to Muff et al. (2019)

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



# 2. Load data and format------

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_ice_bath_distland_final.csv")
head(used_avail)

# drop unnecessary columns
used_avail = subset(used_avail, select=-c(X, field_1, ZONE, EASTING, NORTHING, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S, DIST_LAND, join_Sourc, join_featu, join_scale, join_min_z, distance))
head(used_avail)

# rename some columns
names(used_avail)[14] <- "CONC"
names(used_avail)[15] <- "BATH"
names(used_avail)[16] <- "DIST_LAND"
head(used_avail)

# separate used from available for visualzing data
used <- used_avail %>% filter(USED_AVAIL=="used")
avail <- used_avail %>% filter(USED_AVAIL=="available")



# 3. - SKIP - Testing normality and correlations between covariates------------

head(used_avail)
str(used_avail)

# Note: we have CONC, BATH, and DIST_LAND


  
# test normality of each first

# CONC
hist(used_avail$CONC) # not normal
hist(used$CONC)
hist(avail$CONC)
ggplot(data=used)+ geom_point(aes(x=ID, y=CONC))

shapiro.test(used_avail$CONC) # sample size too large
shapiro.test(used$CONC) # p-value <0.05, therefore not normal

ggqqplot(used_avail$CONC, ylab="CONC") # not normal

# BATH
hist(used_avail$BATH) # normal
hist(used$BATH)
hist(avail$BATH)
ggplot(data=used)+ geom_point(aes(x=ID, y=BATH))

shapiro.test(used$BATH) # p-value <0.05, therefore not normal
ggqqplot(used_avail$BATH, ylab="BATH") # not normal

# DIST_LAND
hist(used_avail$DIST_LAND) # not normal
hist(used$DIST_LAND)
hist(avail$DIST_LAND)
ggplot(data=used)+ geom_point(aes(x=ID, y=DIST_LAND))

shapiro.test(used$DIST_LAND) # p-value <0.05, therefore not normal
ggqqplot(used_avail$DIST_LAND, ylab="DIST_LAND") # not normal




# From 371 RSF lab: "As a rule of thumb if a Pearson correlation coefficient of >0.6 is
  # found between your independent variables, you should use caution in using them in the same model"
  
# But, because data is not normal, we cannot use the pearson correlation test
# Use the kendall rank correlation test instead: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#pearson-correlation-test
  # also: https://towardsdatascience.com/kendall-rank-correlation-explained-dee01d99c535
  # if p-value >0.05 = significantly correlated
# if correlation coefficient (tau) is 0 = no association
      # 1 = strong positive correlation; -1 = strong negative correlation


# visualize correlation
  
pairs(~BATH+CONC+DIST_LAND, data=used_avail, panel=panel.smooth) 
pairs(~BATH+CONC+DIST_LAND, data=used, panel=panel.smooth) 
      # potential negative correlation between BATH and DIST_LAND (to be expected)


  
# test correlation
  
# BATH versus CONC
      # all used and available values
plot(used_avail$BATH, used_avail$CONC) # that's a mess
cor.test(used_avail$BATH, used_avail$CONC, method="kendall")
      # p-value = 0.00497 = not correlated
      # tau = -0.007 = very mild negative correlation

      # used values only
plot(used$BATH, used$CONC) # that's still a mess
ggplot(data=used)+ geom_point(aes(x=BATH, y=CONC))
cor.test(used$BATH, used$CONC, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.1634 = very mild negative correlation

# BATH versus DIST_LAND 
      # all used and available values
plot(used_avail$BATH, used_avail$DIST_LAND) # that's less of a mess
cor.test(used_avail$BATH, used_avail$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.522 = very mild negative correlation

      # used values only
plot(used$BATH, used$DIST_LAND) # that's that's less of a mess
ggplot(data=used)+ geom_point(aes(x=BATH, y=DIST_LAND))
cor.test(used$BATH, used$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.6253 = very mild negative correlation

# CONC versus DIST_LAND 
      # all used and available values
plot(used_avail$CONC, used_avail$DIST_LAND) # that's a mess
cor.test(used_avail$CONC, used_avail$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = 0.0521 = very mild positive correlation

      # used values only
plot(used$CONC, used$DIST_LAND) # that's still a mess
ggplot(data=used)+ geom_point(aes(x=CONC, y=DIST_LAND))
cor.test(used$CONC, used$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = 0.17663 = very mild positive correlation

summary(used)

# 4. Prepare dataframe for RSFs --------

head(avail) 
avail$USED_AVAIL <- 0 # available is now classified as 0

used$USED_AVAIL <- 1 # used is now classified as 1, then combine these together for a new df
head(used)

used_avail_RSF <- rbind(avail, used)
head(used_avail_RSF)
str(used_avail_RSF)

used_avail_RSF$ID <- as.integer(gsub('[a-zA-Z]', "", used_avail_RSF$ID)) # remove X from column
used_avail_RSF$ID <- factor(used_avail_RSF$ID) # make it a factor
used_avail_RSF$USED_AVAIL <- as.numeric(used_avail_RSF$USED_AVAIL) # make numeric
used_avail_RSF$USE <- factor(used_avail_RSF$USED_AVAIL, levels=c(1,0), labels=c("used", "available")) # make use a factor also
str(used_avail_RSF) # Now ID and USE are factors

# scale covariates: BATH, DIST_LAND, CONC
used_avail_RSF$BATH_SCALED <- scale(used_avail_RSF$BATH, scale=TRUE, center=TRUE)
used_avail_RSF$DIST_SCALED <- scale(used_avail_RSF$DIST_LAND, scale=TRUE, center=TRUE)
used_avail_RSF$CONC_SCALED <- scale(used_avail_RSF$CONC, scale=TRUE, center=TRUE)

hist(used_avail_RSF$BATH_SCALED) # more normal
ggqqplot(used_avail_RSF$BATH_SCALED, ylab="BATH_SCALED") # not normal
hist(used_avail_RSF$DIST_SCALED) # not very normal
ggqqplot(used_avail_RSF$DIST_SCALED, ylab="DIST_SCALED") # not normal
hist(used_avail_RSF$CONC_SCALED) # not normal
ggqqplot(used_avail_RSF$CONC_SCALED, ylab="CONC_SCALED") # not normal

      # still good that I did kendall rather than pearson correlation test
      # but this also means I can't use lme4

# log transform covariates
used_avail_RSF$BATH_LOG <- log(used_avail_RSF$BATH) # didn't work
used_avail_RSF$DIST_LOG <- log(used_avail_RSF$DIST_LAND)
used_avail_RSF$CONC_LOG <- log(used_avail_RSF$CONC)

hist(used_avail_RSF$DIST_LOG) # still not normal
hist(used_avail_RSF$CONC_LOG) # still not normal

# test linearity of use
CONC_lm = lm(USED_AVAIL ~ CONC, data=used_avail_RSF) # not linear
plot(CONC_lm)

BATH_lm = lm(USED_AVAIL ~ BATH, data=used_avail_RSF) # not linear
plot(BATH_lm)

DIST_lm = lm(USED_AVAIL ~ DIST_LAND, data=used_avail_RSF) # not linear
plot(DIST_lm)



 # Erik - Skipping log transformation for now
# log10(Y+1) transform covariates: from Zuur et al. 2009
#used_avail_RSF$BATH_LOG10 <- log10(used_avail_RSF$BATH+1) # didn't work
#used_avail_RSF$DIST_LOG10 <- log10(used_avail_RSF$DIST_LAND+1)
#used_avail_RSF$CONC_LOG10 <- log10(used_avail_RSF$CONC+1)

#hist(used_avail_RSF$DIST_LOG10) # starting to become normal
#hist(used_avail_RSF$CONC_LOG10) # still not normal



# 5. - SKIP - Creating basic (no seasons, no reproductive status) RSF models -----

# THIS SECTION IS IRRELEVANT NOW


# cannot use lme4 as it assumes normal distrbution
# "Generalized linear models (GLMM) combine the ideas of generalized linear models with the random effects modeling ideas.." (Faraway, 2016, p. 275)

# use used_avail_RSF dataframe!
head(used_avail_RSF)
unique(used_avail_RSF$USE)
str(used_avail_RSF)



# NULL MODEL
  



# This didn't work because you're using the wrong column as your response
# in your data, "use" is a descriptor that says whether the point is an "available" or "used"
# the response variable you want is used_avail
null <- glmer(USED_AVAIL ~ 1 + (1|ID), family=binomial, data=used_avail_RSF) # singular error again
coef(null) # all the same values
ranef(null) # all the same values


# how many "used" observations per individual out of curiosity
table(used_avail_RSF[which(used_avail_RSF$USED_AVAIL == 1), "ID"])

null2 <- glmer(USED_AVAIL ~ 1 + (1|ID), family=binomial(link = "logit"), data=used_avail_RSF) # singular error again
ranef(null2) # all the same values
coef(null2) # all the same values

# null model error: "boundary (singular) fit: see ?isSingular"
# apparently, the model is over-fit, i.e. the random effects structure is too complex: https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models
# could also be an issue with which lme4 version we're using: https://stats.stackexchange.com/questions/392302/singular-fit-in-lmer-despite-no-high-correlations-of-random-effects
# also the family isn't binomial?

# BATH only

model1 <- glmer(used_avail ~ bath + (1|id), family=binomial, data=dat)
ranef(model1)





# 6. - SKIP - Notes on RSFs (from Muff et al., 2019) -------------------------------------------------------------------

# The reason why your random intercepts are 0
#   - The intercept in the model indicates the probability of all animals using a point when your covariates are at their
#     mean. So the probability that all bears use a point that has mean ice concentration, mean distance to land, etc. 
#   - let's forget random effects for now
m1 <- glm(used_avail ~ bath_scaled + dist_scaled + conc_scaled,
          weight = weights, data = dat, family = binomial(link = "logit"))
summary(m1)
#   - You'll see that the intercept in this model is very low, about -10. We can convert that estimate to probability 
#     so it's easier to interpret using the plogis function
plogis(-10.8)
#   - You can see that the probability of a bear using a point at the mean bath, dist to land, and concentration is about
#     0.00002, or 0.002 %
#   - this is because the intercept is highly influenced by the ratio of used/available points we've constructed, in our case 1/50
#   - The purpose of using random effects (or intercept as you've done) is to estimate a "correction" to this overall intercept for each bear
#   - But really, that correction is still going to be incredibly close to zero because we have such a low ratio of used to available points

# So what do we do.
# First of all, Muff et al. 2019 provide a good discussion that outlines what I wrote above (lines 304 to 320): https://sci-hub.se/https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2656.13087
# Secondly, they extend that discussion into the topic of using random slopes in addition to random intercepts. See their paper for more details.

-----
  
# Model 1 and 2 are essentially the same from Muff et al. (2019), but M1 has 1 environmental variable, and M2 has more
  # Skip Model 1
  
# Model 2 - note that only the intercept of the ID column is random
      # Fixed Effects: bath + dist + conc
      # Random Effects: intercept only ~ id

m2 <- glmmTMB(USED_AVAIL ~ BATH_SCALED + DIST_SCALED + CONC_SCALED + (1|ID), family=binomial(), data=used_avail_RSF)
summary(m1)

-----

# Model 3 - note that the intercepts and slopes of all variables and ID are random
      # this is closer to what we want, but we need a fixed intercept variance (see M4)
      # Fixed Effects: bath + dist + conc
      # Random Effects: intercept ~ id, slope ~ bath + dist + conc

m3 <- glmmTMB(USED_AVAIL ~ BATH_SCALED + DIST_SCALED + CONC_SCALED + (1|ID) + (0+BATH_SCALED|ID) + (0+DIST_SCALED|ID) + (0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF)
summary(m2)

-----
  
# Model 4
      # Fixed intercept variance ~~~~~
      # Fixed Effects: bath + dist + conc
      # Random Effects: intercept(id) with large fixed variance, and slope ~ bath + dist + conc
      # add weights (rationale included in paper) with used = 1, available = 1000
head(used_avail_RSF)
used_avail_RSF$W <- ifelse(used_avail_RSF$USE == "used", 1, 1000)

# check to make sure weights are assigned appropriately
used_avail_RSF[which(used_avail_RSF$USE == "available"), "W"]
used_avail_RSF[which(used_avail_RSF$USE == "used"), "W"]

# We fit the same model as under M3, again using `glmmTMB`. 
# Note that we have to manually fix the variance of the intercept first. Start by setting up the model, but do not yet fit it (doFit = F):
m4_tmp <- glmmTMB(USED_AVAIL ~ BATH_SCALED + DIST_SCALED + CONC_SCALED + (1|ID) + (0+BATH_SCALED|ID) + (0+DIST_SCALED|ID) + (0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)


# Then fix the standard deviation of the first random term, which is the `(1|ID)` component  in the above model equation. We use sigma = 10^3, which corresponds to a variance of 10^6:
m4_tmp$parameters$theta[1] = log(1e3)

# We need to tell `glmmTMB` not to change the first entry of the vector of variances, and give all other variances another indicator to make sure they can be freely estimated:
m4_tmp$mapArg = list(theta = factor(c(NA, 1:3))) # 1:3 references (0+bath_scaled|id) + (0+dist_scaled|id) + (0+conc_scaled|id)

# Then fit the model and look at the results:
m4 <- glmmTMB:::fitTMB(m4_tmp)
summary(m4)

-----

# Model 3b 
      # intercept variance estimated ~~~~~
      # For comparison, we again fit model M4, but without fixing the intercept variance, letting it be estimated instead. Importantly, estimating the intercept variance is the current 
      # standard procedure. For this particular RSF case, it does not lead to a real difference, as expected due to the many observations per individual. This confirms that the decision 
      # to fix or estimate the intercept variance is not critical for RSFs, in contrast to SSFs (see Discussion in the paper).

m4b <- glmmTMB(USED_AVAIL ~ BATH_SCALED + DIST_SCALED + CONC_SCALED + (1|ID) + (0+BATH_SCALED|ID) + (0+DIST_SCALED|ID) + (0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF, weights=W)
summary(m4b)

# take a look at the corrections made to intercept, bath, dist, and conc for each bear
ranef(m4b)
ranef(m4)



-----

#	Need to use their model M4, which “fit the weighted logistic regression model (using W=1,000) ... 
# with random intercept and slopes, with fixed intercept variance at 10^6 … because this is the procedure we recommend”

  
  

# 7. RSFs Using Model #4 from Muff et al. (2019)  -------------------------------------------------

# Redo M4 with all different models

-----
  
# NULL MODEL - note: we cannot create a null M4 (no intercepts/slopes?) - skip lines 393-403

      # create temporary model first
#null_tmp <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
#null_tmp$parameters$theta[1] = log(1e3)

      # alter variances
#null_tmp$mapArg = list(theta = factor(c(NA, 1:1)))

      # fit model
#null <- glmmTMB:::fitTMB(null_tmp) # error: "a map factor length must equal parameter length"

      # use M2 for a null model instead
null <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF)
summary(null)

-----
  
# BATH ONLY (Model 1)
  
      # create temporary model first
model1_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
model1_tmp$parameters$theta[1] = log(1e3)
  
      # alter variances
model1_tmp$mapArg = list(theta = factor(c(NA, 1:1)))

      # fit model
model1 <- glmmTMB:::fitTMB(model1_tmp) 
summary(model1)

-----
  
# CONC ONLY (Model 2)
  
      # create temporary model first
model2_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
model2_tmp$parameters$theta[1] = log(1e3)

      # alter variances
model2_tmp$mapArg = list(theta = factor(c(NA, 1:1)))

      # fit model
model2 <- glmmTMB:::fitTMB(model2_tmp) 
summary(model2)

-----
  
# DIST_LAND ONLY (Model 3)
  
      # create temporary model first
model3_tmp <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
model3_tmp$parameters$theta[1] = log(1e3)

      # alter variances
model3_tmp$mapArg = list(theta = factor(c(NA, 1:1)))

      # fit model
model3 <- glmmTMB:::fitTMB(model3_tmp) 
summary(model3)


-----
  
# DIST_WATER ONLY (Model 4) - COMPLETE ONCE THESE VALUES ARE PULLED
  
# create temporary model first
#model4_tmp <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

# fix standard deviation
#model4_tmp$parameters$theta[1] = log(1e3)

# alter variances
#model4_tmp$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
#model4 <- glmmTMB:::fitTMB(model4_tmp) 
#summary(model4)


-----
  
# BATH + CONC (Model 5)
  
# create temporary model first
model5_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

# fix standard deviation
model5_tmp$parameters$theta[1] = log(1e3)

# alter variances
model5_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model5 <- glmmTMB:::fitTMB(model5_tmp) 
summary(model5)

-----
  
# BATH + DIST_LAND (Model 6)
  
      # create temporary model first
model6_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

    # fix standard deviation
model6_tmp$parameters$theta[1] = log(1e3)

      # alter variances
model6_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

      # fit model
model6 <- glmmTMB:::fitTMB(model6_tmp) 
summary(model6)

-----
  
# BATH + DIST_WATER (Model 7) - WAITING ON THIS FINAL COVARIATE
  
      # create temporary model first
#model7_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
#model7_tmp$parameters$theta[1] = log(1e3)

    # alter variances
#model7_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

    # fit model
#model7 <- glmmTMB:::fitTMB(model7_tmp) 
#summary(model7)

-----
  
# CONC + DIST_LAND (Model 8)
  
      # create temporary model first
model8_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

# fix standard deviation
model8_tmp$parameters$theta[1] = log(1e3)

# alter variances
model8_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model8 <- glmmTMB:::fitTMB(model8_tmp) 
summary(model8)

-----
  
# CONC + DIST_WATER (Model 9) - SKIP FOR NOW
  
      # create temporary model first
#model9_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
#model9_tmp$parameters$theta[1] = log(1e3)

      # alter variances
#model9_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

      # fit model
#model9 <- glmmTMB:::fitTMB(model9_tmp) 
#summary(model9)

-----
  
# DIST_LAND + DIST_WATER (Model 10)
  
      # create temporary model first
#model10_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
# model10_tmp$parameters$theta[1] = log(1e3)

      # alter variances
# model10_tmp$mapArg = list(theta = factor(c(NA, 1:2)))

      # fit model
#model10 <- glmmTMB:::fitTMB(model10_tmp) 
#summary(model10)

-----

# BATH + CONC + DIST_LAND (Model 11)
  
      # create temporary model first
model11_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
model11_tmp$parameters$theta[1] = log(1e3)

      # alter variances
model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))

      # fit model
model11 <- glmmTMB:::fitTMB(model11_tmp) 
summary(model11)

-----
  
# BATH + CONC + DIST_WATER (Model 12) - SKIP FOR NOW
  
      # create temporary model first
#model12_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)

      # fix standard deviation
#model12_tmp$parameters$theta[1] = log(1e3)

      # alter variances
#model12_tmp$mapArg = list(theta = factor(c(NA, 1:3)))

      # fit model
#model12 <- glmmTMB:::fitTMB(model12_tmp) 
#summary(model12)

-----

# CONC + DIST_LAND + DIST_WATER (Model 13) - SKIP FOR NOW
  
      # create temporary model first
#model13_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)
  
      # fix standard deviation
#model13_tmp$parameters$theta[1] = log(1e3)
  
      # alter variances
#model13_tmp$mapArg = list(theta = factor(c(NA, 1:3)))

      # fit model
#model13 <- glmmTMB:::fitTMB(model13_tmp) 
#summary(model13)

-----
  
# BATH + CONC + DIST_LAND + DIST_WATER (Model 14) - SKIP FOR NOW
  
      # create temporary model first
#model14_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_LAND_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_LAND_SCALED|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)
  
      # fix standard deviation
#model14_tmp$parameters$theta[1] = log(1e3)
  
      # alter variances
#model14_tmp$mapArg = list(theta = factor(c(NA, 1:4)))

      # fit model
#model14 <- glmmTMB:::fitTMB(model14_tmp) 
#summary(model14)

-----
  
  
  



