
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



# 2. Load data and format------

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_seasons_Mar2021.csv")
head(used_avail)

# drop unnecessary columns
used_avail = subset(used_avail, select=-c(X.1, X, ZONE, EASTING, NORTHING, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S))
head(used_avail)

# separate used from available for visualzing data
used <- used_avail %>% filter(USED_AVAIL=="used") # 1463
avail <- used_avail %>% filter(USED_AVAIL=="available") # 73,150



# 3. - SKIP - Testing normality and correlations between covariates------------

head(used_avail)
str(used_avail)

# Note: we have CONC, BATH, and DIST_LAND


  
# test normality of each first

# CONC
summary(used_avail$CONC) # range from 0 to 1.0 (open water to 100% conc)
hist(used_avail$CONC) # not normal
hist(used$CONC)
hist(avail$CONC)
#ggplot(data=used) + geom_point(aes(x=ID, y=CONC)) # this is just weird

shapiro.test(used_avail$CONC) # sample size too large
shapiro.test(used$CONC) # p-value <0.05, therefore not normal

ggqqplot(used_avail$CONC, ylab="CONC") # not normal

# BATH
summary(used_avail$BATH) # this is in m: range = 410.8 to -3495.3 
hist(used_avail$BATH) # not normal
hist(used$BATH)
hist(avail$BATH)

shapiro.test(used$BATH) # p-value <0.05, therefore not normal
ggqqplot(used_avail$BATH, ylab="BATH") # not normal

# DIST_LAND
summary(used_avail$DIST_LAND) # this is in m; range = 5 to 421,587
hist(used_avail$DIST_LAND) # not normal
hist(used$DIST_LAND)
hist(avail$DIST_LAND)

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
plot(used_avail$BATH, used_avail$CONC) # that's a hot mess
cor.test(used_avail$BATH, used_avail$CONC, method="kendall")
      # p-value < 0.001 = not correlated
      # tau = 0.1771989 = very mild positive correlation

      # used values only
plot(used$BATH, used$CONC) # that's still a mess
ggplot(data=used) + geom_point(aes(x=BATH, y=CONC))
cor.test(used$BATH, used$CONC, method="kendall")
      # p-value = 0.003516 = not correlated
      # tau = -0.05190126 = very mild negative correlation

# BATH versus DIST_LAND 
      # all used and available values
plot(used_avail$BATH, used_avail$DIST_LAND) # that's less of a mess
cor.test(used_avail$BATH, used_avail$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.365865 = very mild negative correlation

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
      # tau = -0.1619609 = very mild negative correlation

      # used values only
plot(used$CONC, used$DIST_LAND) # that's still a mess
ggplot(data=used)+ geom_point(aes(x=CONC, y=DIST_LAND))
cor.test(used$CONC, used$DIST_LAND, method="kendall")
      # p-value = 0.08044 = correlated
      # tau = 0.03108569 = very mild positive correlation

summary(used)

# 4. Transform data and test for colinearity again --------


# 1. SCALE COVARIATES

used_avail$BATH_SCALED <- scale(used_avail$BATH, scale=TRUE, center=TRUE)
used_avail$DIST_SCALED <- scale(used_avail$DIST_LAND, scale=TRUE, center=TRUE)
used_avail$CONC_SCALED <- scale(used_avail$CONC, scale=TRUE, center=TRUE)

      # visualize
hist(used_avail$BATH_SCALED) # not normal
ggqqplot(used_avail$BATH_SCALED, ylab="BATH_SCALED") # not normal
hist(used_avail$DIST_SCALED) # more normal
ggqqplot(used_avail$DIST_SCALED, ylab="DIST_SCALED") # not normal
hist(used_avail$CONC_SCALED) # not normal
ggqqplot(used_avail$CONC_SCALED, ylab="CONC_SCALED") # not normal

      # test correlation
        # BATH versus CONC
cor.test(used_avail$BATH_SCALED, used_avail$CONC_SCALED, method="kendall")
              # p-value < 0.001 = not correlated
              # tau = 0.1771989 = very mild positive correlation - these are the same values as above

        # BATH versus DIST_LAND
cor.test(used_avail$BATH_SCALED, used_avail$DIST_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.365865 = very mild negative correlation - these are the same values as above

        # CONC versus DIST_LAND
cor.test(used_avail$CONC_SCALED, used_avail$DIST_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.1619609 = very mild negative correlation - these are the same values as above

###


# 2. LOG TRANSFORM COVARIATES

used_avail$BATH_LOG <- log(used_avail$BATH) # didn't work: "NaNs produced"
used_avail$DIST_LOG <- log(used_avail$DIST_LAND)
used_avail$CONC_LOG <- log(used_avail$CONC)

hist(used_avail$DIST_LOG) # still not normal
hist(used_avail$CONC_LOG) # still not normal

      # test correlation
        # BATH versus CONC
cor.test(used_avail$BATH_LOG, used_avail$CONC_LOG, method="kendall")
              # p-value < 0.001 = not correlated
              # tau = -0.1972125 = very mild positive correlation - new values!

        # BATH versus DIST_LAND
cor.test(used_avail$BATH_LOG, used_avail$DIST_LOG, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.3540895 = very mild negative correlation - new values!

        # CONC versus DIST_LAND
cor.test(used_avail$CONC_LOG, used_avail$DIST_LOG, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.1619609 = very mild negative correlation - these are the same values as above

###


# 3. 10x LOG TRANSFORM COVARIATES

# log10(Y+1) transform covariates: from Zuur et al. 2009
used_avail$BATH_LOG10 <- log10(used_avail$BATH+1) # didn't work: NaNs produce
used_avail$DIST_LOG10 <- log10(used_avail$DIST_LAND+1)
used_avail$CONC_LOG10 <- log10(used_avail$CONC+1)

hist(used_avail$DIST_LOG10) # starting to become normal
hist(used_avail$CONC_LOG10) # still not normal

      # test correlation
          # BATH versus CONC
cor.test(used_avail$BATH_LOG10, used_avail$CONC_LOG10, method="kendall")
              # p-value < 0.001 = not correlated
              # tau = -0.2079512 = very mild positive correlation - new values!

          # BATH versus DIST_LAND
cor.test(used_avail$BATH_LOG10, used_avail$DIST_LOG10, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.3173102 = very mild negative correlation - new values!

          # CONC versus DIST_LAND
cor.test(used_avail$CONC_LOG10, used_avail$DIST_LOG10, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.1619609 = very mild negative correlation - these are the same values as above



###


# test linearity of use
head(used_avail)
summary(used_avail)
CONC_lm = lm(used_avail$CONC ~ used_avail$USED_AVAIL) # not linear
plot(CONC_lm)

BATH_lm = lm(used_avail$BATH ~ used_avail$USED_AVAIL) # not linear
plot(BATH_lm)

DIST_lm = lm(used_avail$DIST_LAND ~ used_avail$USED_AVAIL) # not linear
plot(DIST_lm)








# 5. Prepare data for RSFs --------

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


used_avail_RSF$BATH_SCALED <- scale(used_avail_RSF$BATH, scale=TRUE, center=TRUE)
used_avail_RSF$DIST_SCALED <- scale(used_avail_RSF$DIST_LAND, scale=TRUE, center=TRUE)
used_avail_RSF$CONC_SCALED <- scale(used_avail_RSF$CONC, scale=TRUE, center=TRUE)


# Model 4 (Muff et al., 2019) requres that used/avail points are weighted differently
      # set used=1 and avail=1000
head(used_avail_RSF)
used_avail_RSF$W <- ifelse(used_avail_RSF$USE == "used", 1, 1000)

# check to make sure weights are assigned appropriately
used_avail_RSF[which(used_avail_RSF$USE == "available"), "W"]
used_avail_RSF[which(used_avail_RSF$USE == "used"), "W"]


###

# Ignore below

###

# Get info on bears in each season

freeze_used <- freezeup %>% filter(USE=="used") # separate the used points for each season
break_used <- breakup %>% filter(USE=="used")
icefree_used <- icefree %>% filter(USE=="used")
winter_used <- winter %>% filter(USE=="used")

length(unique(freeze_used[["ID"]])) # count number of bears in each dataset
length(unique(break_used[["ID"]]))
length(unique(icefree_used[["ID"]]))
length(unique(winter_used[["ID"]]))

summary(as.data.frame(table(freeze_used$ID))) # get mean number of fixes per dataset
summary(as.data.frame(table(break_used$ID)))
summary(as.data.frame(table(icefree_used$ID)))
summary(as.data.frame(table(winter_used$ID)))
summary(as.data.frame(table(used_avail_RSF$ID)))


# 6. - SKIP - Creating basic (no seasons, no reproductive status) RSF models -----

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





# 7. - SKIP - Notes on RSFs (from Muff et al., 2019) -------------------------------------------------------------------

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

  
  

# 8. Pooled RSFs using Model #4 from Muff et al. (2019)  -------------------------------------------------

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


# 9.0. Seasonal RSFs using Model #4 again (see sections 9.1, 9.2, 9.3, 9.4)-------

# separate dataframe into seasons

head(used_avail_RSF)
unique(used_avail_RSF$SEASON)

freezeup <- used_avail_RSF %>% filter(SEASON=="freeze")
winter <- used_avail_RSF %>% filter(SEASON=="winter")
breakup <- used_avail_RSF %>% filter(SEASON=="break")
icefree <- used_avail_RSF %>% filter(SEASON=="summer")

unique(icefree$SEASON) # test all

summary(icefree) # just 163 used points
unique(icefree$ID)


# 9.1. WINTER ------------


# use M2 for a null model instead
null_winter <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=winter)
summary(null_winter)

###
  
# BATH ONLY (Model 1)
  
# create temporary model first
model1_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model1_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model1_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model1_winter <- glmmTMB:::fitTMB(model1_tmp_winter) 
summary(model1_winter)


###

# CONC ONLY (Model 2)
  
# create temporary model first
model2_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model2_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model2_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model2_winter <- glmmTMB:::fitTMB(model2_tmp_winter) 
summary(model2_winter)


###
  
# DIST_LAND ONLY (Model 3)
  
# create temporary model first
model3_tmp_winter <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model3_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model3_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model3_winter <- glmmTMB:::fitTMB(model3_tmp_winter) 
summary(model3_winter)


###
  
# DIST_WATER ONLY (Model 4) - COMPLETE ONCE THESE VALUES ARE PULLED
  
# create temporary model first
#model4_tmp_winter <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model4_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model4_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
#model4_winter <- glmmTMB:::fitTMB(model4_tmp_winter) 
#summary(model4_winter)


###
  
# BATH + CONC (Model 5)
  
# create temporary model first
model5_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model5_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model5_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model5_winter <- glmmTMB:::fitTMB(model5_tmp_winter) 
summary(model5_winter)


###
  
# BATH + DIST_LAND (Model 6)
  
# create temporary model first
model6_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model6_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model6_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model6_winter <- glmmTMB:::fitTMB(model6_tmp_winter) 
summary(model6_winter)


###
  
# BATH + DIST_WATER (Model 7) - WAITING ON THIS FINAL COVARIATE
  
# create temporary model first
#model7_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model7_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model7_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model7_winter <- glmmTMB:::fitTMB(model7_tmp_winter) 
#summary(model7_winter)


###
  
# CONC + DIST_LAND (Model 8)
  
# create temporary model first
model8_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model8_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model8_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model8_winter <- glmmTMB:::fitTMB(model8_tmp_winter) 
summary(model8_winter)


###
  
# CONC + DIST_WATER (Model 9) - SKIP FOR NOW
  
# create temporary model first
#model9_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model9_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model9_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model9_winter <- glmmTMB:::fitTMB(model9_tmp_winter) 
#summary(model9_winter)

-----
  
# DIST_LAND + DIST_WATER (Model 10)
  
# create temporary model first
#model10_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
# model10_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
# model10_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model10_winter <- glmmTMB:::fitTMB(model10_tmp_winter) 
#summary(model10_winter)


###
  
# BATH + CONC + DIST_LAND (Model 11)
  
# create temporary model first
model11_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)

# fix standard deviation
model11_tmp_winter$parameters$theta[1] = log(1e3)

# alter variances
model11_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
model11_winter <- glmmTMB:::fitTMB(model11_tmp_winter) 
summary(model11_winter)


###
  
# BATH + CONC + DIST_WATER (Model 12) - SKIP FOR NOW
  
# create temporary model first
#model12_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model12_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model12_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model12_winter <- glmmTMB:::fitTMB(model12_tmp_winter) 
#summary(model12_winter)


###

# CONC + DIST_LAND + DIST_WATER (Model 13) - SKIP FOR NOW
  
# create temporary model first
#model13_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model13_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model13_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model13_winter <- glmmTMB:::fitTMB(model13_tmp_winter) 
#summary(model13_winter)


###
  
# BATH + CONC + DIST_LAND + DIST_WATER (Model 14) - SKIP FOR NOW
  
# create temporary model first
#model14_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_LAND_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_LAND_SCALED|ID), family=binomial(), data=winter, doFit=F, weights=W)
  
# fix standard deviation
#model14_tmp_winter$parameters$theta[1] = log(1e3)
  
# alter variances
#model14_tmp_winter$mapArg = list(theta = factor(c(NA, 1:4)))

# fit model
#model14_winter <- glmmTMB:::fitTMB(model14_tmp_winter) 
#summary(model14_winter)


# 9.2. BREAK-UP ---------

# use M2 for a null model instead
null_break <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=breakup)
summary(null_break)

###

# BATH ONLY (Model 1)

# create temporary model first
model1_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model1_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model1_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model1_break <- glmmTMB:::fitTMB(model1_tmp_break) 
summary(model1_break)


###

# CONC ONLY (Model 2)

# create temporary model first
model2_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model2_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model2_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model2_break <- glmmTMB:::fitTMB(model2_tmp_break) 
summary(model2_break)


###

# DIST_LAND ONLY (Model 3)

# create temporary model first
model3_tmp_break <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model3_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model3_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model3_break <- glmmTMB:::fitTMB(model3_tmp_break) 
summary(model3_break)


###

# DIST_WATER ONLY (Model 4) - COMPLETE ONCE THESE VALUES ARE PULLED

# create temporary model first
#model4_tmp_break <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model4_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model4_tmp_breakr$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
#model4_break <- glmmTMB:::fitTMB(model4_tmp_break) 
#summary(model4_break)


###

# BATH + CONC (Model 5)

# create temporary model first
model5_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model5_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model5_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model5_break<- glmmTMB:::fitTMB(model5_tmp_break) 
summary(model5_break)


###

# BATH + DIST_LAND (Model 6)

# create temporary model first
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model6_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)


###

# BATH + DIST_WATER (Model 7) - WAITING ON THIS FINAL COVARIATE

# create temporary model first
#model7_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model7_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model7_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model7_break <- glmmTMB:::fitTMB(model7_tmp_break) 
#summary(model7_break)


###

# CONC + DIST_LAND (Model 8)

# create temporary model first
model8_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model8_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model8_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model8_break <- glmmTMB:::fitTMB(model8_tmp_break) 
summary(model8_break)


###

# CONC + DIST_WATER (Model 9) - SKIP FOR NOW

# create temporary model first
#model9_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model9_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model9_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model9_break <- glmmTMB:::fitTMB(model9_tmp_break) 
#summary(model9_break)


###
  
# DIST_LAND + DIST_WATER (Model 10)
  
# create temporary model first
#model10_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)
  
# fix standard deviation
# model10_tmp_break$parameters$theta[1] = log(1e3)
  
# alter variances
# model10_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model10_break <- glmmTMB:::fitTMB(model10_tmp_break) 
#summary(model10_break)


###

# BATH + CONC + DIST_LAND (Model 11)

# create temporary model first
model11_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
model11_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
model11_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
model11_break <- glmmTMB:::fitTMB(model11_tmp_break) 
summary(model11_break)


###

# BATH + CONC + DIST_WATER (Model 12) - SKIP FOR NOW

# create temporary model first
#model12_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model12_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model12_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model12_break <- glmmTMB:::fitTMB(model12_tmp_break) 
#summary(model12_break)


###

# CONC + DIST_LAND + DIST_WATER (Model 13) - SKIP FOR NOW

# create temporary model first
#model13_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model13_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model13_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model13_break <- glmmTMB:::fitTMB(model13_tmp_break) 
#summary(model13_break)


###

# BATH + CONC + DIST_LAND + DIST_WATER (Model 14) - SKIP FOR NOW

# create temporary model first
#model14_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_LAND_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_LAND_SCALED|ID), family=binomial(), data=breakup, doFit=F, weights=W)

# fix standard deviation
#model14_tmp_break$parameters$theta[1] = log(1e3)

# alter variances
#model14_tmp_break$mapArg = list(theta = factor(c(NA, 1:4)))

# fit model
#model14_break <- glmmTMB:::fitTMB(model14_tmp_break) 
#summary(model14_break)

# 9.3. ICE-FREE ---------

# use M2 for a null model instead
null_icefree <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=icefree)
summary(null_icefree)

###

# BATH ONLY (Model 1)

# create temporary model first
model1_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model1_tmp_icefreer$parameters$theta[1] = log(1e3)

# alter variances
model1_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model1_icefree <- glmmTMB:::fitTMB(model1_tmp_icefree) 
summary(model1_icefree)


###

# CONC ONLY (Model 2)

# create temporary model first
model2_tmp_icefree <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model2_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model2_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model2_icefree <- glmmTMB:::fitTMB(model2_tmp_icefree) 
summary(model2_icefree)


###

# DIST_LAND ONLY (Model 3)

# create temporary model first
model3_tmp_icefree <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model3_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model3_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model3_icefree <- glmmTMB:::fitTMB(model3_tmp_icefree) 
summary(model3_icefree)


###

# DIST_WATER ONLY (Model 4) - COMPLETE ONCE THESE VALUES ARE PULLED

# create temporary model first
#model4_tmp_icefree <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model4_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model4_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
#model4_icefree <- glmmTMB:::fitTMB(model4_tmp_icefree) 
#summary(model4_icefree)


###

# BATH + CONC (Model 5)

# create temporary model first
model5_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model5_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model5_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model5_icefree <- glmmTMB:::fitTMB(model5_tmp_icefree) 
summary(model5_icefree)


###

# BATH + DIST_LAND (Model 6)

# create temporary model first
model6_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model6_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model6_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model6_icefree <- glmmTMB:::fitTMB(model6_tmp_icefree) 
summary(model6_icefree)


###

# BATH + DIST_WATER (Model 7) - WAITING ON THIS FINAL COVARIATE

# create temporary model first
#model7_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model7_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model7_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model7_icefree <- glmmTMB:::fitTMB(model7_tmp_icefree) 
#summary(model7_icefree)


###

# CONC + DIST_LAND (Model 8)

# create temporary model first
model8_tmp_icefree <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model8_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model8_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model8_icefree <- glmmTMB:::fitTMB(model8_tmp_icefree) 
summary(model8_icefree)


###

# CONC + DIST_WATER (Model 9) - SKIP FOR NOW

# create temporary model first
#model9_tmp_icefree <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model9_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model9_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model9_icefree <- glmmTMB:::fitTMB(model9_tmp_icefree) 
#summary(model9_icefree)


###

  
# DIST_LAND + DIST_WATER (Model 10)
  
# create temporary model first
#model10_tmp_icefree <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)
  
# fix standard deviation
# model10_tmp_icefree$parameters$theta[1] = log(1e3)
  
# alter variances
# model10_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model10_icefree<- glmmTMB:::fitTMB(model10_tmp_icefree) 
#summary(model10_icefree)


###

# BATH + CONC + DIST_LAND (Model 11)

# create temporary model first
model11_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
model11_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
model11_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
model11_icefree <- glmmTMB:::fitTMB(model11_tmp_icefree) 
summary(model11_icefree)


###

# BATH + CONC + DIST_WATER (Model 12) - SKIP FOR NOW

# create temporary model first
#model12_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model12_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model12_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model12_icefree <- glmmTMB:::fitTMB(model12_tmp_icefree) 
#summary(model12_icefree)


###

# CONC + DIST_LAND + DIST_WATER (Model 13) - SKIP FOR NOW

# create temporary model first
#model13_tmp_icefree <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model13_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model13_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model13_icefree <- glmmTMB:::fitTMB(model13_tmp_icefree) 
#summary(model13_icefree)


###

# BATH + CONC + DIST_LAND + DIST_WATER (Model 14) - SKIP FOR NOW

# create temporary model first
#model14_tmp_icefree <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_LAND_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_LAND_SCALED|ID), family=binomial(), data=icefree, doFit=F, weights=W)

# fix standard deviation
#model14_tmp_icefree$parameters$theta[1] = log(1e3)

# alter variances
#model14_tmp_icefree$mapArg = list(theta = factor(c(NA, 1:4)))

# fit model
#model14_icefree <- glmmTMB:::fitTMB(model14_tmp_icefree) 
#summary(model14_icefree)

# 9.4. FREEZE-UP -------

# use M2 for a null model instead
null_freezeup <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=freezeup)
summary(null_freezeup)

###

# BATH ONLY (Model 1)

# create temporary model first
model1_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model1_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model1_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model1_freezeup <- glmmTMB:::fitTMB(model1_tmp_freezeup) 
summary(model1_freezeup)


###

# CONC ONLY (Model 2)

# create temporary model first
model2_tmp_freezeup <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model2_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model2_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model2_freezeup <- glmmTMB:::fitTMB(model2_tmp_freezeup) 
summary(model2_freezeup)


###

# DIST_LAND ONLY (Model 3)

# create temporary model first
model3_tmp_freezeup <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model3_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model3_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
model3_freezeup <- glmmTMB:::fitTMB(model3_tmp_freezeup) 
summary(model3_freezeup)


###

# DIST_WATER ONLY (Model 4) - COMPLETE ONCE THESE VALUES ARE PULLED

# create temporary model first
#model4_tmp_freezeup <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model4_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
#model4_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:1)))

# fit model
#model4_freezeup <- glmmTMB:::fitTMB(model4_tmp_freezeup) 
#summary(model4_freezeup)


###

# BATH + CONC (Model 5)

# create temporary model first
model5_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model5_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model5_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model5_freezeup <- glmmTMB:::fitTMB(model5_tmp_freezeup) 
summary(model5_freezeup)


###

# BATH + DIST_LAND (Model 6)

# create temporary model first
model6_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model6_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model6_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model6_freezeup <- glmmTMB:::fitTMB(model6_tmp_freezeup) 
summary(model6_freezeup)


###

# BATH + DIST_WATER (Model 7) - WAITING ON THIS FINAL COVARIATE

# create temporary model first
#model7_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model7_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
#model7_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model7_freezeup <- glmmTMB:::fitTMB(model7_tmp_freezeup) 
#summary(model7_freezeup)


###

# CONC + DIST_LAND (Model 8)

# create temporary model first
model8_tmp_freezeup <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model8_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model8_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
model8_freezeup <- glmmTMB:::fitTMB(model8_tmp_freezeup) 
summary(model8_freezeup)


###

# CONC + DIST_WATER (Model 9) - SKIP FOR NOW

# create temporary model first
#model9_tmp_freezeup <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model9_tmp_wfreezeup$parameters$theta[1] = log(1e3)

# alter variances
#model9_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model9_freezeup <- glmmTMB:::fitTMB(model9_tmp_freezeup) 
#summary(model9_freezeup)


###
  
# DIST_LAND + DIST_WATER (Model 10)
  
# create temporary model first
#model10_tmp_freezeup <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)
  
# fix standard deviation
# model10_tmp_freezeup$parameters$theta[1] = log(1e3)
  
# alter variances
# model10_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:2)))

# fit model
#model10_freezeup <- glmmTMB:::fitTMB(model10_tmp_freezeup) 
#summary(model10_freezeup)


###

# BATH + CONC + DIST_LAND (Model 11)

# create temporary model first
model11_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
model11_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
model11_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
model11_freezeup <- glmmTMB:::fitTMB(model11_tmp_freezeup) 
summary(model11_freezeup)


###

# BATH + CONC + DIST_WATER (Model 12) - SKIP FOR NOW

# create temporary model first
#model12_tmp_freezeup<- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model12_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
#model12_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model12_freezeup <- glmmTMB:::fitTMB(model12_tmp_freezeup) 
#summary(model12_freezeup)


###

# CONC + DIST_LAND + DIST_WATER (Model 13) - SKIP FOR NOW

# create temporary model first
#model13_tmp_freezeup <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model13_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
#model13_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:3)))

# fit model
#model13_freezeup <- glmmTMB:::fitTMB(model13_tmp_freezeup) 
#summary(model13_freezeup)


###

# BATH + CONC + DIST_LAND + DIST_WATER (Model 14) - SKIP FOR NOW

# create temporary model first
#model14_tmp_freezeup <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_LAND_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_LAND_SCALED|ID), family=binomial(), data=freezeup, doFit=F, weights=W)

# fix standard deviation
#model14_tmp_freezeup$parameters$theta[1] = log(1e3)

# alter variances
#model14_tmp_freezeup$mapArg = list(theta = factor(c(NA, 1:4)))

# fit model
#model14_freezeup <- glmmTMB:::fitTMB(model14_tmp_freezeup) 
#summary(model14_freezeup)


# 10. Interpreting and visualizing top model results ---------


summary(model11)
summary(model11_winter)
summary(model11_break)
summary(model11_icefree)
summary(model11_freezeup)

summary(used)
    # pooled mean bath: -351.31m
    # pooled mean conc: 0.7367
    # pooled mean dist_land: 72,160m

summary(winter)
    # winter mean bath: -533.9m
    # winter mean conc: 0.7978
    # winter mean dist_land: 98,117

summary(breakup)
    # breakup mean bath: -371.9m
    # breakup mean conc: 0.8738
    # breakup mean dist_land: 83,502.9

summary(icefree)
    # icefree mean bath: -304.1m
    # icefree mean conc: 0.8698
    # icefree mean dist_land: 55,246.83

summary(freezeup)
    # freezeup mean bath: -545.0m
    # freezeup mean conc: 0.8183
    # freezeup mean dist_land: 103,792.0




RSF_summary <- data.frame(matrix(ncol=4, nrow=5))
x <- c("RSF", "MEAN_BATH", "MEAN_CONC", "MEAN_DIST_LAND")
colnames(RSF_summary) <- x
head(RSF_summary)

RSF_summary$RSF <- c("pooled", "winter", "breakup", "icefree", "freezeup")
RSF_summary$MEAN_BATH <- c("-351.31","-533.9", "-371.9", "-304.1", "-545.0")
RSF_summary$MEAN_CONC <- c("0.7367", "0.7978", "0.8738", "0.8698", "0.8183")
RSF_summary$MEAN_DIST_LAND <- c("72160", "98117", "83502.9", "55246.83", "103792.0")
sd(freezeup$DIST_LAND)
RSF_summary$SD_BATH <- c("514.8118","665.8212", "309.5917", "183.9856", "620.8322")
RSF_summary$SD_CONC <- c("0.1789752", "0.201994", "0.1321105", "0.1617892", "0.2047531")
RSF_summary$SD_DIST_LAND <- c("67715.13", "75412.87", "53227.91", "33300.51", "81504.01")

summary(RSF_summary)

RSF_summary$MEAN_BATH <- as.numeric(RSF_summary$MEAN_BATH) 
RSF_summary$MEAN_CONC <- as.numeric(RSF_summary$MEAN_CONC) 
RSF_summary$MEAN_DIST_LAND <- as.numeric(RSF_summary$MEAN_DIST_LAND) 
RSF_summary$SD_BATH <- as.numeric(RSF_summary$SD_BATH) 
RSF_summary$SD_CONC <- as.numeric(RSF_summary$SD_CONC) 
RSF_summary$SD_DIST_LAND <- as.numeric(RSF_summary$SD_DIST_LAND) 

x <- c("pooled", "winter", "breakup", "icefree", "freezeup")
RSF_summary2 <- RSF_summary %>% mutate(RSF=factor(RSF, levels=x)) %>% arrange(RSF) # reordered

bathplot <- ggplot(RSF_summary2, aes(x=RSF, y=MEAN_BATH)) + geom_pointrange(aes(ymin=MEAN_BATH-SD_BATH, ymax=MEAN_BATH+SD_BATH)) + theme_nuwcru()
bathplot2 <- bathplot + theme(axis.title.x = element_blank()) + labs(y="Mean\nocean depth")
bathplot2

concplot <- ggplot(RSF_summary2, aes(x=RSF, y=MEAN_CONC)) + geom_pointrange(aes(ymin=MEAN_CONC-SD_CONC, ymax=MEAN_CONC+SD_CONC)) + theme_nuwcru()
concplot2 <- concplot + theme(axis.title.x = element_blank()) + labs(y="Mean\nsea ice concentration")
concplot2

distplot <- ggplot(RSF_summary2, aes(x=RSF, y=MEAN_DIST_LAND)) + geom_pointrange(aes(ymin=MEAN_DIST_LAND-SD_DIST_LAND, ymax=MEAN_DIST_LAND+SD_DIST_LAND)) + theme_nuwcru()
distplot2 <- distplot + theme(axis.title.x = element_blank()) + labs(y="Mean\ndistance to land")
distplot2

grid.arrange(bathplot2, distplot2, concplot2, ncol=1)




### 
# IGNORE BELOW FOR NOW
###



# based on new Fieberg et al. (2021) paper
# top models for pooled and seasonal (without dist_water) = model 11

# POOLED
summary(model11) # remember that these are scaled; need to unscale to interpret correctly
coef(model11) # for each individual
summary(model11)$varcor # magnitude of individual variation; this is in the summary as well

model11_coef <- coef(model11)
m <- c(0, mean(used_avail_RSF$BATH))
s <- c(1, sd(used_avail_RSF$BATH))
model11_coef_unscale <- model11_coef*s+m



# rescale coefficients to get correct units
      # "multiply coefficient by the standard deviation of the covariate and add the mean": https://stackoverflow.com/questions/35209579/unscale-predictor-coefficients-lmer-model-fit-with-an-unscaled-response

# using sd and means of original dataset
mean(used_avail_RSF$BATH)
sd(used_avail_RSF$BATH)
(1.55799*514.8118)+(-451.84) # 350.2316
(1.55799+-451.84)*(514.8118) # -231,810.5 - neither of these are correct; it should be negative but not that low


# https://stackoverflow.com/questions/10287545/backtransform-scale-for-plotting

y = 1.5579853*sqrt((sum((used_avail_RSF$BATH - mean(used_avail_RSF$BATH, na.rm=T))^2))/(length(used_avail_RSF$BATH)-1))
y + mean(used_avail_RSF$BATH, na.rm=T) # = 350.2292 for the true BATH value




# plot for visualization: https://terpconnect.umd.edu/~egurarie/research/NWT/Step08_RSF_PartIII.html#mixed_effects_model:_random_slope


coefs.tmb <- summary(model11)$coef$cond %>% as.data.frame
head(coefs.tmb)
coefs.tmb2 <- setDT(coefs.tmb, keep.rownames = TRUE)[]
head(coefs.tmb2)
names(coefs.tmb2)[1] <- "COVARIATE"
names(coefs.tmb2)[2] <- "ESTIMATE"
names(coefs.tmb2)[3] <- "SE"
names(coefs.tmb2)[4] <- "Z"
names(coefs.tmb2)[5] <- "PVALUE"
coefs.tmb3 = coefs.tmb2[-1,]
head(coefs.tmb3)
coefs.tmb3$LOW = coefs.tmb3$ESTIMATE-2*coefs.tmb3$SE
coefs.tmb3$HIGH = coefs.tmb3$ESTIMATE+2*coefs.tmb3$SE


ggplot(data=coefs.tmb3, aes(col=PVALUE<0.05)) +
  geom_point(aes(x=ESTIMATE, y=COVARIATE)) +
  geom_errorbar(aes(xmin=ESTIMATE-SE, xmax=ESTIMATE+SE, y=COVARIATE))


ggplot(data=coefs.tmb3, aes(col=PVALUE<0.05)) +
  geom_point(aes(x=ESTIMATE, y=COVARIATE)) +
  geom_errorbar(aes(xmin=LOW, xmax=HIGH, y=COVARIATE))



###



# WINTER
summary(model11_winter)

# remake model with original data
model11_tmp_winter2 <- glmmTMB(USED_AVAIL~BATH+CONC+DIST_LAND+(1|ID)+(0+BATH|ID)+(0+CONC|ID)+(0+DIST_LAND|ID), family=binomial(), data=winter, doFit=F, weights=W)
model11_tmp_winter2$parameters$theta[1] = log(1e3)
model11_tmp_winter2$mapArg = list(theta = factor(c(NA, 1:3)))
model11_winter2 <- glmmTMB:::fitTMB(model11_tmp_winter2) 
summary(model11_winter2)

      # this is still giving me incorrect values
      # how can BATH be 0.003048? It should be negative






# break-up
summary(model11_break)

# ice-free
summary(model11_icefree)

# freeze-up
summary(model11_freezeup)






