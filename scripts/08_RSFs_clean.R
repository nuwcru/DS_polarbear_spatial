# to get rid of glmmtmb error
#install.packages("glmmTMB", dependencies = TRUE)

library(tidyverse)
library(glmmTMB)

# scale function
scale_var <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)}



# Load data ---------------------------------------------------------------

# load and clean
used_avail <- read_csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_distwater_seasons_Apr2021.csv") %>%
    # convert distance to water into meters
  mutate(DIST_WATER_M = DIST_WATER * 111 * 1000,
         # change used avail to numeric
         USED_AVAIL = as.numeric(case_when(
           USED_AVAIL == "available" ~ "0",
           USED_AVAIL == "used" ~ "1"))) %>%
  dplyr::select(-c(X1, WATER_ID, WATER_LONG, WATER_LAT, ROWID, ICE_LAND, ZONE, EASTING, NORTHING)) %>%
   # Scale covariates
  mutate(BATH_SCALED = scale_var(BATH),
         DIST_SCALED = scale_var(DIST_LAND),
         CONC_SCALED = scale_var(CONC),
         DIST_WATER_SCALED = scale_var(DIST_WATER_M)) %>%
  

    
hist(used_avail$DIST_WATER_SCALED)
# separate used from available for visualzing data
used <- used_avail %>% filter(USED_AVAIL=="used") # 1463
avail <- used_avail %>% filter(USED_AVAIL=="available") # 73,150



# Pooled model ------------------------------------------------------------


## * Data clean --------------------------------------------------------------

# list bears with fewer than 20 fixes
fewer_20 <- used_avail %>% 
  filter(USED_AVAIL == "1") %>%
  group_by(ID) %>%
  tally() %>%
  filter(n < 21)


# Filter out bears with less than 20 fixes
 used_avail <- used_avail %>%
  filter(!ID %in% fewer_20$ID)
 
 
 # Add weights column to be used in the models
 used_avail$W <- ifelse(used_avail$USED_AVAIL == 1, 1, 1000)
 

## * Null -------------------------------------------------------------------

 
 
null <- glmmTMB(USED_AVAIL ~ 1 + (1|ID), 
                family=binomial(), 
                data = used_avail)
 summary(null)
 

### * Bath --------------------------------------------------------------------
 
 # Model 1: BATH ONLY 
 # create temporary model first
 model1_tmp <- glmmTMB(USED_AVAIL ~ BATH_SCALED + (1|ID) + (0 + BATH_SCALED|ID), 
                       family = binomial(), 
                       data = used_avail, 
                       doFit = F, 
                       weights = W)
 
 # fix standard deviation
 model1_tmp$parameters$theta[1] = log(1e3)
 # alter variances
 model1_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
 # fit model
 model1 <- glmmTMB:::fitTMB(model1_tmp) 
 summary(model1)
 
 
 
 

## * Dist to water -----------------------------------------------------------
 
 model4_tmp <- glmmTMB(USED_AVAIL ~ DIST_WATER_SCALED + (1|ID) + (0 + DIST_WATER_SCALED|ID), 
                       family = binomial(), 
                       data = used_avail, 
                       doFit = FALSE, 
                       weights = W)
 model4_tmp$parameters$theta[1] = log(1e3)
 #model4a_tmp$parameters$theta[1] = log(1e3)
 model4_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
 #model4a_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
 model4_fixed_var <- glmmTMB:::fitTMB(model4_tmp) 
 #model4a <- glmmTMB:::fitTMB(model4a_tmp) 
 summary(model4_fixed_var)

 
 # Don't fix intercept variance, but instead estimate it from the data
 model4_free_var <- glmmTMB(USED_AVAIL ~ DIST_WATER_SCALED + (1|ID) + (0 + DIST_WATER_SCALED|ID), 
                       family = binomial(), 
                       data = used_avail, 
                       doFit = TRUE, 
                       weights = W)
 
 summary(model4_free_var)
 