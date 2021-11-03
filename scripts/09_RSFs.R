
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
          #legend.title = element_blank(),
          #legend.position = c(0.9, 0.9),
          legend.key = element_rect(colour = NA, fill = NA),
          legend.background = element_rect(color = "black",
                                           fill = "transparent",
                                           size = 4, linetype = "blank"))
}

setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/")



# 2. Load data and format------

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_distwater_seasons_Apr2021.csv")
head(used_avail)

# drop unnecessary columns
used_avail = subset(used_avail, select=-c(X, WATER_ID, WATER_LONG, WATER_LAT, ROWID, ICE_LAND, ZONE, EASTING, NORTHING))
head(used_avail)

# create new DIST_WATER column that's in meters (to match BATH and DIST_LAND)
used_avail$DIST_WATER_M <- used_avail$DIST_WATER*111*1000

# separate used from available for visualzing data
used <- used_avail %>% filter(USED_AVAIL=="used") # 1463
avail <- used_avail %>% filter(USED_AVAIL=="available") # 73,150

str(used_avail)

# 3. - SKIP - Testing normality and correlations between covariates------------

head(used_avail)
str(used_avail)

# Note: we have CONC, BATH, and DIST_LAND


  
# test normality of each first

# CONC
summary(used_avail$CONC) # range from 0 (open water) to 1.0 (100% conc)
hist(used_avail$CONC) # not normal
hist(used$CONC) # weird amount of 0 values..
hist(avail$CONC)
#ggplot(data=used) + geom_point(aes(x=ID, y=CONC)) # this is just weird

shapiro.test(used_avail$CONC) # error: sample size too large
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

# DIST_WATER_M
summary(used_avail$DIST_WATER_M) # this is in m; range = 242.1 to 1,456,269.0
hist(used_avail$DIST_WATER_M) # not normal
hist(used$DIST_WATER_M)
hist(avail$DIST_WATER_M)

shapiro.test(used$DIST_WATER_M) # p-value <0.05, therefore not normal
ggqqplot(used_avail$DIST_WATER_M, ylab="DIST_WATER_M") # not normal


####


# From 371 RSF lab: "As a rule of thumb if a Pearson correlation coefficient of >0.6 is
  # found between your independent variables, you should use caution in using them in the same model"
  
# But, because data is not normal, we cannot use the pearson correlation test
# Use the kendall rank correlation test instead: http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r#pearson-correlation-test
  # also: https://towardsdatascience.com/kendall-rank-correlation-explained-dee01d99c535
  # if p-value >0.05 = significantly correlated
# if correlation coefficient (tau) is 0 = no association
      # 1 = strong positive correlation; -1 = strong negative correlation
# another source: https://bookdown.org/ndphillips/YaRrr/correlation-cor-test.html


# visualize correlation (the first line takes awhile to plot)
pairs(~BATH+CONC+DIST_LAND+DIST_WATER_M, data=used_avail, panel=panel.smooth) # all data
pairs(~BATH+CONC+DIST_LAND+DIST_WATER_M, data=used, panel=panel.smooth) # used points only
      # potential negative correlation between BATH and DIST_LAND (to be expected)


  
# test correlation between each pair
  
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
plot(used$BATH, used$DIST_LAND) # that's less of a mess
ggplot(data=used)+ geom_point(aes(x=BATH, y=DIST_LAND))
cor.test(used$BATH, used$DIST_LAND, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.6253 = very mild negative correlation

# BATH versus DIST_WATER
      # all used and available values
plot(used_avail$BATH, used_avail$DIST_WATER_M)
cor.test(used_avail$BATH, used_avail$DIST_WATER_M, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = 0.08772621 = very mild positive correlation

      # used values only
plot(used$BATH, used$DIST_WATER_M) 
ggplot(data=used)+ geom_point(aes(x=BATH, y=DIST_WATER_M))
cor.test(used$BATH, used$DIST_WATER_M, method="kendall")
      # p-value = 0.07838 = correlated
      # tau = 0.03153957 = very mild positive correlation
     #### very weird that these are correlated


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


# CONC versus DIST_WATER_M
      # all used and available values
plot(used_avail$CONC, used_avail$DIST_WATER_M) 
cor.test(used_avail$CONC, used_avail$DIST_WATER_M, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = 0.09508064 = mild positive correlation

      # used values only
plot(used$CONC, used$DIST_WATER_M) # that's still a mess
ggplot(data=used)+ geom_point(aes(x=CONC, y=DIST_WATER_M))
cor.test(used$CONC, used$DIST_WATER_M, method="kendall")
      # p-value < 0.001 = not correlated
      # tau = 0.07522532 = very mild positive correlation


# DIST_LAND versus DIST_WATER_M
      # all used and available values
plot(used_avail$DIST_LAND, used_avail$DIST_WATER_M) 
cor.test(used_avail$DIST_LAND, used_avail$DIST_WATER_M, method="kendall")
      # p-value = <0.001 = not correlated
      # tau = -0.1592215 = very mild negative correlation

      # used values only
plot(used$DIST_LAND, used$DIST_WATER_M) # that's still a mess
ggplot(data=used)+ geom_point(aes(x=DIST_LAND, y=DIST_WATER_M))
cor.test(used$DIST_LAND, used$DIST_WATER_M, method="kendall")
      # p-value = <0.001 = correlated
      # tau = -0.09576605 = very mild negative correlation




# 4. - SKIP - Transform data and test for colinearity again --------


# 1. SCALE COVARIATES

used_avail$BATH_SCALED <- scale(used_avail$BATH, scale=TRUE, center=TRUE)
used_avail$DIST_SCALED <- scale(used_avail$DIST_LAND, scale=TRUE, center=TRUE)
used_avail$CONC_SCALED <- scale(used_avail$CONC, scale=TRUE, center=TRUE)
used_avail$DIST_WATER_SCALED <- scale(used_avail$DIST_WATER_M, scale=TRUE, center=TRUE)

write.csv(used_avail, "data/Oct2020work/FINAL DATASET/final_dataset_Jun2021.csv")

used <- used_avail %>% filter(USED_AVAIL=="used") # 1463
avail <- used_avail %>% filter(USED_AVAIL=="available") # 73,150

      # visualize
hist(used_avail$BATH_SCALED) # not normal
ggqqplot(used_avail$BATH_SCALED, ylab="BATH_SCALED") # not normal
hist(used_avail$DIST_SCALED) # more normal
ggqqplot(used_avail$DIST_SCALED, ylab="DIST_SCALED") # not normal
hist(used_avail$CONC_SCALED) # not normal
ggqqplot(used_avail$DIST_WATER_SCALED, ylab="CONC_SCALED") # not normal
hist(used_avail$CONC_SCALED) # not normal
ggqqplot(used_avail$DIST_WATER_SCALED, ylab="DIST_WATER_SCALED") # not normal

      # test correlation
        # BATH versus CONC
cor.test(used_avail$BATH_SCALED, used_avail$CONC_SCALED, method="kendall")
              # p-value < 0.001 = not correlated
              # tau = 0.1771989 = very mild positive correlation - these are the same values as above
cor.test(used$BATH_SCALED, used$CONC_SCALED, method="kendall")
              # p-value 0.5913 = correlated
              # tau = 0.009780978 

        # BATH versus DIST_LAND
cor.test(used_avail$BATH_SCALED, used_avail$DIST_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.365865 = very mild negative correlation - these are the same values as above
cor.test(used$BATH_SCALED, used$DIST_SCALED, method="kendall")
              # p-value <0.001 = not correlated
              # tau = -0.6015717

        # BATH versus DIST_WATER
cor.test(used_avail$BATH_SCALED, used_avail$DIST_WATER_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = 0.08772621 = very mild positive correlation
cor.test(used$BATH_SCALED, used$DIST_WATER_SCALED, method="kendall")
              # p-value 0.07838 = correlated
              # tau = 0.03153957 

        # CONC versus DIST_LAND
cor.test(used_avail$CONC_SCALED, used_avail$DIST_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.1619609 = very mild negative correlation - these are the same values as above
cor.test(used$CONC_SCALED, used$DIST_SCALED, method="kendall")
              # p-value 0.08987 = correlated
              # tau = -0.0309023  

        # CONC versus DIST_WATER
cor.test(used_avail$CONC_SCALED, used_avail$DIST_WATER_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = 0.09508064 = very mild positive correlation 
cor.test(used$CONC_SCALED, used$DIST_WATER_SCALED, method="kendall")
              # p-value <0.001 = not correlated
              # tau = 0.07522532 

        # DIST_LAND versus DIST_WATER
cor.test(used_avail$DIST_SCALED, used_avail$DIST_WATER_SCALED, method="kendall")
              # p-value = <0.001 = not correlated
              # tau = -0.1619609 = very mild negative correlation - these are the same values as above
cor.test(used$DIST_SCALED, used$DIST_WATER_SCALED, method="kendall")
              # p-value <0.001 = not correlated
              # tau = 0.07522532 


####


### Ignore below


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
              # tau = -0.1592215 = very mild negative correlation

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








# 5. - SKIP - Prepare data for RSFs --------

head(avail) 
avail$USED_AVAIL <- 0 # available is now classified as 0

used$USED_AVAIL <- 1 # used is now classified as 1, then combine these together for a new df
head(used)

used_avail_RSF <- rbind(avail, used)
head(used_avail_RSF)
str(used_avail_RSF)


###


# make 2 separate dataframes (one for pooled and one for seasonal RSFs) that only have bears with >20 fixes
# these were determined back in script #1 (data_cleaning_organization)

      # pooled
      # remove: X12078, X12081, X10393, X12082, X03956, X10374, X11974

head(used_avail_RSF)
X12078_pooled <- used_avail_RSF %>% filter(ID=="X12078")
X12081_pooled <- used_avail_RSF %>% filter(ID=="X12081")
X10393_pooled <- used_avail_RSF %>% filter(ID=="X10393")
X12082_pooled <- used_avail_RSF %>% filter(ID=="X12082")
X03956_pooled <- used_avail_RSF %>% filter(ID=="X03956")

used_avail_RSF_pooled1 <- anti_join(used_avail_RSF, X12078_pooled, by="ID")
used_avail_RSF_pooled2 <- anti_join(used_avail_RSF_pooled1, X12081_pooled, by="ID")      
used_avail_RSF_pooled3 <- anti_join(used_avail_RSF_pooled2, X10393_pooled, by="ID")
used_avail_RSF_pooled4 <- anti_join(used_avail_RSF_pooled3, X12082_pooled, by="ID")
used_avail_RSF_pooled_FINAL <- anti_join(used_avail_RSF_pooled4, X03956_pooled, by="ID")
unique(used_avail_RSF_pooled_FINAL$ID)
X12078 <- used_avail_RSF_pooled_FINAL %>% filter(ID=="X12078") # this worked

    # seasonal
unique(used_avail_RSF$SEASON)
      # winter: remove X03956, X10374, X10393, X10695, X10700, X10707, X10709, X11974, X12078, X12081, 
          # X12082, X12086, X12092, X13428, X13746, X30126, X30129, X30140
winter <- used_avail_RSF %>% filter(SEASON=="winter")
X03956_winter <- winter %>% filter(ID=="X03956")
X10374_winter <- winter %>% filter(ID=="X10374")
X10393_winter <- winter %>% filter(ID=="X10393")
X10695_winter <- winter %>% filter(ID=="X10695")
X10700_winter <- winter %>% filter(ID=="X10700")
X10707_winter <- winter %>% filter(ID=="X10707")
X10709_winter <- winter %>% filter(ID=="X10709")
X11974_winter <- winter %>% filter(ID=="X11974")
X12078_winter <- winter %>% filter(ID=="X12078")
X12081_winter <- winter %>% filter(ID=="X12081")
X12082_winter <- winter %>% filter(ID=="X12082")
X12086_winter <- winter %>% filter(ID=="X12086")
X12092_winter <- winter %>% filter(ID=="X12092")
X13428_winter <- winter %>% filter(ID=="X13428")
X13746_winter <- winter %>% filter(ID=="X13746")
X30126_winter <- winter %>% filter(ID=="X30126")
X30129_winter <- winter %>% filter(ID=="X30129")
X30140_winter <- winter %>% filter(ID=="X30140")

used_avail_RSF_winter1 <- anti_join(winter, X03956_winter, by="ID")
used_avail_RSF_winter2 <- anti_join(used_avail_RSF_winter1, X10374_winter, by="ID")      
used_avail_RSF_winter3 <- anti_join(used_avail_RSF_winter2, X10393_winter, by="ID")
used_avail_RSF_winter4 <- anti_join(used_avail_RSF_winter3, X10695_winter, by="ID")
used_avail_RSF_winter5 <- anti_join(used_avail_RSF_winter4, X10700_winter, by="ID")
used_avail_RSF_winter6 <- anti_join(used_avail_RSF_winter5, X10707_winter, by="ID")
used_avail_RSF_winter7 <- anti_join(used_avail_RSF_winter6, X10709_winter, by="ID")
used_avail_RSF_winter8 <- anti_join(used_avail_RSF_winter7, X11974_winter, by="ID")
used_avail_RSF_winter9 <- anti_join(used_avail_RSF_winter8, X12078_winter, by="ID")
used_avail_RSF_winter10 <- anti_join(used_avail_RSF_winter9, X12081_winter, by="ID")
used_avail_RSF_winter11 <- anti_join(used_avail_RSF_winter10, X12082_winter, by="ID")
used_avail_RSF_winter12 <- anti_join(used_avail_RSF_winter11, X12082_winter, by="ID")
used_avail_RSF_winter13 <- anti_join(used_avail_RSF_winter12, X12086_winter, by="ID")
used_avail_RSF_winter14 <- anti_join(used_avail_RSF_winter13, X12092_winter, by="ID")
used_avail_RSF_winter15 <- anti_join(used_avail_RSF_winter14, X13428_winter, by="ID")
used_avail_RSF_winter16 <- anti_join(used_avail_RSF_winter15, X13746_winter, by="ID")
used_avail_RSF_winter17 <- anti_join(used_avail_RSF_winter16, X30126_winter, by="ID")
used_avail_RSF_winter18 <- anti_join(used_avail_RSF_winter17, X30129_winter, by="ID")
used_avail_RSF_winter_FINAL <- anti_join(used_avail_RSF_winter18, X30140_winter, by="ID")
unique(used_avail_RSF_winter_FINAL$ID)
X03956 <- used_avail_RSF_winter_FINAL %>% filter(ID=="X03956") # this worked

      # breakup: remove X03956, X10374, X10393, X11974, X12078, X12081, X12082, X12086, X12092, X13428, 
          # X13437, X13746, X30126, X30129, X30131, X30135, X30140

breakup <- used_avail_RSF %>% filter(SEASON=="break")
X03956_breakup <- breakup %>% filter(ID=="X03956")
X10374_breakup <- breakup %>% filter(ID=="X10374")
X10393_breakup <- breakup %>% filter(ID=="X10393")
X11974_breakup <- breakup %>% filter(ID=="X11974")
X12078_breakup <- breakup %>% filter(ID=="X12078")
X12081_breakup <- breakup %>% filter(ID=="X12081")
X12082_breakup <- breakup %>% filter(ID=="X12082")
X12086_breakup <- breakup %>% filter(ID=="X12086")
X12092_breakup <- breakup %>% filter(ID=="X12092")
X13428_breakup <- breakup %>% filter(ID=="X13428")
X13437_breakup <- breakup %>% filter(ID=="X13437")
X13746_breakup <- breakup %>% filter(ID=="X13746")
X30126_breakup <- breakup %>% filter(ID=="X30126")
X30129_breakup <- breakup %>% filter(ID=="X30129")
X30131_breakup <- breakup %>% filter(ID=="X30131")
X30135_breakup <- breakup %>% filter(ID=="X30135")
X30140_breakup <- breakup %>% filter(ID=="X30140")

used_avail_RSF_breakup1 <- anti_join(breakup, X03956_breakup, by="ID")
used_avail_RSF_breakup2 <- anti_join(used_avail_RSF_breakup1, X10374_breakup, by="ID")   
used_avail_RSF_breakup3 <- anti_join(used_avail_RSF_breakup2, X10393_breakup, by="ID")   
used_avail_RSF_breakup4 <- anti_join(used_avail_RSF_breakup3, X11974_breakup, by="ID")   
used_avail_RSF_breakup5 <- anti_join(used_avail_RSF_breakup4, X12078_breakup, by="ID")   
used_avail_RSF_breakup6 <- anti_join(used_avail_RSF_breakup5, X12081_breakup, by="ID")   
used_avail_RSF_breakup7 <- anti_join(used_avail_RSF_breakup6, X12082_breakup, by="ID")   
used_avail_RSF_breakup8 <- anti_join(used_avail_RSF_breakup7, X12086_breakup, by="ID")   
used_avail_RSF_breakup9 <- anti_join(used_avail_RSF_breakup8, X12092_breakup, by="ID")   
used_avail_RSF_breakup10 <- anti_join(used_avail_RSF_breakup9, X13428_breakup, by="ID")   
used_avail_RSF_breakup11 <- anti_join(used_avail_RSF_breakup10, X13437_breakup, by="ID")   
used_avail_RSF_breakup12 <- anti_join(used_avail_RSF_breakup11, X13746_breakup, by="ID")   
used_avail_RSF_breakup13 <- anti_join(used_avail_RSF_breakup12, X30126_breakup, by="ID")   
used_avail_RSF_breakup14 <- anti_join(used_avail_RSF_breakup13, X30129_breakup, by="ID")   
used_avail_RSF_breakup15 <- anti_join(used_avail_RSF_breakup14, X30131_breakup, by="ID")   
used_avail_RSF_breakup16 <- anti_join(used_avail_RSF_breakup15, X30135_breakup, by="ID")   
used_avail_RSF_breakup_FINAL <- anti_join(used_avail_RSF_breakup16, X30140_breakup, by="ID")   
X10374 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="X10374")

        # freezeup (so few it's easier to keep and merge), so keep: X10695, X13284, X13289, X13292, X30135
freezeup <- used_avail_RSF %>% filter(SEASON=="freeze")
X10695_freezeup <- freezeup %>% filter(ID=="X10695")
X13284_freezeup <- freezeup %>% filter(ID=="X13284")
X13289_freezeup <- freezeup %>% filter(ID=="X13289")
X13292_freezeup <- freezeup %>% filter(ID=="X13292")
X30135_freezeup <- freezeup %>% filter(ID=="X30135")

used_avail_RSF_freezeup_FINAL <- rbind(X10695_freezeup, X13284_freezeup, X13289_freezeup, X13292_freezeup, X30135_freezeup)



###


# final dataframes: used_avail_RSF_pooled_FINAL, used_avail_RSF_winter_FINAL, used_avail_RSF_breakup_FINAL, used_avail_RSF_freezeup_FINAL
# format columns

used_avail_RSF_pooled_FINAL$ID <- as.integer(gsub('[a-zA-Z]', "", used_avail_RSF_pooled_FINAL$ID)) # remove X from column
used_avail_RSF_pooled_FINAL$ID <- factor(used_avail_RSF_pooled_FINAL$ID) # make it a factor

used_avail_RSF_winter_FINAL$ID <- as.integer(gsub('[a-zA-Z]', "", used_avail_RSF_winter_FINAL$ID)) # winter
used_avail_RSF_winter_FINAL$ID <- factor(used_avail_RSF_winter_FINAL$ID) 

used_avail_RSF_breakup_FINAL$ID <- as.integer(gsub('[a-zA-Z]', "", used_avail_RSF_breakup_FINAL$ID)) # breakup
used_avail_RSF_breakup_FINAL$ID <- factor(used_avail_RSF_breakup_FINAL$ID)

used_avail_RSF_freezeup_FINAL$ID <- as.integer(gsub('[a-zA-Z]', "", used_avail_RSF_freezeup_FINAL$ID)) # freezeup
used_avail_RSF_freezeup_FINAL$ID <- factor(used_avail_RSF_freezeup_FINAL$ID) 

used_avail_RSF_pooled_FINAL$USED_AVAIL <- as.numeric(used_avail_RSF_pooled_FINAL$USED_AVAIL) # make numeric
used_avail_RSF_winter_FINAL$USED_AVAIL <- as.numeric(used_avail_RSF_winter_FINAL$USED_AVAIL)
used_avail_RSF_breakup_FINAL$USED_AVAIL <- as.numeric(used_avail_RSF_breakup_FINAL$USED_AVAIL) 
used_avail_RSF_freezeup_FINAL$USED_AVAIL <- as.numeric(used_avail_RSF_freezeup_FINAL$USED_AVAIL) 

used_avail_RSF_pooled_FINAL$USE <- factor(used_avail_RSF_pooled_FINAL$USED_AVAIL, levels=c(1,0), labels=c("used", "available")) # make use a factor also
used_avail_RSF_winter_FINAL$USE <- factor(used_avail_RSF_winter_FINAL$USED_AVAIL, levels=c(1,0), labels=c("used", "available"))  
used_avail_RSF_breakup_FINAL$USE <- factor(used_avail_RSF_breakup_FINAL$USED_AVAIL, levels=c(1,0), labels=c("used", "available"))
used_avail_RSF_freezeup_FINAL$USE <- factor(used_avail_RSF_freezeup_FINAL$USED_AVAIL, levels=c(1,0), labels=c("used", "available")) 

used_avail_RSF_pooled_FINAL$BATH_SCALED <- scale(used_avail_RSF_pooled_FINAL$BATH, scale=TRUE, center=TRUE) # scale values
used_avail_RSF_winter_FINAL$BATH_SCALED <- scale(used_avail_RSF_winter_FINAL$BATH, scale=TRUE, center=TRUE) 
used_avail_RSF_breakup_FINAL$BATH_SCALED <- scale(used_avail_RSF_breakup_FINAL$BATH, scale=TRUE, center=TRUE) 
used_avail_RSF_freezeup_FINAL$BATH_SCALED <- scale(used_avail_RSF_freezeup_FINAL$BATH, scale=TRUE, center=TRUE) 
which(is.na(used_avail_RSF_freezeup_FINAL$BATH_SCALED))

used_avail_RSF_pooled_FINAL$DIST_SCALED <- scale(used_avail_RSF_pooled_FINAL$DIST_LAND, scale=TRUE, center=TRUE)
used_avail_RSF_winter_FINAL$DIST_SCALED <- scale(used_avail_RSF_winter_FINAL$DIST_LAND, scale=TRUE, center=TRUE)
used_avail_RSF_breakup_FINAL$DIST_SCALED <- scale(used_avail_RSF_breakup_FINAL$DIST_LAND, scale=TRUE, center=TRUE)
used_avail_RSF_freezeup_FINAL$DIST_SCALED <- scale(used_avail_RSF_freezeup_FINAL$DIST_LAND, scale=TRUE, center=TRUE)
which(is.na(used_avail_RSF_freezeup_FINAL$DIST_SCALED))

used_avail_RSF_pooled_FINAL$CONC_SCALED <- scale(used_avail_RSF_pooled_FINAL$CONC, scale=TRUE, center=TRUE)
used_avail_RSF_winter_FINAL$CONC_SCALED <- scale(used_avail_RSF_winter_FINAL$CONC, scale=TRUE, center=TRUE)
used_avail_RSF_breakup_FINAL$CONC_SCALED <- scale(used_avail_RSF_breakup_FINAL$CONC, scale=TRUE, center=TRUE)
used_avail_RSF_freezeup_FINAL$CONC_SCALED <- scale(used_avail_RSF_freezeup_FINAL$CONC, scale=TRUE, center=TRUE)
which(is.na(used_avail_RSF_pooled_FINAL$CONC_SCALED))

used_avail_RSF_pooled_FINAL$DIST_WATER_SCALED <- scale(used_avail_RSF_pooled_FINAL$DIST_WATER_M, scale=TRUE, center=TRUE)
used_avail_RSF_winter_FINAL$DIST_WATER_SCALED <- scale(used_avail_RSF_winter_FINAL$DIST_WATER_M, scale=TRUE, center=TRUE)
used_avail_RSF_breakup_FINAL$DIST_WATER_SCALED <- scale(used_avail_RSF_breakup_FINAL$DIST_WATER_M, scale=TRUE, center=TRUE)
used_avail_RSF_freezeup_FINAL$DIST_WATER_SCALED <- scale(used_avail_RSF_freezeup_FINAL$DIST_WATER_M, scale=TRUE, center=TRUE)
which(is.na(used_avail_RSF_freezeup_FINAL$DIST_WATER_SCALED))

# Model 4 (Muff et al., 2019) requres that used/avail points are weighted differently: set used=1 and avail=1000
used_avail_RSF_pooled_FINAL$W <- ifelse(used_avail_RSF_pooled_FINAL$USE == "used", 1, 1000)
used_avail_RSF_winter_FINAL$W <- ifelse(used_avail_RSF_winter_FINAL$USE == "used", 1, 1000)
used_avail_RSF_breakup_FINAL$W <- ifelse(used_avail_RSF_breakup_FINAL$USE == "used", 1, 1000)
used_avail_RSF_freezeup_FINAL$W <- ifelse(used_avail_RSF_freezeup_FINAL$USE == "used", 1, 1000)


str(used_avail_RSF_freezeup_FINAL)


# make final dataframes

write.csv(used_avail_RSF_pooled_FINAL, "data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
write.csv(used_avail_RSF_winter_FINAL, "data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
write.csv(used_avail_RSF_breakup_FINAL, "data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
write.csv(used_avail_RSF_freezeup_FINAL, "data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")







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

  
  

# 8. - SKIP - Pooled RSFs using Model #4 from Muff et al. (2019)  -------------------------------------------------

###
# Use this dataframe: used_avail_RSF_pooled_FINAL
# Need to run through section 4 to get it!!
###


# Redo M4 with all different models

###
  
# NULL MODEL - note: we cannot create a null M4 (no intercepts/slopes?)
      # create temporary model first
#null_tmp <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF, doFit=F, weights=W)
      # fix standard deviation
#null_tmp$parameters$theta[1] = log(1e3)
      # alter variances
#null_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
      # fit model
#null <- glmmTMB:::fitTMB(null_tmp) # error: "a map factor length must equal parameter length"

      # use M2 for a null model instead
null <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL)
summary(null)

###
  
# Model 1: BATH ONLY 
      # create temporary model first
model1_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
      # fix standard deviation
model1_tmp$parameters$theta[1] = log(1e3)
      # alter variances
model1_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
      # fit model
model1 <- glmmTMB:::fitTMB(model1_tmp) 
summary(model1)

###
  
# Model 2: CONC ONLY
      # create temporary model first
model2_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
      # fix standard deviation
model2_tmp$parameters$theta[1] = log(1e3)
      # alter variances
model2_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
      # fit model
model2 <- glmmTMB:::fitTMB(model2_tmp) 
summary(model2)

###
  
# Model 3: DIST_LAND ONLY
model3_tmp <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model3_tmp$parameters$theta[1] = log(1e3)
model3_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
model3 <- glmmTMB:::fitTMB(model3_tmp) 
summary(model3)

###
  
# Model 4: DIST_WATER ONLY 
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model4_tmp <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
#model4_tmp$parameters$theta[1] = log(1e3)
#model4_tmp$mapArg = list(theta = factor(c(NA, 1:1)))
#model4 <- glmmTMB:::fitTMB(model4_tmp) 
#summary(model4)

# Instead, don't fix intercept variance, but  estimate it from the data
model4_free_var <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=TRUE, weights=W)
summary(model4_free_var)


###
  
# Model 5: BATH + CONC
model5_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model5_tmp$parameters$theta[1] = log(1e3)
model5_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
model5 <- glmmTMB:::fitTMB(model5_tmp) 
summary(model5)

###
  
# Model 6: BATH + DIST_LAND
model6_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model6_tmp$parameters$theta[1] = log(1e3)
model6_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
model6 <- glmmTMB:::fitTMB(model6_tmp) 
summary(model6)

###
  
# Model 7: BATH + DIST_WATER 
# Ingnore first attempt, we were getting errors here as well
#head(used_avail_RSF_pooled_FINAL)
#model7_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
#model7_tmp$parameters$theta[1] = log(1e3)
#model7_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
#model7 <- glmmTMB:::fitTMB(model7_tmp) 
#summary(model7)

model7_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=TRUE, weights=W)
summary(model7_free_var)


###
  
# Model 8: CONC + DIST_LAND
model8_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model8_tmp$parameters$theta[1] = log(1e3)
model8_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
model8 <- glmmTMB:::fitTMB(model8_tmp) 
summary(model8)

###
  
# Model 9: CONC + DIST_WATER 
model9_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model9_tmp$parameters$theta[1] = log(1e3)
model9_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
model9 <- glmmTMB:::fitTMB(model9_tmp) 
summary(model9)

###
  
# Model 10: DIST_LAND + DIST_WATER 
model10_tmp <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model10_tmp$parameters$theta[1] = log(1e3)
model10_tmp$mapArg = list(theta = factor(c(NA, 1:2)))
model10 <- glmmTMB:::fitTMB(model10_tmp) 
summary(model10)

###

# Model 11: BATH + CONC + DIST_LAND
model11_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model11_tmp$parameters$theta[1] = log(1e3)
model11_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
model11 <- glmmTMB:::fitTMB(model11_tmp) 
summary(model11)

###
  
# Model 12: BATH + CONC + DIST_WATER 
# NA values here too; ignore first attempt and don't estimate variance
#model12_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
#model12_tmp$parameters$theta[1] = log(1e3)
#model12_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
#model12 <- glmmTMB:::fitTMB(model12_tmp) # throws errors
#summary(model12)

model12_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=TRUE, weights=W)
summary(model12_free_var)


###

# Model 13: CONC + DIST_LAND + DIST_WATER
model13_tmp <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model13_tmp$parameters$theta[1] = log(1e3)
model13_tmp$mapArg = list(theta = factor(c(NA, 1:3)))
model13 <- glmmTMB:::fitTMB(model13_tmp) 
summary(model13)

###

# Model 14: BATH + CONC + DIST_LAND + DIST_WATER
model14_tmp <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_pooled_FINAL, doFit=F, weights=W)
model14_tmp$parameters$theta[1] = log(1e3)
model14_tmp$mapArg = list(theta = factor(c(NA, 1:4)))
model14 <- glmmTMB:::fitTMB(model14_tmp) 
summary(model14)


# 9.0. - SKIP - Notes on Seasonal RSFs using Model #4  (see sections 9.1, 9.2, 9.3)-------

# either run through section 5 first or use these

used_avail_RSF_pooled_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_pooled_FINAL_Apr2021.csv")
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")


# use the dataframes below for the next sections

head(used_avail_RSF_breakup_FINAL)
head(used_avail_RSF_freezeup_FINAL)
head(used_avail_RSF_winter_FINAL)

# there are no bears with >20 fixes in the ice-free season
# part of this is becuase we removed land-based fixes
# don't do ice-free seasonal RSFs







# 9.1. - SKIP - WINTER ------------

# use M2 for a null model instead
null_winter <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
summary(null_winter)

###
  
# BATH ONLY (Model 1)
# create temporary model first
model1_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
# fix standard deviation
model1_tmp_winter$parameters$theta[1] = log(1e3)
# alter variances
model1_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))
# fit model
model1_winter <- glmmTMB:::fitTMB(model1_tmp_winter) 
summary(model1_winter)


###

# CONC ONLY (Model 2)
model2_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model2_tmp_winter$parameters$theta[1] = log(1e3)
model2_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))
model2_winter <- glmmTMB:::fitTMB(model2_tmp_winter) 
summary(model2_winter)


###
  
# DIST_LAND ONLY (Model 3)
model3_tmp_winter <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model3_tmp_winter$parameters$theta[1] = log(1e3)
model3_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))
model3_winter <- glmmTMB:::fitTMB(model3_tmp_winter) 
summary(model3_winter)


###
  
# DIST_WATER ONLY (Model 4)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model4_tmp_winter <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model4_tmp_winter$parameters$theta[1] = log(1e3)
#model4_tmp_winter$mapArg = list(theta = factor(c(NA, 1:1)))
#model4_winter <- glmmTMB:::fitTMB(model4_tmp_winter) 
#summary(model4_winter)

# Instead, don't fix intercept variance, but  estimate it from the data
model4_winter_free_var <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model4_winter_free_var)


###
  
# BATH + CONC (Model 5)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model5_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model5_tmp_winter$parameters$theta[1] = log(1e3)
#model5_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
#model5_winter <- glmmTMB:::fitTMB(model5_tmp_winter) 
#summary(model5_winter)

# Instead, don't fix intercept variance, but estimate it from the data
model5_winter_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model5_winter_free_var)


###
  
# BATH + DIST_LAND (Model 6)
model6_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model6_tmp_winter$parameters$theta[1] = log(1e3)
model6_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
model6_winter <- glmmTMB:::fitTMB(model6_tmp_winter) 
summary(model6_winter)


###
  
# BATH + DIST_WATER (Model 7)
model7_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model7_tmp_winter$parameters$theta[1] = log(1e3)
model7_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
model7_winter <- glmmTMB:::fitTMB(model7_tmp_winter) 
summary(model7_winter)


###
  
# CONC + DIST_LAND (Model 8)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model8_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model8_tmp_winter$parameters$theta[1] = log(1e3)
#model8_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
#model8_winter <- glmmTMB:::fitTMB(model8_tmp_winter) 
#summary(model8_winter)

# Instead, don't fix intercept variance, but estimate it from the data
model8_winter_free_var <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model8_winter_free_var)


###
  
# CONC + DIST_WATER (Model 9) 
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model9_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model9_tmp_winter$parameters$theta[1] = log(1e3)
#model9_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
#model9_winter <- glmmTMB:::fitTMB(model9_tmp_winter) 
#summary(model9_winter)

# Instead, don't fix intercept variance, but estimate it from the data
model9_winter_free_var <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model9_winter_free_var)


###
  
# DIST_LAND + DIST_WATER (Model 10)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model10_tmp_winter <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model10_tmp_winter$parameters$theta[1] = log(1e3)
#model10_tmp_winter$mapArg = list(theta = factor(c(NA, 1:2)))
#model10_winter <- glmmTMB:::fitTMB(model10_tmp_winter) 
#summary(model10_winter)

# Instead, don't fix intercept variance, but estimate it from the data
model10_winter_free_var <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model10_winter_free_var)


###
  
# BATH + CONC + DIST_LAND (Model 11)
model11_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model11_tmp_winter$parameters$theta[1] = log(1e3)
model11_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))
model11_winter <- glmmTMB:::fitTMB(model11_tmp_winter) 
summary(model11_winter)


###
  
# BATH + CONC + DIST_WATER (Model 12)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model12_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
#model12_tmp_winter$parameters$theta[1] = log(1e3)
#model12_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))
#model12_winter <- glmmTMB:::fitTMB(model12_tmp_winter) 
#summary(model12_winter)

# Instead, don't fix intercept variance, but estimate it from the data
model12_winter_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=TRUE, weights=W)
summary(model12_winter_free_var)


###

# CONC + DIST_LAND + DIST_WATER (Model 13)
model13_tmp_winter <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model13_tmp_winter$parameters$theta[1] = log(1e3)
model13_tmp_winter$mapArg = list(theta = factor(c(NA, 1:3)))
model13_winter <- glmmTMB:::fitTMB(model13_tmp_winter) 
summary(model13_winter)


###
  
# BATH + CONC + DIST_LAND + DIST_WATER (Model 14)
model14_tmp_winter <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, doFit=F, weights=W)
model14_tmp_winter$parameters$theta[1] = log(1e3)
model14_tmp_winter$mapArg = list(theta = factor(c(NA, 1:4)))
model14_winter <- glmmTMB:::fitTMB(model14_tmp_winter) 
summary(model14_winter)


# 9.1a WINTER SIMPLIFIED (Nov 2021) -------

###

# New simplified models since our complex ones weren't giving us predicted values
# these ones have no weighting and a random intercept only (as opposed to also a random slope)
# Model #s align with Table 7 in manuscript

###

# import data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
head(used_avail_RSF_winter_FINAL)

###

# Model 1: ID + BATH
bears_winter_m1 <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m1
summary(bears_winter_m1)

# Model 2: ID + CONC
bears_winter_m2 <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m2
summary(bears_winter_m2)

# Model 3: ID + LAND
bears_winter_m3 <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m3
summary(bears_winter_m3)

# Model 4: ID + WATER
bears_winter_m4 <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m4
summary(bears_winter_m4)

# Model 5: ID + BATH + CONC
bears_winter_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m5
summary(bears_winter_m5)

# Model 6: ID + BATH + LAND
bears_winter_m6 <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m6
summary(bears_winter_m6)

# Model 7: ID + CONC + WATER
bears_winter_m7 <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m7
summary(bears_winter_m7)

# Model 8: ID + LAND + WATER
bears_winter_m8 <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m8
summary(bears_winter_m8)



# 9.2. - SKIP - BREAK-UP ---------

# use M2 for a null model instead
null_break <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
summary(null_break)

###

# BATH ONLY (Model 1)
# create temporary model first
model1_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
# fix standard deviation
model1_tmp_break$parameters$theta[1] = log(1e3)
# alter variances
model1_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))
# fit model
model1_break <- glmmTMB:::fitTMB(model1_tmp_break) 
summary(model1_break)


###

# CONC ONLY (Model 2)
model2_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model2_tmp_break$parameters$theta[1] = log(1e3)
model2_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))
model2_break <- glmmTMB:::fitTMB(model2_tmp_break) 
summary(model2_break)


###

# DIST_LAND ONLY (Model 3)
model3_tmp_break<- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model3_tmp_break$parameters$theta[1] = log(1e3)
model3_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))
model3_break <- glmmTMB:::fitTMB(model3_tmp_break) 
summary(model3_break)


###

# DIST_WATER ONLY (Model 4)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
model4_tmp_break <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model4_tmp_break$parameters$theta[1] = log(1e3)
model4_tmp_break$mapArg = list(theta = factor(c(NA, 1:1)))
model4_break <- glmmTMB:::fitTMB(model4_tmp_break) 
summary(model4_break)

# Instead, don't fix intercept variance, but estimate it from the data
# Error here too: "Model convergence problem; non-positive-definite Hessian matrix"
model4_break_free_var <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=TRUE, weights=W)
summary(model4_break_free_var)


###

# BATH + CONC (Model 5)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model5_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
#model5_tmp_break$parameters$theta[1] = log(1e3)
#model5_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
#model5_break <- glmmTMB:::fitTMB(model5_tmp_break) 
#summary(model5_break)

# Instead, don't fix intercept variance, but estimate it from the data
model5_break_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=TRUE, weights=W)
summary(model5_break_free_var)


###

# BATH + DIST_LAND (Model 6)
model6_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model6_tmp_break$parameters$theta[1] = log(1e3)
model6_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model6_break <- glmmTMB:::fitTMB(model6_tmp_break) 
summary(model6_break)


###

# BATH + DIST_WATER (Model 7)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model7_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
#model7_tmp_break$parameters$theta[1] = log(1e3)
#model7_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
#model7_break <- glmmTMB:::fitTMB(model7_tmp_break) 
#summary(model7_break)

# Instead, don't fix intercept variance, but estimate it from the data
model7_break_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=TRUE, weights=W)
summary(model7_break_free_var)


###

# CONC + DIST_LAND (Model 8)
model8_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model8_tmp_break$parameters$theta[1] = log(1e3)
model8_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model8_break <- glmmTMB:::fitTMB(model8_tmp_break) 
summary(model8_break)


###

# CONC + DIST_WATER (Model 9) 
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model9_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
#model9_tmp_break$parameters$theta[1] = log(1e3)
#model9_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
#model9_break <- glmmTMB:::fitTMB(model9_tmp_break) 
#summary(model9_break)

# Instead, don't fix intercept variance, but estimate it from the data
model9_break_free_var <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=TRUE, weights=W)
summary(model9_break_free_var)


###

# DIST_LAND + DIST_WATER (Model 10)
model10_tmp_break <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model10_tmp_break$parameters$theta[1] = log(1e3)
model10_tmp_break$mapArg = list(theta = factor(c(NA, 1:2)))
model10_break <- glmmTMB:::fitTMB(model10_tmp_break) 
summary(model10_break)


###

# BATH + CONC + DIST_LAND (Model 11)
model11_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model11_tmp_break$parameters$theta[1] = log(1e3)
model11_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))
model11_break <- glmmTMB:::fitTMB(model11_tmp_break) 
summary(model11_break)


###

# BATH + CONC + DIST_WATER (Model 12)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model12_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
#model12_tmp_break$parameters$theta[1] = log(1e3)
#model12_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))
#model12_break <- glmmTMB:::fitTMB(model12_tmp_break) 
#summary(model12_break)

# Instead, don't fix intercept variance, but estimate it from the data
model12_break_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=TRUE, weights=W)
summary(model12_break_free_var)


###

# CONC + DIST_LAND + DIST_WATER (Model 13)
model13_tmp_break <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model13_tmp_break$parameters$theta[1] = log(1e3)
model13_tmp_break$mapArg = list(theta = factor(c(NA, 1:3)))
model13_break <- glmmTMB:::fitTMB(model13_tmp_break) 
summary(model13_break)


###

# BATH + CONC + DIST_LAND + DIST_WATER (Model 14)
model14_tmp_break <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL, doFit=F, weights=W)
model14_tmp_break$parameters$theta[1] = log(1e3)
model14_tmp_break$mapArg = list(theta = factor(c(NA, 1:4)))
model14_break <- glmmTMB:::fitTMB(model14_tmp_break) 
summary(model14_break)

# 9.2a BREAK-UP SIMPLIFIED (Nov 2021) -------

###

# New simplified models since our complex ones weren't giving us predicted values
# these ones have no weighting and a random intercept only (as opposed to also a random slope)
# Model #s align with Table 7 in manuscript

###

# import data
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
head(used_avail_RSF_breakup_FINAL)

###

# Model 1: ID + BATH
bears_breakup_m1 <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m1
summary(bears_breakup_m1)

# Model 2: ID + CONC
bears_breakup_m2 <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m2
summary(bears_breakup_m2)

# Model 3: ID + LAND
bears_breakup_m3 <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m3
summary(bears_breakup_m3)

# Model 4: ID + WATER
bears_breakup_m4 <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m4
summary(bears_breakup_m4)

# Model 5: ID + BATH + CONC
bears_breakup_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m5
summary(bears_breakup_m5)

# Model 6: ID + BATH + LAND
bears_breakup_m6 <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m6
summary(bears_breakup_m6)

# Model 7: ID + CONC + WATER
bears_breakup_m7 <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m7
summary(bears_breakup_m7)

# Model 8: ID + LAND + WATER
bears_breakup_m8 <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m8
summary(bears_breakup_m8)


# 9.3. - SKIP - FREEZE-UP -------

# use M2 for a null model instead
null_freeze <- glmmTMB(USED_AVAIL~1+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
summary(null_freeze)

###

# BATH ONLY (Model 1)
model1_tmp_freeze<- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID)+(0+BATH_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model1_tmp_freeze$parameters$theta[1] = log(1e3)
model1_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:1)))
model1_freeze <- glmmTMB:::fitTMB(model1_tmp_freeze) 
summary(model1_freeze)


###

# CONC ONLY (Model 2)
model2_tmp_freeze <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model2_tmp_freeze$parameters$theta[1] = log(1e3)
model2_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:1)))
model2_freeze <- glmmTMB:::fitTMB(model2_tmp_freeze) 
summary(model2_freeze)


###

# DIST_LAND ONLY (Model 3)
model3_tmp_freeze<- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model3_tmp_freeze$parameters$theta[1] = log(1e3)
model3_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:1)))
model3_freeze <- glmmTMB:::fitTMB(model3_tmp_freeze) 
summary(model3_freeze)


###

# DIST_WATER ONLY (Model 4)
model4_tmp_freeze <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model4_tmp_freeze$parameters$theta[1] = log(1e3)
model4_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:1)))
model4_freeze <- glmmTMB:::fitTMB(model4_tmp_freeze) 
summary(model4_freeze)


###

# BATH + CONC (Model 5)
model5_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model5_tmp_freeze$parameters$theta[1] = log(1e3)
model5_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model5_freeze <- glmmTMB:::fitTMB(model5_tmp_freeze) 
summary(model5_freeze)


###

# BATH + DIST_LAND (Model 6)
model6_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model6_tmp_freeze$parameters$theta[1] = log(1e3)
model6_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model6_freeze <- glmmTMB:::fitTMB(model6_tmp_freeze) 
summary(model6_freeze)


###

# BATH + DIST_WATER (Model 7)
# ignore the first attempt, we get errors and  NAs for AIC, BIC, LogLik, and deviance
#model7_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
#model7_tmp_freeze$parameters$theta[1] = log(1e3)
#model7_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
#model7_freeze <- glmmTMB:::fitTMB(model7_tmp_freeze) 
#summary(model7_freeze)

# Instead, don't fix intercept variance, but estimate it from the data
model7_freeze_free_var <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=TRUE, weights=W)
summary(model7_freeze_free_var)


###

# CONC + DIST_LAND (Model 8)
model8_tmp_freeze <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model8_tmp_freeze$parameters$theta[1] = log(1e3)
model8_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model8_freeze <- glmmTMB:::fitTMB(model8_tmp_freeze) 
summary(model8_freeze)


###

# CONC + DIST_WATER (Model 9) 
model9_tmp_freeze <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model9_tmp_freeze$parameters$theta[1] = log(1e3)
model9_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model9_freeze <- glmmTMB:::fitTMB(model9_tmp_freeze) 
summary(model9_freeze)


###

# DIST_LAND + DIST_WATER (Model 10)
model10_tmp_freeze <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model10_tmp_freeze$parameters$theta[1] = log(1e3)
model10_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:2)))
model10_freeze <- glmmTMB:::fitTMB(model10_tmp_freeze) 
summary(model10_freeze)


###

# BATH + CONC + DIST_LAND (Model 11)
model11_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model11_tmp_freeze$parameters$theta[1] = log(1e3)
model11_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:3)))
model11_freeze <- glmmTMB:::fitTMB(model11_tmp_freeze) 
summary(model11_freeze)


###

# BATH + CONC + DIST_WATER (Model 12)
model12_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model12_tmp_freeze$parameters$theta[1] = log(1e3)
model12_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:3)))
model12_freeze <- glmmTMB:::fitTMB(model12_tmp_freeze) 
summary(model12_freeze)


###

# CONC + DIST_LAND + DIST_WATER (Model 13)
model13_tmp_freeze <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model13_tmp_freeze$parameters$theta[1] = log(1e3)
model13_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:3)))
model13_freeze <- glmmTMB:::fitTMB(model13_tmp_freeze) 
summary(model13_freeze)


###

# BATH + CONC + DIST_LAND + DIST_WATER (Model 14)
model14_tmp_freeze <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID)+(0+BATH_SCALED|ID)+(0+CONC_SCALED|ID)+(0+DIST_SCALED|ID)+(0+DIST_WATER_SCALED|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL, doFit=F, weights=W)
model14_tmp_freeze$parameters$theta[1] = log(1e3)
model14_tmp_freeze$mapArg = list(theta = factor(c(NA, 1:4)))
model14_freeze <- glmmTMB:::fitTMB(model14_tmp_freeze) 
summary(model14_freeze)


# 9.3a FREEZE-UP SIMPLIFIED (Nov 2021) -------

###

# New simplified models since our complex ones weren't giving us predicted values
# these ones have no weighting and a random intercept only (as opposed to also a random slope)
# Model #s align with Table 7 in manuscript

###

# import data
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")
head(used_avail_RSF_freezeup_FINAL)

###

# Model 1: ID + BATH
bears_freezeup_m1 <- glmmTMB(USED_AVAIL~BATH_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m1
summary(bears_freezeup_m1)

# Model 2: ID + CONC
bears_freezeup_m2 <- glmmTMB(USED_AVAIL~CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m2
summary(bears_freezeup_m2)

# Model 3: ID + LAND
bears_freezeup_m3 <- glmmTMB(USED_AVAIL~DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m3
summary(bears_freezeup_m3)

# Model 4: ID + WATER
bears_freezeup_m4 <- glmmTMB(USED_AVAIL~DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m4
summary(bears_freezeup_m4)

# Model 5: ID + BATH + CONC
bears_freezeup_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5
summary(bears_freezeup_m5)

# Model 6: ID + BATH + LAND
bears_freezeup_m6 <- glmmTMB(USED_AVAIL~BATH_SCALED+DIST_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m6
summary(bears_freezeup_m6)

# Model 7: ID + CONC + WATER
bears_freezeup_m7 <- glmmTMB(USED_AVAIL~CONC_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m7
summary(bears_freezeup_m7)

# Model 8: ID + LAND + WATER
bears_freezeup_m8 <- glmmTMB(USED_AVAIL~DIST_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m8
summary(bears_freezeup_m8)


# 10a. Interpreting and visualizing top model results ---------


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


# 10b. - SKIP - Figures for manuscript ----------

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/final_dataset_Jun2021.csv")
head(used_avail)
used_avail$W <- ifelse(used_avail$USE == "used", 1, 1000)


used <- used_avail %>% filter(USED_AVAIL=="used")
Freeze <- used_avail %>% filter(SEASON=="freeze")
Winter <- used_avail %>% filter(SEASON=="winter")
Break <- used_avail %>% filter(SEASON=="break")


###

# pooled top model included: BATH, CONC, and DIST_LAND

ggplot(used_avail) + geom_density(aes(x=BATH, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Depth (m)", y="Relative Probability") +
  ggtitle("Bathymetry") +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(used_avail) + geom_density(aes(x=CONC, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Concentration (%)", y="Relative Probability") +
  ggtitle("Sea ice concentration") +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=c("0", "20", "40", "60", "80", "100")) +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(used_avail) + geom_density(aes(x=DIST_LAND, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to land") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(0, 500000), breaks=c(0, 100000, 200000, 300000, 400000, 500000), labels=c("0", "100000", "200000", "300000", "400000", "500000")) +
  theme_nuwcru()


### Seasonal models

# freeze-up top model included: BATH, and DIST_WATER

ggplot(Freeze) + geom_density(aes(x=BATH, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Depth (m)", y="Relative Probability") +
  ggtitle("Bathymetry") +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(Freeze) + geom_density(aes(x=DIST_WATER, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to water") +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

# winter top model included: CONC and DIST_LAND
ggplot(Winter) + geom_density(aes(x=CONC, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Concentration (%)", y="Relative Probability") +
  ggtitle("Sea ice concentration") +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=c("0", "20", "40", "60", "80", "100")) +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(Winter) + geom_density(aes(x=DIST_LAND, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to land") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(0, 500000), breaks=c(0, 100000, 200000, 300000, 400000, 500000), labels=c("0", "100000", "200000", "300000", "400000", "500000")) +
  theme_nuwcru()
summary(Winter)

# break-up top model included: CONC and DIST_LAND
ggplot(Break) + geom_density(aes(x=CONC, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Concentration (%)", y="Relative Probability") +
  ggtitle("Sea ice concentration") +
  scale_x_continuous(breaks=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), labels=c("0", "20", "40", "60", "80", "100")) +
  theme(legend.title=element_blank()) +
  theme_nuwcru()

ggplot(Break) + geom_density(aes(x=DIST_LAND, fill=USED_AVAIL), alpha=0.2) +
  labs(x="Distance (km)", y="Relative Probability") +
  ggtitle("Distance to land") +
  theme(legend.title=element_blank()) +
  scale_x_continuous(limits=c(0, 400000), breaks=c(0, 100000, 200000, 300000, 400000), labels=c("0", "100000", "200000", "300000", "400000")) +
  theme_nuwcru()






###

# IGNORE BELOW

###


# TESTING PLOTS

# frequency - i.e., histograms

hist(used$BATH)

      # OR
h <- hist(used$BATH, breaks=100, plot=FALSE)
h$counts=h$counts/sum(h$counts)
plot(h)

      # OR
ggplot(data=used) + geom_histogram(aes(x=BATH))


###


# density
hist(used$BATH, xlab="Ocean depth (m)", freq=FALSE)
lines(density(used$BATH))
#rug(used$BATH)

      # OR
BATH_d <- density(used$BATH)
plot(BATH_d)


###


# relative frequency

h <- hist(used$BATH, plot=FALSE)
h$density = h$counts/sum(h$counts) * 100
plot(h, main="Bathymetry", xlab="Depth (m)", ylab="Percent", col="grey", freq=FALSE)

      # OR
ggplot(data=used) +
  geom_histogram(mapping=aes(x=BATH, y=..count../sum(..count..)*100), bins=50) +
  geom_line(aes(x=density(BATH))) +
  ggtitle("Bathymetry") +
  xlab("Depth (m)") +
  ylab("Percent")

      # OR

p <- ggplot(used) +
  geom_histogram(aes(x=BATH, y=..density..), binwidth=100, fill="grey", color="black")
p




# plotting seasons altogether
      # using ggplot
plot(density(used$BATH))
head(used)
ggplot(used) + geom_density(aes(x=BATH, colour=SEASON))
ggplot(used) + geom_density(aes(x=BATH, fill=SEASON), alpha=0.2)
ggplot(used_avail) + geom_density(aes(x=BATH)) + facet_wrap(~SEASON) + theme_nuwcru() # https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html
ggplot(used_avail) + geom_density(aes(x=BATH, fill=USED_AVAIL), alpha=0.2) + 
  facet_wrap(~SEASON) + theme_nuwcru() # used and avail together

      # using lattice
densityplot(~BATH | SEASON, data=used) # https://homepage.divms.uiowa.edu/~luke/classes/STAT4580/histdens.html


###





# 11. Relative probability plots for simplified models --------

###

# Code is from Peter!

###

# run top models

# winter: Model 5 (ID + BATH + CONC)
bears_winter_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m5
summary(bears_winter_m5)

# Model 5: ID + BATH + CONC with weighting
#bears_winter_m5_weight <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL, weights=W)
#bears_winter_m5_weight
#summary(bears_winter_m5_weight)

# break-up: Model 5 (ID + BATH + CONC)
bears_breakup_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m5
summary(bears_breakup_m5)

# freeze-up: Model 5 (ID + BATH + CONC)
bears_freezeup_m5 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5
summary(bears_freezeup_m5)


###

# winter
      # CONC
winter_median_BATH = median(used_avail_RSF_winter_FINAL$BATH_SCALED)
coefs_winter = coef(bears_winter_m5)
coefs_winter = coefs_winter$cond$ID
winter_prediction = winter_median_BATH * coefs_winter$BATH_SCALED[1] + coefs_winter$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$CONC)) / sd(used_avail_RSF_winter_FINAL$CONC) * coefs_winter$CONC_SCALED[1] + winter_prediction))), 
      xlim = range(used_avail_RSF_winter_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Winter", ylab = "Relative probability of selection")
      # BATH
winter_median_CONC = median(used_avail_RSF_winter_FINAL$CONC_SCALED)
winter_prediction2 = winter_median_CONC * coefs_winter$CONC_SCALED[1] + coefs_winter$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$BATH)) / sd(used_avail_RSF_winter_FINAL$BATH) * coefs_winter$BATH_SCALED[1] + winter_prediction2))), 
      xlim = range(used_avail_RSF_winter_FINAL$BATH), ylim = c(0,1), xlab = "Ocean depth (m) \n Winter", ylab = "Relative probability of selection")

      # CONC: weighting
#winter_median_BATH = median(used_avail_RSF_winter_FINAL$BATH_SCALED)
#coefs_winter = coef(bears_winter_m5_weight)
#coefs_winter = coefs_winter$cond$ID
#winter_prediction = winter_median_BATH * coefs_winter$BATH_SCALED[1] + coefs_winter$`(Intercept)`[1]
#curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$CONC)) / sd(used_avail_RSF_winter_FINAL$CONC) * coefs_winter$CONC_SCALED[1] + winter_prediction))), 
 #     xlim = range(used_avail_RSF_winter_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Winter", ylab = "Relative probability of selection")
      # BATH: weighting
#winter_median_CONC = median(used_avail_RSF_winter_FINAL$CONC_SCALED)
#winter_prediction2 = winter_median_CONC * coefs_winter$CONC_SCALED[1] + coefs_winter$`(Intercept)`[1]
#curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$BATH)) / sd(used_avail_RSF_winter_FINAL$BATH) * coefs_winter$BATH_SCALED[1] + winter_prediction2))), 
 #     xlim = range(used_avail_RSF_winter_FINAL$BATH), ylim = c(0,1), xlab = "Ocean depth (m) \n Winter", ylab = "Relative probability of selection")



# break-up
      # CONC
break_median_BATH = median(used_avail_RSF_breakup_FINAL$BATH_SCALED)
coefs_break = coef(bears_breakup_m5)
coefs_break = coefs_break$cond$ID
break_prediction = break_median_BATH * coefs_break$BATH_SCALED[1] + coefs_break$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_breakup_FINAL$CONC)) / sd(used_avail_RSF_breakup_FINAL$CONC) * coefs_break$CONC_SCALED[1] + break_prediction))), 
      xlim = range(used_avail_RSF_breakup_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Break-up", ylab = "Relative probability of selection")
      # BATH
break_median_CONC = median(used_avail_RSF_breakup_FINAL$CONC_SCALED)
break_prediction2 = break_median_CONC * coefs_break$CONC_SCALED[1] + coefs_break$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_breakup_FINAL$BATH)) / sd(used_avail_RSF_breakup_FINAL$BATH) * coefs_break$BATH_SCALED[1] + break_prediction2))), 
      xlim = range(used_avail_RSF_breakup_FINAL$BATH), ylim = c(0,1), xlab = "Ocean depth (m) \n Break-up", ylab = "Relative probability of selection")


# freeze-up
      # CONC
freeze_median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freeze = coef(bears_freezeup_m5)
coefs_freeze = coefs_freeze$cond$ID
freeze_prediction = freeze_median_BATH * coefs_freeze$BATH_SCALED[1] + coefs_freeze$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * coefs_freeze$CONC_SCALED[1] + freeze_prediction))), 
      xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), xlab = "Sea ice concentration \n Freeze-up", ylab = "Relative probability of selection")
      # BATH
freeze_median_CONC = median(used_avail_RSF_freezeup_FINAL$CONC_SCALED)
freeze_prediction2 = freeze_median_CONC * coefs_freeze$CONC_SCALED[1] + coefs_freeze$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * coefs_freeze$BATH_SCALED[1] + freeze_prediction2))), 
      xlim = range(used_avail_RSF_freezeup_FINAL$BATH), ylim = c(0,1), xlab = "Ocean depth (m) \n Freeze-up", ylab = "Relative probability of selection")






