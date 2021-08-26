# 1. load libraries ---------------------

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sp)
library(maptools)
library(maps)
library(mapdata)
library(lubridate)
library(amt) # step length, turning angles, and speed
library(argosfilter) # step length, turning angles, and speed - specifically for Argos data
#library(plyr) # for mean of step angles

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


# might need these?
#library(ggplot2)
#library(ggmap)
#library(dplyr)
#library(tidyr)
#library(stringr)
#library(gridExtra)



# 2. import data and format ----------


# ~~~~ NOTE: THIS HAS NOT BEEN REDONE WITH bears_final_Nov2020 THAT WAS MADE AFTER LIMITING FOR EXCESS SPEEDS ~~~~~


bears <- read.csv("data/Oct2020work/bears_seasons.csv")
bears[1, 3] = "1-11-01" # fix this date
bears$DATE2 <- as.Date(bears$DATE, "%m-%d-%y")
str(bears)
#bears$TIMESTAMP <- as.POSIXct(paste(bears$DATE, bears$TIME), format="%m-%d-%y %H:%M")
      # the timestamp column isn't necessary because there are so many NA values in the time column and I don't have daily duplicates


# SKIP - 3. plot dataset --------

# plot by month
bears$MONTH2 <- factor(bears$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

ggplot(data=bears) +
  geom_point(aes(x=LONG, y=LAT, color=MONTH2)) +
  labs(title="Location of all collared female polar bears\nin Davis Strait from 1991 to 2001", x="Longitude",
       y="Latitude", color="Month")

# plot by year
ggplot(data=bears) +
  geom_point(aes(x=LONG, y=LAT, color=YEAR)) +
  labs(title="Location of all collared female polar bears\nin Davis Strait from 1991 to 2001", x="Longitude",
       y="Latitude", color="Year")

# plot by bear
ggplot(data=bears) +
  geom_point(aes(x=LONG, y=LAT, color=ID)) +
  labs(title="Location of all collared female polar bears\nin Davis Strait from 1991 to 2001", x="Longitude",
       y="Latitude", color="Bear ID")


# easier to do maps in QGIS




# SKIP - 4. Test for spatial autocorrelation ------------------------------------------------------------------
# First visually test if there are 2 different clusters of bears (N versus S)

# Separate data into each bear, find mean of each bears' lat and long values
bear_autocorr <- bears %>%
  group_by(ID) %>%
  summarize(lat.avg=mean(LAT), long.avg=mean(LONG))

# Plot each bear's mean lat and long point, then see if there are distinct groups
ggplot(data=bear_autocorr, aes(x=long.avg, y=lat.avg)) +
  geom_point(aes(colour=ID), size=2) +
  xlab("Average Longitude") +
  ylab("Average Latitude")

# there doesn't seem to be two groups at all - tested in QGIS as well using below csv file
write.csv(bear_autocorr, "data/Oct2020work/bear_autocorr.csv")


# SKIP - 5. Movement rates - use Step 6 instead ----------------------------------------------------------------

# Used this help forum: https://stackoverflow.com/questions/30606360/subtract-value-from-previous-row-by-group?noredirect=1&lq=1
bears2 <- bears %>%
  group_by(ID) %>%
  arrange(DATE2, .by_group=TRUE) %>%
  mutate(diff=DATE2 - lag(DATE2, default = first(DATE2)))
bears2 <- as.data.frame(bears2)
str(bears2)
bears2$diff <- as.numeric(bears2$diff)
summary(bears2)
ungroup(bears)

      # difference between mean steps - values range from 3.7 to 17.0; mean of mean step length = 8.60
mean_step <- bears2 %>%
  group_by(ID) %>%
  summarize(mean(diff)) 
colnames(mean_step) = c("ID", "MEAN_STEP")

mean_step <- as.data.frame(mean_step)
mean_step$MEAN_STEP <- as.numeric(mean_step$MEAN_STEP)
summary(mean_step) 

plot(mean_step$ID, mean_step$MEAN_STEP)

ggplot(data=mean_step, aes(x=MEAN_STEP, y=ID)) +
  geom_col() +
  scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 30), labels=c("0", "5", "10", "15", "20", "25", "30")) +
  ylab("Bear ID") +
  xlab("Mean movement rate\n(mean # days between fixes)")



# 6. Step length with argosfilter package -----

# bearing: https://stackoverflow.com/questions/30633973/calculating-bearing-in-r
# distance: https://stackoverflow.com/questions/29585759/calculating-distances-from-latitude-and-longitude-coordinates-in-r
# days: https://stackoverflow.com/questions/46015550/r-programming-how-to-find-a-difference-in-value-for-every-two-consecutive-dates

# separate each bear
X30140 <- bears %>% filter(ID=="X30140")
X30135 <- bears %>% filter(ID=="X30135")
X30131 <- bears %>% filter(ID=="X30131")
X30129 <- bears %>% filter(ID=="X30129")
X30126 <- bears %>% filter(ID=="X30126")
X12080 <- bears %>% filter(ID=="X12080")
X12092 <- bears %>% filter(ID=="X12092")
X12086 <- bears %>% filter(ID=="X12086")
X11975 <- bears %>% filter(ID=="X11975")
X13746 <- bears %>% filter(ID=="X13746")
X11974 <- bears %>% filter(ID=="X11974")
X13292 <- bears %>% filter(ID=="X13292")
X13289 <- bears %>% filter(ID=="X13289")
X13437 <- bears %>% filter(ID=="X13437")
X10374 <- bears %>% filter(ID=="X10374")
X13428 <- bears %>% filter(ID=="X13428")
X10700 <- bears %>% filter(ID=="X10700")
X13284 <- bears %>% filter(ID=="X13284")
X10703 <- bears %>% filter(ID=="X10703")
X10709 <- bears %>% filter(ID=="X10709")
X12078 <- bears %>% filter(ID=="X12078")
X03956 <- bears %>% filter(ID=="X03956")
X12082 <- bears %>% filter(ID=="X12082")
X10393 <- bears %>% filter(ID=="X10393")
X12083 <- bears %>% filter(ID=="X12083")
X12081 <- bears %>% filter(ID=="X12081")
X10707 <- bears %>% filter(ID=="X10707")
X10695 <- bears %>% filter(ID=="X10695")

# bear X30140
X30140$DATE <- as.Date(X30140$DATE, "%m-%d-%y")
X30140$ANGLE[2:nrow(X30140)] <- bearingTrack(X30140$LAT, X30140$LONG) # degrees
X30140$DIST_KM[2:nrow(X30140)] <- distanceTrack(X30140$LAT, X30140$LONG) # km
X30140$DIFF_DATE <- c(NA, abs(diff(X30140$DATE2))) # number of days
X30140$PER_DAY <- X30140$DIST_KM/X30140$DIFF_DATE # km/day
X30140$PER_HR <- X30140$PER_DAY/24 # km/h
X30140$M_PER_HR <- X30140$PER_HR*1000 #m/h

# bear X30135
X30135$DATE <- as.Date(X30135$DATE, "%m-%d-%y")
X30135$ANGLE[2:nrow(X30135)] <- bearingTrack(X30135$LAT, X30135$LONG) # degrees
X30135$DIST_KM[2:nrow(X30135)] <- distanceTrack(X30135$LAT, X30135$LONG) # km
X30135$DIFF_DATE <- c(NA, abs(diff(X30135$DATE2))) # number of days
X30135$PER_DAY <- X30135$DIST_KM/X30135$DIFF_DATE # km/day
X30135$PER_HR <- X30135$PER_DAY/24 # km/h
X30135$M_PER_HR <- X30135$PER_HR*1000 #m/h

# bear X30131
X30131$DATE <- as.Date(X30131$DATE, "%m-%d-%y")
X30131$ANGLE[2:nrow(X30131)] <- bearingTrack(X30131$LAT, X30131$LONG) # degrees
X30131$DIST_KM[2:nrow(X30131)] <- distanceTrack(X30131$LAT, X30131$LONG) # km
X30131$DIFF_DATE <- c(NA, abs(diff(X30131$DATE2))) # number of days
X30131$PER_DAY <- X30131$DIST_KM/X30131$DIFF_DATE # km/day
X30131$PER_HR <- X30131$PER_DAY/24 # km/h
X30131$M_PER_HR <- X30131$PER_HR*1000 #m/h

# bear X30129
X30129$DATE <- as.Date(X30129$DATE, "%m-%d-%y")
X30129$ANGLE[2:nrow(X30129)] <- bearingTrack(X30129$LAT, X30129$LONG) # degrees
X30129$DIST_KM[2:nrow(X30129)] <- distanceTrack(X30129$LAT, X30129$LONG) # km
X30129$DIFF_DATE <- c(NA, abs(diff(X30129$DATE2))) # number of days
X30129$PER_DAY <- X30129$DIST_KM/X30129$DIFF_DATE # km/day
X30129$PER_HR <- X30129$PER_DAY/24 # km/h
X30129$M_PER_HR <- X30129$PER_HR*1000 #m/h

# bear X30126
X30126$DATE <- as.Date(X30126$DATE, "%m-%d-%y")
X30126$ANGLE[2:nrow(X30126)] <- bearingTrack(X30126$LAT, X30126$LONG) # degrees
X30126$DIST_KM[2:nrow(X30126)] <- distanceTrack(X30126$LAT, X30126$LONG) # km
X30126$DIFF_DATE <- c(NA, abs(diff(X30126$DATE2))) # number of days
X30126$PER_DAY <- X30126$DIST_KM/X30126$DIFF_DATE # km/day
X30126$PER_HR <- X30126$PER_DAY/24 # km/h
X30126$M_PER_HR <- X30126$PER_HR*1000 #m/h

# bear X12080
X12080$DATE <- as.Date(X12080$DATE, "%m-%d-%y")
X12080$ANGLE[2:nrow(X12080)] <- bearingTrack(X12080$LAT, X12080$LONG) # degrees
X12080$DIST_KM[2:nrow(X12080)] <- distanceTrack(X12080$LAT, X12080$LONG) # km
X12080$DIFF_DATE <- c(NA, abs(diff(X12080$DATE2))) # number of days
X12080$PER_DAY <- X12080$DIST_KM/X12080$DIFF_DATE # km/day
X12080$PER_HR <- X12080$PER_DAY/24 # km/h
X12080$M_PER_HR <- X12080$PER_HR*1000 #m/h

# bear X12092
X12092$DATE <- as.Date(X12092$DATE, "%m-%d-%y")
X12092$ANGLE[2:nrow(X12092)] <- bearingTrack(X12092$LAT, X12092$LONG) # degrees
X12092$DIST_KM[2:nrow(X12092)] <- distanceTrack(X12092$LAT, X12092$LONG) # km
X12092$DIFF_DATE <- c(NA, abs(diff(X12092$DATE2))) # number of days
X12092$PER_DAY <- X12092$DIST_KM/X12092$DIFF_DATE # km/day
X12092$PER_HR <- X12092$PER_DAY/24 # km/h
X12092$M_PER_HR <- X12092$PER_HR*1000 #m/h

# bear X12086
X12086$DATE <- as.Date(X12086$DATE, "%m-%d-%y")
X12086$ANGLE[2:nrow(X12086)] <- bearingTrack(X12086$LAT, X12086$LONG) # degrees
X12086$DIST_KM[2:nrow(X12086)] <- distanceTrack(X12086$LAT, X12086$LONG) # km
X12086$DIFF_DATE <- c(NA, abs(diff(X12086$DATE2))) # number of days
X12086$PER_DAY <- X12086$DIST_KM/X12086$DIFF_DATE # km/day
X12086$PER_HR <- X12086$PER_DAY/24 # km/h
X12086$M_PER_HR <- X12086$PER_HR*1000 #m/h

# bear X11975
X11975$DATE <- as.Date(X11975$DATE, "%m-%d-%y")
X11975$ANGLE[2:nrow(X11975)] <- bearingTrack(X11975$LAT, X11975$LONG) # degrees
X11975$DIST_KM[2:nrow(X11975)] <- distanceTrack(X11975$LAT, X11975$LONG) # km
X11975$DIFF_DATE <- c(NA, abs(diff(X11975$DATE2))) # number of days
X11975$PER_DAY <- X11975$DIST_KM/X11975$DIFF_DATE # km/day
X11975$PER_HR <- X11975$PER_DAY/24 # km/h
X11975$M_PER_HR <- X11975$PER_HR*1000 #m/h

# bear X13746
X13746$DATE <- as.Date(X13746$DATE, "%m-%d-%y")
X13746$ANGLE[2:nrow(X13746)] <- bearingTrack(X13746$LAT, X13746$LONG) # degrees
X13746$DIST_KM[2:nrow(X13746)] <- distanceTrack(X13746$LAT, X13746$LONG) # km
X13746$DIFF_DATE <- c(NA, abs(diff(X13746$DATE2))) # number of days
X13746$PER_DAY <- X13746$DIST_KM/X13746$DIFF_DATE # km/day
X13746$PER_HR <- X13746$PER_DAY/24 # km/h
X13746$M_PER_HR <- X13746$PER_HR*1000 #m/h

# bear X11974
X11974$DATE <- as.Date(X11974$DATE, "%m-%d-%y")
X11974$ANGLE[2:nrow(X11974)] <- bearingTrack(X11974$LAT, X11974$LONG) # degrees
X11974$DIST_KM[2:nrow(X11974)] <- distanceTrack(X11974$LAT, X11974$LONG) # km
X11974$DIFF_DATE <- c(NA, abs(diff(X11974$DATE2))) # number of days
X11974$PER_DAY <- X11974$DIST_KM/X11974$DIFF_DATE # km/day
X11974$PER_HR <- X11974$PER_DAY/24 # km/h
X11974$M_PER_HR <- X11974$PER_HR*1000 #m/h

# bear X13292
X13292$DATE <- as.Date(X13292$DATE, "%m-%d-%y")
X13292$ANGLE[2:nrow(X13292)] <- bearingTrack(X13292$LAT, X13292$LONG) # degrees
X13292$DIST_KM[2:nrow(X13292)] <- distanceTrack(X13292$LAT, X13292$LONG) # km
X13292$DIFF_DATE <- c(NA, abs(diff(X13292$DATE2))) # number of days
X13292$PER_DAY <- X13292$DIST_KM/X13292$DIFF_DATE # km/day
X13292$PER_HR <- X13292$PER_DAY/24 # km/h
X13292$M_PER_HR <- X13292$PER_HR*1000 #m/h

# bear X13289
X13289$DATE <- as.Date(X13289$DATE, "%m-%d-%y")
X13289$ANGLE[2:nrow(X13289)] <- bearingTrack(X13289$LAT, X13289$LONG) # degrees
X13289$DIST_KM[2:nrow(X13289)] <- distanceTrack(X13289$LAT, X13289$LONG) # km
X13289$DIFF_DATE <- c(NA, abs(diff(X13289$DATE2))) # number of days
X13289$PER_DAY <- X13289$DIST_KM/X13289$DIFF_DATE # km/day
X13289$PER_HR <- X13289$PER_DAY/24 # km/h
X13289$M_PER_HR <- X13289$PER_HR*1000 #m/h

# bear X13437
X13437$DATE <- as.Date(X13437$DATE, "%m-%d-%y")
X13437$ANGLE[2:nrow(X13437)] <- bearingTrack(X13437$LAT, X13437$LONG) # degrees
X13437$DIST_KM[2:nrow(X13437)] <- distanceTrack(X13437$LAT, X13437$LONG) # km
X13437$DIFF_DATE <- c(NA, abs(diff(X13437$DATE2))) # number of days
X13437$PER_DAY <- X13437$DIST_KM/X13437$DIFF_DATE # km/day
X13437$PER_HR <- X13437$PER_DAY/24 # km/h
X13437$M_PER_HR <- X13437$PER_HR*1000 #m/h

# bear X10374
X10374$DATE <- as.Date(X10374$DATE, "%m-%d-%y")
X10374$ANGLE[2:nrow(X10374)] <- bearingTrack(X10374$LAT, X10374$LONG) # degrees
X10374$DIST_KM[2:nrow(X10374)] <- distanceTrack(X10374$LAT, X10374$LONG) # km
X10374$DIFF_DATE <- c(NA, abs(diff(X10374$DATE2))) # number of days
X10374$PER_DAY <- X10374$DIST_KM/X10374$DIFF_DATE # km/day
X10374$PER_HR <- X10374$PER_DAY/24 # km/h
X10374$M_PER_HR <- X10374$PER_HR*1000 #m/h

# bear X13428
X13428$DATE <- as.Date(X13428$DATE, "%m-%d-%y")
X13428$ANGLE[2:nrow(X13428)] <- bearingTrack(X13428$LAT, X13428$LONG) # degrees
X13428$DIST_KM[2:nrow(X13428)] <- distanceTrack(X13428$LAT, X13428$LONG) # km
X13428$DIFF_DATE <- c(NA, abs(diff(X13428$DATE2))) # number of days
X13428$PER_DAY <- X13428$DIST_KM/X13428$DIFF_DATE # km/day
X13428$PER_HR <- X13428$PER_DAY/24 # km/h
X13428$M_PER_HR <- X13428$PER_HR*1000 #m/h

# bear X10700
X10700$DATE <- as.Date(X10700$DATE, "%m-%d-%y")
X10700$ANGLE[2:nrow(X10700)] <- bearingTrack(X10700$LAT, X10700$LONG) # degrees
X10700$DIST_KM[2:nrow(X10700)] <- distanceTrack(X10700$LAT, X10700$LONG) # km
X10700$DIFF_DATE <- c(NA, abs(diff(X10700$DATE2))) # number of days
X10700$PER_DAY <- X10700$DIST_KM/X10700$DIFF_DATE # km/day
X10700$PER_HR <- X10700$PER_DAY/24 # km/h
X10700$M_PER_HR <- X10700$PER_HR*1000 #m/h

# bear X13284
X13284$DATE <- as.Date(X13284$DATE, "%m-%d-%y")
X13284$ANGLE[2:nrow(X13284)] <- bearingTrack(X13284$LAT, X13284$LONG) # degrees
X13284$DIST_KM[2:nrow(X13284)] <- distanceTrack(X13284$LAT, X13284$LONG) # km
X13284$DIFF_DATE <- c(NA, abs(diff(X13284$DATE2))) # number of days
X13284$PER_DAY <- X13284$DIST_KM/X13284$DIFF_DATE # km/day
X13284$PER_HR <- X13284$PER_DAY/24 # km/h
X13284$M_PER_HR <- X13284$PER_HR*1000 #m/h

# bear X10703
X10703$DATE <- as.Date(X10703$DATE, "%m-%d-%y")
X10703$ANGLE[2:nrow(X10703)] <- bearingTrack(X10703$LAT, X10703$LONG) # degrees
X10703$DIST_KM[2:nrow(X10703)] <- distanceTrack(X10703$LAT, X10703$LONG) # km
X10703$DIFF_DATE <- c(NA, abs(diff(X10703$DATE2))) # number of days
X10703$PER_DAY <- X10703$DIST_KM/X10703$DIFF_DATE # km/day
X10703$PER_HR <- X10703$PER_DAY/24 # km/h
X10703$M_PER_HR <- X10703$PER_HR*1000 #m/h

# bear X10709
X10709$DATE <- as.Date(X10709$DATE, "%m-%d-%y")
X10709$ANGLE[2:nrow(X10709)] <- bearingTrack(X10709$LAT, X10709$LONG) # degrees
X10709$DIST_KM[2:nrow(X10709)] <- distanceTrack(X10709$LAT, X10709$LONG) # km
X10709$DIFF_DATE <- c(NA, abs(diff(X10709$DATE2))) # number of days
X10709$PER_DAY <- X10709$DIST_KM/X10709$DIFF_DATE # km/day
X10709$PER_HR <- X10709$PER_DAY/24 # km/h
X10709$M_PER_HR <- X10709$PER_HR*1000 #m/h

# bear X12078
X12078$DATE <- as.Date(X12078$DATE, "%m-%d-%y")
X12078$ANGLE[2:nrow(X12078)] <- bearingTrack(X12078$LAT, X12078$LONG) # degrees
X12078$DIST_KM[2:nrow(X12078)] <- distanceTrack(X12078$LAT, X12078$LONG) # km
X12078$DIFF_DATE <- c(NA, abs(diff(X12078$DATE2))) # number of days
X12078$PER_DAY <- X12078$DIST_KM/X12078$DIFF_DATE # km/day
X12078$PER_HR <- X12078$PER_DAY/24 # km/h
X12078$M_PER_HR <- X12078$PER_HR*1000 #m/h

# bear X03956
X03956$DATE <- as.Date(X03956$DATE, "%m-%d-%y")
X03956$ANGLE[2:nrow(X03956)] <- bearingTrack(X03956$LAT, X03956$LONG) # degrees
X03956$DIST_KM[2:nrow(X03956)] <- distanceTrack(X03956$LAT, X03956$LONG) # km
X03956$DIFF_DATE <- c(NA, abs(diff(X03956$DATE2))) # number of days
X03956$PER_DAY <- X03956$DIST_KM/X03956$DIFF_DATE # km/day
X03956$PER_HR <- X03956$PER_DAY/24 # km/h
X03956$M_PER_HR <- X03956$PER_HR*1000 #m/h

# bear X12082
X12082$DATE <- as.Date(X12082$DATE, "%m-%d-%y")
X12082$ANGLE[2:nrow(X12082)] <- bearingTrack(X12082$LAT, X12082$LONG) # degrees
X12082$DIST_KM[2:nrow(X12082)] <- distanceTrack(X12082$LAT, X12082$LONG) # km
X12082$DIFF_DATE <- c(NA, abs(diff(X12082$DATE2))) # number of days
X12082$PER_DAY <- X12082$DIST_KM/X12082$DIFF_DATE # km/day
X12082$PER_HR <- X12082$PER_DAY/24 # km/h
X12082$M_PER_HR <- X12082$PER_HR*1000 #m/h

# bear X10393
X10393$DATE <- as.Date(X10393$DATE, "%m-%d-%y")
X10393$ANGLE[2:nrow(X10393)] <- bearingTrack(X10393$LAT, X10393$LONG) # degrees
X10393$DIST_KM[2:nrow(X10393)] <- distanceTrack(X10393$LAT, X10393$LONG) # km
X10393$DIFF_DATE <- c(NA, abs(diff(X10393$DATE2))) # number of days
X10393$PER_DAY <- X10393$DIST_KM/X10393$DIFF_DATE # km/day
X10393$PER_HR <- X10393$PER_DAY/24 # km/h
X10393$M_PER_HR <- X10393$PER_HR*1000 #m/h

# bear X12083
X12083$DATE <- as.Date(X12083$DATE, "%m-%d-%y")
X12083$ANGLE[2:nrow(X12083)] <- bearingTrack(X12083$LAT, X12083$LONG) # degrees
X12083$DIST_KM[2:nrow(X12083)] <- distanceTrack(X12083$LAT, X12083$LONG) # km
X12083$DIFF_DATE <- c(NA, abs(diff(X12083$DATE2))) # number of days
X12083$PER_DAY <- X12083$DIST_KM/X12083$DIFF_DATE # km/day
X12083$PER_HR <- X12083$PER_DAY/24 # km/h
X12083$M_PER_HR <- X12083$PER_HR*1000 #m/h

# bear X12081
X12081$DATE <- as.Date(X12081$DATE, "%m-%d-%y")
X12081$ANGLE[2:nrow(X12081)] <- bearingTrack(X12081$LAT, X12081$LONG) # degrees
X12081$DIST_KM[2:nrow(X12081)] <- distanceTrack(X12081$LAT, X12081$LONG) # km
X12081$DIFF_DATE <- c(NA, abs(diff(X12081$DATE2))) # number of days
X12081$PER_DAY <- X12081$DIST_KM/X12081$DIFF_DATE # km/day
X12081$PER_HR <- X12081$PER_DAY/24 # km/h
X12081$M_PER_HR <- X12081$PER_HR*1000 #m/h

# bear X10707
X10707$DATE <- as.Date(X10707$DATE, "%m-%d-%y")
X10707$ANGLE[2:nrow(X10707)] <- bearingTrack(X10707$LAT, X10707$LONG) # degrees
X10707$DIST_KM[2:nrow(X10707)] <- distanceTrack(X10707$LAT, X10707$LONG) # km
X10707$DIFF_DATE <- c(NA, abs(diff(X10707$DATE2))) # number of days
X10707$PER_DAY <- X10707$DIST_KM/X10707$DIFF_DATE # km/day
X10707$PER_HR <- X10707$PER_DAY/24 # km/h
X10707$M_PER_HR <- X10707$PER_HR*1000 #m/h

# bear X10695
X10695$DATE <- as.Date(X10695$DATE, "%m-%d-%y")
X10695$ANGLE[2:nrow(X10695)] <- bearingTrack(X10695$LAT, X10695$LONG) # degrees
X10695$DIST_KM[2:nrow(X10695)] <- distanceTrack(X10695$LAT, X10695$LONG) # km
X10695$DIFF_DATE <- c(NA, abs(diff(X10695$DATE2))) # number of days
X10695$PER_DAY <- X10695$DIST_KM/X10695$DIFF_DATE # km/day
X10695$PER_HR <- X10695$PER_DAY/24 # km/h
X10695$M_PER_HR <- X10695$PER_HR*1000 #m/h


# combine all together and plot
bears_step_angles <- rbind(X30140, X30135, X30131, X30129, X30126, X12080, X12092, X12086, X11975, X13746, X11974, X13292, X13289, X13437, X10374, X13428, X10700, X13284, X10703, X10709, X12078, X03956, X12082, X10393, X12083, X12081, X10707, X10695)

summary(bears_step_angles)

      # plot step length
ggplot(data=bears_step_angles, aes(DIST_KM, ID)) +
  geom_point(colour="darkgrey") +
  geom_vline(xintercept=62.21, linetype="dashed", colour="darkred", size=.75) + #mean
  scale_x_continuous(breaks=c(0, 200, 400, 600, 800, 1000), labels=c("0", "200", "400", "600", "800", "1000")) +
  ylab("Bear ID") +
  xlab("Step length (Km)")

steplength_boxplot <- ggplot(data=bears_step_angles, aes(y=DIST_KM)) +
  geom_boxplot(width=0.5, alpha=0.2) +
  scale_y_continuous(breaks=c(0, 200, 400, 600, 800, 1000), labels=c("0", "200", "400", "600", "800", "1000")) +
  ylab("Distance (Km) between consecutive points") +
  xlab("Total step lengths per bear")
steplength_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

      # plot mean step lengths 
bears_distance <- bears_step_angles
bears_distance=na.omit(bears_distance[, c("ID", "DIST_KM")])
str(bears_distance)
bears_distance_mean <- bears_distance %>%
  group_by(ID) %>%
  summarize(avg=mean(DIST_KM))
summary(bears_distance_mean)

meansteplength_boxplot <- ggplot(data=bears_distance_mean, aes(y=avg)) +
  geom_boxplot(width=0.5, alpha=0.2) +
  ylab("Distance (Km) between consecutive points") +
  xlab("Mean step lengths per bear")
meansteplength_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


      # plot movement rate
movementrate_boxplot <- ggplot(data=bears_step_angles, aes(y=DIFF_DATE)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(breaks=c(0, 50, 100, 150, 200, 250, 300, 350, 400), labels=c("0", "50", "100", "150", "200", "250", "300", "350", "400")) +
  ylab("Number of days between consecutive points") +
  xlab("Total movement rates per bear")
movementrate_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

      # plot mean movement rates
bears_days <- bears_step_angles
bears_days=na.omit(bears_days[, c("ID", "DIFF_DATE")])
str(bears_days)
as.data.frame(bears_days)
bears_days_mean <- bears_days %>%
  group_by(ID) %>%
  summarize(avg=mean(DIFF_DATE))
summary(bears_days_mean)

meandays_boxplot <- ggplot(data=bears_days_mean, aes(y=avg)) +
  geom_boxplot(width=0.5, alpha=0.2) +
  ylab("Number of days between consecutive points") +
  xlab("Mean movement rates per bear")
meandays_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


      # plot turning angles
angles_boxplot <- ggplot(data=bears_step_angles, aes(y=ANGLE)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(breaks=c(-200, -100, 0, 100, 200), labels=c("-200", "-100", "0", "100", "200")) +
  ylab("Turning angles between consecutive points (degrees)")
angles_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


      # plot proper movement rates using PER_HR columns
summary(bears_step_angles)
ggplot(data=bears_step_angles, aes(PER_HR, ID)) +
  geom_point(colour="darkgrey") +
  geom_vline(xintercept=0.45005, linetype="dashed", colour="darkred", size=.75) + #mean
  scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10), labels=c("0", "2", "4", "6", "8", "10")) +
  ylab("Bear ID") +
  xlab("Movement rates (Km/h)\n\nNote: dashed red line indicates mean (0.45 km/h)")

ggplot(data=bears_step_angles, aes(PER_HR)) +
  geom_histogram(colour="darkgrey") +
  xlab("Movement rates (Km/h)")

ggplot(data=bears_step_angles, aes(M_PER_HR)) +
  geom_histogram(colour="darkgrey") +
  xlab("Movement rates (m/h)")













