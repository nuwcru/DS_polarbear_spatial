# SKIP - 1. load libraries ---------------------

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(argosfilter) # for section 6 (speed limiter)
library(lubridate)
library(ggpubr) # section 6 (ggdensity function)

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


# SKIP - 2. Load data and format ---------

# original dataset from ECCC - original has 2751 records
bears <- read.csv("data/DS telemetry __ with highlighted changes.csv", stringsAsFactors = F)
colnames(bears)

# remove unneccessary columns
bears=subset(bears, select=-c(POP, S_LONG, LONG, DEN, X24H.1, X24H.2, X24H.3, X24H.4, TEMP1, TEMP2, C0.LOC, REMARK, DATA))

# remove any NA values
bears=na.omit(bears[, c("BEAR", "DATE", "DAY", "MONTH", "YEAR", "TIME", "LAT", "LONGn")])

# Add ROWID column
bears <- bears %>% mutate(ROWID = row_number())

# rename ID and longitude columns - now we have 1903 records
colnames(bears) = c("ID", "DATE", "DAY", "MONTH", "YEAR", "TIME", "LAT", "LONG", "ROWID")
head(bears)

# export as csv and view in QGIS to determine outliers or other issues
write.csv(bears, "data/Oct2020work/bears.csv")

# SKIP - 3. Remove outliers, duplicates, or other issues -----------

# remove male bear (X12079) - now we have 1895 records
bears <- bears[bears$ID != 'X12079', ]
bears %>% filter(ID=="X12079") # this worked

# remove 1 outlier (E. coast of Greenland)
plot(bears$LONG, bears$LAT)
bears %>% filter(LONG>-40) # Checked that ROWID 1541 is the outlier in QGIS as well 
bears <- bears[bears$ROWID != '1541', ]
bears %>% filter(ROWID=="1541") # this worked

# finding daily duplicates
      # used this site: https://stackoverflow.com/questions/16905425/find-duplicate-values-in-r
duplicates <- data.frame(table(bears$DATE, bears$ID)) # this tells me how many fixes I have for one bear on each date
duplicates2 <- duplicates[duplicates$Freq>1,] # this tells me where there are duplicates of the same bear on one date (n=34)

# remove daily duplicates 
      # reviewed first, deleted them altogether at the end
      # kept first entry of the day unless there was no time associated with one, then I removed the one with no time
      
      # X10695 on 4-04-91
bears %>% filter(DATE=="4-04-91" & ID=="X10695") # Remove ROWID 3
      #	X10695 on 6-02-91
bears %>% filter(DATE=="6-02-91" & ID=="X10695") # Remove ROWID 21
      # X10700 on 10-04-93
bears %>% filter(DATE=="10-04-93" & ID=="X10700") # Remove ROWID 578
      # X10700 on 11-08-93
bears %>% filter(DATE=="11-08-93" & ID=="X10700") # Remove ROWID 613
      # X10700 on 4-07-92
bears %>% filter(DATE=="4-07-92" & ID=="X10700") # Remove ROWID 90
      # X10700 on 5-07-92
bears %>% filter(DATE=="5-07-92" & ID=="X10700") # Remove ROWID 122
      # X10700 on 5-17-92
bears %>% filter(DATE=="5-17-92" & ID=="X10700") # Remove ROWID 130
      # X10700 on 6-11-93
bears %>% filter(DATE=="6-11-93" & ID=="X10700") # Remove ROWID 455
      # X10703 on 4-08-92
bears %>% filter(DATE=="4-08-92" & ID=="X10703") # Remove ROWID 92
      # X10703 on 9-25-92
bears %>% filter(DATE=="9-25-92" & ID=="X10703") # Remove ROWID 217
      # X10707 on 3-30-93
bears %>% filter(DATE=="3-30-93" & ID=="X10707") # Remove ROWID 342
      # X10707 on 4-09-92
bears %>% filter(DATE=="4-09-92" & ID=="X10707") # Remove ROWID 94
      # X10707 on 8-02-93
bears %>% filter(DATE=="8-02-93" & ID=="X10707") # Remove ROWID 523
      # X10707 on 9-01-92 - there are 5 fixes here!
bears %>% filter(DATE=="9-01-92" & ID=="X10707") # Remove ROWID 205, 207, 208, 209
      # X10707 on 9-06-92 - 3 here
bears %>% filter(DATE=="9-06-92" & ID=="X10707") # Remove ROWID 213, 214
      # X10709 on 10-06-93
bears %>% filter(DATE=="10-06-93" & ID=="X10709") # Remove ROWID 581
      # X10709 on 11-10-93
bears %>% filter(DATE=="11-10-93" & ID=="X10709") # Remove ROWID 615
      # X10709 on 4-09-92
bears %>% filter(DATE=="4-09-92" & ID=="X10709") # Remove ROWID 96
      # X10709 on 4-24-93	
bears %>% filter(DATE=="4-24-93" & ID=="X10709") # Remove ROWID 384
      # X10709 on 6-28-93
bears %>% filter(DATE=="6-28-93" & ID=="X10709") # Remove ROWID 483
      # X10709 on 7-18-93
bears %>% filter(DATE=="7-18-93" & ID=="X10709") # Remove ROWID 508
      # X10709 on 8-02-93 - 3 here
bears %>% filter(DATE=="8-02-93" & ID=="X10709") # Remove ROWID 524, 526
      # X10709 on 9-01-93
bears %>% filter(DATE=="9-01-93" & ID=="X10709") # Remove ROWID 553
      # X10709 on 9-26-93	- 3 here
bears %>% filter(DATE=="9-26-93" & ID=="X10709") # Remove ROWID 568, 570
      # X11975 on 1-10-95
bears %>% filter(DATE=="1-10-95" & ID=="X11975") # Remove ROWID 1231
      # X11975 on 4-05-93 - no time on one fix here
bears %>% filter(DATE=="4-05-93" & ID=="X11975") # Remove ROWID 355
      # X12080 on 2-05-95 - both the exact same
bears %>% filter(DATE=="2-05-95" & ID=="X12080") # Remove ROWID 1258
      # X12080 on 3-22-94
bears %>% filter(DATE=="3-22-94" & ID=="X12080") # Remove ROWID 762
      # X12080 on 6-05-95
bears %>% filter(DATE=="6-05-95" & ID=="X12080") # Remove ROWID 1449
      # X12080 on 9-18-95
bears %>% filter(DATE=="9-18-95" & ID=="X12080") # Remove ROWID 1519
      # X12086 on 2-13-95 - both the exact same
bears %>% filter(DATE=="2-13-95" & ID=="X12086") # Remove ROWID 1267
      # X12086 on 2-28-95 - both the exact same
bears %>% filter(DATE=="2-28-95" & ID=="X12086") # Remove ROWID 1285
      # X12092 on 6-25-94
bears %>% filter(DATE=="6-25-94" & ID=="X12092") # Remove ROWID 1011
      # X30126 on 9-04-99
bears %>% filter(DATE=="9-04-99" & ID=="X30126") # Remove ROWID 1881


duplicates_toremove <- bears %>%
  filter(ROWID %in% c("3", "21", "578", "613", "90", "122", "130", "455", "92", "217", "94", "523", "205", "207", "208", "209", "213", "214", "581", "615", "96", "384", "483", "508", "524", "526", "553", "568", "570", "1231", "355", "1258", "762", "1449", "1519", "1267", "1285", "1011", "1881"))

bears2 <- anti_join(bears, duplicates_toremove) # 1855 fixes now
bears2 %>% filter(ROWID=="3") # this worked

# test that there aren't any more duplicates
test <- data.frame(table(bears2$DATE, bears2$ID)) 
test2 <- test[test$Freq>1,] 

# missed one! - X10707 on 3-30-93
bears2 %>% filter(DATE=="3-30-93" & ID=="X10707") # Remove ROWID 342
bears2 <- bears2[bears2$ROWID != '342', ]

# check again
test3 <- data.frame(table(bears2$DATE, bears2$ID)) 
test4 <- duplicates[test3$Freq>1,] # this worked

# bears2 now has 1854 variables

# export as csv and view in QGIS to determine any other issues and add land/ice column
write.csv(bears2, "data/Oct2020work/bears2.csv")



# SKIP - 4. Add land_ice column for analyzing later and create final dataframe ------------

# import and format new csv file of land fixes that I made in QGIS
bears_land <- read.csv("data/Oct2020work/bears_land.csv")
str(bears_land)
as.Date(bears_land$DATE, "%m-%d-%Y")
head(bears_land) # already has ICE_LAND column

# fill in ICE_LAND column for bears_land and remove extra column
bears_land$ICE_LAND <- 'land'
head(bears_land) # this worked
bears_land=subset(bears_land, select=-c(field_1))

# use anti_join to create a bears_ice dataframe - these are all the fixes not on land
bears_ice <- anti_join(bears2, bears_land)
write.csv(bears_ice, "data/Oct2020work/bears_ice.csv") # made this for mapping in QGIS

# create ICE_LAND column in bears_ice and fill
list <- 1:1390 # same length as bears2
ICE_LAND <- rep("ice", length(list))
bears_ice <- cbind(bears_ice, ICE_LAND)
head(bears_ice) # this worked

# combine dataframes and make final dataframe
bears_final <- rbind(bears_land, bears_ice) # this works!

write.csv(bears_final, "data/Oct2020work/bears_final.csv")



# Use bears_final from now on!




# SKIP - 5. Reviewing spatial duplicates ----------


# finding spatial duplicates
spatial_duplicates <- data.frame(table(bears_final$LAT, bears_final$LONG))
spatial_duplicates2 <- spatial_duplicates[spatial_duplicates$Freq>1,]
head(spatial_duplicates2)
colnames(spatial_duplicates2) = c("LAT", "LONG", "Freq")



# review bears_final to see if lat_long_duplicates2 have the same bear ID

      # 64.5	-65.783
bears_final %>% filter(LAT=="64.5" & LONG=="-65.783") # same bear=X10709; ROWIDs = 321, 327
      # 64.655	-65.247 - there are 20 records here!!
bears_final %>% filter(LAT=="64.655" & LONG=="-65.247") # same bear=X13292
              # ROWIDs = 1079, 1096, 1102, 1144, 1159, 1177, 1209, 1216, 1223, 1229, 1244, 1250, 1256, 1262, 1270, 1276, 1283, 1313, 1335, 1349
      # 63.85	-65.183
bears_final %>% filter(LAT=="63.85" & LONG=="-65.183") # same bear=X10700; ROWIDs = 296, 340
      # 61.617	-64.983
bears_final %>% filter(LAT=="61.617" & LONG=="-64.983") # same bear=X10374; ROWIDs = 1236, 1248
      # 64.433	-64.867
bears_final %>% filter(LAT=="64.433" & LONG=="-64.867") # same bear=X10709; ROWIDs = 385, 391
      # 60.333	-64.517 - 4 records
bears_final %>% filter(LAT=="60.333" & LONG=="-64.517") # same bear=X12092; ROWIDs = 1161, 1198, 1203, 1315
      # 60.383	-64.517
bears_final %>% filter(LAT=="60.383" & LONG=="-64.517") # same bear=X12092; ROWIDs = 1068, 1077
      # 60.333	-64.5 - 5 records
bears_final %>% filter(LAT=="60.333" & LONG=="-64.5") # same bear=X12092; ROWIDs = 1192, 1208, 1214, 1221, 1243
      # 66.344	-64.08 - there are 12!
bears_final %>% filter(LAT=="66.344" & LONG=="-64.08") # same bear=X13746; ROWIDs = 1520, 1522, 1524, 1526, 1527, 1529, 1531, 1532, 1534, 1535, 1536, 1537
      # 67.088	-63.871 - 25 here!!
bears_final %>% filter(LAT=="67.088" & LONG=="-63.871") # same bear=X13746
              # ROWIDs = 1109, 1111, 1112, 1113, 1116, 1118, 1120, 1122, 1125, 1126, 1127, 1129, 1130, 1133, 1134, 1137, 1145, 1146, 1148, 1150, 1152, 1153, 1154, 1163, 1167
      # 62.769	-63.849 - 14 here!
bears_final %>% filter(LAT=="62.769" & LONG=="-63.849") # same bear=X13289
              # ROWIDs = 643, 647, 666, 678, 686, 697, 703, 709, 717, 722, 727, 747, 760, 775
      # 65.091	-63.723
bears_final %>% filter(LAT=="65.091" & LONG=="-63.723") # same bear=X13289, ROWIDs = 565, 572, 586, 592, 599
      # 59.1	-63.683
bears_final %>% filter(LAT=="59.1" & LONG=="-63.683") # same bear=X11974, ROWIDs = 699, 746
      # 64.897	-63.569
bears_final %>% filter(LAT=="64.897" & LONG=="-63.569") # same bear=X13292
              # ROWIDs = 533, 539, 546, 549, 554, 558, 561, 564, 571, 577, 585, 591, 598
      # 58.7	-63.167
bears_final %>% filter(LAT=="58.7" & LONG=="-63.167") # same bear=X11975, ROWIDs = 594, 710
      # 58.85	-63.117
bears_final %>% filter(LAT=="58.85" & LONG=="-63.117") # same bear=X11975, ROWIDs = 1070, 1075
      # 58.517	-63.017
bears_final %>% filter(LAT=="58.517" & LONG=="-63.017") # same bear=X11975, ROWIDs = 1319, 1439
      # 58.533	-62.983
bears_final %>% filter(LAT=="58.533" & LONG=="-62.983") # same bear=X11975, ROWIDs = 1329, 1352
      # 58.617	-62.917
bears_final %>% filter(LAT=="58.617" & LONG=="-62.917") # same bear=X11975, ROWIDs = 362, 881
      # 58.733	-62.917
bears_final %>% filter(LAT=="58.733" & LONG=="-62.917") # same bear=X11975, ROWIDs = 1257, 1528
      # 58.567	-62.8
bears_final %>% filter(LAT=="58.567" & LONG=="-62.8") # same bear=X11975, ROWIDs = 934, 1263, 1296; note that ROWID=954 is a different bear
      # 58.55	-62.733
bears_final %>% filter(LAT=="58.55" & LONG=="-62.733") # same bear=X11975, ROWIDs = 982, 1230
      # 58.567	-62.633
bears_final %>% filter(LAT=="58.567" & LONG=="-62.633") # different bears
      # 58.367	-62.55
bears_final %>% filter(LAT=="58.367" & LONG=="-62.55") # different bears
      # 58.2	-62.533
bears_final %>% filter(LAT=="58.2" & LONG=="-62.533") # same bear=X11974, ROWIDs = 1369, 1410
      # 58.383	-62.533
bears_final %>% filter(LAT=="58.383" & LONG=="-62.533") # same bear=X12086, ROWIDs = 1486, 1496
      # 58.317	-62.383
bears_final %>% filter(LAT=="58.317" & LONG=="-62.383") # same bear=X11975, ROWIDs = 378, 392
      # 	66.474	-62.167 - 25!!!
bears_final %>% filter(LAT=="66.474" & LONG=="-62.167") # same bear=X13428
              # ROWIDs = 1076, 1082, 1106, 1115, 1124, 1140, 1147, 1156, 1162, 1172, 1179, 1187, 1195, 1201, 1211, 1220, 1228, 1239, 1254, 1261, 1273, 1279, 1289, 1301, 1318
      # 57.883	-62.15
bears_final %>% filter(LAT=="57.883" & LONG=="-62.15") # same bear=X12086, ROWIDs = 1141, 1183, 1217, 1245, 1268
      # 66.567	-61.383
bears_final %>% filter(LAT=="66.567" & LONG=="-61.383") # same bear=X10700, ROWIDs = 566, 573


# make dataframe of only the duplicates
bears_spatial_duplicates <- bears_final %>%
  filter(ROWID %in% c("321", "327", "1079", "1096", "1102", "1144", "1159", "1177", "1209", "1216", "1223", "1229", "1244", "1250", "1256",
  "1262", "1270", "1276", "1283", "1313", "1335", "1349", "296", "340", "1236", "1248", "385", "391", "1161", "1198", "1203", "1315",
  "1068", "1077", "1192", "1208", "1214", "1221", "1243", "1520", "1522", "1524", "1526", "1527", "1529", "1531", "1532", "1534", "1535",
  "1536", "1537", "1109", "1111", "1112", "1113", "1116", "1118", "1120", "1122", "1125", "1126", "1127", "1129", "1130", "1133", "1134",
  "1137", "1145", "1146", "1148", "1150", "1152", "1153", "1154", "1163", "1167", "643", "647", "666", "678", "686", "697", "703", "709",
  "717", "722", "727", "747", "760", "775", "565", "572", "586", "592", "599", "699", "746", "533", "539", "546", "549", "554", "558",
  "561", "564", "571", "577", "585", "591", "598", "594", "710", "1070", "1075", "1319", "1439", "1329", "1352", "362", "881", "1257",
  "1528", "934", "1263", "1296", "982", "1230", "1369", "1410", "1486", "1496", "1076", "1082", "1106", "1115", "1124", "1140",
  "1147", "1156", "1162", "1172", "1179", "1187", "1195", "1201", "1211", "1220", "1228", "1239", "1254", "1261", "1273", "1279", "1289",
  "1301", "1318", "1141", "1183", "1217", "1245", "1268", "566", "573"))

unique(bears_spatial_duplicates$YEAR) # oddly this only happens in 1993, 1994, and 1995
unique(bears_spatial_duplicates$MONTH) # the only month that there aren't duplicates is July
unique(bears_spatial_duplicates$LAT) # 26 different duplicate locations


write.csv(bears_spatial_duplicates, "data/Oct2020work/bears_spatial_duplicates.csv")

# determine how many are on land versus ice
bears_spatial_duplicates_summary <- bears_spatial_duplicates %>%
  group_by(ICE_LAND) %>%
  summarize(count=n())
      # there are 36 ice spatial duplications and 128 land

# summarize per bear per date
bearsID_spatialduplicates <- bears_spatial_duplicates %>%
  group_by(ID, LAT, LONG) %>%
  summarize(count=n())
write.csv(bearsID_spatialduplicates, "data/Oct2020work/bearsID_spatialduplicates.csv")

unique(bearsID_spatialduplicates$ID)


# SKIP - 6. Reviewing movement rates with argosfilter package -----

# bearing: https://stackoverflow.com/questions/30633973/calculating-bearing-in-r
# days: https://stackoverflow.com/questions/46015550/r-programming-how-to-find-a-difference-in-value-for-every-two-consecutive-dates



# Note: bears_seasons was made using bears_final and sea ice analysis data
# I added 2 columns in excel that listed which season each fix was in using 2 different methods
# We decided to use method 2, so the column with method 1 is irrelevant

bears_seasons <- read.csv("data/Oct2020work/bears_seasons.csv")

# separate each bear
X30140 <- bears_seasons %>% filter(ID=="X30140")
X30135 <- bears_seasons %>% filter(ID=="X30135")
X30131 <- bears_seasons %>% filter(ID=="X30131")
X30129 <- bears_seasons %>% filter(ID=="X30129")
X30126 <- bears_seasons %>% filter(ID=="X30126")
X12080 <- bears_seasons %>% filter(ID=="X12080")
X12092 <- bears_seasons %>% filter(ID=="X12092")
X12086 <- bears_seasons %>% filter(ID=="X12086")
X11975 <- bears_seasons %>% filter(ID=="X11975")
X13746 <- bears_seasons %>% filter(ID=="X13746")
X11974 <- bears_seasons %>% filter(ID=="X11974")
X13292 <- bears_seasons %>% filter(ID=="X13292")
X13289 <- bears_seasons %>% filter(ID=="X13289")
X13437 <- bears_seasons %>% filter(ID=="X13437")
X10374 <- bears_seasons %>% filter(ID=="X10374")
X13428 <- bears_seasons %>% filter(ID=="X13428")
X10700 <- bears_seasons %>% filter(ID=="X10700")
X13284 <- bears_seasons %>% filter(ID=="X13284")
X10703 <- bears_seasons %>% filter(ID=="X10703")
X10709 <- bears_seasons %>% filter(ID=="X10709")
X12078 <- bears_seasons %>% filter(ID=="X12078")
X03956 <- bears_seasons %>% filter(ID=="X03956")
X12082 <- bears_seasons %>% filter(ID=="X12082")
X10393 <- bears_seasons %>% filter(ID=="X10393")
X12083 <- bears_seasons %>% filter(ID=="X12083")
X12081 <- bears_seasons %>% filter(ID=="X12081")
X10707 <- bears_seasons %>% filter(ID=="X10707")
X10695 <- bears_seasons %>% filter(ID=="X10695")

# bear X30140
X30140[1,3]="3-11-01" # fix missing date value 
X30140$DATE <- as.Date(X30140$DATE, "%m-%d-%y")
X30140$ANGLE[2:nrow(X30140)] <- bearingTrack(X30140$LAT, X30140$LONG) # degrees
X30140$DIST_KM[2:nrow(X30140)] <- distanceTrack(X30140$LAT, X30140$LONG) # km
X30140$DIFF_DATE <- c(NA, abs(diff(X30140$DATE))) # number of days
X30140$PER_DAY <- X30140$DIST_KM/X30140$DIFF_DATE # km/day
X30140$PER_HR <- X30140$PER_DAY/24 # km/h
X30140$M_PER_HR <- X30140$PER_HR*1000 #m/h
X30140$M_PER_S <- X30140$M_PER_HR/60/60 #m/s

# bear X30135
X30135$DATE <- as.Date(X30135$DATE, "%m-%d-%y")
X30135$ANGLE[2:nrow(X30135)] <- bearingTrack(X30135$LAT, X30135$LONG) # degrees
X30135$DIST_KM[2:nrow(X30135)] <- distanceTrack(X30135$LAT, X30135$LONG) # km
X30135$DIFF_DATE <- c(NA, abs(diff(X30135$DATE))) # number of days
X30135$PER_DAY <- X30135$DIST_KM/X30135$DIFF_DATE # km/day
X30135$PER_HR <- X30135$PER_DAY/24 # km/h
X30135$M_PER_HR <- X30135$PER_HR*1000 #m/h
X30135$M_PER_S <- X30135$M_PER_HR/60/60 #m/s

# bear X30131
X30131$DATE <- as.Date(X30131$DATE, "%m-%d-%y")
X30131$ANGLE[2:nrow(X30131)] <- bearingTrack(X30131$LAT, X30131$LONG) # degrees
X30131$DIST_KM[2:nrow(X30131)] <- distanceTrack(X30131$LAT, X30131$LONG) # km
X30131$DIFF_DATE <- c(NA, abs(diff(X30131$DATE))) # number of days
X30131$PER_DAY <- X30131$DIST_KM/X30131$DIFF_DATE # km/day
X30131$PER_HR <- X30131$PER_DAY/24 # km/h
X30131$M_PER_HR <- X30131$PER_HR*1000 #m/h
X30131$M_PER_S <- X30131$M_PER_HR/60/60 #m/s

# bear X30129
X30129$DATE <- as.Date(X30129$DATE, "%m-%d-%y")
X30129$ANGLE[2:nrow(X30129)] <- bearingTrack(X30129$LAT, X30129$LONG) # degrees
X30129$DIST_KM[2:nrow(X30129)] <- distanceTrack(X30129$LAT, X30129$LONG) # km
X30129$DIFF_DATE <- c(NA, abs(diff(X30129$DATE))) # number of days
X30129$PER_DAY <- X30129$DIST_KM/X30129$DIFF_DATE # km/day
X30129$PER_HR <- X30129$PER_DAY/24 # km/h
X30129$M_PER_HR <- X30129$PER_HR*1000 #m/h
X30129$M_PER_S <- X30129$M_PER_HR/60/60 #m/s

# bear X30126
X30126$DATE <- as.Date(X30126$DATE, "%m-%d-%y")
X30126$ANGLE[2:nrow(X30126)] <- bearingTrack(X30126$LAT, X30126$LONG) # degrees
X30126$DIST_KM[2:nrow(X30126)] <- distanceTrack(X30126$LAT, X30126$LONG) # km
X30126$DIFF_DATE <- c(NA, abs(diff(X30126$DATE))) # number of days
X30126$PER_DAY <- X30126$DIST_KM/X30126$DIFF_DATE # km/day
X30126$PER_HR <- X30126$PER_DAY/24 # km/h
X30126$M_PER_HR <- X30126$PER_HR*1000 #m/h
X30126$M_PER_S <- X30126$M_PER_HR/60/60 #m/s

# bear X12080
X12080$DATE <- as.Date(X12080$DATE, "%m-%d-%y")
X12080$ANGLE[2:nrow(X12080)] <- bearingTrack(X12080$LAT, X12080$LONG) # degrees
X12080$DIST_KM[2:nrow(X12080)] <- distanceTrack(X12080$LAT, X12080$LONG) # km
X12080$DIFF_DATE <- c(NA, abs(diff(X12080$DATE))) # number of days
X12080$PER_DAY <- X12080$DIST_KM/X12080$DIFF_DATE # km/day
X12080$PER_HR <- X12080$PER_DAY/24 # km/h
X12080$M_PER_HR <- X12080$PER_HR*1000 #m/h
X12080$M_PER_S <- X12080$M_PER_HR/60/60 #m/s

# bear X12092
X12092$DATE <- as.Date(X12092$DATE, "%m-%d-%y")
X12092$ANGLE[2:nrow(X12092)] <- bearingTrack(X12092$LAT, X12092$LONG) # degrees
X12092$DIST_KM[2:nrow(X12092)] <- distanceTrack(X12092$LAT, X12092$LONG) # km
X12092$DIFF_DATE <- c(NA, abs(diff(X12092$DATE))) # number of days
X12092$PER_DAY <- X12092$DIST_KM/X12092$DIFF_DATE # km/day
X12092$PER_HR <- X12092$PER_DAY/24 # km/h
X12092$M_PER_HR <- X12092$PER_HR*1000 #m/h
X12092$M_PER_S <- X12092$M_PER_HR/60/60 #m/s

# bear X12086
X12086$DATE <- as.Date(X12086$DATE, "%m-%d-%y")
X12086$ANGLE[2:nrow(X12086)] <- bearingTrack(X12086$LAT, X12086$LONG) # degrees
X12086$DIST_KM[2:nrow(X12086)] <- distanceTrack(X12086$LAT, X12086$LONG) # km
X12086$DIFF_DATE <- c(NA, abs(diff(X12086$DATE))) # number of days
X12086$PER_DAY <- X12086$DIST_KM/X12086$DIFF_DATE # km/day
X12086$PER_HR <- X12086$PER_DAY/24 # km/h
X12086$M_PER_HR <- X12086$PER_HR*1000 #m/h
X12086$M_PER_S <- X12086$M_PER_HR/60/60 #m/s

# bear X11975
X11975$DATE <- as.Date(X11975$DATE, "%m-%d-%y")
X11975$ANGLE[2:nrow(X11975)] <- bearingTrack(X11975$LAT, X11975$LONG) # degrees
X11975$DIST_KM[2:nrow(X11975)] <- distanceTrack(X11975$LAT, X11975$LONG) # km
X11975$DIFF_DATE <- c(NA, abs(diff(X11975$DATE))) # number of days
X11975$PER_DAY <- X11975$DIST_KM/X11975$DIFF_DATE # km/day
X11975$PER_HR <- X11975$PER_DAY/24 # km/h
X11975$M_PER_HR <- X11975$PER_HR*1000 #m/h
X11975$M_PER_S <- X11975$M_PER_HR/60/60 #m/s

# bear X13746
X13746$DATE <- as.Date(X13746$DATE, "%m-%d-%y")
X13746$ANGLE[2:nrow(X13746)] <- bearingTrack(X13746$LAT, X13746$LONG) # degrees
X13746$DIST_KM[2:nrow(X13746)] <- distanceTrack(X13746$LAT, X13746$LONG) # km
X13746$DIFF_DATE <- c(NA, abs(diff(X13746$DATE))) # number of days
X13746$PER_DAY <- X13746$DIST_KM/X13746$DIFF_DATE # km/day
X13746$PER_HR <- X13746$PER_DAY/24 # km/h
X13746$M_PER_HR <- X13746$PER_HR*1000 #m/h
X13746$M_PER_S <- X13746$M_PER_HR/60/60 #m/s

# bear X11974
X11974$DATE <- as.Date(X11974$DATE, "%m-%d-%y")
X11974$ANGLE[2:nrow(X11974)] <- bearingTrack(X11974$LAT, X11974$LONG) # degrees
X11974$DIST_KM[2:nrow(X11974)] <- distanceTrack(X11974$LAT, X11974$LONG) # km
X11974$DIFF_DATE <- c(NA, abs(diff(X11974$DATE))) # number of days
X11974$PER_DAY <- X11974$DIST_KM/X11974$DIFF_DATE # km/day
X11974$PER_HR <- X11974$PER_DAY/24 # km/h
X11974$M_PER_HR <- X11974$PER_HR*1000 #m/h
X11974$M_PER_S <- X11974$M_PER_HR/60/60 #m/s

# bear X13292
X13292$DATE <- as.Date(X13292$DATE, "%m-%d-%y")
X13292$ANGLE[2:nrow(X13292)] <- bearingTrack(X13292$LAT, X13292$LONG) # degrees
X13292$DIST_KM[2:nrow(X13292)] <- distanceTrack(X13292$LAT, X13292$LONG) # km
X13292$DIFF_DATE <- c(NA, abs(diff(X13292$DATE))) # number of days
X13292$PER_DAY <- X13292$DIST_KM/X13292$DIFF_DATE # km/day
X13292$PER_HR <- X13292$PER_DAY/24 # km/h
X13292$M_PER_HR <- X13292$PER_HR*1000 #m/h
X13292$M_PER_S <- X13292$M_PER_HR/60/60 #m/s

# bear X13289
X13289$DATE <- as.Date(X13289$DATE, "%m-%d-%y")
X13289$ANGLE[2:nrow(X13289)] <- bearingTrack(X13289$LAT, X13289$LONG) # degrees
X13289$DIST_KM[2:nrow(X13289)] <- distanceTrack(X13289$LAT, X13289$LONG) # km
X13289$DIFF_DATE <- c(NA, abs(diff(X13289$DATE))) # number of days
X13289$PER_DAY <- X13289$DIST_KM/X13289$DIFF_DATE # km/day
X13289$PER_HR <- X13289$PER_DAY/24 # km/h
X13289$M_PER_HR <- X13289$PER_HR*1000 #m/h
X13289$M_PER_S <- X13289$M_PER_HR/60/60 #m/s

# bear X13437
X13437$DATE <- as.Date(X13437$DATE, "%m-%d-%y")
X13437$ANGLE[2:nrow(X13437)] <- bearingTrack(X13437$LAT, X13437$LONG) # degrees
X13437$DIST_KM[2:nrow(X13437)] <- distanceTrack(X13437$LAT, X13437$LONG) # km
X13437$DIFF_DATE <- c(NA, abs(diff(X13437$DATE))) # number of days
X13437$PER_DAY <- X13437$DIST_KM/X13437$DIFF_DATE # km/day
X13437$PER_HR <- X13437$PER_DAY/24 # km/h
X13437$M_PER_HR <- X13437$PER_HR*1000 #m/h
X13437$M_PER_S <- X13437$M_PER_HR/60/60 #m/s

# bear X10374
X10374$DATE <- as.Date(X10374$DATE, "%m-%d-%y")
X10374$ANGLE[2:nrow(X10374)] <- bearingTrack(X10374$LAT, X10374$LONG) # degrees
X10374$DIST_KM[2:nrow(X10374)] <- distanceTrack(X10374$LAT, X10374$LONG) # km
X10374$DIFF_DATE <- c(NA, abs(diff(X10374$DATE))) # number of days
X10374$PER_DAY <- X10374$DIST_KM/X10374$DIFF_DATE # km/day
X10374$PER_HR <- X10374$PER_DAY/24 # km/h
X10374$M_PER_HR <- X10374$PER_HR*1000 #m/h
X10374$M_PER_S <- X10374$M_PER_HR/60/60 #m/s

# bear X13428
X13428$DATE <- as.Date(X13428$DATE, "%m-%d-%y")
X13428$ANGLE[2:nrow(X13428)] <- bearingTrack(X13428$LAT, X13428$LONG) # degrees
X13428$DIST_KM[2:nrow(X13428)] <- distanceTrack(X13428$LAT, X13428$LONG) # km
X13428$DIFF_DATE <- c(NA, abs(diff(X13428$DATE))) # number of days
X13428$PER_DAY <- X13428$DIST_KM/X13428$DIFF_DATE # km/day
X13428$PER_HR <- X13428$PER_DAY/24 # km/h
X13428$M_PER_HR <- X13428$PER_HR*1000 #m/h
X13428$M_PER_S <- X13428$M_PER_HR/60/60 #m/s

# bear X10700
X10700$DATE <- as.Date(X10700$DATE, "%m-%d-%y")
X10700$ANGLE[2:nrow(X10700)] <- bearingTrack(X10700$LAT, X10700$LONG) # degrees
X10700$DIST_KM[2:nrow(X10700)] <- distanceTrack(X10700$LAT, X10700$LONG) # km
X10700$DIFF_DATE <- c(NA, abs(diff(X10700$DATE))) # number of days
X10700$PER_DAY <- X10700$DIST_KM/X10700$DIFF_DATE # km/day
X10700$PER_HR <- X10700$PER_DAY/24 # km/h
X10700$M_PER_HR <- X10700$PER_HR*1000 #m/h
X10700$M_PER_S <- X10700$M_PER_HR/60/60 #m/s

# bear X13284
X13284$DATE <- as.Date(X13284$DATE, "%m-%d-%y")
X13284$ANGLE[2:nrow(X13284)] <- bearingTrack(X13284$LAT, X13284$LONG) # degrees
X13284$DIST_KM[2:nrow(X13284)] <- distanceTrack(X13284$LAT, X13284$LONG) # km
X13284$DIFF_DATE <- c(NA, abs(diff(X13284$DATE))) # number of days
X13284$PER_DAY <- X13284$DIST_KM/X13284$DIFF_DATE # km/day
X13284$PER_HR <- X13284$PER_DAY/24 # km/h
X13284$M_PER_HR <- X13284$PER_HR*1000 #m/h
X13284$M_PER_S <- X13284$M_PER_HR/60/60 #m/s

# bear X10703
X10703$DATE <- as.Date(X10703$DATE, "%m-%d-%y")
X10703$ANGLE[2:nrow(X10703)] <- bearingTrack(X10703$LAT, X10703$LONG) # degrees
X10703$DIST_KM[2:nrow(X10703)] <- distanceTrack(X10703$LAT, X10703$LONG) # km
X10703$DIFF_DATE <- c(NA, abs(diff(X10703$DATE))) # number of days
X10703$PER_DAY <- X10703$DIST_KM/X10703$DIFF_DATE # km/day
X10703$PER_HR <- X10703$PER_DAY/24 # km/h
X10703$M_PER_HR <- X10703$PER_HR*1000 #m/h
X10703$M_PER_S <- X10703$M_PER_HR/60/60 #m/s

# bear X10709
X10709$DATE <- as.Date(X10709$DATE, "%m-%d-%y")
X10709$ANGLE[2:nrow(X10709)] <- bearingTrack(X10709$LAT, X10709$LONG) # degrees
X10709$DIST_KM[2:nrow(X10709)] <- distanceTrack(X10709$LAT, X10709$LONG) # km
X10709$DIFF_DATE <- c(NA, abs(diff(X10709$DATE))) # number of days
X10709$PER_DAY <- X10709$DIST_KM/X10709$DIFF_DATE # km/day
X10709$PER_HR <- X10709$PER_DAY/24 # km/h
X10709$M_PER_HR <- X10709$PER_HR*1000 #m/h
X10709$M_PER_S <- X10709$M_PER_HR/60/60 #m/s

# bear X12078
X12078$DATE <- as.Date(X12078$DATE, "%m-%d-%y")
X12078$ANGLE[2:nrow(X12078)] <- bearingTrack(X12078$LAT, X12078$LONG) # degrees
X12078$DIST_KM[2:nrow(X12078)] <- distanceTrack(X12078$LAT, X12078$LONG) # km
X12078$DIFF_DATE <- c(NA, abs(diff(X12078$DATE))) # number of days
X12078$PER_DAY <- X12078$DIST_KM/X12078$DIFF_DATE # km/day
X12078$PER_HR <- X12078$PER_DAY/24 # km/h
X12078$M_PER_HR <- X12078$PER_HR*1000 #m/h
X12078$M_PER_S <- X12078$M_PER_HR/60/60 #m/s

# bear X03956
X03956$DATE <- as.Date(X03956$DATE, "%m-%d-%y")
X03956$ANGLE[2:nrow(X03956)] <- bearingTrack(X03956$LAT, X03956$LONG) # degrees
X03956$DIST_KM[2:nrow(X03956)] <- distanceTrack(X03956$LAT, X03956$LONG) # km
X03956$DIFF_DATE <- c(NA, abs(diff(X03956$DATE))) # number of days
X03956$PER_DAY <- X03956$DIST_KM/X03956$DIFF_DATE # km/day
X03956$PER_HR <- X03956$PER_DAY/24 # km/h
X03956$M_PER_HR <- X03956$PER_HR*1000 #m/h
X03956$M_PER_S <- X03956$M_PER_HR/60/60 #m/s

# bear X12082
X12082$DATE <- as.Date(X12082$DATE, "%m-%d-%y")
X12082$ANGLE[2:nrow(X12082)] <- bearingTrack(X12082$LAT, X12082$LONG) # degrees
X12082$DIST_KM[2:nrow(X12082)] <- distanceTrack(X12082$LAT, X12082$LONG) # km
X12082$DIFF_DATE <- c(NA, abs(diff(X12082$DATE))) # number of days
X12082$PER_DAY <- X12082$DIST_KM/X12082$DIFF_DATE # km/day
X12082$PER_HR <- X12082$PER_DAY/24 # km/h
X12082$M_PER_HR <- X12082$PER_HR*1000 #m/h
X12082$M_PER_S <- X12082$M_PER_HR/60/60 #m/s

# bear X10393
X10393$DATE <- as.Date(X10393$DATE, "%m-%d-%y")
X10393$ANGLE[2:nrow(X10393)] <- bearingTrack(X10393$LAT, X10393$LONG) # degrees
X10393$DIST_KM[2:nrow(X10393)] <- distanceTrack(X10393$LAT, X10393$LONG) # km
X10393$DIFF_DATE <- c(NA, abs(diff(X10393$DATE))) # number of days
X10393$PER_DAY <- X10393$DIST_KM/X10393$DIFF_DATE # km/day
X10393$PER_HR <- X10393$PER_DAY/24 # km/h
X10393$M_PER_HR <- X10393$PER_HR*1000 #m/h
X10393$M_PER_S <- X10393$M_PER_HR/60/60 #m/s

# bear X12083
X12083$DATE <- as.Date(X12083$DATE, "%m-%d-%y")
X12083$ANGLE[2:nrow(X12083)] <- bearingTrack(X12083$LAT, X12083$LONG) # degrees
X12083$DIST_KM[2:nrow(X12083)] <- distanceTrack(X12083$LAT, X12083$LONG) # km
X12083$DIFF_DATE <- c(NA, abs(diff(X12083$DATE))) # number of days
X12083$PER_DAY <- X12083$DIST_KM/X12083$DIFF_DATE # km/day
X12083$PER_HR <- X12083$PER_DAY/24 # km/h
X12083$M_PER_HR <- X12083$PER_HR*1000 #m/h
X12083$M_PER_S <- X12083$M_PER_HR/60/60 #m/s

# bear X12081
X12081$DATE <- as.Date(X12081$DATE, "%m-%d-%y")
X12081$ANGLE[2:nrow(X12081)] <- bearingTrack(X12081$LAT, X12081$LONG) # degrees
X12081$DIST_KM[2:nrow(X12081)] <- distanceTrack(X12081$LAT, X12081$LONG) # km
X12081$DIFF_DATE <- c(NA, abs(diff(X12081$DATE))) # number of days
X12081$PER_DAY <- X12081$DIST_KM/X12081$DIFF_DATE # km/day
X12081$PER_HR <- X12081$PER_DAY/24 # km/h
X12081$M_PER_HR <- X12081$PER_HR*1000 #m/h
X12081$M_PER_S <- X12081$M_PER_HR/60/60 #m/s

# bear X10707
X10707$DATE <- as.Date(X10707$DATE, "%m-%d-%y")
X10707$ANGLE[2:nrow(X10707)] <- bearingTrack(X10707$LAT, X10707$LONG) # degrees
X10707$DIST_KM[2:nrow(X10707)] <- distanceTrack(X10707$LAT, X10707$LONG) # km
X10707$DIFF_DATE <- c(NA, abs(diff(X10707$DATE))) # number of days
X10707$PER_DAY <- X10707$DIST_KM/X10707$DIFF_DATE # km/day
X10707$PER_HR <- X10707$PER_DAY/24 # km/h
X10707$M_PER_HR <- X10707$PER_HR*1000 #m/h
X10707$M_PER_S <- X10707$M_PER_HR/60/60 #m/s

# bear X10695
X10695$DATE <- as.Date(X10695$DATE, "%m-%d-%y")
X10695$ANGLE[2:nrow(X10695)] <- bearingTrack(X10695$LAT, X10695$LONG) # degrees
X10695$DIST_KM[2:nrow(X10695)] <- distanceTrack(X10695$LAT, X10695$LONG) # km
X10695$DIFF_DATE <- c(NA, abs(diff(X10695$DATE))) # number of days
X10695$PER_DAY <- X10695$DIST_KM/X10695$DIFF_DATE # km/day
X10695$PER_HR <- X10695$PER_DAY/24 # km/h
X10695$M_PER_HR <- X10695$PER_HR*1000 #m/h
X10695$M_PER_S <- X10695$M_PER_HR/60/60 #m/s


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

# Note these last couple plots helped us understand that we should limit everything >5km/hr - see section 7

  


# 7. Removing fixes based on movement rates (from section 6) --------


# limit speed based on section 6 results using filter and anti_join

# first, find all > 5km/hour

topspeed <- bears_step_angles %>% filter(PER_HR>=5)

bears_final_Nov2020 <- anti_join(bears_step_angles, topspeed) # 1850 fixes now

# remove second season column
bears_final_Nov2020=subset(bears_final_Nov2020, select=-c(X1_Season))

# rename columns
colnames(bears_final_Nov2020)
names(bears_final_Nov2020)[12] <- "SEASON"
names(bears_final_Nov2020)[16] <- "KM_PER_DAY"
names(bears_final_Nov2020)[17] <- "KM_PER_HR"

# make new csv file
write.csv(bears_final_Nov2020, "data/Oct2020work/FINAL DATASET/bears_final_Nov2020.csv")


-----
  
  # Redo movement rate plot with this final dataset
  head(bears_final_Nov2020)
summary(bears_final_Nov2020) # mean of KM_PER_HR = 0.43471

ggplot(data=bears_final_Nov2020, aes(KM_PER_HR, ID)) +
  geom_point(colour="darkgrey") +
  geom_vline(xintercept=0.43471, colour="darkred", size=.75) + #mean
  scale_x_continuous(breaks=c(0, 1, 2, 3, 4, 5), labels=c("0", "1", "2", "3", "4", "5")) +
  ylab("Bear ID") +
  xlab("Movement rates (Km/h)\n\nNote: red line indicates mean (0.43 km/h)")

ggplot(data=bears_final_Nov2020, aes(KM_PER_HR)) +
  geom_histogram(colour="darkgrey") +
  xlab("Movement rates (Km/h)")

ggplot(data=bears_final_Nov2020, aes(KM_PER_DAY)) +
  geom_histogram(colour="darkgrey") +
  scale_x_continuous(limits=c(0, 120), breaks=c(0, 20, 40, 60, 80, 100, 120), labels=c("0", "20", "40", "60", "80", "100", "120")) +
  scale_y_continuous(limits=c(0, 400)) +
  xlab("Movement rates (Km/day)") +
  theme_nuwcru()

ggplot(data=bears_final_Nov2020, aes(M_PER_HR)) +
  geom_histogram(colour="darkgrey") +
  xlab("Movement rates (m/h)")


str(bears_final_Nov2020)

km_day_boxplot <- ggplot(bears_final_Nov2020, aes(y=KM_PER_DAY)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  #scale_y_continuous(limits=c(0, 5), breaks=c(0, 1, 2, 3, 4, 5), labels=c("0", "1", "2", "3", "4", "5")) +
  ylab("Movement rate (Km/day)") +
  scale_y_continuous(limits=c(0, 120), breaks=c(0, 20, 40, 60, 80, 100, 120), labels=c("0", "20", "40", "60", "80", "100", "120")) +
  theme_nuwcru()
km_day_boxplot
km_day_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


-----
  
# determine any more outliers
str(bears_final_Nov2020)
summary(bears_final_Nov2020$KM_PER_DAY)
bears_final_Nov2020_2 <- bears_final_Nov2020
bears_final_Nov2020_2=na.omit(bears_final_Nov2020_2[, c("ID", "DATE", "SEASON", "KM_PER_DAY")]) # remove NA's in this column for calculating below
bears_final_Nov2020_2 <- as.data.frame(bears_final_Nov2020_2) # now there are 1822 records (not 1850)
str(bears_final_Nov2020_2$KM_PER_DAY)
summary(bears_final_Nov2020_2$KM_PER_DAY)
head(bears_final_Nov2020_2)


# test if data are normally distributed: http://www.sthda.com/english/wiki/normality-test-in-r
hist(bears_final_Nov2020_2$KM_PER_DAY) # doesn't appear to be
ggdensity(bears_final_Nov2020_2$KM_PER_DAY) # as above
ggqqplot(bears_final_Nov2020_2$KM_PER_DAY) # as above
shapiro.test(bears_final_Nov2020_2$KM_PER_DAY) # normal if p-value is >0.05, which is not the case here
# because it's non-normal, we cannot calculate z or t

# Notes
      # non-normal distributions: https://www.statisticshowto.com/probability-and-statistics/non-normal-distributions/
      # Poisson distribution: https://www.statisticshowto.com/poisson-distribution/
      # detecting outliers (here is where it says to log transform): https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h.htm

# log transform the distances
bears_final_Nov2020_3 <- bears_final_Nov2020_2 %>% mutate(LOG_KM_PER_DAY=log(KM_PER_DAY))
hist(bears_final_Nov2020_3$LOG_KM_PER_DAY) # more normal
summary(bears_final_Nov2020_3) # minimum is "-Inf" because it cannot transform distances of 0, therefore remove those
bears_final_Nov2020_3[sapply(bears_final_Nov2020_3, is.infinite)] <- NA # make all infinite values NAs
bears_final_Nov2020_3=na.omit(bears_final_Nov2020_3[, c("ID", "DATE", "SEASON", "KM_PER_DAY", "LOG_KM_PER_DAY")]) # remove NAs; now there are 1703

# get z-scores: https://datascienceplus.com/how-to-compute-the-z-score-with-r/
bears_final_Nov2020_3 <- bears_final_Nov2020_3 %>% mutate(ZSCORE=(bears_final_Nov2020_3$LOG_KM_PER_DAY-mean(bears_final_Nov2020_3$LOG_KM_PER_DAY))/sd(bears_final_Nov2020_3$LOG_KM_PER_DAY))
hist(bears_final_Nov2020_3$ZSCORE)
sd(bears_final_Nov2020_3$LOG_KM_PER_DAY) # standard deviation = 1.39565
sd(bears_final_Nov2020_3$ZSCORE) # standard deviation = 1

# if we removed anything outside of -3 to 3, we would only be removing really small movements, rather than large ones (which doesn't make sense)
# no need to remove any more points



# SKIP - 8. Counting fixes and collars for final dataset -----------------


bears_final_Nov2020 <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020.csv")

# total fixes per ID
bear_total <- as.data.frame(table(bears_final_Nov2020$ID))

# total fixes per month
month_total <- bears_final_Nov2020 %>%
  group_by(MONTH) %>%
  summarize(n())
ungroup(bears_final_Nov2020)

# total fixes per year
year_total <- bears_final_Nov2020 %>%
  group_by(YEAR) %>%
  summarize(n())
ungroup(bears_final_Nov2020)

# total monthly fixes per bear 
month_bear_total <- bears_final_Nov2020 %>%
  group_by(MONTH, ID) %>%
  summarize(n())
ungroup(bears_final_Nov2020)

# total monthly fixes per bear separated by year
month_year_bear_total <- bears_final_Nov2020 %>%
  group_by(YEAR, MONTH, ID) %>%
  summarize(n())
ungroup(bears_final_Nov2020)

# total points per date
date_total <- bears_final_Nov2020 %>%
  group_by(ID, DATE) %>%
  summarize(n())
ungroup(bears_final_Nov2020)

# total annual fixes per bear
bear_year_total <- bears_final_Nov2020 %>%
  group_by(YEAR, ID) %>%
  summarize(n())
colnames(bear_year_total) = c("YEAR", "ID", "COUNT")
bear_year_total2 <- bear_year_total %>%
  group_by(YEAR) %>%
  summarize(count=sum(COUNT), numBears=n())
ungroup(bears_final_Nov2020)


# analyzing bear_year_total 

unique(bear_year_total$ID)

X10695_counts <- bear_year_total %>% filter(ID=="X10695")
X10700_counts <- bear_year_total %>% filter(ID=="X10700")
X10703_counts <- bear_year_total %>% filter(ID=="X10703")
X10707_counts <- bear_year_total %>% filter(ID=="X10707")
X10709_counts <- bear_year_total %>% filter(ID=="X10709")
X13284_counts <- bear_year_total %>% filter(ID=="X13284")
X13289_counts <- bear_year_total %>% filter(ID=="X13289")
X13292_counts <- bear_year_total %>% filter(ID=="X13292")
X11974_counts <- bear_year_total %>% filter(ID=="X11974")
X11975_counts <- bear_year_total %>% filter(ID=="X11975")
X13428_counts <- bear_year_total %>% filter(ID=="X13428")
X13437_counts <- bear_year_total %>% filter(ID=="X13437")
X03956_counts <- bear_year_total %>% filter(ID=="X03956")
X10374_counts <- bear_year_total %>% filter(ID=="X10374")
X10393_counts <- bear_year_total %>% filter(ID=="X10393")
X12078_counts <- bear_year_total %>% filter(ID=="X12078")
X12080_counts <- bear_year_total %>% filter(ID=="X12080")
X12081_counts <- bear_year_total %>% filter(ID=="X12081")
X12082_counts <- bear_year_total %>% filter(ID=="X12082")
X12083_counts <- bear_year_total %>% filter(ID=="X12083")
X12086_counts <- bear_year_total %>% filter(ID=="X12086")
X12092_counts <- bear_year_total %>% filter(ID=="X12092")
X13746_counts <- bear_year_total %>% filter(ID=="X13746")
X30126_counts <- bear_year_total %>% filter(ID=="X30126") # 1 more fix in 1998 (n=39) than the last time I did this (i.e. with this final dataset)
X30129_counts <- bear_year_total %>% filter(ID=="X30129")
X30131_counts <- bear_year_total %>% filter(ID=="X30131")
X30135_counts <- bear_year_total %>% filter(ID=="X30135")
X30140_counts <- bear_year_total %>% filter(ID=="X30140")

# find yearly start and end dates for each bear
X10695_total <- bears_final_Nov2020 %>% filter(ID=="X10695")
unique(X10695_total$YEAR) # only 1991 (Apr 2 to Dec 29) and 1992 (Jan 6 to Aug 27)

X10700_total <- bears_final_Nov2020 %>% filter(ID=="X10700")
unique(X10700_total$YEAR) # 1992 (Apr 7 to Sep 4), 1993 (Feb 6 to Dec 28), 1994 (Jan 2 to Sep 14)

X10703_total <- bears_final_Nov2020 %>% filter(ID=="X10703")
unique(X10703_total$YEAR) # 1992 (Apr 8 to Sep 25), 1993 (Mar 14 to Dec 24), 1994 (Jan 8 to Jul 17)

X10707_total <- bears_final_Nov2020 %>% filter(ID=="X10707")
unique(X10707_total$YEAR) # 1992 (Apr 7 to Dec 20), 1993 (Feb 3 to Nov 10)

X10709_total <- bears_final_Nov2020 %>% filter(ID=="X10709")
unique(X10709_total$YEAR) # 1992 (Apr 7 to Sep 6), 1993 (Feb 8 to Dec 30), 1994 (Jan 11 to Jun 15)

X13284_total <- bears_final_Nov2020 %>% filter(ID=="X13284")
unique(X13284_total$YEAR) # 1992 (Oct 1 to Dec 27), 1993 (Jan 4 to Dec 30), 1994 (Jan 3 to Aug 3)

X13289_total <- bears_final_Nov2020 %>% filter(ID=="X13289")
unique(X13289_total$YEAR) # 1992 (Oct 3 to Dec 26), 1993 (Jan 1 to Dec 15), 1994 (Jan 2 to Dec 28), 1995 (Jan 3 to Jul 14)

X13292_total <- bears_final_Nov2020 %>% filter(ID=="X13292")
unique(X13292_total$YEAR) # 1992 (Oct 3 to Dec 26), 1993 (Jan 1 to Dec 27), 1994 (Jan 2 to Dec 28), 1995 (Jan 3 to Aug 25)

X11974_total <- bears_final_Nov2020 %>% filter(ID=="X11974")
unique(X11974_total$YEAR) # 1993 (Apr 5 to Dec 30), 1994 (Feb 3 to Jun 28), 1995 (Mar 25 to Oct 11)

X11975_total <- bears_final_Nov2020 %>% filter(ID=="X11975")
unique(X11975_total$YEAR) # 1993 (Apr 5 to Oct 17), 1994 (Feb 14 to Dec 31), 1995 (Jan 5 to Oct 17)

X13428_total <- bears_final_Nov2020 %>% filter(ID=="X13428")
unique(X13428_total$YEAR) # 1993 (Oct 7 to Dec 30), 1994 (Jan 11 to Dec 31), 1995 (Jan 6 to Mar 25)

X13437_total <- bears_final_Nov2020 %>% filter(ID=="X13437")
unique(X13437_total$YEAR) # 1993 (Oct 8 to Dec 30), 1994 (Jan 11 to Dec 3), 1995 (Feb 23 to May 13)

X03956_total <- bears_final_Nov2020 %>% filter(ID=="X03956")
unique(X03956_total$YEAR) # 1994 (Apr 4 to Jun 4)

X10374_total <- bears_final_Nov2020 %>% filter(ID=="X10374")
unique(X10374_total$YEAR) # 1994 (Apr 5 to Jul 4), 1995 (Jan 5 to May 10)

X10393_total <- bears_final_Nov2020 %>% filter(ID=="X10393")
unique(X10393_total$YEAR) # 1994 (Mar 11 to May 7)

X12078_total <- bears_final_Nov2020 %>% filter(ID=="X12078")
unique(X12078_total$YEAR) # 1994 (Mar 19 to Jun 6)

X12080_total <- bears_final_Nov2020 %>% filter(ID=="X12080")
unique(X12080_total$YEAR) # 1994 (Mar 12 to Dec 27), 1995 (Jan 6 to Oct 28)

X12081_total <- bears_final_Nov2020 %>% filter(ID=="X12081")
unique(X12081_total$YEAR) # 1994 (Mar 23 to Apr 29)

X12082_total <- bears_final_Nov2020 %>% filter(ID=="X12082")
unique(X12082_total$YEAR) # 1994 (Mar 30 to May 14)

X12083_total <- bears_final_Nov2020 %>% filter(ID=="X12083")
unique(X12083_total$YEAR) # 1994 (Apr 4 to May 4)

X12086_total <- bears_final_Nov2020 %>% filter(ID=="X12086")
unique(X12086_total$YEAR) # 1994 (Apr 4 to Dec 31), 1995 (Jan 19 to Oct 23)

X12092_total <- bears_final_Nov2020 %>% filter(ID=="X12092")
unique(X12092_total$YEAR) # 1994 (Apr 6 to Dec 27), 1995 (Jan 1 to Oct 28)

X13746_total <- bears_final_Nov2020 %>% filter(ID=="X13746")
unique(X13746_total$YEAR) # 1994 (Sep 8 to Dec 31), 1995 (Jan 6 to Oct 14)

X30126_total <- bears_final_Nov2020 %>% filter(ID=="X30126")
unique(X30126_total$YEAR) # 1997 (Nov 1 to Dec 31), 1998 (Jan 6 to Dec 2), 1999 (Jan 7 to Sep 10)

X30129_total <- bears_final_Nov2020 %>% filter(ID=="X30129")
unique(X30129_total$YEAR) # 1997 (Nov 1 to Dec 31), 1998 (Jan 6 to Dec 20), 1999 (Jan 7 to Sep 16)

X30131_total <- bears_final_Nov2020 %>% filter(ID=="X30131")
unique(X30131_total$YEAR) # 1997 (Nov 1 to Dec 31), 1998 (Jan 6 to Dec 14), 1999 (Jan 1 to Sep 16)

X30135_total <- bears_final_Nov2020 %>% filter(ID=="X30135")
unique(X30135_total$YEAR) # 1997 (Nov 4 to Dec 27), 1998 (Jan 2 to Dec 28), 1999 (Jan 1 to Dec 13)

X30140_total <- bears_final_Nov2020 %>% filter(ID=="X30140")
unique(X30140_total$YEAR) # 1997 (Nov 4 to Dec 27), 1998 (Jan 2 to Dec 28), 1999 (Jan 3 to Mar; 4 just these 2), 2000 (Feb 21 to May 3), 2001 (Jan 22 to Mar 11)


# put all these into an excel file - see DScollardata_finaldataframe.csv


# plot above info

# Make dataframe
k <- c(10695, 10695, 10703, 10703, 10703, 10709, 10709, 10709, 10707, 10707, 10700, 10700, 10700, 13284, 13284, 13284, 13292, 13292, 13292, 13292, 13289, 13289, 13289, 13289, 11974, 11974, 11974, 11975, 11975, 11975, 13428, 13428, 13428, 13437, 13437, 13437, 10393, 12080, 12080, 12078, 12081, 12082, 12083, 12086, 12086, 3956, 10374, 10374, 12092, 12092, 13746, 13746, 30129, 30129, 30129, 30126, 30126, 30126, 30131, 30131, 30131, 30140, 30140, 30140, 30140, 30140, 30135, 30135, 30135) # ID
k <- factor(k)
l <- c(1991, 1992, 1992, 1993, 1994, 1992, 1993, 1994, 1992, 1993, 1992, 1993, 1994, 1992, 1993, 1994, 1992, 1993, 1994, 1995, 1992, 1993, 1994, 1995, 1993, 1994, 1995, 1993, 1994, 1995, 1993, 1994, 1995, 1993, 1994, 1995, 1994, 1994, 1995, 1994, 1994, 1994, 1994, 1994, 1995, 1994, 1994, 1995, 1994, 1995, 1994, 1995, 1997, 1998, 1999, 1997, 1998, 1999, 1997, 1998, 1999, 1997, 1998, 1999, 2000, 2001, 1997, 1998, 1999) # Years

bears_data_years <- data.frame(k, l)
names(bears_data_years) <- c('ID', 'YEAR' )
str(bears_data_years)

# Plot it
bears_data_years_plot <- ggplot(bears_data_years, aes(x=YEAR, y=ID)) +
  geom_point() +
  geom_line() +
  ylab("Bear identification number") +
  xlab("Year of data") +
  theme(panel.grid.minor.x=element_blank()) +
  scale_y_discrete(breaks=c(10695, 10707, 10700, 10703, 10709, 13284, 13289, 13292, 13437, 13428, 11974, 11975, 12078, 12081, 12082, 12083, 10393, 3956, 10374, 12080, 12086, 12092, 13746, 30126, 30129, 30131, 30135, 30140)) +
  scale_x_continuous(breaks=c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001))

print(bears_data_years_plot)

# Reorder IDs so that the plot looks better
bears_data_years2 <- bears_data_years %>%
  mutate(ID2=factor(ID, levels=c("10695", "10707", "10700", "10703", "10709", "13284", "13289", "13292", "13437", "13428", "11974", "11975", "12078", "12081", "12082", "12083", "10393", "3956", "10374", "12080", "12086", "12092", "13746", "30126", "30129", "30131", "30135", "30140")))

bears_data_years_plot2 <- ggplot(bears_data_years2, aes(x=YEAR, y=ID2)) +
  geom_point() +
  geom_line() +
  ylab("Bear identification number") +
  xlab("Year of data") +
  theme(panel.grid.minor.x=element_blank()) +
  scale_y_discrete(breaks=c(10695, 10707, 10700, 10703, 10709, 13284, 13289, 13292, 13437, 13428, 11974, 11975, 12078, 12081, 12082, 12083, 10393, 3956, 10374, 12080, 12086, 12092, 13746, 30126, 30129, 30131, 30135, 30140)) +
  scale_x_continuous(breaks=c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001))

print(bears_data_years_plot2)














