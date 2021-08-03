

# Note: this script is for calculating final home range areas (i.e. using UTM only)
# All previous HR scripts can be ignored



# 1. Import libraries  --------------

library(adehabitatHR) # to use MCP function
library(adehabitatHS) # to use MCP area function
library(adehabitatLT)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(raster)
library(rgdal)
library(dplyr) # to use the filter function
library(tidyverse) # to use scale_x_continous function in plotting
library(ggplot2)
library(rgeos) # to be able to use fortify()
library(ks) # for kernel density
library(maptools) # for plotting kernel
library(sp)
library(PBSmapping)
library(rgeos)
library(foreign)
library(maptools)
library(FNN)
library(lme4) # for step #12 - mixed model

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

# SKIP - 2. Import and format data ---------------

# import final bear dataset with UTM coordinates
# Note that I imported the final dataset made in script 01 into QGIS and got the UTM zones using clip points to polygon tool

bears_final_Nov2020_UTM <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTMzones.csv")
bears_final_Nov2020 <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020.csv")
difference <- anti_join(bears_final_Nov2020, bears_final_Nov2020_UTM) # these are the same, just one has the UTM

# Some values that fell directly on the grid lines in QGIS were classified as the wrong zone
# all ROWID #s on grid lines = 26, 142, 146, 512, 507, 67, 1777, 1132, 371, 447, 426, 649, 1277, 1342, 1298, 1233
        # Check in Google Earth - not all were classified incorrectly
# fix incorrect zones for ROWID #s: 26, 142, 146, 512, 507, 1777, 1298

bears_final_Nov2020_UTM %>% filter(ROWID=="1233")

bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==26, "ZONE"] <- 18
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==142, "ZONE"] <- 17
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==146, "ZONE"] <- 19
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==512, "ZONE"] <- 20
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==507, "ZONE"] <- 20
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==1777, "ZONE"] <- 20
bears_final_Nov2020_UTM[bears_final_Nov2020_UTM$ROWID==1298, "ZONE"] <- 21

# separate each zone into separate dataframes

unique(bears_final_Nov2020_UTM$ZONE) # zones are 17, 18, 19, 20, 21, and 22

zone17 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="17")
zone18 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="18")
zone19 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="19")
zone20 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="20")
zone21 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="21")
zone22 <- bears_final_Nov2020_UTM %>% filter(bears_final_Nov2020_UTM$ZONE=="22")


# convert to SpatialPointsDataFrames and then to UTM coordinates

coordinates(zone17) <- ~LONG+LAT
proj4string(zone17) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone17_utm <- spTransform(zone17, CRS("+proj=utm +zone=17 ellps=WGS84"))

coordinates(zone18) <- ~LONG+LAT
proj4string(zone18) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone18_utm <- spTransform(zone18, CRS("+proj=utm +zone=18 ellps=WGS84"))

coordinates(zone19) <- ~LONG+LAT
proj4string(zone19) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone19_utm <- spTransform(zone19, CRS("+proj=utm +zone=19 ellps=WGS84"))

coordinates(zone20) <- ~LONG+LAT
proj4string(zone20) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone20_utm <- spTransform(zone20, CRS("+proj=utm +zone=20 ellps=WGS84"))

coordinates(zone21) <- ~LONG+LAT
proj4string(zone21) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone21_utm <- spTransform(zone21, CRS("+proj=utm +zone=21 ellps=WGS84"))

coordinates(zone22) <- ~LONG+LAT
proj4string(zone22) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
zone22_utm <- spTransform(zone22, CRS("+proj=utm +zone=22 ellps=WGS84"))


# convert to dataframes and combine
zone17_df <- as.data.frame(zone17_utm)
zone18_df <- as.data.frame(zone18_utm)
zone19_df <- as.data.frame(zone19_utm)
zone20_df <- as.data.frame(zone20_utm)
zone21_df <- as.data.frame(zone21_utm)
zone22_df <- as.data.frame(zone22_utm)

bears_final_Nov2020_UTM_complete <- rbind(zone17_df, zone18_df, zone19_df, zone20_df, zone21_df, zone22_df)

colnames(bears_final_Nov2020_UTM_complete)


# removed unnecessary columns and updated column names
bears_final_Nov2020_UTM_complete = subset(bears_final_Nov2020_UTM_complete, select = -c(field_1, X, DATE, DAY, MONTH, YEAR, TIME, ICE_LAND, SEASON, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S))
head(bears_final_Nov2020_UTM_complete)
names(bears_final_Nov2020_UTM_complete)[4] <- "EASTING"
names(bears_final_Nov2020_UTM_complete)[5] <- "NORTHING"

# combined bears_final_Nov2020_UTM_complete with bears_final_Nov2020 to have both lat/long and utm in one dataframe
bears_final_Nov2020_UTM_LATLONG <- merge(bears_final_Nov2020_UTM_complete, bears_final_Nov2020_UTM, on='ROWID')

write.csv(bears_final_Nov2020_UTM_LATLONG, "data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")


# 3. POOLED HOME RANGES: Import final dataset, separate bears, count fixes  ---------

# import data
bears_final_Nov2020_UTM_LATLONG <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")

# separate each bear from the rest of the dataset 
unique(bears_final_Nov2020_UTM_LATLONG$ID)

X10695 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10695")
X10703 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10703")
X10709 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10709")
X10707 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10707")
X10700 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10700")
X13284 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13284")
X13292 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292")
X13289 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289")
X11974 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11974")
X11975 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11975")
X13428 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13428")
X13437 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13437")
X10393 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10393")
X12080 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12080")
X12078 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12078")
X12081 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12081")
X12082 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12082")
X12083 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12083")
X12086 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12086")
X03956 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X03956")
X10374 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10374")
X12092 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12092")
X13746 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13746")
X30129 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30129")
X30126 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30126")
X30131 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30131")
X30140 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140")
X30135 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30135")

# check which bears have >100 fixes
str(X10695) # 110
str(X10709) # 86 
str(X12083) # 5 
str(X12092) # 51
str(X11975) # 88 
str(X30140) # 42
str(X30135) # 71 
str(X30129) # 79 
str(X30131) # 79 
str(X30126) # 87
str(X10703) # 100
str(X10707) # 75
str(X10700) # 97
str(X13284) # 90
str(X13292) # 125
str(X13289) # 139
str(X11974) # 53
str(X13428) # 75
str(X13437) # 76
str(X12086) # 54
str(X03956) # 12
str(X10374) # 27
str(X12080) # 93
str(X12078) # 5
str(X13746) # 108 
str(X10393) # 8
str(X12081) # 6
str(X12082) # 9



# 4. Pooled MCPs ---------

# Need only bears with >=100 fixes: X10695, X10703, X13292, X13289, X13746
# get projections here: https://proj.org/operations/projections/stere.html#id1

# make a new list for bears with >100 fixes
bear_list_greater100 <- list(X10695, X10703, X13292, X13289, X13746)

# name items in list
names(bear_list_greater100) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
head(bear_list_greater100$X10695_mcp_utm) # check that it worked
names(bear_list_greater100)

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_list_greater100)){
  coordinates(bear_list_greater100[[i]]) <- c("EASTING", "NORTHING") 
  proj4string(bear_list_greater100[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=75") 
  temp <- bear_list_greater100[[i]]       
  temp[["ID"]] <- as.factor(temp[["ID"]]) 
  bear_list_greater100[[i]] <- temp         
}

# rename items in list
names(bear_list_greater100) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
names(bear_list_greater100)
head(bear_list_greater100$'X10695_mcp_utm') # note that easting and northing columns are gone because they're used to make it a spatial object

# Check projections
proj4string(bear_list_greater100[[1]])
proj4string(bear_list_greater100$'X13289_mcp_utm')

# create loop for the MCPs
# Make an empty list for the MCPs
bear_list_greater100_mcps <- list()

# create 95% MCP for each (Step 5)
for(i in 1: length(bear_list_greater100)){
  temp_dat <- bear_list_greater100[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_list_greater100_mcps[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}

# Name items in the list
names(bear_list_greater100_mcps) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
head(bear_list_greater100_mcps$X13289_mcp_utm) # check that it worked

# since area is calculated here, just turn this into a dataframe and don't use mcp.area()
bear_list_greater100_mcps_df <- data.frame(Reduce(rbind, bear_list_greater100_mcps))
summary(bear_list_greater100_mcps_df)

# test plot - it worked! 
plot(bear_list_greater100_mcps$X13289_mcp_utm)

# update df column names
colnames(bear_list_greater100_mcps_df)
names(bear_list_greater100_mcps_df)[1] <- "ID"
names(bear_list_greater100_mcps_df)[2] <- "AREA"

# add method column for plotting later
mcp_list <- 1:5
mcp_list2 <- rep("MCP", length(mcp_list))
bear_list_greater100_mcps_df <- cbind(bear_list_greater100_mcps_df, mcp_list2)
names(bear_list_greater100_mcps_df)[3] <- "METHOD"


# 5. Pooled Href Kernels ------ --------


# make a list of only the bears with >30 fixes (across all years) - Seaman et al. 1999
bear_href_list <- list(X10695, X10700, X10703, X10707, X10709, X11974, X11975, X12080, X12086, X12092, X13284, X13289, X13292, X13428, X13437, X13746, X30126, X30129, X30131, X30135, X30140)

# name items in list
names(bear_href_list) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_list$X13289) # check that it worked
names(bear_href_list) 

# Make empty list for the kernel and then extract it with loop
bear_href <- list()

for(i in 1:length(bear_href_list)){
  temp_dat <- bear_href_list[[i]]
  bear_href[[i]] <- kernelUD(SpatialPoints(temp_dat[, 6:5]), h="href")
} # this worked! no warnings!


plot(bear_href[[1]]) # test plot

# Name items in the list  
names(bear_href) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href$X13289) 

# Make empty list for the vertices and then extract with loop (for plotting)
bear_href_ver <- list()

for(i in 1:length(bear_href)){ 
  temp_dat2 <- bear_href[[i]]
  bear_href_ver[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2') 
} # this worked!

plot(bear_href_ver[[1]]) 

# Name items in the list
names(bear_href_ver) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_ver$X10695) # this worked!

# calculate area and make into dataframe

# create empty dataframe to put in the areas and fill first row with IDs
bears_href_area_df <- data.frame(ID = rep(NA, length(bear_href_list)), AREA_M = rep(NA, length(bear_href_list)))
bears_href_area_df$ID <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")

# Make an empty list
bears_href_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_href_ver)){
  bears_href_area[[i]] <- gArea(bear_href_ver[[i]])
}

# flip new dataframe
bears_href_area <- t(bears_href_area)
bears_href_area <- unlist(bears_href_area)

# add values from new dataframe into bears_95mcp_summary
bears_href_area_df$AREA_M <- bears_href_area
# make a new column that is km2 (previous was m2) and make into a spreadsheet
bears_href_area_df$AREA <- bears_href_area_df$AREA_M*0.000001

# drop AREA_M column
bears_href_area_df =subset(bears_href_area_df, select=-c(AREA_M))


# add method column for plotting later
href_list <- 1:21
href_list2 <- rep("HREF", length(href_list))
bears_href_area_df <- cbind(bears_href_area_df, href_list2)
names(bears_href_area_df)[3] <- "METHOD"





# 6. Pooled Hpi Kernels ------

# use same list we used for href kernels at the beginning (bear_href_list) - both need >= 30 fixes

# Make empty list and then create samse pilot with loop
bear_hpi_amsepilot_utm <- list()

for(i in 1:length(bear_href_list)){
  temp_dat <- bear_href_list[[i]]
  bear_hpi_amsepilot_utm[[i]] <- Hpi(temp_dat[, 6:5], pilot="amse", binned=T) 
} 

# Make empty list and then make samse contours
bear_polygon_utm <- list()

for(i in 1:length(bear_hpi_amsepilot_utm)){
  bear_hpi_amse_contour_utm <- kde(bear_href_list[[i]][6:5], H=bear_hpi_amsepilot_utm[[i]]) 
  bear_contourlevels_utm <- contourLevels(bear_hpi_amse_contour_utm, cont = 95)               
  bear_lines_utm <- contourLines(x=bear_hpi_amse_contour_utm$eval.points[[1]], y=bear_hpi_amse_contour_utm$eval.points[[2]], 
                                 z=bear_hpi_amse_contour_utm$estimate, level=bear_contourlevels_utm) 
  bear_sldf_utm = ContourLines2SLDF(bear_lines_utm)  
  bear_polyset_utm = SpatialLines2PolySet(bear_sldf_utm) 
  bear_polygon_utm[[i]] = PolySet2SpatialPolygons(bear_polyset_utm) 
} 

# name items in list (same order as bear_KDE_list)
names(bear_polygon_utm) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_polygon_utm$X13289) # check that it worked
names(bear_polygon_utm) 

# test area
gArea(bear_polygon_utm[[21]])
gArea(bear_polygon_utm$X30140)

# create empty dataframe to put in the areas and fill first row with IDs
bear_hpi_area_df <- data.frame(ID = rep(NA, length(bear_href_list)))
bear_hpi_area_df$ID <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")

# Make an empty list
bear_hpi_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_polygon_utm)){
  bear_hpi_area[[i]] <- gArea(bear_polygon_utm[[i]])
}

# add values into new dataframe
bear_hpi_area_df$AREA_M <- bear_hpi_area
str(bear_hpi_area_df)
bear_hpi_area_df$AREA_M <- as.numeric(bear_hpi_area_df$AREA_M)

# make a new column that is km2 (previous was m2) and make into a spreadsheet
head(bear_hpi_area_df)
bear_hpi_area_df$AREA <- bear_hpi_area_df$AREA_M*0.000001

# drop AREA_M column
bear_hpi_area_df=subset(bear_hpi_area_df, select=-c(AREA_M))


# add method column for plotting later
hpi_list <- 1:21
hpi_list2 <- rep("HPI", length(hpi_list))
bear_hpi_area_df <- cbind(bear_hpi_area_df, hpi_list2)
names(bear_hpi_area_df)[3] <- "METHOD"



# 7. ANNUAL HOME RANGES: Separate bear years, count fixes -------

# separate bears_final_Nov2020_UTM_LATLONG into bear years
X03956_1994 <- X03956
X10374_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10374" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X10374_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10374" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X10393_1994 <- X10393
X10695_1991 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10695" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1991")
X10695_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10695" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X10700_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10700" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X10700_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10700" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X10700_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10700" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X10703_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10703" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X10703_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10703" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X10703_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10703" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X10707_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10707" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X10707_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10707" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X10709_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10709" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X10709_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10709" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X10709_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10709" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X11974_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11974" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X11974_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11974" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X11974_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11974" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X11975_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11975" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X11975_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11975" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X11975_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11975" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X12078_1994 <- X12078
X12080_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12080" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X12080_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12080" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X12081_1994 <- X12081
X12082_1994 <- X12082
X12083_1994 <- X12083
X12086_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12086" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X12086_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12086" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X12092_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12092" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X12092_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12092" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X13284_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13284" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X13284_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13284" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X13284_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13284" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13289_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X13289_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X13289_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13289_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X13292_1992 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1992")
X13292_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X13292_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13292_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X13428_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13428" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X13428_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13428" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13428_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13428" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X13437_1993 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13437" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1993")
X13437_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13437" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13437_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13437" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X13746_1994 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13746" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1994")
X13746_1995 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13746" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1995")
X30126_1997 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30126" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1997")
X30126_1998 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30126" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1998")
X30126_1999 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30126" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1999")
X30129_1997 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30129" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1997")
X30129_1998 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30129" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1998")
X30129_1999 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30129" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1999")
X30131_1997 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30131" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1997")
X30131_1998 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30131" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1998")
X30131_1999 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30131" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1999")
X30135_1997 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30135" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1997")
X30135_1998 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30135" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1998")
X30135_1999 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30135" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1999")
X30140_1997 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1997")
X30140_1998 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1998")
X30140_1999 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140" & bears_final_Nov2020_UTM_LATLONG$YEAR=="1999")
X30140_2000 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140" & bears_final_Nov2020_UTM_LATLONG$YEAR=="2000")
X30140_2001 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140" & bears_final_Nov2020_UTM_LATLONG$YEAR=="2001")


# Count # fixes per bear year
str(X03956_1994) # 12
str(X10374_1994) # 15
str(X10374_1995) # 12
str(X10393_1994) # 8
str(X10695_1991) # 62
str(X10695_1992) # 48
str(X10700_1992) # 17
str(X10700_1993) # 45
str(X10700_1994) # 35
str(X10703_1992) # 24
str(X10703_1993) # 38
str(X10703_1994) # 38
str(X10707_1992) # 36
str(X10707_1993) # 39
str(X10709_1992) # 24
str(X10709_1993) # 48
str(X10709_1994) # 14
str(X11974_1993) # 15
str(X11974_1994) # 21
str(X11974_1995) # 17
str(X11975_1993) # 13
str(X11975_1994) # 41
str(X11975_1995) # 34
str(X12078_1994) # 5
str(X12080_1994) # 50
str(X12080_1995) # 43
str(X12081_1994) # 6
str(X12082_1994) # 9
str(X12083_1994) # 5
str(X12086_1994) # 23
str(X12086_1995) # 31
str(X12092_1994) # 31
str(X12092_1995) # 20
str(X13284_1992) # 19
str(X13284_1993) # 47
str(X13284_1994) # 24
str(X13289_1992) # 15
str(X13289_1993) # 45
str(X13289_1994) # 55
str(X13289_1995) # 24
str(X13292_1992) # 13
str(X13292_1993) # 56
str(X13292_1994) # 36
str(X13292_1995) # 20
str(X13428_1993) # 15
str(X13428_1994) # 50
str(X13428_1995) # 10
str(X13437_1993) # 12
str(X13437_1994) # 18
str(X13437_1995) # 46
str(X13746_1994) # 50
str(X13746_1995) # 58
str(X30126_1997) # 10
str(X30126_1998) # 39
str(X30126_1999) # 38
str(X30129_1997) # 10
str(X30129_1998) # 30
str(X30129_1999) # 39
str(X30131_1997) # 8
str(X30131_1998) # 32
str(X30131_1999) # 39
str(X30135_1997) # 10
str(X30135_1998) # 29
str(X30135_1999) # 32
str(X30140_1997) # 8
str(X30140_1998) # 21
str(X30140_1999) # 2
str(X30140_2000) # 8
str(X30140_2001) # 3

# 8. Annual MCPs ---------

# Note, use bears with >= 50 fixes: X10695_1991, X12080_1994, X13289_1994, X13292_1993, X13428_1994, X13746_1994, X13746_1995

# make a list 
bear_annualMCP_list <- list(X10695_1991, X12080_1994, X13289_1994, X13292_1993, X13428_1994, X13746_1994, X13746_1995)
names(bear_annualMCP_list)

# name items in list
names(bear_annualMCP_list) <- c("X10695_1991", "X12080_1994", "X13289_1994", "X13292_1993", "X13428_1994", "X13746_1994", "X13746_1995")
head(bear_annualMCP_list$X10695_1991) # check that it worked
names(bear_annualMCP_list) 

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_annualMCP_list)){
  coordinates(bear_annualMCP_list[[i]]) <- c("EASTING", "NORTHING")
  proj4string(bear_annualMCP_list[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=75") 
  temp <- bear_annualMCP_list[[i]]         
  temp[["ID"]] <- as.factor(temp[["ID"]]) 
  bear_annualMCP_list[[i]] <- temp        
}

head(bear_annualMCP_list$X10695_1991) # note that easting and northing columns are gone because they're used to make it a spatial object


# Check projections
proj4string(bear_annualMCP_list[[1]])
proj4string(bear_annualMCP_list$'X10695_1991')


# create loop for the MCPs
# Make an empty list for the MCPs
bear_annualMCPs <- list()


# create 95% MCP for each (Step 5)
for(i in 1: length(bear_annualMCP_list)){
  temp_dat <- bear_annualMCP_list[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_annualMCPs[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}


# Name items in the list
names(bear_annualMCPs) <- c("X10695_1991_utm", "X12080_1994_utm", "X13289_1994_utm", "X13292_1993_utm", "X13428_1994_utm", "X13746_1994_utm", "X13746_1995_utm")
head(bear_annualMCPs$X10695_1991_utm) # check that it worked


# since area is calculated here, just turn this into a dataframe and don't use mcp.area()
bear_annualMCPs_df <- data.frame(Reduce(rbind, bear_annualMCPs))
summary(bear_annualMCPs_df)


# update column names and add more columns
colnames(bear_annualMCPs_df)
names(bear_annualMCPs_df)[1] <- "ID"
names(bear_annualMCPs_df)[2] <- "AREA"
bear_annualMCPs_df$ID_YEAR<- c("X10695_1991", "X12080_1994", "X13289_1994", "X13292_1993", "X13428_1994", "X13746_1994", "X13746_1995")
bear_annualMCPs_df$YEAR <- c("1991", "1994", "1994", "1993", "1994", "1994", "1995") 


# add method column for plotting later
mcp_annuallist <- 1:7
mcp_annuallist2 <- rep("MCP", length(mcp_annuallist))
bear_annualMCPs_df <- cbind(bear_annualMCPs_df, mcp_annuallist2)
names(bear_annualMCPs_df)[5] <- "METHOD"


# test plot - it worked! 
plot(bear_annualMCPs$X10695_1991_utm)


summary(bear_annualMCPs_df)


# yearly stats
unique(bear_annualMCPs_df$YEAR)

MCP_1991 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$YEAR=="1991")
summary(MCP_1991)

MCP_1993 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$YEAR=="1993")
summary(MCP_1993)

MCP_1994 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$YEAR=="1994")
summary(MCP_1994)

MCP_1995 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$YEAR=="1995")
summary(MCP_1995)



# 9. Annual Href Kernels ------

# make list of bears with >=30 bear years
bear_annualhref_list <- list(X10695_1991, X10695_1992, X10700_1993, X10700_1994, X10703_1993, X10703_1994, X10707_1992, X10707_1993, X10709_1993, X11975_1994, X11975_1995, X12080_1994, X12080_1995,
                             X12086_1995, X12092_1994, X13284_1993, X13289_1993, X13289_1994, X13292_1993, X13292_1994, X13428_1994, X13437_1995, X13746_1994, X13746_1995, X30126_1998, X30126_1999,
                             X30129_1998, X30129_1999, X30131_1998, X30131_1999, X30135_1999)
names(bear_annualhref_list)
names(bear_annualhref_list) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref_list$X10695_1991)

# Make empty list for the kernel and then extract it with loop
bear_annualhref <- list()

for(i in 1:length(bear_annualhref_list)){
  temp_dat <- bear_annualhref_list[[i]]
  bear_annualhref[[i]] <- kernelUD(SpatialPoints(temp_dat[, 6:5]), h="href")
} # this worked! no warnings!

plot(bear_annualhref[[1]]) # test plot

# Name items in the list  
names(bear_annualhref) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref$X10695_1991) 

# Make empty list for the vertices and then extract with loop (for plotting)
bear_annualhref_ver <- list()

for(i in 1:length(bear_annualhref)){ 
  temp_dat2 <- bear_annualhref[[i]]
  bear_annualhref_ver[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2') 
} # this worked!! no warnings!

plot(bear_annualhref_ver[[1]]) 

# Name items in the list
names(bear_annualhref_ver) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref_ver$X10695_1991) # this worked!

# calculate area and make into dataframe

# create empty dataframe to put in the areas and fill first row with IDs
bear_annualhref_area_df <- data.frame(ID_YEAR=rep(NA, length(bear_annualhref_list)), ID = rep(NA, length(bear_annualhref_list)), YEAR = rep(NA, length(bear_annualhref_list)), AREA_M = rep(NA, length(bear_annualhref_list)))
bear_annualhref_area_df$ID_YEAR <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bear_annualhref_area_df$ID <- c("X10695", "X10695", "X10700", "X10700", "X10703", "X10703", "X10707", "X10707", "X10709", "X11975", "X11975", "X12080", "X12080", "X12086", "X12092", "X13284", "X13289", "X13289", "X13292", "X13292", "X13428", "X13437", "X13746", "X13746", "X30126", "X30126", "X30129", "X30129", "X30131", "X30131", "X30135")
bear_annualhref_area_df$YEAR <- c("1991", "1992", "1993", "1994", "1993", "1994", "1992", "1993", "1993", "1994", "1995", "1994", "1995", "1995", "1994", "1993", "1993", "1994", "1993", "1994", "1994", "1995", "1994", "1995", "1998", "1999", "1998", "1999", "1998", "1999", "1999")

# Make an empty list
bears_annualhref_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_annualhref_ver)){
  bears_annualhref_area[[i]] <- gArea(bear_annualhref_ver[[i]])
}

# flip new dataframe
bears_annualhref_area <- t(bears_annualhref_area)
bears_annualhref_area <- unlist(bears_annualhref_area)

# add values from new dataframe into bears_95mcp_summary
bear_annualhref_area_df$AREA_M <- bears_annualhref_area

# make a new column that is km2 (previous was m2) and make into a spreadsheet
head(bear_annualhref_area_df)
bear_annualhref_area_df$AREA <- bear_annualhref_area_df$AREA_M*0.000001

# drop AREA_M column
bear_annualhref_area_df =subset(bear_annualhref_area_df, select=-c(AREA_M))

# add method column for plotting later
href_annuallist <- 1:31
href_annuallist2 <- rep("HREF", length(href_annuallist))
bear_annualhref_area_df <- cbind(bear_annualhref_area_df, href_annuallist2)
names(bear_annualhref_area_df)[5] <- "METHOD"


# number of bear years = number of names in annual list (since its already separated by bear and year) 
names(bear_annualhref_list)

# yearly stats
annualhref_1991 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1991")
summary(annualhref_1991)

annualhref_1992 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1992")
summary(annualhref_1992)

annualhref_1993 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1993")
summary(annualhref_1993)

annualhref_1994 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1994")
summary(annualhref_1994)

annualhref_1995 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1995")
summary(annualhref_1995)

annualhref_1998 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1998")
summary(annualhref_1998)

annualhref_1999 <- bear_annualhref_area_df %>% filter(bear_annualhref_area_df$YEAR=="1999")
summary(annualhref_1999)



# 10. Annual Hpi Kernels -------

# use same list as href (bear_annualhref_list) - both need >=30 fixes

# Make empty list and then create amse pilot with loop
bear_annualhpi_amsepilot_utm <- list()

for(i in 1:length(bear_annualhref_list)){
  temp_dat <- bear_annualhref_list[[i]]
  bear_annualhpi_amsepilot_utm[[i]] <- Hpi(temp_dat[, 6:5], pilot="amse", binned=T) 
} 

# Make empty list and then make amse contours
bear_annualpolygon_utm <- list()

for(i in 1:length(bear_annualhpi_amsepilot_utm)){
  bear_annualhpi_amse_contour_utm <- kde(bear_annualhref_list[[i]][6:5], H=bear_annualhpi_amsepilot_utm[[i]]) 
  bear_annualcontourlevels_utm <- contourLevels(bear_annualhpi_amse_contour_utm, cont = 95)               
  bear_annuallines_utm <- contourLines(x=bear_annualhpi_amse_contour_utm$eval.points[[1]], y=bear_annualhpi_amse_contour_utm$eval.points[[2]], 
                                       z=bear_annualhpi_amse_contour_utm$estimate, level=bear_annualcontourlevels_utm) 
  bear_annualsldf_utm = ContourLines2SLDF(bear_annuallines_utm)  
  bear_annualpolyset_utm = SpatialLines2PolySet(bear_annualsldf_utm) 
  bear_annualpolygon_utm[[i]] = PolySet2SpatialPolygons(bear_annualpolyset_utm) 
} 

# name items in list 
names(bear_annualpolygon_utm) <- c("X10695_1991_poly", "X10695_1992_poly", "X10700_1993_poly", "X10700_1994_poly", "X10703_1993_poly", "X10703_1994_poly", "X10707_1992_poly", "X10707_1993_poly", "X10709_1993_poly", "X11975_1994_poly", "X11975_1995_poly", "X12080_1994_poly", "X12080_1995_poly", "X12086_1995_poly", "X12092_1994_poly", "X13284_1993_poly", "X13289_1993_poly", "X13289_1994_poly", "X13292_1993_poly", "X13292_1994_poly", "X13428_1994_poly", "X13437_1995_poly", "X13746_1994_poly", "X13746_1995_poly", "X30126_1998_poly", "X30126_1999_poly", "X30129_1998_poly", "X30129_1999_poly", "X30131_1998_poly", "X30131_1999_poly", "X30135_1999_poly")
head(bear_annualpolygon_utm$X10695_1991_poly) # check that it worked - should be SpatialPolygons class
names(bear_annualpolygon_utm) 

# test area
gArea(bear_annualpolygon_utm[[6]]) 

# create empty dataframe to put in the areas and fill first row with IDs
bear_hpi_annualarea_df <- data.frame(ID_YEAR=rep(NA, length(bear_annualhref_list)), ID = rep(NA, length(bear_annualhref_list)), YEAR = rep(NA, length(bear_annualhref_list)), AREA_M = rep(NA, length(bear_annualhref_list)))
bear_hpi_annualarea_df$ID_YEAR <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bear_hpi_annualarea_df$ID <- c("X10695", "X10695", "X10700", "X10700", "X10703", "X10703", "X10707", "X10707", "X10709", "X11975", "X11975", "X12080", "X12080", "X12086", "X12092", "X13284", "X13289", "X13289", "X13292", "X13292", "X13428", "X13437", "X13746", "X13746", "X30126", "X30126", "X30129", "X30129", "X30131", "X30131", "X30135")
bear_hpi_annualarea_df$YEAR <- c("1991", "1992", "1993", "1994", "1993", "1994", "1992", "1993", "1993", "1994", "1995", "1994", "1995", "1995", "1994", "1993", "1993", "1994", "1993", "1994", "1994", "1995", "1994", "1995", "1998", "1999", "1998", "1999", "1998", "1999", "1999")

# Make an empty list
bear_hpi_annualarea <- list()

# create loop to calculate the areas
for(i in 1:length(bear_annualpolygon_utm)){
bear_hpi_annualarea[[i]] <- gArea(bear_annualpolygon_utm[[i]]) 
}

# Below I would put the area values into a dataframe, but it doesn't work because of the above loop

# add values into new dataframe
bear_hpi_annualarea_df$AREA_M <- bear_hpi_annualarea
str(bear_hpi_annualarea_df)
bear_hpi_annualarea_df$AREA_M <- as.numeric(bear_hpi_annualarea_df$AREA_M)

# make a new column that is km2 (previous was m2) and make into a spreadsheet
head(bear_hpi_annualarea_df)
bear_hpi_annualarea_df$AREA <- bear_hpi_annualarea_df$AREA_M*0.000001

# drop AREA_M column
bear_hpi_annualarea_df =subset(bear_hpi_annualarea_df, select=-c(AREA_M))

# add method column for plotting later
hpi_annuallist <- 1:31
hpi_annuallist2 <- rep("HPI", length(hpi_annuallist))
bear_hpi_annualarea_df <- cbind(bear_hpi_annualarea_df, hpi_annuallist2)
names(bear_hpi_annualarea_df)[5] <- "METHOD"



# yearly stats

annualhpi_1991 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1991")
summary(annualhpi_1991)

annualhpi_1992 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1992")
summary(annualhpi_1992)

annualhpi_1993 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1993")
summary(annualhpi_1993)

annualhpi_1994 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1994")
summary(annualhpi_1994)

annualhpi_1995 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1995")
summary(annualhpi_1995)

annualhpi_1998 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1998")
summary(annualhpi_1998)

annualhpi_1999 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1999")
summary(annualhpi_1999)


# 11. Create final dataframes for all HRs and create figures ------


# Create pooled dataframe
head(bear_list_greater100_mcps_df) # columns are: ID, AREA, METHOD
head(bears_href_area_df) # columns are: ID, AREA, METHOD
head(bear_hpi_area_df) # columns are: ID, AREA, METHOD

pooled_homerange_areas <- rbind(bear_list_greater100_mcps_df, bears_href_area_df, bear_hpi_area_df)
write.csv(pooled_homerange_areas, "data/Oct2020work/pooled_homerange_areas.csv")

# Create annual dataframe
head(bear_annualMCPs_df) # columns are: ID, AREA, ID_YEAR, YEAR, METHOD
head(bear_annualhref_area_df) # columns are: ID_YEAR, ID, YEAR, AREA, METHOD
head(bear_hpi_annualarea_df) # columns are: ID_YEAR, ID, YEAR, AREA, METHOD

annual_homerange_areas <- rbind(bear_annualMCPs_df, bear_annualhref_area_df, bear_hpi_annualarea_df)
write.csv(annual_homerange_areas, "data/Oct2020work/annual_homerange_areas.csv")


# summarize data
summary(pooled_homerange_areas)
pooled_homerange_summary <- pooled_homerange_areas %>% group_by(METHOD) %>% summarize(mean_area=mean(AREA), count=n())
summary(annual_homerange_areas)
annual_homerange_summary <- annual_homerange_areas %>% group_by(METHOD) %>% summarize(mean_area=mean(AREA), count=n())

-----
  
# Plotting


      # POOLED

# line/point plot
pooled_homerange_areas_plot1 <- ggplot(pooled_homerange_areas, aes(x=ID, y=AREA, group=METHOD, colour=METHOD)) +
  geom_line(colour="lightgrey") +
  geom_point() +
  #scale_colour_manual(values=c("palegreen3", "skyblue3", "palevioletred3")) +
  scale_y_continuous(limits=c(0, 1100000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000")) +
  ylab("Area (Km2)") +
  xlab("Bear ID") +
  theme_nuwcru()
pooled_homerange_areas_plot1

# bar plot
pooled_homerange_areas_barplot1 <- ggplot(pooled_homerange_areas, aes(x=ID, y=AREA, fill=METHOD)) +
  geom_col(position="dodge", width=0.6) +
  scale_fill_manual(values=c("palegreen3", "skyblue3", "palevioletred3")) +
  scale_y_continuous(limits=c(0, 1100000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000")) +
  ylab("Area (Km2)") +
  xlab("Bear ID") +
  theme_nuwcru()
pooled_homerange_areas_barplot1

# boxplot
head(pooled_homerange_summary)
str(pooled_homerange_summary)
str(pooled_homerange_areas)

plotlabels_pooled <- pooled_homerange_summary[-c(3)]
plotlabels_pooled$mean_area_round <- round(plotlabels_pooled$mean_area, digits=2)

ggplot(pooled_homerange_areas) +
  geom_boxplot(aes(y=AREA, x=METHOD), varwidth = T, alpha=0.2) +
  geom_abline(intercept=188294.78, col="darkred", linetype="dashed") + # intercept=mean of all methods
  scale_y_continuous(limits=c(-5, 1800000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000, 1600000, 1400000, 1800000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000", "1600000", "1400000", "1800000")) +
  ylab("Area (Km2)") +
  xlab("Method") +
  geom_text(data=plotlabels_pooled, aes(x=METHOD, y=mean_area_round, label=mean_area_round), size=3.5,  vjust=-12) +
  theme_nuwcru()



      # ANNUAL

# line/point plot
annual_homerange_areas_plot1 <- ggplot(annual_homerange_areas, aes(x=ID_YEAR, y=AREA, group=METHOD, colour=METHOD)) +
  geom_line(colour="lightgrey") +
  geom_point() +
  #scale_colour_manual(values=c("palegreen3", "skyblue3", "palevioletred3")) +
  scale_y_continuous(limits=c(0, 1800000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000, 1400000, 1600000, 1800000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000", "1400000", "1600000", "1800000")) +
  ylab("Area (Km2)") +
  xlab("Bear Year") +
  theme_nuwcru()
annual_homerange_areas_plot1

# bar plot
annual_homerange_areas_barplot1 <- ggplot(annual_homerange_areas, aes(x=ID_YEAR, y=AREA, fill=METHOD)) +
  geom_col(position="dodge", width=0.6) +
  scale_fill_manual(values=c("palegreen3", "skyblue3", "palevioletred3")) +
  scale_y_continuous(limits=c(0, 1800000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000, 1400000, 1600000, 1800000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000", "1400000", "1600000", "1800000")) +
  ylab("Area (Km2)") +
  xlab("Bear Year") +
  theme_nuwcru()
annual_homerange_areas_barplot1

# boxplot
head(annual_homerange_summary)

plotlabels_annual <- annual_homerange_summary[-c(3)]
plotlabels_annual$mean_area_round <- round(plotlabels_annual$mean_area, digits=2)

ggplot(annual_homerange_areas) +
  geom_boxplot(aes(y=AREA, x=METHOD), varwidth = T, alpha=0.2) +
  geom_abline(intercept=187300.70 , col="darkred", linetype="dashed") + # intercept=mean of all methods
  scale_y_continuous(limits=c(-5, 1800000), breaks=c(0, 200000, 400000, 600000, 800000, 1000000, 1200000, 1600000, 1400000, 1800000), labels=c("0", "200000", "400000", "600000", "800000", "1000000", "1200000", "1600000", "1400000", "1800000")) +
  ylab("Area (Km2)") +
  xlab("Method") +
  geom_text(data=plotlabels_annual, aes(x=METHOD, y=mean_area_round, label=mean_area_round), size=3.5,  vjust=-10) +
  theme_nuwcru()






# 11a. Create final annual HR figures for manuscript (without pooled and Href) ------

# import data and format
annual_homerange_areas <- read.csv("data/Oct2020work/annual_homerange_areas.csv")
head(annual_homerange_areas)
unique(annual_homerange_areas$METHOD)
annual_homerange_areas2 <- annual_homerange_areas %>% filter(METHOD=="MCP" | METHOD=="HPI")
unique(annual_homerange_areas2$METHOD)

      # calculate standard deviation for both methods
MCP <- annual_homerange_areas2 %>% filter(METHOD=="MCP")
sd(MCP$AREA) # 73984.9
summary(MCP)
HPI <- annual_homerange_areas2 %>% filter(METHOD=="HPI")
sd(HPI$AREA) # 59687.65

      # create summary dataframe and format
annual_homerange_summary <- annual_homerange_areas2 %>% group_by(METHOD) %>% summarize(mean_area=mean(AREA), count=n())
head(annual_homerange_summary) # HPI is first
annual_homerange_summary$SD_area <- c("59687.65", "73984.9")
str(annual_homerange_summary)
annual_homerange_summary$mean_area <- as.numeric(annual_homerange_summary$mean_area)
annual_homerange_summary$SD_area <- as.numeric(annual_homerange_summary$SD_area)

      # calculate mean of both
head(annual_homerange_summary) # MCP: 137,335; HPI = 66,215
(137335+66215)/2 # = 101775

      # create summary dataframe per year and format
annual_summary <- annual_homerange_areas2 %>% group_by(METHOD, YEAR) %>% summarize(mean_area=mean(AREA), count=n())
unique(annual_summary$YEAR) # 1991-1999
              # get standard deviations per year for each method
MCP_1991 <- annual_homerange_areas2 %>% filter(METHOD=="MCP" & YEAR=="1991")
sd(MCP_1991$AREA) # NA: only 1
MCP_1993 <- annual_homerange_areas2 %>% filter(METHOD=="MCP" & YEAR=="1993")
sd(MCP_1993$AREA) # NA: only 1
MCP_1994 <- annual_homerange_areas2 %>% filter(METHOD=="MCP" & YEAR=="1994")
sd(MCP_1994$AREA) # 100091.3
MCP_1995 <- annual_homerange_areas2 %>% filter(METHOD=="MCP" & YEAR=="1995")
sd(MCP_1995$AREA) # NA: only 1
HPI_1991 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1991")
sd(HPI_1991$AREA) # NA: only 1
HPI_1992 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1992")
sd(HPI_1992$AREA) # 39579.65
HPI_1993 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1993")
sd(HPI_1993$AREA) # 50445.11
HPI_1994 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1994")
sd(HPI_1994$AREA) # 86635.87
HPI_1995 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1995")
sd(HPI_1995$AREA) # 55720.25
HPI_1998 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1998")
sd(HPI_1998$AREA) # 56381.19
HPI_1999 <- annual_homerange_areas2 %>% filter(METHOD=="HPI" & YEAR=="1999")
sd(HPI_1999$AREA) # 22086.38

head(annual_summary) # HPI is first - all years (ordered chronologically), followed by MCP (also chronologically)
tail(annual_summary) # HPI only has data for 1991-1995, 1998-1999; MCP only has 1991, 1993-1995
annual_summary$SD_area <- c("NA", "39579.65", "50445.11", "86635.87", "55720.25", "56381.19", "22086.38", # these are HPI
                            "NA", "NA", "100091.3", "NA") # these are MCP
head(annual_summary)
str(annual_summary)
annual_summary$SD_area <- as.numeric(annual_summary$SD_area)

      # means per year
total_annual_summary <- annual_summary %>% group_by(YEAR) %>% summarize(total_mean=mean(mean_area))
head(total_annual_summary)

annual_summary2 <- merge(total_annual_summary, annual_summary)


###

# changed all "HPI" to "KDE" for manuscript

annual_homerange_summary$METHOD[annual_homerange_summary$METHOD == "HPI"] <- "KDE"
annual_summary2$METHOD[annual_summary2$METHOD == "HPI"] <- "KDE" 

# all together (i.e, using annual_homerange_summary)
error_plot <- ggplot(annual_homerange_summary, aes(x=METHOD, y=mean_area, colour=METHOD)) +
  geom_pointrange(aes(ymin=mean_area-SD_area, ymax=mean_area+SD_area)) + 
  geom_errorbar(aes(ymin=mean_area-SD_area, ymax=mean_area+SD_area), width=0.2) + 
  geom_abline(intercept=101775 , col="black", linetype="dashed") + 
  scale_colour_manual(values=c("palegreen3", "skyblue3")) +
  scale_y_continuous(limits=c(0, 250000), breaks=c(0, 50000, 100000, 150000, 200000, 250000), labels=c("0", "50,000", "100,000", "150,000", "200,000", "250,000")) +
  theme_nuwcru()
error_plot2 <- error_plot + theme(axis.title.x = element_blank()) + labs(y="Home range size (km2)")
error_plot2

# annual (using annual_summary)
pd <- position_dodge(width=0.4)
error_annualplot <- ggplot(annual_summary2, aes(x=YEAR, y=mean_area, colour=METHOD)) +
  geom_pointrange(aes(ymin=mean_area-SD_area, ymax=mean_area+SD_area), position=pd) + 
  geom_errorbar(aes(ymin=mean_area-SD_area, ymax=mean_area+SD_area), width=0.2, position=pd) + 
  scale_colour_manual(values=c("palegreen3", "skyblue3")) +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000), labels=c("0", "50,000", "100,000", "150,000", "200,000", "250,000")) +
  scale_x_continuous(breaks=c(1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999), labels=c("1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999")) +
  theme_nuwcru()
error_annualplot
error_annualplot2 <- error_annualplot + 
  theme(axis.title.x = element_blank()) + labs(y="Home range size (km2)") +
  geom_text(aes(label=count), vjust=-1.0, colour="black")
error_annualplot2
#error_annualplot3 <- error_annualplot2 +
  #geom_point(aes(x=YEAR, y=total_mean), colour="red", shape=4)
#error_annualplot3


###


# 12. Compare HR methods -------------

# import final dataframes
pooled_homerange_areas <- read.csv("data/Oct2020work/pooled_homerange_areas.csv")
annual_homerange_areas <- read.csv("data/Oct2020work/annual_homerange_areas.csv")

# pooled model
head(pooled_homerange_areas)
hist(pooled_homerange_areas$AREA)

pooled_model <- lmer(AREA~METHOD+(1|ID), data=pooled_homerange_areas)
summary(pooled_model) # Model summary: important #s = coefficient estimates (estimate column)
ranef(pooled_model) # Difference b/n individual and pop. average
coef(pooled_model) # Coefficients for each individual


# annual model
head(annual_homerange_areas)
hist(annual_homerange_areas$AREA)

annual_model <- lmer(AREA~METHOD+(1|ID), data=annual_homerange_areas)
summary(annual_model) # Model summary: important #s = coefficient estimates (estimate column)
ranef(annual_model) # Difference b/n individual and pop. average
coef(annual_model) # Coefficients for each individual



------
  
# try standardizing the data first

head(annual_homerange_areas)
annual_homerange_areas$SCALE_AREA <- scale(annual_homerange_areas$AREA)
hist(annual_homerange_areas$SCALE_AREA)

head(pooled_homerange_areas)
pooled_homerange_areas$SCALE_AREA <- scale(pooled_homerange_areas$AREA)
hist(pooled_homerange_areas$SCALE_AREA)

------
  
# redo models with standardized data

# pooled model
pooled_model_scale <- lmer(SCALE_AREA~METHOD+(1|ID), data=pooled_homerange_areas)
summary(pooled_model_scale) 
ranef(pooled_model_scale)
coef(pooled_model_scale) 


# annual model
annual_model_scale <- lmer(SCALE_AREA~METHOD+(1|ID), data=annual_homerange_areas)
summary(annual_model_scale) 
ranef(annual_model_scale) 
coef(annual_model_scale) 


-----

# plotting

# pooled boxplot
pooled_homerange_boxplot <- ggplot(pooled_homerange_areas, aes(y=SCALE_AREA, fill=METHOD)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  ylab("Scaled Area") +
  theme_nuwcru()
pooled_homerange_boxplot
pooled_homerange_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

# annual boxplot
annual_homerange_boxplot <- ggplot(annual_homerange_areas, aes(y=SCALE_AREA, fill=METHOD)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  ylab("Scaled Area") +
  theme_nuwcru()
annual_homerange_boxplot
annual_homerange_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

# pooled pairwise
      # first separate methods
pooledMCP <- pooled_homerange_areas %>% filter(METHOD=="MCP")
pooledHREF <- pooled_homerange_areas %>% filter(METHOD=="HREF")
pooledHPI <- pooled_homerange_areas %>% filter(METHOD=="HPI")
      # plot
plot(x=pooledHPI$AREA, y=pooledHREF$AREA)
plot(x=pooledHPI$SCALE_AREA, y=pooledHREF$SCALE_AREA)
      # note that I can't compare MCP to kernels because they have different # of records

# annual pairwise
      # first separate methods
annualMCP <- annual_homerange_areas %>% filter(METHOD=="MCP")
annualHREF <- annual_homerange_areas %>% filter(METHOD=="HREF")
annualHPI <- annual_homerange_areas %>% filter(METHOD=="HPI")
      # plot
plot(x=annualHPI$AREA, y=annualHREF$AREA)
plot(x=annualHPI$SCALE_AREA, y=annualHREF$SCALE_AREA)
      # note that I can't compare MCP to kernels because they have different # of records


# 13. Analyzing changes in HR size over time --------------

# import data and create scaled area
annual_homerange_areas <- read.csv("data/Oct2020work/annual_homerange_areas.csv")
head(annual_homerange_areas)

# review data
head(annual_homerange_areas)
unique(annual_homerange_areas$YEAR) # 1991-1999 (no 1996)

MCP <- annual_homerange_areas %>% filter(METHOD=="MCP") # 31 bear years
MCP_year_mean <- MCP %>% group_by(YEAR) %>% summarize(MCP_MEAN=mean(AREA))
head(MCP_year_mean)

HREF <- annual_homerange_areas %>% filter(METHOD=="HREF") # 31 bear years
HREF_year_mean  <- HREF %>% group_by(YEAR) %>% summarize(HREF_MEAN=mean(AREA))
head(HREF_year_mean)

HPI <- annual_homerange_areas %>% filter(METHOD=="HPI") # 7 bear years
HPI_year_mean  <- HPI %>% group_by(YEAR) %>% summarize(HPI_MEAN=mean(AREA))
head(HPI_year_mean)

unique(MCP$ID) # 6 individuals
unique(HREF$ID) # 19 individuals
unique(HPI$ID) # 19 individuals

MCP_year_total <- MCP %>% group_by(YEAR) %>% summarize(n())
# years(n bears): 1991(1), 1993(1), 1994(4), 1995(1)

HREF_year_total <- HREF %>% group_by(YEAR) %>% summarize(n())
# 1991(1), 1992(2), 1993(7), 1994(9), 1995(5), 1998(3), 1999(4)
# HPI will be the same as HREF

# visualize data
hist(MCP$AREA)
plot(MCP_year_mean$YEAR, MCP_year_mean$MCP_MEAN)

hist(HREF$AREA)
plot(HREF_year_mean$YEAR, HREF_year_mean$HREF_MEAN)

hist(HPI$AREA)
plot(MCP_year_mean$YEAR, MCP_year_mean$HPI_MEAN)


###

# MCPs over time
MCP_model <- lm(MCP_year_mean$MCP_MEAN ~ MCP_year_mean$YEAR)
summary(MCP_model) # p-value=0.4264
MCP_year_mean$MCP_PREDICT <- predict(MCP_model)

ggplot(data=MCP_year_mean) +
  geom_point(color="grey21", pch=19, aes(x=YEAR, y=MCP_MEAN)) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkred", size=1.0, aes(x=YEAR, y=MCP_PREDICT)) +
  labs(x="Year", y="Mean area") +
  scale_x_continuous(breaks=c(1990, 1992, 1994, 1996, 1998, 2000, 2002), labels=c("1990", "1992", "1994", "1996", "1998", "2000", "2002")) +
  theme_nuwcru()

###

# HREF over time
HREF_model <- lm(HREF_year_mean$HREF_MEAN ~ HREF_year_mean$YEAR)
summary(HREF_model) # p-value=0.2891
HREF_year_mean$HREF_PREDICT <- predict(HREF_model)

ggplot(data=HREF_year_mean) +
  geom_point(color="grey21", pch=19, aes(x=YEAR, y=HREF_MEAN)) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkred", size=1.0, aes(x=YEAR, y=HREF_PREDICT)) +
  labs(x="Year", y="Mean area") +
  scale_x_continuous(breaks=c(1990, 1992, 1994, 1996, 1998, 2000, 2002), labels=c("1990", "1992", "1994", "1996", "1998", "2000", "2002")) +
  theme_nuwcru()

###

# HPI over time
HPI_model <- lm(HPI_year_mean$HPI_MEAN ~ HPI_year_mean$YEAR)
summary(HPI_model) # p-value=0.1157
HPI_year_mean$HPI_PREDICT <- predict(HPI_model)

ggplot(data=HPI_year_mean) +
  geom_point(color="grey21", pch=19, aes(x=YEAR, y=HPI_MEAN)) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkred", size=1.0, aes(x=YEAR, y=HPI_PREDICT)) +
  labs(x="Year", y="Mean area") +
  scale_x_continuous(breaks=c(1990, 1992, 1994, 1996, 1998, 2000, 2002), labels=c("1990", "1992", "1994", "1996", "1998", "2000", "2002")) +
  theme_nuwcru()

###

# Plotting all together

# first combine the datsets back together
head(MCP_year_mean)
head(HREF_year_mean)
head(HPI_year_mean)
HR_models <- merge(MCP_year_mean, HREF_year_mean, by="YEAR", all=TRUE)
HR_models2 <- merge(HR_models, HPI_year_mean, by="YEAR", all=TRUE)
head(HR_models2)

summary(HR_models2) # mean areas range from 32,801 to 513,922

ggplot(data=HR_models2) +
  geom_point(pch=19, aes(x=YEAR, y=MCP_MEAN, colour="MCP")) +
  geom_point(pch=19, aes(x=YEAR, y=HREF_MEAN, colour="HREF")) +
  geom_point(pch=19, aes(x=YEAR, y=HPI_MEAN, colour="HPI")) +
  scale_colour_manual(values=c("darkgreen", "darkblue", "darkred")) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkred", size=1.0, aes(x=YEAR, y=MCP_PREDICT)) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkblue", size=1.0, aes(x=YEAR, y=HREF_PREDICT)) +
  geom_line(stat="smooth", method="lm", alpha=0.5, color="darkgreen", size=1.0, aes(x=YEAR, y=HPI_PREDICT)) +
  labs(x="Year", y="Mean home range size") +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000), labels=c("0", "50000", "100000", "150000", "200000", "250000", "300000", "350000", "400000", "450000", "500000", "550000")) +
  scale_x_continuous(breaks=c(1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000), labels=c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000")) +
  theme_nuwcru()



#


# SKIP - comparing HR methods using sjstats package with Kylee ----------


performance::icc(pooled_model_scale)

# gives us the intraclass correlation coefficient


pooled_hr_HPI_HREF <- pooled_homerange_areas %>% filter(pooled_homerange_areas$METHOD=="HPI" | pooled_homerange_areas$METHOD=="HPI")
pooled_hr_HPI_HREF <- pooled_homerange_areas %>% 
  filter(`METHOD` %in% c(HPI, HREF))
head(pooled_hr_HPI_HREF)


pooled_model2 <- lmer(SCALE_AREA~1+(1|ID), data=pooled_hr_HPI_HREF)
performance::icc(pooled_model2)