# 1. load libraries ---------------------

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


# 2. import bears dataset and format -----------------

bears <- read.csv("data/bear_FINAL.csv")

bears$LAT <- gsub(",","",bears$LAT)  
bears$LAT <- as.numeric(bears$LAT)
bears$LONG <- as.numeric(as.character((bears$LONG)))

bears$MONTH2 <- factor(bears$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


# 3. Make bear dataset a spatial points object in preparation for HRs ----------

# first, remove duplicates from "bears" that I found earlier - note: I fixed this in script#2! Change this
# Create subset made up of duplicates
duplicates <- bears %>%
  filter(ROWID %in% c("217", "553", "94", "523", "1278"))
# Remove subset
bears_2 <- anti_join(bears, duplicates)
write.csv(bears_2, "data/bears_2.csv") # delete this file

# Make bears_2 a spatial points object

bears_proj <- bears_2
coordinates(bears_proj) <- c("LAT", "LONG")
is.projected(bears_proj)
proj4string(bears_proj) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
is.projected(bears_proj)
plot(bears_proj)
plot(x=bears_proj$LAT, y=bears_proj$LONG, xlab="Latitude", ylab="Longitude") # no visible outliers - these were removed in "PB_datacleaning_prelimexploration.R"

unique(bears_proj$ID)
bears_proj$ID <- as.factor(bears_proj$ID)

plot(bears_proj, col=bears_proj$ID)

# 4. import new bear dataset with UTM zones, convert to UTM coordinates, combine with lat/long dataset ------------
# This dataset has to have UTM coordinates in order for the mcp.area function to work
# Added the zones in QGIS using the "Clip points with polygons" tool and the new shapefile from ArcGIS Hub

# import dataset with UTM zones added
bears_2_utm <- read.csv("data/bears_2_withUTMzones.csv")

# noticed that the fixes that had fallen directly on the UTM grid line were often incorrectly classified (checked against Google Earth)
# Change the incorrect zone values

bears_2_utm <-bears_2_utm[order(bears_2_utm$ROWID),] # reordered so that it was easier to check steps below

bears_2_utm[bears_2_utm$ROWID==142, "ZONE"] <- 17
bears_2_utm[bears_2_utm$ROWID==26, "ZONE"] <- 18
bears_2_utm[bears_2_utm$ROWID==512, "ZONE"] <- 20
bears_2_utm[bears_2_utm$ROWID==649, "ZONE"] <- 21
bears_2_utm[bears_2_utm$ROWID==1290, "ZONE"] <- 21
bears_2_utm[bears_2_utm$ROWID==67, "ZONE"] <- 20
bears_2_utm[bears_2_utm$ROWID==1769, "ZONE"] <- 20
bears_2_utm[bears_2_utm$ROWID==146, "ZONE"] <- 19

# separate each zone into separate dataframes

bears_2_utm <-bears_2_utm[order(bears_2_utm$ZONE),]
unique(bears_2_utm$ZONE) # zones are 17, 18, 19, 20, 21, and 22

zone17 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="17")
zone18 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="18")
zone19 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="19")
zone20 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="20")
zone21 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="21")
zone22 <- bears_2_utm %>% filter(bears_2_utm$ZONE=="22")

# convert to SpatialPointsDataFrames and then to UTM coordinates
# used this forum: https://stat.ethz.ch/pipermail/r-sig-geo/2011-August/012444.html

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

# THIS WORKED
# tested new dataset against old lat/long
# used QGIS to choose a random point, entered in lat/long into Google Earth, checked to see location seemed similar, opened "Get info" and compared UTM against UTM for same ROWID here in R

# combine them all back together into one dataframe
# first have to convert them back to dataframes
zone17_df <- as.data.frame(zone17_utm)
zone18_df <- as.data.frame(zone18_utm)
zone19_df <- as.data.frame(zone19_utm)
zone20_df <- as.data.frame(zone20_utm)
zone21_df <- as.data.frame(zone21_utm)
zone22_df <- as.data.frame(zone22_utm)

bears_2_utm <- rbind(zone17_df, zone18_df, zone19_df, zone20_df, zone21_df, zone22_df)

# removed unnecessary columns
bears_2_utm = subset(bears_2_utm, select = -c(field_1, X))

# changed column titles from lat/long to east/north
head(bears_2_utm)
names(bears_2_utm) <- c('ID', 'DATE', 'DAY', 'MONTH', 'YEAR', 'TIME', 'ROWID', 'testDate', "MONTH2", "ZONE", "EASTING", "NORTHING")

# combined bears_2_utm with bears_2 to have both lat/long and utm in one dataframe
bears_FINAL <- merge(bears_2, bears_2_utm, on='ROWID')

# test that it worked - it did
test_bears_2 <- subset(bears_2, ROWID=="88")
test_bears_2_utm <- subset(bears_2_utm, ROWID=="88")
test_bears_FINAL <- subset(bears_FINAL, ROWID=="88")
head(test_bears_2)
head(test_bears_2_utm)
head(test_bears_FINAL)

# removed unnecessary columns
head(bears_FINAL)
bears_FINAL = subset(bears_FINAL, select = -c(testDate, X))

write.csv(bears_FINAL, "data/bears_FINAL.csv") 

summary(bears_FINAL)

# 5. SKIP -- testing MCP with individual bear  ----------------
# Used these 2 sites for help:
# https://www.r-bloggers.com/home-range-estimation-mcp/ 
# https://jamesepaterson.github.io/jamespatersonblog/03_trackingworkshop_homeranges

# filter bear X13289 out
X13289 <- bears_2_utm %>%
  filter(bears_2_utm$ID=="X13289")

# Make it into spatial points dataframe
X13289_proj <- X13289
coordinates(X13289_proj) <- c("NORTHING", "EASTING")
is.projected(X13289_proj)
proj4string(X13289_proj) <- CRS("+proj=utm +ellps=WGS84") 
proj4string(X13289_proj)
plot(X13289_proj)
plot(x=X13289_proj$EASTING, y=X13289_proj$NORTHING, xlab="Easting", ylab="Northing") 

unique(X13289_proj$ID)
X13289_proj$ID <- as.factor(X13289_proj$ID)

# make and plot 95% mcp
X13289_mcp95 <- mcp(X13289_proj[, "ID"], percent=95)
plot(X13289_mcp95)
points(X13289_proj)

# Calculating the areas of the MCP
X13289_95mcp_area <- mcp.area(X13289_proj, percent=95, unin=c("m"), unout=c("km2")) 
X13289_95mcp_area <- t(X13289_95mcp_area)
summary(X13289_95mcp_area) # 95% mcp = 141,330km2, which sounds right


# 6. SKIP -- Plotting/Mapping test MCP for individual bear ----------

# converting points and mcp to correct format
X13289$NORTHING <- gsub(",","",X13289$NORTHING)  
X13289$NORTHING <- as.numeric(X13289$NORTHING)
X13289$EASTING <- as.numeric(as.character((X13289$EASTING)))
X13289_mcp95_df <- fortify(X13289_mcp95) 

# renamed lat and long columns, since they are utm
colnames(X13289_mcp95_df)
X13289_mcp95_df <- setNames(X13289_mcp95_df, c('northing', 'easting', 'order', 'hole', 'piece', 'id', 'group'))

# formatted the new df to prep for mapping using this help pdf (see p. 15-16): https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
head(X13289_mcp95_df, n=2)
X13289_mcp95$id <- row.names(X13289_mcp95)
head(X13289_mcp95@data, n=2)
X13289_mcp95_df <- left_join(X13289_mcp95_df, X13289_mcp95@data)


# mapping polygon and points on a grid
X13289_mcp_plot <- ggplot() +
  geom_point(data=X13289, mapping=aes(x=EASTING, y=NORTHING, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_mcp95_df, aes(easting, northing, group=group), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% MCP) of bear X13289\nin Davis Strait", x="Easting",
       y="Northing", color="Year", shape="Month")
X13289_mcp_plot





# mapping points (colored background)

# get  basemap set up
register_google(key="AIzaSyDlEjfjRdaU3Ex-aR8L-T21rRGn4i_ZkcQ", write=T) # note: this key won't work for anyone else, if you want these maps just let me know!
DS <- "Davis Strait"
DS_box <- c(left=-90, bottom=50, right=-40, top=80)
terrain <- get_stamenmap(DS_box, zoom=4, maptype = "terrain-background", crop=T) %>% ggmap() #best
toner <- get_stamenmap(DS_box, zoom=4, maptype = "toner-lite", crop=T) %>% ggmap() # this is good as a non-coloured background, but it has labels

# ds_basemap <- get_map(location=ds_bbox, maptype="satellite", source="google", color="color")
X13289_95mcp_map <- get_stamenmap(DS_box, zoom=4, maptype = "terrain-background", crop=T) %>%
  ggmap() +
  geom_point(data=X13289, mapping=aes(x=EASTING, y=NORTHING, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_mcp95_df, aes(easting, northing, group=group), size=.2, color="black", alpha=0) + 
  labs(title="Home range (100% MCP) of bear X13289\nin Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month") +
  theme(legend.position = "right")
X13289_95mcp_map

# will not plot the points/polygon because they're in UTM - waiting on response from Stack Overflow
# realized that the projection was wrong here - need to also fix it below (see line 184)











# map the points and polygon with ggmap
# converting  mcp to correct format
X13289_mcp95_df <- fortify(X13289_mcp95) 

# flipped lat and long columns, since they were incorrect
colnames(X13289_mcp95_df)
X13289_mcp95_df <- setNames(X13289_mcp95_df, c('lat', 'long', 'order', 'hole', 'piece', 'id', 'group'))

# formatted the new df to prep for mapping 
head(X13289_mcp95_df, n=2)
X13289_mcp95_df$id <- row.names(X13289_mcp95)
head(X13289_mcp95@data, n=2)
X13289_mcp95_df <- left_join(X13289_mcp95_df, X13289_mcp95@data)

# Calculating the areas of the MCP - already calculated above, just review summary for 95%
summary(X13289_100mcp_area) # for a 95% MCP, the home range is 33.36 km2

# mapping points (colored background and same shapes for months)
X13289_95mcp_map <- ggmap(ds_basemap) +
  geom_point(data=X13289_locations, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR))) +
  geom_polygon(data=X13289_mcp95_df, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% MCP)\nof bear X13289 in Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month") +
  theme(legend.position = "right")
X13289_95mcp_map

# mapping points (colored background and different shapes for months)
X13289_95mcp_map2 <- ggmap(ds_basemap) +
  geom_point(data=X13289_locations, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_mcp95_df, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% MCP)\nof bear X13289 in Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month") +
  theme(legend.position = "right")
X13289_95mcp_map2

# plotting points (white background)
X13289_mcp_plot <- ggplot() +
  geom_point(data=X13289_locations, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_mcp95_df, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% MCP)\nof bear X13289 in Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month")
X13289_mcp_plot


# 7. SKIP -- Create loops to calculate 95% MCPs for each bear (pooled across all years) --------------

# separate each bear from the rest of the dataset 
unique(bears_FINAL$ID)

X10695 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695")
X10703 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703")
X10709 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709")
X10707 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707")
X10700 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700")
X13284 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284")
X13292 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292")
X13289 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289")
X11974 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11974")
X11975 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975")
X13428 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428")
X13437 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437")
X10393 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10393")
X12080 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080")
X12078 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12078")
X12081 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12081")
X12082 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12082")
X12083 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12083")
X12086 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12086")
X03956 <- bears_FINAL %>% filter(bears_FINAL$ID=="X03956")
X10374 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10374")
X12092 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12092")
X13746 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746")
X30129 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129")
X30126 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126")
X30131 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131")
X30140 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30140")
X30135 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30135")

# check that the number of fixes for each match "DScollardata.xlsx"
str(X30135) # X12078 and X12083 have only 5 observations


# make a list out of each of the new dataframes (Step 1)
bear_list <- list(X12078, X12083, X10695, X10703, X10709, X10707, X10700, X13284, X13292, X13289, X11974, X11975, X13428, X13437, X10393, X12080, X12081, X12082, X12086, X03956, X10374, X12092, X13746, X30129, X30126, X30131, X30140, X30135)
# name items in list
names(bear_list) <- c("X12078", "X12083", "X10695", "X10703", "X10709", "X10707", "X10700", "X13284", "X13292", "X13289", "X11974", "X11975", "X13428", "X13437", "X10393", "X12080", "X12081", "X12082", "X12086", "X03956", "X10374", "X12092", "X13746", "X30129", "X30126", "X30131", "X30140", "X30135")
head(bear_list$X13289) # check that it worked
names(bear_list) # check that X12078 and X12083 aren't in there

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_list)){
  coordinates(bear_list[[i]]) <- c("EASTING", "NORTHING") # Step 2
  proj4string(bear_list[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_list[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_list[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

# Check projections
proj4string(bear_list[[1]])
proj4string(bear_list$'X13289')


# create loop for the MCPs
# Make an empty list for the MCPs
bear_list_mcps <- list()


# create 95% MCP for each (Step 5)
for(i in 1: length(bear_list)){
  temp_dat <- bear_list[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_list_mcps[i] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}


# Name items in the list
names(bear_list_mcps) <- c("X12078", "X12083", "X10695", "X10703", "X10709", "X10707", "X10700", "X13284", "X13292", "X13289", "X11974", "X11975", "X13428", "X13437", "X10393", "X12080", "X12081", "X12082", "X12086", "X03956", "X10374", "X12092", "X13746", "X30129", "X30126", "X30131", "X30140", "X30135")
head(bear_list_mcps$X13289) # check that it worked


# since area is calculated here, just turn this into a dataframe and don't use mcp.area()
bear_list_mcps_df <- data.frame(Reduce(rbind, bear_list_mcps))
summary(bear_list_mcps_df)


# test plot - it worked! 
plot(bear_list_mcps$X13289)

# to add points on top, first unlist them
bears <- unlist(bear_list)
# Name the items
names(bears) <- c("X12078", "X12083", "X10695", "X10703", "X10709", "X10707", "X10700", "X13284", "X13292", "X13289", "X11974", "X11975", "X13428", "X13437", "X10393", "X12080", "X12081", "X12082", "X12086", "X03956", "X10374", "X12092", "X13746", "X30129", "X30126", "X30131", "X30140", "X30135")
head(bears$X13289) # check that it worked

# then plot them again with the points on top
plot(bear_list_mcps$X13289)
plot(bears$X13289, add=T) # This works! 



# 8. POOLED - UTM loops to calculate 95% MCPs for bears with >100 fixes   --------------

# separate each bear from the rest of the dataset 
unique(bears_FINAL$ID)

X10695 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695")
X10703 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703")
X10709 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709")
X10707 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707")
X10700 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700")
X13284 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284")
X13292 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292")
X13289 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289")
X11974 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11974")
X11975 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975")
X13428 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428")
X13437 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437")
X10393 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10393")
X12080 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080")
X12078 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12078")
X12081 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12081")
X12082 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12082")
X12083 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12083")
X12086 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12086")
X03956 <- bears_FINAL %>% filter(bears_FINAL$ID=="X03956")
X10374 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10374")
X12092 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12092")
X13746 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746")
X30129 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129")
X30126 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126")
X30131 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131")
X30140 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30140")
X30135 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30135")

# check which bears have >100 fixes
str(X10695) # 110 - include
str(X10709) # 86 
str(X12083) # 5 
str(X12092) # 51
str(X11975) # 88 
str(X30140) # 43 
str(X30135) # 71 
str(X30129) # 79 
str(X30131) # 79 
str(X30126) # 86
str(X10703) # 101 - include
str(X10707) # 75
str(X10700) # 97
str(X13284) # 90
str(X13292) # 125 - include
str(X13289) # 139 - include
str(X11974) # 54
str(X13428) # 75
str(X13437) # 77
str(X12086) # 54
str(X03956) # 12
str(X10374) # 27
str(X12080) # 93
str(X12078) # 5
str(X13746) # 108 - include
str(X10393) # 8
str(X12081) # 6
str(X12082) # 9

# make a new list out of each of only bears X10695, X10703, X13292, X13289, and X13746
bear_list_greater100 <- list(X10695, X10703, X13292, X13289, X13746)

# name items in list
names(bear_list_greater100) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
head(bear_list_greater100$X10695_mcp_utm) # check that it worked
names(bear_list_greater100)

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_list_greater100)){
  coordinates(bear_list_greater100[[i]]) <- c("EASTING", "NORTHING") # Step 2
  proj4string(bear_list_greater100[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_list_greater100[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_list_greater100[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

head(bear_list_greater100$X10695_mcp_utm) # note that easting and northing columns are gone because they're used to make it a spatial object

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

# to add points on top, first unlist them
bears_greater100 <- unlist(bear_list_greater100)
# Name the items
names(bears_greater100) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
head(bears_greater100$X13289_mcp_utm) # check that it worked

# then plot them again with the points on top
plot(bear_list_greater100_mcps$X13289_mcp_utm)
plot(bears_greater100$X13289, add=T) # This works! 






# 9. POOLED - LAT/LONG loops to calculate 95% MCPs for bears with >100 fixes  --------------

# make a new list out of each of only bears X10695, X10703, X13292, X13289, and X13746
bear_list_greater100_latlong <- list(X10695, X10703, X13292, X13289, X13746)

# name items in list
names(bear_list_greater100_latlong) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
head(bear_list_greater100_latlong$X10695_mcp) # check that it worked
names(bear_list_greater100_latlong)

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_list_greater100_latlong)){
  coordinates(bear_list_greater100_latlong[[i]]) <- c("LONG", "LAT") # Step 2
  proj4string(bear_list_greater100_latlong[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_list_greater100_latlong[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_list_greater100_latlong[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

head(bear_list_greater100_latlong$X10695_mcp) # note that lat/long columns are gone because they're used to make it a spatial object

# Check projections
proj4string(bear_list_greater100_latlong[[1]])
proj4string(bear_list_greater100_latlong$'X13289_mcp')

# create loop for the MCPs
# Make an empty list for the MCPs
bear_list_greater100_mcps_latlong <- list()

# create 95% MCP for each (Step 5)
for(i in 1: length(bear_list_greater100_latlong)){
  temp_dat <- bear_list_greater100_latlong[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_list_greater100_mcps_latlong[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}

# Name items in the list
names(bear_list_greater100_mcps_latlong) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
head(bear_list_greater100_mcps_latlong$X13289_mcp) # check that it worked - remember the area is going to be incorrect here

# test plot - it worked! 
plot(bear_list_greater100_mcps_latlong$X13289_mcp) # see the change in shape using lat/long instead of utm!

# to add points on top, first unlist them
bears_greater100_latlong <- unlist(bear_list_greater100_latlong)
# Name the items
names(bears_greater100_latlong) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
head(bears_greater100_latlong$X13289_mcp) # check that it worked

# then plot them again with the points on top
plot(bear_list_greater100_mcps_latlong$X13289_mcp)
plot(bears_greater100_latlong$X13289, add=T) # This works! 



# 10. POOLED - UTM Plotting 95% MCPs for bears with >100 fixes ------------------------

# lineplot
ggplot(data=bear_list_greater100_mcps_df) +
  geom_col(aes(y=id, x=area)) +
  scale_x_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000, 1100000)) +
  labs(title="95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 100 fixes", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# lineplot reordered based on area
unique(bear_list_greater100_mcps_df$id)

bear_list_greater100_mcps_reordered <- bear_list_greater100_mcps_df %>%
  mutate(ID2=factor(id, levels=c("X13292", "X10695", "X13289", "X13746", "X10703")))


ggplot(data=bear_list_greater100_mcps_reordered) +
  geom_col(aes(y=ID2, x=area)) +
  scale_x_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 100 fixes", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# boxplot of summary 
summary(bear_list_greater100_mcps_df)

bears_95mcp_greater100_boxplot <- ggplot(data=bear_list_greater100_mcps_df, aes(y=area)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 100 fixes", y="Area (Km2)") +
  theme_nuwcru()

bears_95mcp_greater100_boxplot
bears_95mcp_greater100_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# 11. POOLED - UTM Prepping data for mapping ------------------------

# select only points for the 5 bears that have >100 fixes
bears_mcppoints_greater100 <- bears_FINAL%>%
  filter(ID=="X10695" | ID=="X10703" | ID=="X13292" | ID=="X13289" | ID=="X13746")
unique(bears_mcppoints_greater100$ID)

# converting points to correct format
bears_mcppoints_greater100$NORTHING <- gsub(",","",bears_mcppoints_greater100$NORTHING)  
bears_mcppoints_greater100$NORTHING <- as.numeric(bears_mcppoints_greater100$NORTHING)
bears_mcppoints_greater100$EASTING <- as.numeric(as.character((bears_mcppoints_greater100$EASTING)))

# converting polygon to correct format
# split list into items

lapply(names(bear_list_greater100_mcps), function(x) assign(x, bear_list_greater100_mcps[[x]], envir=.GlobalEnv))
# these should all now be SpatialPolygonsDataframes that are separated from the list; check environment 
# use these names below: "X10695" "X10703" "X13292" "X13289" "X13746"
# fortify each separately
X10695_fortify <- fortify(X10695)
X10703_fortify <- fortify(X10703)
X13292_fortify <- fortify(X13292)
X13289_fortify <- fortify(X13289)
X13746_fortify <- fortify(X13746)

# renamed lat and long columns, since they are utm
colnames(X10695_fortify)
X10695_fortify <- setNames(X10695_fortify, c('easting', 'northing', 'order', 'hole', 'piece', 'id', 'group'))

colnames(X10703_fortify)
X10703_fortify <- setNames(X10703_fortify, c('easting', 'northing', 'order', 'hole', 'piece', 'id', 'group'))

colnames(X13292_fortify)
X13292_fortify <- setNames(X13292_fortify, c('easting', 'northing', 'order', 'hole', 'piece', 'id', 'group'))

colnames(X13289_fortify)
X13289_fortify <- setNames(X13289_fortify, c('easting', 'northing', 'order', 'hole', 'piece', 'id', 'group'))

colnames(X13746_fortify)
X13746_fortify <- setNames(X13746_fortify, c('easting', 'northing', 'order', 'hole', 'piece', 'id', 'group'))

# formatted new dfs to prep for mapping using this help pdf (see p. 15-16): https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
head(X10695_fortify, n=2)
X10695_fortify$id <- row.names(X10695_fortify)
head(X10695_fortify@data, n=2)
X10695_fortify <- left_join(X10695_fortify, X10695@data)

head(X10703_fortify, n=2)
X10703_fortify$id <- row.names(X10703_fortify)
head(X10703_fortify@data, n=2)
X10703_fortify <- left_join(X10703_fortify, X10703@data)

head(X13292_fortify, n=2)
X13292_fortify$id <- row.names(X13292_fortify)
head(X13292_fortify@data, n=2)
X13292_fortify <- left_join(X13292_fortify, X13292@data)

head(X13289_fortify, n=2)
X13289_fortify$id <- row.names(X13289_fortify)
head(X13289_fortify@data, n=2)
X13289_fortify <- left_join(X13289_fortify, X13289@data)

head(X13746_fortify, n=2)
X13746_fortify$id <- row.names(X13746_fortify)
head(X13746_fortify@data, n=2)
X13746_fortify <- left_join(X13746_fortify, X13746@data)




# 12. POOLED - LAT/LONG Prepping data for mapping ------------------------

# select only points for the 5 bears that have >100 fixes
bears_mcppoints_greater100_latlong <- bears_FINAL %>%
  filter(ID=="X10695" | ID=="X10703" | ID=="X13292" | ID=="X13289" | ID=="X13746")
unique(bears_mcppoints_greater100_latlong$ID)

# converting points to correct format
bears_mcppoints_greater100_latlong$LAT <- gsub(",","",bears_mcppoints_greater100_latlong$LAT)  
bears_mcppoints_greater100_latlong$LAT <- as.numeric(bears_mcppoints_greater100_latlong$LAT)
bears_mcppoints_greater100_latlong$LONG <- as.numeric(as.character((bears_mcppoints_greater100_latlong$LONG)))

# converting polygon to correct format
# split list into items
lapply(names(bear_list_greater100_mcps_latlong), function(x) assign(x, bear_list_greater100_mcps_latlong[[x]], envir=.GlobalEnv))

# these should all now be SpatialPolygonsDataframes that are separated from the list; check environment 
# use these names below: "X10695" "X10703" "X13292" "X13289" "X13746"
# fortify each separately
X10695_fortify <- fortify(X10695_mcp)
X10703_fortify <- fortify(X10703_mcp)
X13292_fortify <- fortify(X13292_mcp)
X13289_fortify <- fortify(X13289_mcp)
X13746_fortify <- fortify(X13746_mcp)

# formatted new dfs to prep for mapping using this help pdf (see p. 15-16): https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
head(X10695_fortify, n=2)
X10695_fortify$id <- row.names(X10695_fortify)
head(X10695_fortify@data, n=2)
X10695_fortify <- left_join(X10695_fortify, X10695@data)

head(X10703_fortify, n=2)
X10703_fortify$id <- row.names(X10703_fortify)
head(X10703_fortify@data, n=2)
X10703_fortify <- left_join(X10703_fortify, X10703@data)

head(X13292_fortify, n=2)
X13292_fortify$id <- row.names(X13292_fortify)
head(X13292_fortify@data, n=2)
X13292_fortify <- left_join(X13292_fortify, X13292@data)

head(X13289_fortify, n=2)
X13289_fortify$id <- row.names(X13289_fortify)
head(X13289_fortify@data, n=2)
X13289_fortify <- left_join(X13289_fortify, X13289@data)

head(X13746_fortify, n=2)
X13746_fortify$id <- row.names(X13746_fortify)
head(X13746_fortify@data, n=2)
X13746_fortify <- left_join(X13746_fortify, X13746@data)

# 13. SKIP -- UTM Mapping 95% MCPs for bears with >100 fixes (grid background) ------------------------

# mapping polygon and points on a grid
ggplot() +
  geom_point(data=bears_mcppoints_greater100_latlong, mapping=aes(x=EASTING, y=NORTHING, colour=ID, shape=factor(YEAR))) +
  scale_shape_manual(values=c(16, 17, 18, 3, 8)) +
  scale_color_manual(values=c("#FF9900", "#FF0000", "#9933CC", "#3399FF", "#33CC00")) + # got hexadecimal color codes here: http://www.sthda.com/english/wiki/colors-in-r
  geom_polygon(data=X10695_fortify, aes(easting, northing, group=group), size=.5, color="#FF9900", alpha=0) + 
  geom_polygon(data=X10703_fortify, aes(easting, northing, group=group), size=.5, color="#FF0000", alpha=0) + 
  geom_polygon(data=X13292_fortify, aes(easting, northing, group=group), size=.5, color="#9933CC", alpha=0) + 
  geom_polygon(data=X13289_fortify, aes(easting, northing, group=group), size=.5, color="#3399FF", alpha=0) + 
  geom_polygon(data=X13746_fortify, aes(easting, northing, group=group), size=.5, color="#33CC00", alpha=0) + 
  scale_x_continuous(limits=c(200000, 900000), breaks=c(200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000), labels=c( "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000")) +
  labs(title="95% MCPs of all collared female polar bears\nwith greater than 100 fixes in Davis Strait", x="Easting",
       y="Northing", color="Bear (ID)", shape="Year")


summary(bears_mcppoints_greater100)

# 14. POOLED - LAT/LONG Mapping 95% MCPs for bears with >100 fixes (grid background) ------------------------

# all bears together
ggplot() +
  geom_point(data=bears_mcppoints_greater100_latlong, size=1, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(YEAR))) +
  scale_shape_manual(values=c(16, 17, 18, 3, 8)) +
  scale_color_manual(values=c("#FF9900", "#FF0000", "#9933CC", "#3399FF", "#33CC00")) + # got hexadecimal color codes here: http://www.sthda.com/english/wiki/colors-in-r
  geom_polygon(data=X10695_fortify, aes(long, lat, group=group), size=.2, color="#FF9900", alpha=0) + 
  geom_polygon(data=X10703_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X13292_fortify, aes(long, lat, group=group), size=.2, color="#9933CC", alpha=0) + 
  geom_polygon(data=X13289_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X13746_fortify, aes(long, lat, group=group), size=.2, color="#33CC00", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCPs of all collared female polar bears\nwith greater than 100 fixes in Davis Strait", x="Longitude",
       y="Latitude", color="Bear (ID)", shape="Year")

# bears separately
# note: rerun original dataframes so that the points are each separate still

    # X10695
unique(X10695$YEAR) # 2 years, therefore 2 colours for points
ggplot() +
  geom_point(data=X10695, size=1, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  scale_color_manual(values=c("#3399FF", "#9933CC")) +
  geom_polygon(data=X10695_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCP for bear X10695", x="Longitude",
       y="Latitude", color="Year", shape="Month")

    # X10703
unique(X10703$YEAR) # 3 years, therefore 3 colours for points
ggplot() +
  geom_point(data=X10703, size=1, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  scale_color_manual(values=c("#3399FF", "#9933CC", "#33CC00")) + 
  geom_polygon(data=X10703_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCP for bear X10703", x="Longitude",
       y="Latitude", color="Year", shape="Month")

    # X13292
unique(X13292$YEAR) # 4 years, therefore 4 colours for points
ggplot() +
  geom_point(data=X13292, size=1, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  scale_color_manual(values=c("#3399FF", "#9933CC", "#33CC00", "#FF0000")) + 
  geom_polygon(data=X13292_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCP for bear X13292", x="Longitude",
       y="Latitude", color="Year", shape="Month")

    # X13289
unique(X13289$YEAR) # 4 years, therefore 4 colours for points
ggplot() +
  geom_point(data=X13289, size=1, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  scale_color_manual(values=c("#3399FF", "#9933CC", "#33CC00", "#FF0000")) + 
  geom_polygon(data=X13289_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCP for bear X13289", x="Longitude",
       y="Latitude", color="Year", shape="Month")

    # X13746
unique(X13746$YEAR) # 2 years, therefore 2 colours for points
ggplot() +
  geom_point(data=X13746, size=1, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  scale_color_manual(values=c("#3399FF", "#9933CC")) + 
  geom_polygon(data=X13746_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) + # kept the grid size the exact same as href KDEs for comparison
  labs(title="95% MCP for bear X13746", x="Longitude",
       y="Latitude", color="Year", shape="Month") 


# 15. POOLED - Mapping 95% MCPs for bears with >100 fixes (colored background) ------------------------

# NOTE: GGMAP IS NO LONGER WORKING - ENDED UP DOING THIS IN QGIS

# combine all points together into one df and export - use lat/long version, not the UTM!
bears_mcp_over100fixes <- bears_2 %>%
  filter(ID=="X10703" | ID=="X13746" | ID=="X13289" | ID=="X10695" | ID=="X13292")
unique(bears_mcp_over100fixes$ID)
write_csv(bears_mcp_over100fixes, "data/bears_mcp_over100fixes.csv")





# export all polygons as shapefiles

# separate each bear from mcp list
X10703_95mcp <- bear_list_greater100_mcps$X10703
X13746_95mcp <- bear_list_greater100_mcps$X13746
X13289_95mcp <- bear_list_greater100_mcps$X13289
X10695_95mcp <- bear_list_greater100_mcps$X10695
X13292_95mcp <- bear_list_greater100_mcps$X13292

str(X10703_95mcp)
proj4string(X10703_95mcp)


# export each separately
writeOGR(X10703_95mcp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/MCPs/X10703_95mcp.shp", layer="X10703_95mcp", driver="ESRI Shapefile")
writeOGR(X13746_95mcp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/MCPs/X13746_95mcp.shp", layer="X13746_95mcp", driver="ESRI Shapefile")
writeOGR(X13289_95mcp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/MCPs/X13289_95mcp.shp", layer="X13289_95mcp", driver="ESRI Shapefile")
writeOGR(X10695_95mcp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/MCPs/X10695_95mcp.shp", layer="X10695_95mcp", driver="ESRI Shapefile")
writeOGR(X13292_95mcp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/MCPs/X13292_95mcp.shp", layer="X13292_95mcp", driver="ESRI Shapefile")

X10703_95mcp_df <- as.data.frame(X10703_95mcp)



# This isn't working - when I import into QGIS, all the polygons are in Japan
# I think it's because they have UTM coords and I need lat/long for QGIS












# 16. ANNUAL- UTM loops to calculate 95% MCPs for bears with >50 fixes per year -------------

# separate bears with >50 fixes in one year into bear years (see DS_collar.csv for numbers)
X10695_1991 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1991")
X12080_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1994")
X13289_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1994")
X13292_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1993") 
X13428_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428" & bears_FINAL$YEAR=="1994") 
X13746_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1994") 
X13746_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1995") 

# make a list 
bear_annualMCP_list <- list(X10695_1991, X12080_1994, X13289_1994, X13292_1993, X13428_1994, X13746_1994, X13746_1995)
names(bear_annualMCP_list)

# name items in list
names(bear_annualMCP_list) <- c("X10695_1991", "X12080_1994", "X13289_1994", "X13292_1993", "X13428_1994", "X13746_1994", "X13746_1995")
head(bear_annualMCP_list$X10695_1991) # check that it worked
names(bear_annualMCP_list) 

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_annualMCP_list)){
  coordinates(bear_annualMCP_list[[i]]) <- c("EASTING", "NORTHING") # Step 2
  proj4string(bear_annualMCP_list[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_annualMCP_list[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_annualMCP_list[[i]] <- temp          # put the ith bear back into the list in the ith slot
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

# add more columns to the new dataframe
bear_annualMCPs_df$ID_year <- c("X10695_1991", "X12080_1994", "X13289_1994", "X13292_1993", "X13428_1994", "X13746_1994", "X13746_1995")
bear_annualMCPs_df$year <- c("1991", "1994", "1994", "1993", "1994", "1994", "1995") # look at the dataframe and make sure the IDs and years match up across rows

# test plot - it worked! 
plot(bear_annualMCPs$X10695_1991_utm)

# to add points on top, first unlist them
bear_annualMCPs_unlisted <- unlist(bear_annualMCP_list)
# Name the items
names(bear_annualMCPs_unlisted) <- c("X10695_1991_utm", "X12080_1994_utm", "X13289_1994_utm", "X13292_1993_utm", "X13428_1994_utm", "X13746_1994_utm", "X13746_1995_utm")
head(bear_annualMCPs_unlisted$X10695_1991_utm) # check that it worked

# then plot them again with the points on top
plot(bear_annualMCPs$X10695_1991_utm)
plot(bear_annualMCPs_unlisted$X10695_1991_utm, add=T) # This works! 


summary(bear_annualMCPs_df)


# yearly stats
unique(bear_annualMCPs_df$year)

MCP_1991 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$year=="1991")
summary(MCP_1991)

MCP_1993 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$year=="1993")
summary(MCP_1993)

MCP_1994 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$year=="1994")
summary(MCP_1994)

MCP_1995 <- bear_annualMCPs_df %>%
  filter(bear_annualMCPs_df$year=="1995")
summary(MCP_1995)






# 17. ANNUAL - UTM loops - >40 fixes ------------- 


# separate bears with >40 fixes in one year into bear years (see DS_collar.csv for numbers)
X10695_1991 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1991")
X10695_1992 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1992")
X10700_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700" & bears_FINAL$YEAR=="1993")
X10709_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709" & bears_FINAL$YEAR=="1993")
X11975_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975" & bears_FINAL$YEAR=="1994")
X12080_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1994")
X12080_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1995")
X13284_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284" & bears_FINAL$YEAR=="1993")
X13289_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1993")
X13289_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1994")
X13292_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1993")
X13428_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428" & bears_FINAL$YEAR=="1994")
X13437_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437" & bears_FINAL$YEAR=="1995")
X13746_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1994")
X13746_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1995")

unique(X13746_1995$DATE) # check all to make sure there's >40

# make a list 
bear_annualMCPover40_list <- list(X10695_1991, X10695_1992, X10700_1993, X10709_1993, X11975_1994, X12080_1994, X12080_1995, X13284_1993, X13289_1993, X13289_1994, X13292_1993, X13428_1994, X13437_1995, X13746_1994, X13746_1995)
names(bear_annualMCPover40_list)

# name items in list
names(bear_annualMCPover40_list) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10709_1993", "X11975_1994", "X12080_1994", "X12080_1995", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995")
head(bear_annualMCPover40_list$X10695_1991) # check that it worked
names(bear_annualMCPover40_list) 

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_annualMCPover40_list)){
  coordinates(bear_annualMCPover40_list[[i]]) <- c("EASTING", "NORTHING") # Step 2
  proj4string(bear_annualMCPover40_list[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_annualMCPover40_list[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_annualMCPover40_list[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

head(bear_annualMCPover40_list$X10695_1991) # note that easting and northing columns are gone because they're used to make it a spatial object

# Check projections
proj4string(bear_annualMCPover40_list[[1]])
proj4string(bear_annualMCPover40_list$'X10695_1991')

# create loop for the MCPs
# Make an empty list for the MCPs
bear_annualMCPover40_list2 <- list()

# create 95% MCP for each (Step 5)
for(i in 1: length(bear_annualMCPover40_list)){
  temp_dat <- bear_annualMCPover40_list[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_annualMCPover40_list2[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}

# Name items in the list
names(bear_annualMCPover40_list2) <- c("X10695_1991_utm", "X10695_1992_utm", "X10700_1993_utm", "X10709_1993_utm", "X11975_1994_utm", "X12080_1994_utm", "X12080_1995_utm", "X13284_1993_utm", "X13289_1993_utm", "X13289_1994_utm", "X13292_1993_utm", "X13428_1994_utm", "X13437_1995_utm", "X13746_1994_utm", "X13746_1995_utm")
head(bear_annualMCPover40_list2$X10695_1991_utm) # check that it worked

# since area is calculated here, just turn this into a dataframe and don't use mcp.area()
bear_annualMCPover40_df <- data.frame(Reduce(rbind, bear_annualMCPover40_list2))
summary(bear_annualMCPover40_df)

# add more columns to the new dataframe
bear_annualMCPover40_df$ID_year <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10709_1993", "X11975_1994", "X12080_1994", "X12080_1995", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995")
bear_annualMCPover40_df$year <- c("1991", "1992", "1993", "1993", "1994", "1994", "1995", "1993", "1993", "1994", "1993", "1994", "1995", "1994", "1995") # look at the dataframe and make sure the IDs and years match up across rows

# test plot - it worked! 
plot(bear_annualMCPover40_list2$X10695_1991_utm)

# to add points on top, first unlist them
bear_annualMCPover40_unlist <- unlist(bear_annualMCPover40_list)
# Name the items
names(bear_annualMCPover40_unlist) <- c("X10695_1991_utm", "X10695_1992_utm", "X10700_1993_utm", "X10709_1993_utm", "X11975_1994_utm", "X12080_1994_utm", "X12080_1995_utm", "X13284_1993_utm", "X13289_1993_utm", "X13289_1994_utm", "X13292_1993_utm", "X13428_1994_utm", "X13437_1995_utm", "X13746_1994_utm", "X13746_1995_utm")
head(bear_annualMCPover40_unlist$X10695_1991_utm) # check that it worked

# then plot them again with the points on top
plot(bear_annualMCPover40_list2$X10695_1991_utm)
plot(bear_annualMCPover40_unlist$X10695_1991_utm, add=T) # This works! 

summary(bear_annualMCPover40_df)


# yearly stats - not neccesary right now
unique(bear_annualMCPover40_df$year) 

MCP_over40_1991 <- bear_annualMCPover40_df %>%
  filter(bear_annualMCPover40_df$year=="1991")
summary(MCP_over40_1991)

MCP_over40_1992 <- bear_annualMCPover40_df %>%
  filter(bear_annualMCPover40_df$year=="1992")
summary(MCP_over40_1992)

MCP_over40_1993 <- bear_annualMCPover40_df %>%
  filter(bear_annualMCPover40_df$year=="1993")
summary(MCP_over40_1993)

MCP_over40_1994 <- bear_annualMCPover40_df %>%
  filter(bear_annualMCPover40_df$year=="1994")
summary(MCP_over40_1994)

MCP_over40_1995 <- bear_annualMCPover40_df %>%
  filter(bear_annualMCPover40_df$year=="1995")
summary(MCP_over40_1995)


# 18. ANNUAL - UTM loops - >30 fixes ------------

# separate bears with >40 fixes in one year into bear years (see DS_collar.csv for numbers)
X10695_1991 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1991")
X10695_1992 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1992")
X10700_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700" & bears_FINAL$YEAR=="1993")
X10700_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700" & bears_FINAL$YEAR=="1994")
X10703_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703" & bears_FINAL$YEAR=="1993")
X10703_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703" & bears_FINAL$YEAR=="1994")
X10707_1992 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707" & bears_FINAL$YEAR=="1992")
X10707_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707" & bears_FINAL$YEAR=="1993")
X10709_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709" & bears_FINAL$YEAR=="1993")
X11975_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975" & bears_FINAL$YEAR=="1994")
X11975_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975" & bears_FINAL$YEAR=="1995")
X12080_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1994")
X12080_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1995")
X12086_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12086" & bears_FINAL$YEAR=="1995")
X12092_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12092" & bears_FINAL$YEAR=="1994")
X13284_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284" & bears_FINAL$YEAR=="1993")
X13289_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1993")
X13289_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1994")
X13292_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1993")
X13292_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1994")
X13428_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428" & bears_FINAL$YEAR=="1994")
X13437_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437" & bears_FINAL$YEAR=="1995")
X13746_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1994")
X13746_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1995")
X30126_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126" & bears_FINAL$YEAR=="1998")
X30126_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126" & bears_FINAL$YEAR=="1999")
X30129_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129" & bears_FINAL$YEAR=="1998")
X30129_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129" & bears_FINAL$YEAR=="1999")
X30131_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131" & bears_FINAL$YEAR=="1998")
X30131_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131" & bears_FINAL$YEAR=="1999")
X30135_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30135" & bears_FINAL$YEAR=="1999")


unique(X30135_1999$DATE) #check that all have >30



# make a list 
bear_annualMCPover30_list <- list(X10695_1991, X10695_1992, X10700_1993, X10700_1994, X10703_1993, X10703_1994, X10707_1992,
                                  X10707_1993, X10709_1993, X11975_1994, X11975_1995, X12080_1994, X12080_1995, X12086_1995,
                                  X12092_1994, X13284_1993, X13289_1993, X13289_1994, X13292_1993, X13292_1994, X13428_1994,
                                  X13437_1995, X13746_1994, X13746_1995, X30126_1998, X30126_1999, X30129_1998, X30129_1999,
                                  X30131_1998, X30131_1999, X30135_1999)
names(bear_annualMCPover30_list)

# name items in list
names(bear_annualMCPover30_list) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994",
                                      "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994",
                                      "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994",
                                      "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995",
                                      "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999",
                                      "X30135_1999")
head(bear_annualMCPover30_list$X10695_1991) # check that it worked
names(bear_annualMCPover30_list) 

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_annualMCPover30_list)){
  coordinates(bear_annualMCPover30_list[[i]]) <- c("EASTING", "NORTHING") # Step 2
  proj4string(bear_annualMCPover30_list[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_annualMCPover30_list[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_annualMCPover30_list[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

head(bear_annualMCPover30_list$X10695_1991) # note that easting and northing columns are gone because they're used to make it a spatial object


# Check projections
proj4string(bear_annualMCPover30_list[[1]])
proj4string(bear_annualMCPover30_list$'X10695_1991')


# create loop for the MCPs
# Make an empty list for the MCPs
bear_annualMCPover30_list2 <- list()


# create 95% MCP for each (Step 5)
for(i in 1: length(bear_annualMCPover30_list)){
  temp_dat <- bear_annualMCPover30_list[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_annualMCPover30_list2[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}


# Name items in the list
names(bear_annualMCPover30_list2) <- c("X10695_1991_utm", "X10695_1992_utm", "X10700_1993_utm", "X10700_1994_utm",
                                      "X10703_1993_utm", "X10703_1994_utm", "X10707_1992_utm", "X10707_1993_utm", 
                                      "X10709_1993_utm", "X11975_1994_utm", "X11975_1995_utm", "X12080_1994_utm", 
                                      "X12080_1995_utm", "X12086_1995_utm", "X12092_1994_utm", "X13284_1993_utm", 
                                      "X13289_1993_utm", "X13289_1994_utm", "X13292_1993_utm", "X13292_1994_utm", 
                                      "X13428_1994_utm", "X13437_1995_utm", "X13746_1994_utm", "X13746_1995_utm",
                                      "X30126_1998_utm", "X30126_1999_utm", "X30129_1998_utm", "X30129_1999_utm", 
                                      "X30131_1998_utm", "X30131_1999_utm", "X30135_1999_utm")
head(bear_annualMCPover30_list2$X10695_1991_utm) # check that it worked


# since area is calculated here, just turn this into a dataframe and don't use mcp.area()
bear_annualMCPover30_df <- data.frame(Reduce(rbind, bear_annualMCPover30_list2))
summary(bear_annualMCPover30_df)

# add more columns to the new dataframe
bear_annualMCPover30_df$ID_year <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994",
                                     "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994",
                                     "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994",
                                     "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995",
                                     "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bear_annualMCPover30_df$year <- c("1991", "1992", "1993", "1994", "1993", "1994",
                                  "1992", "1993", "1993", "1994", "1995", "1994",
                                  "1995", "1995", "1994", "1993", "1993", "1994",
                                  "1993", "1994", "1994", "1995", "1994", "1995",
                                  "1998", "1999", "1998", "1999", "1998", "1999", "1999") # look at the dataframe and make sure the IDs and years match up across rows

# test plot - it worked! 
plot(bear_annualMCPover30_list2$X10695_1991_utm)

# to add points on top, first unlist them
bear_annualMCPover30_unlist <- unlist(bear_annualMCPover30_list)
# Name the items
names(bear_annualMCPover30_unlist) <- c("X10695_1991_utm", "X10695_1992_utm", "X10700_1993_utm", "X10700_1994_utm",
                                        "X10703_1993_utm", "X10703_1994_utm", "X10707_1992_utm", "X10707_1993_utm", 
                                        "X10709_1993_utm", "X11975_1994_utm", "X11975_1995_utm", "X12080_1994_utm", 
                                        "X12080_1995_utm", "X12086_1995_utm", "X12092_1994_utm", "X13284_1993_utm", 
                                        "X13289_1993_utm", "X13289_1994_utm", "X13292_1993_utm", "X13292_1994_utm", 
                                        "X13428_1994_utm", "X13437_1995_utm", "X13746_1994_utm", "X13746_1995_utm",
                                        "X30126_1998_utm", "X30126_1999_utm", "X30129_1998_utm", "X30129_1999_utm", 
                                        "X30131_1998_utm", "X30131_1999_utm", "X30135_1999_utm")
head(bear_annualMCPover30_unlist$X10695_1991_utm) # check that it worked

# then plot them again with the points on top
plot(bear_annualMCPover30_list2$X10695_1991_utm)
plot(bear_annualMCPover30_unlist$X10695_1991_utm, add=T) # This works! 


summary(bear_annualMCPover30_df)


# yearly stats - not neccesary yet
unique(bear_annualMCPover30_df$year) 

MCP_over30_1991 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1991")
summary(MCP_over30_1991)

MCP_over30_1992 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1992")
summary(MCP_over30_1992)

MCP_over30_1993 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1993")
summary(MCP_over30_1993)

MCP_over30_1994 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1994")
summary(MCP_over30_1994)

MCP_over30_1995 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1995")
summary(MCP_over30_1995)

MCP_over30_1998 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1998")
summary(MCP_over30_1998)

MCP_over30_1999 <- bear_annualMCPover30_df %>%
  filter(bear_annualMCPover30_df$year=="1999")
summary(MCP_over30_1999)



# create summary dataframe of MCPs with >30, >40, and >50 fixes --------

compare_mcps <- data.frame("SAMPLE_SIZE"=c(30, 40, 50), "NUM_BEARYRS"=c(31, 15, 7), "MEAN_AREA"=c(103462.8, 126019, 137335), "MIN_AREA"=c(786.2, 2928, 49362), "MAX_AREA"=c(321591.2, 280538, 280538))
write.csv(compare_mcps, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/compare_mcps.csv")


ggplot(data=compare_mcps, aes(x=SAMPLE_SIZE, y=MEAN_AREA)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE, aes(colour="linear")) +
  geom_smooth(method="loess", se=FALSE, span=0.75, aes(colour="75% loess")) +
  scale_x_continuous(breaks=c(30, 40, 50), labels=c("30 fixes", "40 fixes", "50 fixes")) +
  xlab("Sample size") +
  ylab("Mean area (Km2)")




# 19. ANNUAL - UTM Plotting 95% MCPs for bears with >50 fixes per year ------------------------

summary(bear_annualMCPs_df)

# lineplot
ggplot(data=bear_annualMCPs_df) +
  geom_col(aes(y=ID_year, x=area)) +
  scale_x_continuous(limits=c(0, 300000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000)) +
  labs(title="Annual 95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 50 fixes per year", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# lineplot reordered based on area
unique(bear_annualMCPs_df$ID_year)

bear_annualMCPs_df_reordered <- bear_annualMCPs_df %>%
  mutate(ID2=factor(ID_year, levels=c("X12080_1994", "X10695_1991", "X13746_1995", "X13428_1994", "X13292_1993", "X13289_1994", "X13746_1994")))


ggplot(data=bear_annualMCPs_df_reordered) +
  geom_col(aes(y=ID2, x=area)) +
  scale_x_continuous(limits=c(0, 300000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000)) +
  labs(title="Annual 95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 50 fixes per year", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# boxplot of summary 
summary(bear_annualMCPs_df)

bear_annualMCPs_boxplot <- ggplot(data=bear_annualMCPs_df, aes(y=area)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1700000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000", "1000000", "1100000", "1200000", "1300000", "1400000", "1500000", "1600000", "1700000")) +
  labs(title="Annual 95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)\nthat have greater than 50 fixes per year", y="Area (Km2)") +
  theme_nuwcru()

bear_annualMCPs_boxplot
bear_annualMCPs_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# average of each year
# create summarized dataframe (avg area/year and # bears/year)
bear_annualMCPs_summarized <- bear_annualMCPs_df %>%
  group_by(year) %>%
  summarize(AVG_AREA_KM2=mean(area), COUNT=n())

summary(bear_annualMCPs_summarized)

# plot
ggplot(data=bear_annualMCPs_summarized, aes(x=year, y=AVG_AREA_KM2)) +
  geom_line(group=1) +
  geom_point(size=2, colour="red") +
  geom_text(aes(label=COUNT), vjust=-.75) +
  scale_y_continuous(limits=c(0, 600000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000")) +
  scale_x_discrete(breaks=c(1991, 1992, 1993, 1994, 1995, 1998, 1999), labels=c("1991", "1992", "1993", "1994", "1995", "1998", "1999")) +
  labs(title="Annual average 95% MCP home range areas of\nDavis Strait female polar bears", x="Year", y="Area (Km2)") +
  theme_nuwcru()

# boxplot separated by year 
ggplot(data=bear_annualMCPs_df) +
  geom_boxplot(aes(x=year, y=area), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1700000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000", "1000000", "1100000", "1200000", "1300000", "1400000", "1500000", "1600000", "1700000")) +
  labs(title="Yearly 95% MCP home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()



# 20. ANNUAL - LAT/LONG loops to calculate 95% MCPs for bears with >50 fixes per year --------------


# make a new list out of each of only bears X10695, X10703, X13292, X13289, and X13746
bear_annualMCP_latlong <- list(X10695_1991, X12080_1994, X13289_1994, X13292_1993, X13428_1994, X13746_1994, X13746_1995)

# name items in list
names(bear_annualMCP_latlong) <- c("X10695_1991_mcp", "X12080_1994_mcp", "X13289_1994_mcp", "X13292_1993_mcp", "X13428_1994_mcp", "X13746_1994_mcp", "X13746_1995_mcp")
head(bear_annualMCP_latlong$X10695_1991_mcp) # check that it worked
names(bear_annualMCP_latlong)

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_annualMCP_latlong)){
  coordinates(bear_annualMCP_latlong[[i]]) <- c("LONG", "LAT") # Step 2
  proj4string(bear_annualMCP_latlong[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs") # Step 3
  temp <- bear_annualMCP_latlong[[i]]          # extract the ith bear
  temp[["ID"]] <- as.factor(temp[["ID"]]) # convert the ith bears "ID" to factor
  bear_annualMCP_latlong[[i]] <- temp          # put the ith bear back into the list in the ith slot
}

head(bear_annualMCP_latlong$X10695_1991_mcp) # note that lat/long columns are gone because they're used to make it a spatial object

# Check projections
proj4string(bear_annualMCP_latlong[[1]])
proj4string(bear_annualMCP_latlong$'X10695_1991_mcp')

# create loop for the MCPs
# Make an empty list for the MCPs
bear_annualMCP_latlong_list2 <- list()

# create 95% MCP for each (Step 5)
for(i in 1: length(bear_annualMCP_latlong)){
  temp_dat <- bear_annualMCP_latlong[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_annualMCP_latlong_list2[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}

# Name items in the list
names(bear_annualMCP_latlong_list2) <- c("X10695_1991_mcp", "X12080_1994_mcp", "X13289_1994_mcp", "X13292_1993_mcp", "X13428_1994_mcp", "X13746_1994_mcp", "X13746_1995_mcp")
head(bear_annualMCP_latlong_list2$X10695_1991_mcp) # check that it worked - remember the area is going to be incorrect here

# test plot - it worked! 
plot(bear_annualMCP_latlong_list2$X10695_1991_mcp) # see the change in shape using lat/long instead of utm!

# to add points on top, first unlist them
bear_annualMCP_latlong_list2 <- unlist(bear_annualMCP_latlong_list2)
# Name the items
names(bear_annualMCP_latlong_list2) <- c("X10695_1991_mcp", "X12080_1994_mcp", "X13289_1994_mcp", "X13292_1993_mcp", "X13428_1994_mcp", "X13746_1994_mcp", "X13746_1995_mcp")
head(bear_annualMCP_latlong_list2$X10695_1991_mcp) # check that it worked

# then plot them again with the points on top
plot(bear_annualMCP_latlong_list2$X10695_1991_mcp)
plot(bear_annualMCP_latlong$X10695_1991_mcp, add=T) # This works! 

# 21. ANNUAL - LAT/LONG Prepping data for mapping ------------------------

# converting polygons to correct format
# split list into items
lapply(names(bear_annualMCP_latlong_list2), function(x) assign(x, bear_annualMCP_latlong_list2[[x]], envir=.GlobalEnv))

# these should all now be SpatialPolygonsDataframes that are separated from the list; check environment 
# fortify each separately
X10695_1991_fortify <- fortify(X10695_1991_mcp)
X12080_1994_fortify <- fortify(X12080_1994_mcp)
X13289_1994_fortify <- fortify(X13289_1994_mcp)
X13292_1993_fortify <- fortify(X13292_1993_mcp)
X13428_1994_fortify <- fortify(X13428_1994_mcp)
X13746_1994_fortify <- fortify(X13746_1994_mcp)
X13746_1995_fortify <- fortify(X13746_1995_mcp)




# 22. ANNUAL - LAT/LONG mapping of MCP polygons (grid background) -----------------


# X10695_1991
ggplot() +
  geom_point(data=X10695_1991, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_1991_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10695 in 1991", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X12080_1994
ggplot() +
  geom_point(data=X12080_1994, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12080_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12080 in 1994", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X13289_1994
ggplot() +
  geom_point(data=X13289_1994, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13289 in 1994", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X13292_1993
ggplot() +
  geom_point(data=X13292_1993, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13292_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13292 in 1993", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X13428_1994
ggplot() +
  geom_point(data=X13428_1994, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13428_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13428 in 1994", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X13746_1994
ggplot() +
  geom_point(data=X13746_1994, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13746 in 1994", x="Longitude",
       y="Latitude", color="Year", shape="Month")

# X13746_1995
ggplot() +
  geom_point(data=X13746_1995, size=1, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13746 in 1995", x="Longitude",
       y="Latitude", color="Year", shape="Month")








# 23. ALL BEARS - LAT/LONG 100% MCP for all bears; use this for sea ice analysis boundary ----------

# Note that loops aren't required because this is not a list

bear_FINAL_100MCP <- bears_FINAL
str(bear_FINAL_100MCP)
summary(bear_FINAL_100MCP)

# make it a spatial points dataframe and set projections
coordinates(bear_FINAL_100MCP) <- c("LONG", "LAT")
proj4string(bear_FINAL_100MCP) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
proj4string(bear_FINAL_100MCP)
summary(bear_FINAL_100MCP) # note that lat/long columns are gone because they're used to make it a spatial object

# create 100% MCP
bear_FINAL_100MCP_poly <- mcp(bear_FINAL_100MCP[, "ID"], percent=100, unin="m", unout='km2')
plot(bear_FINAL_100MCP_poly)
plot(bear_FINAL_100MCP, add=T)

# cannot figure out how to combine all the points together - cannot remove "[, "ID"]" from line 1546
# decided to make all ID #s the same in this new dataframe and tried again

bear_FINAL_100MCP$ID <- rep("1", length(bear_FINAL_100MCP))
head(bear_FINAL_100MCP)

# create 100% MCP
bear_FINAL_100MCP_poly <- mcp(bear_FINAL_100MCP[, "ID"], percent=100, unin="m", unout='km2')
plot(bear_FINAL_100MCP_poly)
plot(bear_FINAL_100MCP, add=T)

# this works! Now make it a shapefile so I can bring it into QGIS
writeOGR(bear_FINAL_100MCP_poly, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Sea ice boundary/", layer="bear_FINAL_100MCP_poly", driver="ESRI Shapefile")


# 24. ALL BEARS (no land) - LAT/LONG 100% MCP for all bears with land fixes excluded; use this for sea ice analysis boundary ----------

# import dataset
bears_noland <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Bear_FINAL_minusland.csv")
str(bears_noland)
summary(bears_noland)

# make it a spatial points dataframe and set projections
coordinates(bears_noland) <- c("LONG", "LAT")
proj4string(bears_noland) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
proj4string(bears_noland)
summary(bears_noland) # note that lat/long columns are gone because they're used to make it a spatial object

# make all ID #s the same in this df so that it makes one larger MCP

bears_noland$ID <- rep("1", length(bears_noland))
head(bears_noland)

# create 100% MCP
bears_noland_poly <- mcp(bears_noland[, "ID"], percent=100, unin="m", unout='km2')
plot(bears_noland_poly)
plot(bears_noland, add=T)

# this works! Now make it a shapefile so I can bring it into QGIS
writeOGR(bears_noland_poly, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Sea ice boundary/", layer="bears_noland_poly", driver="ESRI Shapefile")








