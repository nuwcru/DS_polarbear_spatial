

# 1. Load libraries ------

library(ggplot2)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(raster)
library(sp)
library(rgdal)
library(maptools)
library(mapview)
library(rgeos)
library(sf) # for st_read function
library(rgeos) # gDistance tool
library(rgdal) # distance from points
library(geosphere) # distance to water
library(viridis) # for plotting (section 10)
library(RANN) # for section 12
library(dplyr)

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


# 2. - SKIP - Import raster_list and crop out land -----

# import raster_list
      # sea ice ("final" is the version that's already been masked - use raster_list_78 instead)
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20.rds")
      # was getting this error: "vector memory exhausted (limit reached?)
      # found a fix here: https://stackoverflow.com/questions/51295402/r-on-macos-error-vector-memory-exhausted-limit-reached
      # made the changes in terminal (had to change it to 250, rather than 100) and this worked
#raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_78.rds")
names(raster_list)
proj4string(raster_list$`19781026`) # check that it's in polar stereographic
plot(raster_list$`19781026`)



###

# import ocean mask
ocean_mask <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/gis/oceanmask_fordistwater_0.3deg.shp")
proj4string(ocean_mask) # lat/long
ocean_mask_proj <- spTransform(ocean_mask, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(ocean_mask_proj)
plot(ocean_mask_proj) # this plots a lot faster now that the buffer has smoothed the edges


# attempt mask with the date of lowest sea ice area for entire study period: 1999-10-01
raster_19991001 <- raster_list$'19991001' # create single raster and plot
plot(raster_19991001)
proj4string(raster_19991001)

raster_19991001_crop1 <- crop(raster_19991001, ocean_mask4_proj, snap='out') # crop
raster_19991001_crop1 <- mask(raster_19991001, ocean_mask4_proj, snap='out')

plot(raster_19991001_crop1)
summary(raster_19991001_crop1[]) # the vast majority is NA, with lots of 0s (i.e. lots of water)

raster_19991001_crop1[is.na(raster_19991001_crop1[])] <- 100 # replace all NA values with 100 
raster_19991001_crop1_100 = as(raster_19991001_crop1, "SpatialPoints")[raster_19991001_crop1[]==100]  # pull all 100 values out to see where they are
plot(raster_19991001_crop1_100)
water_19991001 = as(raster_19991001_crop1, "SpatialPoints")[raster_19991001_crop1[]==0]  # specify 0 for water
plot(water_19991001)
proj4string(water_19991001)

summary(coordinates(raster_19991001_crop1))

plot(raster_19991001_crop1, xlim=c(-3830000, 370000), ylim=c(-5350000, 58000))
plot(water_19991001, col="red", pch=20, add=TRUE)
plot(ocean_mask4_proj, add=TRUE)


###


# apply crop to entire raster_list
raster_list <- lapply(X=raster_list, FUN=crop, y=ocean_mask_proj, snap='out') 

# mask pixels on land
for(i in 1:length(raster_list)){
  raster_list[[i]] <- mask(raster_list[[i]], ocean_mask_proj, snap='out')
}

# look at resulting raster - this will take all night to plot, but it looks good
plot(raster_list$`20191231`) 
plot(ocean_mask_proj, add = TRUE) 
#plot(used_avail_spatial, col="red", pch=20, add=TRUE) # need to run lines in section 3 for this to work


saveRDS(object = raster_list, file = "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")




# 3. Load final dataframes, format, check projections, and visualize (for section 8)  -----

# using this: https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r

# import used and available points
used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_points_withbath_Mar2021.csv") # this is the correct one
head(used_avail)
unique(used_avail$ICE_LAND) # these are all ice only (i.e., land fixes have been removed)

used_avail=subset(used_avail, select=-c(field_1, X, X.2, X.1, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S)) # remove unneccessary columns
head(used_avail)
names(used_avail)[17] <- "BATH"
summary(used_avail)

ggplot(data=used_avail) +
  geom_point(aes(x=LONG, y=LAT, colour=USED_AVAIL, alpha=USED_AVAIL)) +
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -80, -70, -60, -50), labels=c("-90", "-80", "-70", "-60", "-50")) +
  scale_y_continuous(limits=c(50, 70), breaks=c(50, 55, 60, 65, 70), labels=c("50", "55", "60", "65", "70")) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme_nuwcru()

# make into spatial dataframe
used_avail_spatial <- used_avail
coordinates(used_avail_spatial) <- c("LONG", "LAT")
proj4string(used_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")
used_avail_spatial <- spTransform(used_avail_spatial, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
plot(used_avail_spatial)


###
# import cropped raster_list
#raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_distwaterversion.rds")

# import cropped raster_list (it's fine that 2020 data isn't in here; we don't have bear data for 2020)
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")
names(raster_list)
proj4string(raster_list$`19781026`) # check that it's in polar stereographic


# plot one raster with points
plot(raster_list$`19781026`)
points(used_avail_spatial) # this works!

###

# 4. - SKIP - Extract sea ice concentration (test with one fix and one raster) ------

# Notes:
# https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r
# randomly chose this test point: ROWID=796, date=1994-04-04, USED_AVAIL=used
# use matching sea ice date: 19940404 

      # filter one bear and set projection
testbear <- used_avail %>% filter(ROWID=="796" & USED_AVAIL=="used")
head(testbear)
testbear_spatial <- testbear
coordinates(testbear_spatial) <- c("LONG", "LAT")
proj4string(testbear_spatial)
proj4string(testbear_spatial) <- CRS("+proj=longlat +datum=WGS84")
testbear_spatial <- spTransform(testbear_spatial, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
     
      # plot bear and raster layer with matching date
plot(raster_list$'19940404')
points(testbear_spatial, pch=20) # this works!

      # extract concentration value at that point
value <- raster::extract(raster_list$'19940404', testbear_spatial)
# gave me 0.928; judging from the plot this looks right



# 5. Loop to extract all sea ice concentrations ----------

# loop below to extract values


# need the uncropped version of raster_list
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20.rds")
plot(raster_list$`19781026`)
proj4string(raster_list$`19781026`)


# import point dataset and format
used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_points_withbath_Mar2021.csv")
used_avail=subset(used_avail, select=-c(field_1, X, X.2, X.1, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S)) # remove unneccessary columns
head(used_avail)
names(used_avail)[17] <- "BATH"
used_avail$date_char <- stringr::str_replace_all(used_avail$DATE, "/", "")
head(used_avail)


# make an ice list then run loop 

ice <- list()

for(i in 1:nrow(used_avail)){
  
  # uncomment, and run each line individually if you want to test this for i = 1...N
     #i = 1
  
  # convert ith ROWID to spatial
    testbear <- used_avail
    testbear_spatial <- testbear
    coordinates(testbear_spatial) <- c("LONG", "LAT")
    proj4string(testbear_spatial) <- CRS("+proj=longlat +datum=WGS84")
    testbear_spatial <- spTransform(testbear_spatial, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))
  
  # extract matching raster using the date_char
   # matching_raster <- raster_list[which(names(raster_list) == testbear$date_char)] ------> this didn't work anymore
    matching_raster <- raster_list[which(names(raster_list) == testbear[i, "date_char"])]
    
  # uncomment if you're testing and want to see the ith raster/point
    # plot(matching_raster[[1]])
    # points(testbear_spatial, pch=20) 
  # extract value
    #used_avail[i, "ice_value"] <- raster::extract(matching_raster[[i]], testbear_spatial) -----> this didn't work anymore
    ice[[i]] <- data.frame(raster::extract(matching_raster[[i]], testbear_spatial))
}


ice_df <- as.data.frame(ice) # 74,613 values which matches the used_avail
summary(ice_df) # no NA values
names(ice_df)[1] <- "CONC"

ice_df_0 <- ice_df %>% filter(CONC=="0") #1,466/70,788 (~2%) are 0 values, which isn't bad!!

used_avail_ice <- cbind(used_avail, ice_df)
head(used_avail_ice)
summary(used_avail_ice)

hist(used_avail_ice$CONC)

used_ice <- used_avail_ice %>% filter(USED_AVAIL=="used") # 1463 used points
hist(used_ice$CONC)
summary(used_ice) # mean=0.760

avail_ice <- used_avail_ice %>% filter(USED_AVAIL=="available") # 73150 available points
hist(avail_ice$CONC)
summary(avail_ice) # mean=0.8416


write.csv(used_avail_ice, "data/Oct2020work/FINAL DATASET/used_avail_points_withbath_andice_Mar2021.csv")



# 6. - SKIP - (This didn't work) Convert distance to land values from decimal degrees to meters -------

# NOTE: This didn't actually work, refer to section 6 instead

# import data and format
      # NOTES: bathymetry values (BATH column) were done in QGIS
          # also, distance is in decimal degrees; also done in QGIS 
ice_bath_distland <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_ice_bath_distland.csv")
head(ice_bath_distland)
ice_bath_distland = subset(ice_bath_distland, select=-c(field_1, DIST_LAND, join_Sourc, join_featu, join_scale, join_min_z))
head(ice_bath_distland)
names(ice_bath_distland)[24] <- "ICE_CONC"
names(ice_bath_distland)[25] <- "BATH"
names(ice_bath_distland)[26] <- "DISTLAND_DECDEG"

# convert decimal degrees column to meters
      # equation from: https://sciencing.com/convert-distances-degrees-meters-7858322.html
ice_bath_distland$DISTLAND_M <- ice_bath_distland$DISTLAND_DECDEG*111139
head(ice_bath_distland)
summary(ice_bath_distland)

# view histograms of covariates
hist(ice_bath_distland$ICE_CONC)
hist(ice_bath_distland$BATH)
hist(ice_bath_distland$DISTLAND_DECDEG)
hist(ice_bath_distland$DISTLAND_M) # follows same shape as above, which is good


# 7. Get distance to land another way -----

# using sp and rgeos packages: http://rstudio-pubs-static.s3.amazonaws.com/7993_6b081819ba184047802a508a7f3187cb.html

# import data and make sure they're the same projection
land = readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/RSFs_Dec2020/Clipped_Land_Merged_NaturalEarthData.shp")
class(land)
crs(land)
extent(land)
plot(land) 

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_points_withbath_andice_Mar2021.csv")
head(used_avail)

used_avail_spatial <- used_avail
coordinates(used_avail_spatial) <- c("LONG", "LAT")
proj4string(used_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")

plot(land)
plot(used_avail_spatial, add=TRUE) # this works


###

# reproject both
used_avail_spatial_proj <- spTransform(used_avail_spatial, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
land_proj <- spTransform(land, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

plot(land_proj)
plot(used_avail_spatial_proj, add=TRUE) # this works


####

  
# attempting loop using https://stackoverflow.com/questions/39276754/finding-the-nearest-distances-between-a-vector-of-points-and-a-polygon-in-r
# also used this to help put it into origial df: https://stackoverflow.com/questions/24619783/for-loop-r-create-and-populate-new-column-with-output

used_avail$DIST_loop <- 0

g = rep(NA, dim(used_avail_spatial_proj)[1])
for(i in 1:dim(used_avail_spatial_proj)[1]){
  g[i] <- gDistance(used_avail_spatial_proj[i,],land_proj)
  used_avail$DIST_loop[i] <- g[i]
}

head(used_avail)
names(used_avail)[21] <- "DIST_LAND"

summary(used_avail)

# make used_avail into new csv and bring into QGIS to test some of the distances
# I randomly tested 5 bears comparing measurement tool to results from this, and it worked!
# this new column is in meters

write.csv(used_avail, "data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_Mar2021.csv")


-----


# 8. Get distance to water (test with one date) -------

# separate one date for points (19940404) and make spatial
bear19940404 <- filter(used_avail, DATE=="1994/04/04")
bear19940404_spatial <- bear19940404
coordinates(bear19940404_spatial) <- c("LONG", "LAT")
proj4string(bear19940404_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(bear19940404_spatial)

# separate one date for rasters and make spatial
raster_19940404 <- raster_list$'19940404'
proj4string(raster_19940404) # polar stereographic
plot(raster_19940404)
coordinates(raster_19940404) # super weird

# reproject the raster and plot altogether: https://datacarpentry.org/r-raster-vector-geospatial/03-raster-reproject-in-r/
raster_19940404_latlon <- projectRaster(raster_19940404, crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
plot(raster_19940404_latlon) # this seems better, but the plot is a bit warped
plot(bear19940404_spatial, add=TRUE) # it looks like it's plotting these points in the right spot!

      # check projections and coordinates
coordinates(raster_19940404_latlon) 
summary(coordinates(raster_19940404_latlon))
coordinates(bear19940404_spatial)
summary(coordinates(bear19940404_spatial)) # these look good

crs(bear19940404_spatial) # these are the same
crs(raster_19940404_latlon)


###

# get distance from middle of water pixels to all other pixels
      # https://gis.stackexchange.com/questions/321295/calculating-distance-between-each-pixel-of-rasterstack-image

      # line 374 (separating the water pixels), doesn't work when there are NA values
      # so we need to deal with them first - since we can't remove them, replace them with a value that's unrealistic
summary(raster_list$'19940404'[]) 
summary(raster_19940404_latlon[]) # this one seems off
summary(raster_19940404[]) # this one matches line 312; i.e., they both have the same # of NA values

raster_19940404[is.na(raster_19940404[])] <- 100 # replace all NA values with 100 
raster_19940404[] # this worked
raster_100 = as(raster_19940404, "SpatialPoints")[raster_19940404[]==100]  # pull all 100 values out to see where they are
plot(raster_100) # this is everything on land, and potentially in the open water area

water_19940404 = as(raster_19940404, "SpatialPoints")[raster_19940404[]==0]  # specify 0 for water
crs(water_19940404) # polar stereographic
water_19940404_latlon <- spTransform(water_19940404, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


      # this works now because the NA values are now 100
plot(raster_19940404_latlon)
plot(ocean_mask, add=TRUE)
plot(water_19940404_latlon, col="red", add=TRUE) # now it looks like it's classifying the correct areas as open water, except for the shoreline
plot(bear19940404_spatial, col="blue", add=TRUE)

# change projection of raster and plot with points to check
crs(water_19940404) # polar stereographic
water_19940404_latlon <- spTransform(water_19940404, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
crs(water_19940404_latlon)
coordinates(water_19940404_latlon) # THIS WORKS
head(water_19940404_latlon)
plot(water_19940404_latlon) # this looks like it might have worked
plot(bear19940404_spatial, col="red", add=TRUE) # the points seem to be in the right spot


# get distance to closest water point
summary(coordinates(bear19940404_spatial))
summary(coordinates(water_19940404_latlon))

polarbear_lat  <- coordinates(bear19940404_spatial)[,2]
polarbear_long <- coordinates(bear19940404_spatial)[,1]

openwater_lat  <- coordinates(water_19940404_latlon)[,2]
openwater_long <- coordinates(water_19940404_latlon)[,1]
  
# xy_bear is good to go  
xy_bear <- SpatialPointsDataFrame(
    matrix(c(polarbear_long, polarbear_lat), ncol=2), data.frame(ID=seq(1:length(polarbear_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# running this relies on lines 331 and 332
xy_water <- SpatialPointsDataFrame(
  matrix(c(openwater_long, openwater_lat), ncol=2), data.frame(ID=seq(1:length(openwater_long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

mdist <- geosphere::dist2Line(xy_bear, xy_water) 
mdist_df <- as.data.frame(mdist)
summary(mdist_df) # distance is in meters: range from 170,106 to 365,086

# merge with bear dataframe
head(bear19940404)
head(mdist_df)

bear19940404_distwater <- cbind(bear19940404, mdist_df)
colnames(bear19940404_distwater)
names(bear19940404_distwater)[23] <- "DIST_WATER"
names(bear19940404_distwater)[24] <- "WATER_LONG"
names(bear19940404_distwater)[25] <- "WATER_LAT"
colnames(bear19940404_distwater)





###


# IGNORE BELOW


# Compare above to method using gDistance()

# get distance values
DIST_WATER = gDistance(water_19940404_latlon, bear19940404_spatial, byid=TRUE) # ignore the error message
dim(DIST_WATER)
str(DIST_WATER)
class(DIST_WATER)
DIST_WATER_df = as.data.frame(DIST_WATER)
head(DIST_WATER_df) # this gives me the distance to every water pixel

# I need to get the lowest value for each row and separate into its own df
# then combine this with bear19940404 - should have an identifier column in DIST_WATER2 (so we know which point to assign it to)

# get minimum
DIST_WATER_final <- apply(DIST_WATER_df, 1, min)
DIST_WATER_final = as.data.frame(DIST_WATER_final)

head(DIST_WATER_final)
head(min_d_df) # these are very different, so that's great!

summary(DIST_WATER_final)



# 10. Loops to extract all distance to water values --------

# import and format data
used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_Mar2021.csv")
head(used_avail)
used_avail=subset(used_avail, select=-c(X, X.1)) # remove unneccessary columns
used_avail$DIST_WATER <- as.numeric(rep(NA, nrow(used_avail)))
head(used_avail)
str(used_avail)

used_avail_spatial <- used_avail
coordinates(used_avail_spatial) <- c("LONG", "LAT")
proj4string(used_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")

head(used_avail_spatial) # lat and long are missing, which is good
class(used_avail_spatial) # spatialpointsdataframe

      # this covariate requires the cropped raster_list since shoreline pixels were being classified as 0, making distances much less than they should be
#raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_distwaterversion.rds")
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")
plot(raster_list$`19781026`)

###


# Steps
      # [x] 1. For all rasters in raster_list, change NA values to 100 (necessary for next step to work)
      # [x] 2. For all rasters, pull out water pixels (0 values) and make into new water list of spatial points
      # [x] 3. Make sure names of new water list match names of raster_list
      # [x] 4. Make sure that projections of used_avail and water list match
      # [x] 5. Set coordinates of used_avail and water points (using coordinates())
      # [x] 6. Make both into SpatialPointsDataFrame
      # [] 7. Use both SpatialPointsDataFrame and dist2Line() to get distance to water values
      # [] 8. merge new values with original used_avail points to have data altogether


###


# Step 1: make all NA values in raster_list = 100
for (i in 1:length(raster_list)){
  raster_list[[i]][is.na(raster_list[[i]][])] <- 100
}

raster_list$`19781026`[] # this worked


###


# Step 2: make new list for water pixels and pull out all 0 values; set projection; pull coordinates
# Step 5: set coordinates for this new list (water_coordinates)
# Step 6: make into spatialpointsdataframe (water_coordinates)

water <- list() 
water_spatial <- list()
water_coordinates <- list()
for(i in 1:length(raster_list)){
  #i = 1 # this was in the loop, so the loop was only ever producing a list of "1"
  water[[i]] = as(raster_list[[i]], "SpatialPoints")[raster_list[[i]][]==0]  # pull out the water pixels
  water_spatial[[i]] <-  spTransform(water[[i]], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # set the projection
  lat <- coordinates(water_spatial[[i]])[,2] 
  long <-  coordinates(water_spatial[[i]])[,1]
  #water_coordinates[[i]] <- cbind(lat, long)
  water_coordinates[[i]] <- SpatialPointsDataFrame(matrix(c(long, lat), ncol=2), data.frame(ID=seq(1:length(water[[i]]))), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
} 
# this new water_coordinates line works in that it makes the SpatialPointsDataFrames we need (so we can skip the xy_water part)
# but I'm not sure what the ID column is doing


###

# Steps 3 and 4: match names and check projections
names(water_coordinates) 
names(water_coordinates) <- names(raster_list) # can I just do that?
head(water_coordinates$`19781026`) # the ID column is just a list of numbers
tail(water_coordinates$`19781026`) # it must be counting the # of water pixels for each dataframe in the list
class(water_coordinates$`19781026`) # but they're the right class now
summary(coordinates(water_coordinates$`19781026`)) # coords.x1 = lat, coords.x2 = long
proj4string(used_avail_spatial)
proj4string(water_coordinates$`19781026`) # this looks right

head(water_coordinates)
str(water_coordinates)
###


# at this point we have the bear points as a spatialpointsdataframe (xy_bear or used_avail_spatial)
# we also have a list of the water pixels as spatialpointsdataframes (water_coordinates), with the dates as the names
# all of these have matching projections, so we should just be able to get the distances now


###


# Steps 7 and 8: pull distance to water values and add to used_avail dataframe

dim(used_avail)


for(i in 1:nrow(used_avail)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  
  
  bear_lat <- coordinates(used_avail_spatial)[i,2]
  bear_long <- coordinates(used_avail_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

  
  used_avail[i, "DIST_WATER"] <- geosphere::dist2Line(xy_bear, xy_water)
}


head(used_avail) # this might not have worked
summary(used_avail)
names(used_avail)[27] <- "DIST_WATER"




###

# TEST WITH A SUBSET

###

used_avail=subset(used_avail, select=-c(DIST_WATER))
used_avail_subset <- used_avail %>% filter(date_char==19940404)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))
str(used_avail_subset)
used_avail_subset_spatial <- used_avail_subset
#coordinates(used_avail_subset_spatial) <- c("LONG", "LAT")
str(used_avail_subset_spatial)
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

mdist <- list() # dist2Line: original option
mdist2 <- list() # gDistance: new option that doesn't work

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  mdist2[[i]] <- as.data.frame(apply(gDistance(xy_bear, xy_water, byid=TRUE),2,min))
  #mdist[[i]] <- data.frame(DIST_WATER=geosphere::dist2Line(xy_bear, xy_water)[,1],
                      #lon=geosphere::dist2Line(xy_bear, xy_water)[,2],
                      #lat=geosphere::dist2Line(xy_bear, xy_water)[,3])
}

mdist_df <- bind_rows(mdist)
bears_distwater <- cbind(used_avail_subset, mdist_df)
head(bears_distwater)
summary(bears_distwater)

mdist2_df <- bind_rows(mdist2) # nope


# test plot 

proj4string(raster_list$'19940404')
proj4string(water_coordinates$'19940404')

raster_19940404 <- raster_list$'19940404'
water_19940404 <- water_coordinates$'19940404'
raster_19940404_latlon <- projectRaster(raster_19940404, crs="+proj=longlat +datum=WGS84 +no_defs")

head(bears_distwater)
str(bears_distwater)
bears_distwater_spatial <- bears_distwater
coordinates(bears_distwater_spatial) <- ~lon + lat
proj4string(bears_distwater_spatial) <- CRS("+proj=longlat +datum=WGS84")
plot(bears_distwater_spatial)

plot(raster_19940404_latlon, col=(viridis(5)), zlim=c(0, 1))
plot(water_19940404, pch=20, col=rgb(1, 0, 0, 0.2), add=TRUE)
points(bear19940404_spatial, col="red")
plot(bears_distwater_spatial, col="black", pch=20, add=TRUE)


# figure out what is going on

str(xy_water) 
xy_water_df <- as.data.frame(xy_water)
head(xy_water_df)
plot(xy_water_df$coords.x1, xy_water_df$coords.x2, panel.first=grid()) # these are all the water points for raster 19940404
points(mdist_df$lon, mdist_df$lat, col="red") # these are the closest points to the bears; see that the southern line do not align with the black points (even though they should)
points(bear19940404$LONG, bear19940404$LAT, col="blue") # these are the bears

wrong_water <- mdist_df %>% filter(lat<=53) # there are 24
points(wrong_water$lon, wrong_water$lat, col="purple") # these are correct
head(wrong_water)
summary(wrong_water)

wrong_water_spatial <- wrong_water
coordinates(wrong_water_spatial) <- ~lon + lat
duplicates <- zerodist(wrong_water_spatial) # 0 = they're all unique points


head(xy_water_df)
head(mdist_df)
names(xy_water_df)[2] <- "lon"
names(xy_water_df)[3] <- "lat"
difference <- anti_join(xy_water_df, mdist_df)

points(difference$lon, difference$lat, col="pink") # IT DOESN'T EVEN PLOT THE WRONG ONES



# I DON'T GET IT













###

# TEST WITH A DIFFERNT SUBSET

###

used_avail_subset <- used_avail %>% filter(date_char==20010311)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))
str(used_avail_subset)
used_avail_subset_spatial <- used_avail_subset
#coordinates(used_avail_subset_spatial) <- c("LONG", "LAT")
str(used_avail_subset_spatial)
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

mdist <- list()


for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  mdist[[i]] <- data.frame(DIST_WATER=geosphere::dist2Line(xy_bear, xy_water)[,1],
                           lon=geosphere::dist2Line(xy_bear, xy_water)[,2],
                           lat=geosphere::dist2Line(xy_bear, xy_water)[,3])
}

mdist_df <- bind_rows(mdist)
bears_distwater <- cbind(used_avail_subset, mdist_df)
head(bears_distwater)
summary(bears_distwater)

# test plot 

proj4string(raster_list$'20010311')
proj4string(water_coordinates$'20010311')

raster_20010311 <- raster_list$'20010311'
water_20010311 <- water_coordinates$'20010311'
raster_20010311_latlon <- projectRaster(raster_20010311, crs="+proj=longlat +datum=WGS84 +no_defs")

plot(raster_20010311_latlon)
points(water_20010311) # looks good!

# plot better

head(bears_distwater)
str(bears_distwater)
bears_distwater_spatial <- bears_distwater
coordinates(bears_distwater_spatial) <- ~lon + lat
proj4string(bears_distwater_spatial) <- CRS("+proj=longlat +datum=WGS84")
plot(bears_distwater_spatial)

plot(raster_20010311_latlon, col=(viridis(5)), zlim=c(0, 1))
plot(water_20010311, pch=20, col=rgb(1, 0, 0, 0.2), add=TRUE)
points(used_avail_subset_spatial, col="pink")
plot(bears_distwater_spatial, col="black", pch=20, add=TRUE)

# this one looked like it worked!



###

# TEST WITH ANOTHER DIFFERNT SUBSET

###
used_avail_subset <- used_avail %>% filter(date_char==19980319)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))
str(used_avail_subset)
used_avail_subset_spatial <- used_avail_subset
#coordinates(used_avail_subset_spatial) <- c("LONG", "LAT")
str(used_avail_subset_spatial)
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

mdist <- list()


for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
  mdist[[i]] <- data.frame(DIST_WATER=geosphere::dist2Line(xy_bear, xy_water)[,1],
                           lon=geosphere::dist2Line(xy_bear, xy_water)[,2],
                           lat=geosphere::dist2Line(xy_bear, xy_water)[,3])
}

mdist_df <- bind_rows(mdist)
bears_distwater <- cbind(used_avail_subset, mdist_df)
head(bears_distwater)
summary(bears_distwater)

# test plot 

proj4string(raster_list$'19980319')
proj4string(water_coordinates$'19980319')

raster_19980319 <- raster_list$'19980319'
water_19980319 <- water_coordinates$'19980319'
raster_19980319_latlon <- projectRaster(raster_19980319, crs="+proj=longlat +datum=WGS84 +no_defs")

plot(raster_19980319_latlon)
points(water_19980319) # looks good!

# plot better

head(bears_distwater)
str(bears_distwater)
bears_distwater_spatial <- bears_distwater
coordinates(bears_distwater_spatial) <- ~lon + lat
proj4string(bears_distwater_spatial) <- CRS("+proj=longlat +datum=WGS84")
plot(bears_distwater_spatial)

plot(raster_19980319_latlon, col=(viridis(5)), zlim=c(0, 1))
plot(water_19980319, pch=20, col=rgb(1, 0, 0, 0.2), add=TRUE)
points(used_avail_subset_spatial, col="pink")
plot(bears_distwater_spatial, col="black", pch=20, add=TRUE)

# this one looked like it worked!




# 11. Import and format data for section 12 below ---------


# BEAR DATA

used_avail <- read_csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_Mar2021.csv") 
head(used_avail)
used_avail <- used_avail %>%
  mutate(DIST_WATER = as.numeric(rep(NA))) %>%
  select(-X,-X1)
head(used_avail)


used_avail_spatial <- used_avail
coordinates(used_avail_spatial) <- c("LONG", "LAT")
proj4string(used_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")


###


# SEA ICE DATA (this section takes a little while)


# this covariate requires the cropped raster_list since shoreline pixels were being classified as 0, making distances much less than they should be

# Larissa's
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")

# Erik's
raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_distwaterversion.rds")

# make all NA values in raster_list = 100
for (i in 1:length(raster_list)){
  raster_list[[i]][is.na(raster_list[[i]][])] <- 100
}


# get water points
water <- list() 
water_spatial <- list()
water_coordinates <- list()
for(i in 1:length(raster_list)){
  #i = 1 # this was in the loop, so the loop was only ever producing a list of "1"
  water[[i]] = as(raster_list[[i]], "SpatialPoints")[raster_list[[i]][]==0]  # pull out the water pixels
  water_spatial[[i]] <-  spTransform(water[[i]], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # set the projection
  lat <- coordinates(water_spatial[[i]])[,2] 
  long <-  coordinates(water_spatial[[i]])[,1]
  water_coordinates[[i]] <- SpatialPointsDataFrame(matrix(c(long, lat), ncol=2), data.frame(ID=seq(1:length(water[[i]]))), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
} 


# match names/projections
names(water_coordinates) <- names(raster_list) # can I just do that?


# ERIK:
# I wonder if that second loop should be changed so that water_coordinates
# comes out as an sf object rather than the SpatialPointsDataFrames?
# The st_as_sf function is where I'm stuck at in the next section.






# 12. Loop to extract all dist_water using FNN package ------


# OPTION 1: doesn't work
# st_as_sf error: cannot derive coordinates from non-numeric matrix

for(i in 1:nrow(used_avail)){
  
  i=1
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- st_as_sf( 
    coords=c(water_long, water_lat), crs=4326)
  bear_lat <- coordinates(used_avail_spatial)[i,2]
  bear_long <- coordinates(used_avail_spatial)[i,1]
  xy_bear <- st_as_sf(coords=c(bear_long, bear_lat), crs=4326)
  used_avail[i, "DIST_WATER"] <- RANN::n22(xy_water, xy_bear, k=1)
}


# OPTION 2: doesn't work 
# st_as_sf error: "trying ot get slot 'coords' from an object of basic class 'list' with no slots"

for(i in 1:nrow(used_avail)){
  i=1
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail[i,"date_char"])]
  xy_water <- st_as_sf(coords=c(matching_raster@coords$coords.x2, matching_raster@coords$coords.x1), crs=4326)
  xy_bear <- st_as_sf(coords=c(used_avail$LONG, used_avail$LAT), crs=4326)
  used_avail[i, "DIST_WATER"] <- RANN::n22(xy_water, xy_bear, k=1)
}


# OPTION 3: doesn't work
# error in water_lat and water_long lines: "incorrect number of dimensions"

for(i in 1:nrow(used_avail)){ 
  i=1
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- st_as_sf(coords=c(water_long, water_lat), crs=4326)
  xy_bear <- st_as_sf(coords=c(used_avail$LONG, used_avail$LAT), crs=4326)
  used_avail[i, "DIST_WATER"] <- RANN::n22(xy_water, xy_bear, k=1)
} 




# 12A. Erik's code (distance to water) -------------------------------------------------------------





# Load data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

used_avail <- read_csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_Mar2021.csv") %>%
  dplyr::select(-X,-X1)


# Larissa's ice
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")

# Erik's ice
# raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_distwaterversion.rds")


# make all NA values in raster_list = 100
for (i in 1:length(raster_list)){
  raster_list[[i]][is.na(raster_list[[i]][])] <- 100
}

# get water points
water <- list() 
water_spatial <- list()
water_coordinates <- list()
for(i in 1:length(raster_list)){
  #i = 1 # this was in the loop, so the loop was only ever producing a list of "1"
  water[[i]] = as(raster_list[[i]], "SpatialPoints")[raster_list[[i]][]==0]  # pull out the water pixels
  water_spatial[[i]] <-  spTransform(water[[i]], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # set the projection
  lat <- coordinates(water_spatial[[i]])[,2] 
  long <-  coordinates(water_spatial[[i]])[,1]
  water_coordinates[[i]] <- SpatialPointsDataFrame(matrix(c(long, lat), ncol=2), data.frame(ID=seq(1:length(water[[i]]))), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
} 


# match names/projections
names(water_coordinates) <- names(raster_list) 



# Finish loading data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# let's shave down raster/water_coordinates lists to match the used_avail df
dates <- unique(used_avail$date_char)

# don't think we're even using the raster_list here, but this is how you'd do it
water_coords_subset <- water_coordinates[paste0(dates)]


# loop is very quick now, 
bears_water <- list()

for (i in 1:length(water_coords_subset)){
  print(i)
  id <- names(water_coords_subset[i])
  
  bear_df <- used_avail %>%
    filter(date_char == id) 
  
  water_df <- as.data.frame(water_coords_subset[paste0(id)])
  water_df <- as_tibble(water_df) %>%
    rename(id = 1, long = 2, lat = 3, misc = 4)
  
  bear_spatial <- bear_df %>%
    sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326)
  water_spatial <- water_df %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326)
  
  # package coordinates
  bear_coords <- do.call(rbind, st_geometry(bear_spatial))
  water_coords <- do.call(rbind, st_geometry(water_spatial))
  
  # calculate closest open water point to each bear point
  closest <- nn2(water_coords, 
                 bear_coords,
                 k = 1) # find single closest point
  
  # clean closest point info, and bind with bear points
  bears_water[[i]] <- sapply(closest, cbind) %>%
    as_tibble() %>%
    dplyr::select(id = 1, dist = 2) %>%
    left_join(dplyr::select(water_df, id, water_long = long, water_lat = lat), by = "id") %>%
    bind_cols(bear_df)
  
}

# flatten list
# and this is your data. I think we should check to make sure that the distances (dist2water$dist) are accurate.
dist2water <- bind_rows(bears_water) 

head(dist2water)
names(dist2water)[1] <- "WATER_ID"
names(dist2water)[2] <- "DIST_WATER"
names(dist2water)[3] <- "WATER_LONG"
names(dist2water)[4] <- "WATER_LAT"

# export final dataframe
write.csv(dist2water, "data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_distwater_seasons_Apr2021.csv")


## Visualize ~~~~~~~~~~~~~~~~~~~~~~

# "dates" is a vector of all the unique dates in our data, we made it above. 
# let's pull the first date out of the vector and see what the distances to water look like
# if this looks ok, you can randomly pull dates out by changing "1" to whatever number you like


date <- dates[1]


water_df <- as.data.frame(water_coords_subset[paste0(date)]) %>%
  as_tibble() %>%
  dplyr::select(id = 1, x = 2, y = 3)
bear_df <- dist2water %>%
  filter(date_char == date) %>%
  dplyr::select(bear_x = LONG, bear_y = LAT,water_x = water_long, water_y = water_lat)
head(bear_df)



# Plot
plot(x = water_df$x, y = water_df$y, col = "#D0E0E8", pch = 16,  axes = F, ann = FALSE)
within(bear_df,{
     segments(x0 = bear_x, y0 = bear_y, x1 = water_x, y1 = water_y, col = "#CCCCCC")
     points(x = bear_x, y = bear_y, col = "#7F7F7F", pch = 16)
     points(x = water_x, y = water_y, col = "#004366", pch = 16)
     })



# End of Erik's section ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
























  
  
  

###

# Ignore below


# package coordinates
bear_coords2 <- do.call(rbind, st_geometry(bear_spatial2))
water_coords2 <- do.call(rbind, st_geometry(water_spatial2))


closest2 <- nn2(water_coords2, 
                bear_coords2,
                k = 1) # find single closest point

# clean closest point info, and bind with bear points
bear_water2 <- sapply(closest2, cbind) %>% 
  as_tibble() %>%
  bind_cols(bear_df2)


# Plot water
plot(x = water_df2$coords.x1, y = water_df2$coords.x2, col = "#AFC0C9", pch = 16,  axes = F, ann = FALSE)

# plot paths from bears to closest water
for (i in 1:nrow(bear_water2)){
  water_index <- as.numeric(bear_water2[i,1])
  
  water_x2 <- as.numeric(water_df2[water_index,2])
  water_y2 <- as.numeric(water_df2[water_index,3])
  
  bear_x2 <- as.numeric(bear_water2[i,"LONG"])
  bear_y2 <- as.numeric(bear_water2[i, "LAT"])
  
  points(x = water_x2, y = water_y2, col = "#AD1520", pch = 16)
  segments(x0 = bear_x2, y0 = bear_y2, 
           x1 = water_x2, y1 = water_y2,
           col = "pink")
}

# plot bear points
points(y = bear_water2$LAT, x = bear_water2$LONG, col = "#5E5E5E", pch = 16)









