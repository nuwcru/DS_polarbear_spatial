

# 1. Load libraries ------

library(ggplot2)
library(dplyr)
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
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78.rds")
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




# 3. Load final datasets, format, check projections, and visualize (for section 8) -----

# using this: https://www.neonscience.org/resources/learning-hub/tutorials/extract-values-rasters-r

# import used and available points
# note that this was made in script #7 - all used land fixes have been removed, and we think we've dealt with on-land available too
used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_points.csv")
head(used_avail)
used_avail=subset(used_avail, select=-c(X)) # remove unneccessary columns

ggplot(data=used_avail) +
  geom_point(aes(x=LONG, y=LAT, colour=USED_AVAIL, alpha=USED_AVAIL)) +
  scale_x_continuous(limits=c(-90, -40), breaks=c(-90, -80, -70, -60, -50, -40), labels=c("-90", "-80", "-70", "-60", "-50", "-40")) +
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
<<<<<<< HEAD
/Users/erikhedlin/Downloads/Long-term_Ecosystem_Monitoring_Project_WILDTRAX_REPORT 17.csv
# import cropped raster_list
raster_list <- readRDS("/Users/erikhedlin/Downloads/raster_list_distwaterversion.rds")
=======

# import cropped raster_list (it's fine that 2020 data isn't in here; we don't have bear data for 2020)
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_distwaterversion.rds")
>>>>>>> 91cadab36217f31e5b3d8e6461daa8c6a7e28550
names(raster_list)
proj4string(raster_list$`19781026`) # check that it's in polar stereographic


# plot one raster with points
plot(raster_list$`19781026`)
points(used_avail_spatial) # this works!




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
# need a different loop to pull matching raster for each date? Or can this be a line?
# also need the values to go into a dataframe; the test went into "Values"..?


head(used_avail)

# let's make a column that matches the raster_list names
used_avail$date_char <- stringr::str_replace_all(used_avail$DATE, "-", "")
used_avail$ice_value <- as.numeric(rep(NA, nrow(used_avail)))


for(i in 1:nrow(used_avail)){
  
  # uncomment, and run each line individually if you want to test this for i = 1...N
    # i = 1
  
  # convert ith ROWID to spatial
    testbear <- used_avail[i,]
    testbear_spatial <- testbear
    coordinates(testbear_spatial) <- c("LONG", "LAT")
    proj4string(testbear_spatial) <- CRS("+proj=longlat +datum=WGS84")
    testbear_spatial <- spTransform(testbear_spatial, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'))
  
  # extract matching raster using the date_char
    matching_raster <- raster_list[which(names(raster_list) == testbear$date_char)]
  
  # uncomment if you're testing and want to see the ith raster/point
    # plot(matching_raster[[1]])
    # points(testbear_spatial, pch=20) 
  # extract value
    used_avail[i, "ice_value"] <- raster::extract(matching_raster[[1]], testbear_spatial)

}



hist(used_avail$ice_value)
land <- used_avail %>% filter(used_avail$ice_value=="0") # there are 14198/74613 points (or 19%)

land_used <- land %>% filter(USED_AVAIL=="used") # 295 used points
land_avail <- land %>% filter(USED_AVAIL=="available") # 13903 available points
# this will still show that bears are probably choosing NOT to use open-water

no_values <- used_avail[is.na(used_avail$ice_value),] # at least there aren't any NAs now; i.e. none are outside the raster

land_avail_spatial <- land_avail
coordinates(land_avail_spatial) <- c("LONG", "LAT")
proj4string(land_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")
proj4string(land_avail_spatial)

mapview(land_avail_spatial) # it looks like they're not on land!

write.csv(used_avail, "data/used_avail_withicevalues.csv")



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
plot(land) # looks like mercader

used_avail <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_ice_bath_distland.csv")
used_avail_spatial <- used_avail
coordinates(used_avail_spatial) <- c("LONG", "LAT")
proj4string(used_avail_spatial) <- CRS("+proj=longlat +datum=WGS84")

plot(land)
plot(used_avail_spatial, add=TRUE) # this works

# gDistance(land2, used_avail_spatial) 
# above only gives me one value, and it's saying we need to project both

used_avail_spatial_proj <- spTransform(used_avail_spatial, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))
land_proj <- spTransform(land, crs("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"))

plot(land_proj)
plot(used_avail_spatial_proj, add=TRUE) # this works

-----
# looks like below works but I need it in a dataframe
#distances = gDistance(land_proj, used_avail_spatial_proj, byid=TRUE)
#distances2 <- as.data.frame(distances)
-----
  
# attempting loop using https://stackoverflow.com/questions/39276754/finding-the-nearest-distances-between-a-vector-of-points-and-a-polygon-in-r
# also used this to help put it into origial df: https://stackoverflow.com/questions/24619783/for-loop-r-create-and-populate-new-column-with-output

head(used_avail_spatial_proj)

used_avail$DIST_loop <- 0

g = rep(NA, dim(used_avail_spatial_proj)[1])
for(i in 1:dim(used_avail_spatial_proj)[1]){
  g[i] <- gDistance(used_avail_spatial_proj[i,],land_proj)
  used_avail$DIST_loop[i] <- g[i]
}

head(used_avail)

# make used_avail into new csv and bring into QGIS to test some of the distances
# I randomly tested 5 bears comparing measurement tool to results from this, and it worked!
# this new column is in meters
write.csv(used_avail, "data/Oct2020work/FINAL DATASET/used_avail_ice_bath_distland_final.csv")


-----


# 8. Get distance to water (test with one date) -------

# separate one date for points (19940404) and make spatial
bear19940404 <- filter(used_avail, DATE=="1994-04-04")
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
plot(raster_100) # this is everything outside of the water mask, and potentially in the open water area

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




# make column in used_avail to match raster names and empty column for distance to water values
head(used_avail)
used_avail$date_char <- stringr::str_replace_all(used_avail$DATE, "-", "")
used_avail$DIST_WATER <- as.numeric(rep(NA, nrow(used_avail)))
head(used_avail)

# make all NA values in raster_list = 100
for(i in 1:length(raster_list)){
  raster_list[[i]][is.na(raster_list[[i]][])] <- 100
}






# make new list for water pixels and pull out all 0 values, also make sure that they're in the right projection
      # I'm not sure how to do this one
      # we need to pull out the water values for every raster, but do we put those values into a new list?
      # we need the date attached to them to make sure that we can pull the correct point for each date

water <- list() #?
water_spatial <- list()
water_coordinates <- list()
for(i in 1:length(raster_list)){
  i = 1
  water[[i]] = as(raster_list[[i]], "SpatialPoints")[raster_list[[i]][]==0]  # pull out the water pixels
  water_spatial[[i]] <-  spTransform(water[[i]], CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # set the projection
  lat <- coordinates(water_spatial[[i]])[,2] # I'm not actually sure this is lat? double check
  long <-  coordinates(water_spatial[[i]])[,1]
  water_coordinates[[i]] <- cbind(lat, long)
}


# create matrix of bear/random points
xy_bear <- SpatialPointsDataFrame(
  matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# create matrix of all water points
xy_water <- SpatialPointsDataFrame(
  matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(openwater_long))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))


# set projection/coordinates for used_avail
for(i in 1:nrow(used_avail)){
  # i = 1
  # convert ith ROWID to spatial
  test <- used_avail[i,]
  used_avail_spatial <- test
  coordinates(used_avail_spatial) <- c("LONG", "LAT")
  proj4string(used_avail_spatial) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
  bear_lat  <- coordinates(used_avail_spatial)[,2]
  bear_long <- coordinates(used_avail_spatial)[,1]
}

# pull distance to water values 
for(i in 1:nrow(used_avail)){
  # extract matching raster using the date_char
  matching_raster <- water[which(names(water) == used_avail$date_char)]
  used_avail[i, "DIST_WATER"] <- geosphere::dist2Line(xy_bear, water[i])
}




