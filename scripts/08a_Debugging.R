
# 1. Load libraries ------

library(ggplot2)
library(tidyverse)
library(raster)
library(sp)
library(sf) # for st_read function
library(viridis) # for plotting (section 10)
library(dplyr)
library(RANN)
setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/DS_polarbear_spatial/")



# 2. Import and format data --------

# BEAR DATA

used_avail <- read_csv("data/Oct2020work/FINAL DATASET/used_avail_bath_ice_distland_Mar2021.csv") %>%
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



# New method --------------------------------------------------------------



# Build new way to determine minimum distance to open water


head(used_avail)

# Pull out the lame raster and bear points

# Erik
#bear_df <- used_avail %>%
  #filter(date_char==19940404) %>%
  #dplyr::select(LONG, LAT, -DIST_WATER) # I got an error saying that the DIST_WATER didn't exist

# Larissa
bear_df <- used_avail %>%
  filter(date_char==19940404) %>%
  dplyr::select(LONG, LAT)

water_df <- as_tibble(water_coordinates$'19940404')

# spatial dfs
# Erik
#bear_spatial <- used_avail %>%
  #filter(date_char==19940404) %>%
  #dplyr::select(LONG, LAT, -DIST_WATER) %>%
  #sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326) # same error here

# Larissa
bear_spatial <- used_avail %>%
  filter(date_char==19940404) %>%
  dplyr::select(LONG, LAT) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326)

water_spatial <- st_as_sf(water_coordinates$'19940404', crs = 4326)


# package coordinates
bear_coords <- do.call(rbind, st_geometry(bear_spatial))
water_coords <- do.call(rbind, st_geometry(water_spatial))


closest <- nn2(water_coords, 
               bear_coords,
                k = 1) # find single closest point

# clean closest point info, and bind with bear points
bear_water <- sapply(closest, cbind) %>% 
  as_tibble() %>%
  bind_cols(bear_df)


# Plot water
plot(x = water_df$coords.x1, y = water_df$coords.x2, col = "#AFC0C9", pch = 16,  axes = F, ann = FALSE)

# plot paths from bears to closest water
for (i in 1:nrow(bear_water)){
  water_index <- as.numeric(bear_water[i,1])
  
  water_x <- as.numeric(water_df[water_index,2])
  water_y <- as.numeric(water_df[water_index,3])
  
  bear_x <- as.numeric(bear_water[i,"LONG"])
  bear_y <- as.numeric(bear_water[i, "LAT"])
  
  points(x = water_x, y = water_y, col = "#AD1520", pch = 16)
  segments(x0 = bear_x, y0 = bear_y, 
           x1 = water_x, y1 = water_y,
           col = "pink")
}

# plot bear points
points(y = bear_water$LAT, x = bear_water$LONG, col = "#5E5E5E", pch = 16)



# end of new code. Next step is to apply this to all rasters
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# New method using a different raster --------------------------------------------------------------

# Erik
#bear_df <- used_avail %>%
#filter(date_char==19940404) %>%
#dplyr::select(LONG, LAT, -DIST_WATER) # I got an error saying that the DIST_WATER didn't exist

# Larissa
bear_df2 <- used_avail %>%
  filter(date_char==20010311) %>%
  dplyr::select(LONG, LAT)

water_df2 <- as_tibble(water_coordinates$'20010311')

# spatial dfs

# Erik
#bear_spatial <- used_avail %>%
#filter(date_char==19940404) %>%
#dplyr::select(LONG, LAT, -DIST_WATER) %>%
#sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326) # same error here

# Larissa
bear_spatial2 <- used_avail %>%
  filter(date_char==20010311) %>%
  dplyr::select(LONG, LAT) %>%
  sf::st_as_sf(coords = c("LONG", "LAT"), crs = 4326)

water_spatial2 <- st_as_sf(water_coordinates$'20010311', crs = 4326)


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







# 3. Loop #1 with problem subset (using: dist2Line, gDistance, get.knnx, or spDists) ---------------


used_avail=subset(used_avail, select=-c(DIST_WATER))
used_avail_subset <- used_avail %>% filter(date_char==19940404)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))

used_avail_subset_spatial <- used_avail_subset
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

# Erik, for the loop below I was testing different options of the last part where we get mdist
# I think this is what is giving us the incorrect values 
# Use mdist (not mdist2), I just wanted to keep it in here so that I remember that I tried it!
# Also, if you try mdist2, you need to use the polar projection, rather than lat/long

#mdist <- list() # dist2Line: original option 
#mdist2 <- list() # gDistance: doesn't work (water pixels aren't identified)
#mdist3 <- list() # get.knnx: doesn't work (same as above)
mdist4 <- list() # spDists: doesn't work (same as aboe)

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
  # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  #mdist2[[i]] <- as.data.frame(apply(gDistance(xy_bear, xy_water, byid=TRUE),2,min))
  mdist3[[i]] <- as.data.frame(get.knnx(coordinates(xy_water), coordinates(xy_bear), k=1))
  #mdist4[[i]] <- as.data.frame(spDists(xy_water, xy_bear), longlat=TRUE)
  #mdist[[i]] <- data.frame(DIST_WATER=geosphere::dist2Line(xy_bear, xy_water)[,1],
  #lon=geosphere::dist2Line(xy_bear, xy_water)[,2],
  #lat=geosphere::dist2Line(xy_bear, xy_water)[,3])
}



mdist_df <- bind_rows(mdist)
bears_distwater <- cbind(used_avail_subset, mdist_df)
head(bears_distwater)
summary(bears_distwater)

#mdist2_df <- bind_rows(mdist2) # nope


###


# VISUALIZING MDIST3 - this doesn't work

# mdist3 using FNN from here: https://stackoverflow.com/questions/27782488/r-calculating-the-shortest-distance-between-two-point-layers
summary(mdist3) # there are 153 points, which is right
str(mdist3)
class(mdist3) # list
mdist3_df <- bind_rows(mdist3)
head(mdist3_df)
unique(mdist3_df$nn.index) # apparently these are the IDs of the nearest water pixels: 247, 274, 838, 864, 910
unique(used_avail_subset$ROWID_2) # the numbers above don't match these, so I think they're linked to the water pixels (I was worried I had the bear and water datasets backwards in the function)

water_19940404$ID # So we could try to separate those matching ID values and plot them?
str(water_19940404)

water_19940404_247 <- water_19940404[247,] # separate them
water_19940404_274 <- water_19940404[274,]
water_19940404_838 <- water_19940404[838,]
water_19940404_864 <- water_19940404[864,]
water_19940404_910 <- water_19940404[910,]

plot(used_avail_subset_spatial, pch=20)
plot(water_19940404, col="red", pch=20, add=TRUE)
plot(water_19940404_247, col="black", pch=8, add=TRUE) # nothing plots
plot(water_19940404_274, col="green", pch=8, add=TRUE) # see top left corner of red dots!
plot(water_19940404_838, col="purple", pch=8, add=TRUE) # see middle of red dots - this one makes no sense
plot(water_19940404_838, col="pink", pch=8, add=TRUE) # this plots over the last one
plot(water_19940404_910, col="blue", pch=8, add=TRUE) # this plots a little south - again, this makes no sense


plot(gNearestPoints(used_avail_subset_spatial, water_19940404), col="cyan", pch=17, add=TRUE) # this just plots 2 points, not all of them - the same 2 plot when you switch the order of the points
# the above just shows you which points from each subset are the closest 


###


# VISUALIZING MDIST4 

# mdist3 using FNN from here: https://stackoverflow.com/questions/27782488/r-calculating-the-shortest-distance-between-two-point-layers
summary(mdist4) # there are 153 points, which is right
mdist4_df <- bind_rows(mdist4)
head(mdist4_df)

# this is an easy way to get the closest points, but I can't identify them





###

# test plot (original - dist2Line)

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
points(bear19940404_spatial, col="pink")
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

points(difference$lon, difference$lat, col="pink") # IT DOESN'T EVEN PLOT THE WRONG ONES - I am very confused






# 4. Loop #2 with problem subset (using: gDistance again but adding min)  ----------


used_avail=subset(used_avail, select=-c(DIST_WATER))
used_avail_subset <- used_avail %>% filter(date_char==19940404)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))

used_avail_subset_spatial <- used_avail_subset
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

###

mdist5 <- list()
mdist6 <- list()

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  mdist5[[i]] <- as.data.frame(gDistance(xy_water, xy_bear, byid=TRUE))
  mdist6[[i]] <- unlist(mdist5[which(mdist5==min(mdist5[[i]]))]) # this line gives me this error: "'list' object cannot be coerced to type 'double'"
}





# 5. Loop #3 with problem subset (using: st_nearest_point)  ----------


used_avail=subset(used_avail, select=-c(DIST_WATER))
used_avail_subset <- used_avail %>% filter(date_char==19940404)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))

used_avail_subset_spatial <- used_avail_subset
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

###

mdist7 <- list()

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  mdist7[[i]] <- as.data.frame(st_nearest_points(st_as_sfc(xy_water), st_as_sfc(xy_bear)))
}


class(mdist7)
mdist7_df <- bind_rows(mdist7) 
head(mdist7_df)
str(mdist7_df)
class(mdist7_df)


mdist7_unlist <- unlist(mdist7)
mdist7_unlist2 <- matrix(mdist7_unlist, ncol=4, byrow=TRUE)
mdist7_unlist3 <- head(as.data.frame(mdist7_unlist2))
head(mdist7_unlist3)

# I don't really know what ended up happening there



###



# 6. Loop #4 with problem subset (using: nn2)  ----------


used_avail=subset(used_avail, select=-c(DIST_WATER))
used_avail_subset <- used_avail %>% filter(date_char==19940404)
unique(used_avail_subset$date_char)
used_avail_subset$date_char <- as.numeric(used_avail_subset$date_char)
used_avail_subset$LAT <- as.numeric(as.character(used_avail_subset$LAT))
used_avail_subset$LONG <- as.numeric(as.character(used_avail_subset$LONG))

used_avail_subset_spatial <- used_avail_subset
coordinates(used_avail_subset_spatial) <- ~LONG + LAT
proj4string(used_avail_subset_spatial) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset)
head(used_avail_subset_spatial)
str(used_avail_subset_spatial)

###

# https://www.gis-blog.com/nearest-neighbour-search-for-spatial-points-in-r/

mdist8 <- list()

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  mdist8[[i]] <- as.data.frame(nn2(as.data.frame(xy_bear), as.data.frame(xy_water), k=1))
}


class(mdist8)
mdist8_df <- bind_rows(mdist8) 
head(mdist8_df)
unique(mdist8_df$nn.idx) # there's only one ID #, so this seems wrong



# try the same loop with xy_bear and xy_water switched in mdist8 line

mdist9 <- list()

for(i in 1:nrow(used_avail_subset)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset[i,"date_char"])]
  water_lat <- coordinates(matching_raster)[,3]
  water_long <- coordinates(matching_raster)[,2]
  xy_water <- SpatialPointsDataFrame(
    matrix(c(water_long, water_lat), ncol=2), data.frame(ID=seq(1:length(water_long))),
    # proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # gDistance needs projected coordinates
  bear_lat <- coordinates(used_avail_subset_spatial)[i,2]
  bear_long <- coordinates(used_avail_subset_spatial)[i,1]
  xy_bear <- SpatialPointsDataFrame(
    matrix(c(bear_long, bear_lat), ncol=2), data.frame(ID=seq(1:length(bear_long))),
    #proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")) # use for dist2Line
    proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")) # use for gDistance
  mdist9[[i]] <- as.data.frame(nn2(as.data.frame(xy_water), as.data.frame(xy_bear), k=1))
}


class(mdist9)
mdist9_df <- bind_rows(mdist9) 
head(mdist9_df)
unique(mdist9_df$nn.idx) # now there's only 2 ... not sure what's happening




###


# 5. Loop with a subset that works (using dist2Line) ----------

used_avail_subset2 <- used_avail %>% filter(date_char==20010311)
unique(used_avail_subset2$date_char)
used_avail_subset2$date_char <- as.numeric(used_avail_subset2$date_char)
used_avail_subset2$LAT <- as.numeric(as.character(used_avail_subset2$LAT))
used_avail_subset2$LONG <- as.numeric(as.character(used_avail_subset2$LONG))
str(used_avail_subset2)
used_avail_subset_spatial2 <- used_avail_subset2
#coordinates(used_avail_subset_spatial) <- c("LONG", "LAT")
str(used_avail_subset_spatial2)
coordinates(used_avail_subset_spatial2) <- ~LONG + LAT
proj4string(used_avail_subset_spatial2) <- CRS("+proj=longlat +datum=WGS84")
head(used_avail_subset2)
head(used_avail_subset_spatial2)
str(used_avail_subset_spatial2)

mdist2 <- list()

for(i in 1:nrow(used_avail_subset2)){
  matching_raster <- water_coordinates[which(names(water_coordinates) == used_avail_subset2[i,"date_char"])]
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
  mdist2[[i]] <- data.frame(DIST_WATER=geosphere::dist2Line(xy_bear, xy_water)[,1],
                           lon=geosphere::dist2Line(xy_bear, xy_water)[,2],
                           lat=geosphere::dist2Line(xy_bear, xy_water)[,3])
}

mdist2_df <- bind_rows(mdist2)
bears_distwater2 <- cbind(used_avail_subset2, mdist2_df)
head(bears_distwater2)
summary(bears_distwater2)

# test plot 

proj4string(raster_list$'20010311')
proj4string(water_coordinates$'20010311')

raster_20010311 <- raster_list$'20010311'
water_20010311 <- water_coordinates$'20010311'
raster_20010311_latlon <- projectRaster(raster_20010311, crs="+proj=longlat +datum=WGS84 +no_defs")

bears_distwater2_spatial <- bears_distwater2
coordinates(bears_distwater2_spatial) <- ~lon + lat
proj4string(bears_distwater2_spatial) <- CRS("+proj=longlat +datum=WGS84")
plot(bears_distwater2_spatial)

plot(raster_20010311_latlon, col=(viridis(5)), zlim=c(0, 1))
plot(water_20010311, pch=20, col=rgb(1, 0, 0, 0.2), add=TRUE)
points(used_avail_subset_spatial2, col="pink")
plot(bears_distwater2_spatial, col="black", pch=20, add=TRUE)

# this one looked like it worked!






