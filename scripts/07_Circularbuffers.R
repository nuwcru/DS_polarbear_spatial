

# 1. Load libraries -------------

library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(spatialEco) # for buffers
library(mapview) # to plot buffers
library(sp)
library(sf)
library(rgdal)
library(arsenal) # to compare dataframes
#library(nuwcru)

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



# 2. Load data ------------

bears <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")
#ice_bears <- bears %>% filter(bears$ICE_LAND=="ice") # 1388/1850 points are on ice - use this below instead
used_oniceonly <- read.csv("data/Oct2020work/FINAL DATASET/used_oniceonly.csv") #1388 points

# use used_oniceonly instead of ice_bears
# this new df was made in QGIS with the updated ocean mask Erik made
# this way, the on-ice used points and their circular buffers are all created using the same shapefile

head(used_oniceonly)
      # drop unnecessary columns, like ICE_LAND which is now wrong
used_oniceonly <- subset(used_oniceonly, select=-c(field_1, X.1, X, ICE_LAND, featurecla))



# 3. Create circular buffers  ------

# info on projections: https://www.nceas.ucsb.edu/sites/default/files/2020-04/OverviewCoordinateReferenceSystems.pdf
# NSIDC polar stereographic codes: https://nsidc.org/data/polar-stereo/ps_grids.html


  
      # create buffers using spatialEco package
bears_spatial_latlong <- used_oniceonly
coordinates(bears_spatial_latlong) <- c("LONG", "LAT")
proj4string(bears_spatial_latlong) <- CRS("+proj=longlat +datum=WGS84")
proj4string(bears_spatial_latlong)

buffers <- geo.buffer(x=bears_spatial_latlong, r=93900, sf=TRUE) # 93.9km radius = 93900m
str(buffers) # dataframe with new column added that is a spatial polygon - so each point has a polygon attached

plot(buffers$geometry) # they're not all circles, but I checked them in QGIS and they worked
plot(buffers[500,]) # all #s plot the same shape - so they should all be circular
#st_write(buffers, "data/Oct2020work/buffers.shp") # imported this into QGIS, and they look good


# plotting using: https://stackoverflow.com/questions/56862241/sf-object-created-as-list-in-r-data-table

sf <- sf::st_as_sf( buffers )

ggplot() +
  geom_sf(data=sf) # this looks good (uneven circles but again, in QGIS they seem fine)

ggplot() +
  geom_sf(data=sf) + 
  geom_point(data=ice_bears, aes(x=LONG, y=LAT)) # this looks good


mapview(buffers[1,]) + bears_spatial_latlong[1,] 
mapview(buffers) + bears_spatial_latlong # looks good!




dim(buffers)

# 4. Create random points ------


# clip buffers to ocean mask first
# by doing this, we can ensure all randomly generated points are on the ocean

      # import ocean mask
ocean <- st_read("gis/oceanmask_fordistwater_0.3deg.shp")

      # view unclipped buffers
plot(st_geometry(ocean), col="lightblue")
plot(st_geometry(buffers), col = rgb(1,1,1,0.3), add = TRUE)

st_crs(buffers)
st_crs(ocean)  # use this instead of proj4string, apparently

      # create clipped buffers
clipped_buffers <- st_intersection(ocean, buffers)
clipped_buffers2 <- clipped_buffers
clipped_buffers2$geometry <- st_cast(clipped_buffers2$geometry, "MULTIPOLYGON") # need geometry column as polygon
str(clipped_buffers2) # this works
class(clipped_buffers2)

-----

# now generate random points within clipped buffers
set.seed(500)
samples_per_polygon <- rep(50, nrow(clipped_buffers2)) # this works
sample <- st_sample(clipped_buffers2$geometry, samples_per_polygon)
      # above from here: https://stackoverflow.com/questions/63653396/specific-number-of-points-within-each-polygon-of-a-shape-in-r

plot(sample)
class(sample)

      # test plot
plot(st_geometry(ocean), col="lightblue")
plot(st_geometry(clipped_buffers2$geometry), col=rgb(1,1,1,0.3), add=TRUE)
plot(sample, add=TRUE) # This works!

#st_write(clipped_buffers, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/RSFs_Dec2020/clipped_buffers.shp") 

-----

# now finalize dataframe
  
      # put random points back into the original data frame
ran_points <- as.data.frame(sample)
ran_points2 <- ran_points %>% mutate(ran_long = unlist(map(ran_points$geometry,1)), ran_lat = unlist(map(ran_points$geometry,2)))
head(ran_points2)

      # because we have 73,150 points, we have to blow the original dataframe up to this size
dat_all <- clipped_buffers2 %>% slice(rep(1:n(), each=50))
dat_all <- cbind(dat_all, ran_points2) 

      # plot the first 5000 points / polygons
dat_all %>% filter(row_number() %in% 1:5000) %>%
  ggplot() +
  geom_sf(aes()) +
  geom_point(aes(x = ran_long, y = ran_lat)) +
  theme_nuwcru()



# THIS WORKED!



-----

# Ignore below

# plotting using: https://stackoverflow.com/questions/56862241/sf-object-created-as-list-in-r-data-table

sf_clip <- sf::st_as_sf( clipped_buffers )
ggplot() +
  geom_sf(data=sf_clip) # this looks good 
ggplot() +
  geom_sf(data=sf_clip) + 
  geom_point(data=ice_bears, aes(x=LONG, y=LAT)) # this looks good
mapview(clipped_buffers[1,]) + bears_spatial_latlong[1,] 



# 5. Make final dataframe of used + available points for RSFs --------

# format available points dataframe
str(dat_all)
available <- as.data.frame(dat_all)
head(available)
available=subset(available, select=-c(featurecla, scalerank, min_zoom, ZONE, EASTING, NORTHING, geometry, geometry.1, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S)) # drop unnessary columns including the polygon column
head(available)
available$USED_AVAIL <- 'available' # add column for "available"
head(available)
names(available)[9] <- "LONG" # rename x and y columns
names(available)[10] <- "LAT"
head(available)

# add "USED_AVAIL" column to used_oniceonly
head(used_oniceonly)
used_oniceonly$USED_AVAIL <- 'used'
head(used_oniceonly)

# combine together
str(used_oniceonly) # 1463 rows
str(available) # 73,150 rows
      # final dataset should then have 74,613 rows
used_avail <- merge(available, used_oniceonly, all=TRUE) # this worked

# Make a better ROWID column
head(used_avail)
ROWID_2 <- rownames(used_avail)
used_avail <- cbind(ROWID_2=ROWID_2, used_avail)
head(used_avail)

# export
write.csv(used_avail, "data/Oct2020work/FINAL DATASET/used_avail_points_Mar2021.csv")



# SKIP - 6. Dealing with available points on land -------

# import available points on land
# made this dataframe in QGIS

avail_landonly <- read.csv("data/Oct2020work/FINAL DATASET/avail_points_landonly.csv")
unique(avail_landonly$USED_AVAIL) # these are only available points (i.e. no used points)

head(avail_landonly)
str(avail_landonly)
avail_landonly = subset(avail_landonly, select=-c(field_1, X.3, X.2, X.1, ICE_LAND)) # remove unnecessary columns

head(used_avail)
used_avail = subset(used_avail, select=-c(X.3, X.2, X.1, X)) # remove unnecessary columns
used_avail$ROWID_2 <- as.numeric(used_avail$ROWID_2) # fix this column

used_avail_iceonly <- anti_join(used_avail, avail_landonly, by="ROWID_2")
# this looks like it worked: 70788 original points - 9308 on-land available points = 61480 ice only points


# look at how many random points per used point
head(used_avail_iceonly)

used_avail_iceonly_stats <- used_avail_iceonly %>% group_by(DATE, ID) %>% summarize(count=n())

used_avail_iceonly_stats2 <- used_avail_iceonly_stats %>% filter(count<"51")
unique(used_avail_iceonly_stats2$ID) # 26/28 bears
d <- used_avail_iceonly_stats2

d$year <- year(d$DATE)

d %>% group_by(ID, year) %>% summarize(mean = mean(count)) %>% filter(ID == "X03956")

d %>% 
  ggplot() +
  geom_point(aes(x = year, y = count)) +
  geom_line(aes(x = year, y = count)) +
  facet_wrap(. ~ ID)


# SKIP - 6. Extra code --------

# Note: these were other options we tried for creating buffers/points
library(amt) # for random points
library(lwgeom) # for random points
library(rgeos) # for buffers with lat/long
library(sp) # to make spatial points df
library(sf) # for buffers

# BUFFERS

# create buffers with rgeos package: https://stackoverflow.com/questions/25411251/buffer-geospatial-points-in-r-with-gbuffer

# make bear locations into spatial object
bears_spatial <- bears
coordinates(bears_spatial) <- c("LAT", "LONG")
proj4string(bears_spatial) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
# note: this projection is the same as sea ice data and gBuffer requires it to be projected
plot(bears_spatial) # visualize
proj4string(bears_spatial)

buffers_rgeos <- gBuffer(bears_spatial, width=93900, byid=TRUE) # unsure if in m
head(buffers_rgeos)
plot(buffers_rgeos) # shows just one circle, but the line is thick indicating there are multiple overlapping
plot(buffers_rgeos[1,]) # shows just one circle with thinner line
proj4string(buffers_rgeos) # correct but with warning

# visualize
# this works, but I want them altogether and looking more like a map
# when you remove "[1,]" they're all stacked
plot(buffers_rgeos[1,], col=c("grey"))
plot(bears_spatial[1,], add=TRUE)

# there are 1850 buffers, which is correct (1 for each observation)
# but when I fortify it below, suddenly there are way more (fortify tip from: https://stackoverflow.com/questions/20320377/how-to-change-a-spatialpointsdataframe-into-spatialpolygonsdataframe-in-r-to-use)
buffers_fortify <- fortify(buffers_rgeos, region="X.2") # this gives me 38850 buffers
str(buffers_fortify)

ggplot() + geom_polygon(data=buffers_fortify, aes(x=long, y=lat, group=group)) # this is wrong



# OR


# create buffers using sf package: https://stackoverflow.com/questions/55797908/difficulty-with-gbuffer-in-r-resulting-buffer-is-incorrect-size
bears_spatial_sf <- bears
bears_spatial_sf <- sf::st_as_sf(bears, coords=c("LONG", "LAT"))
str(bears_spatial_sf) # new column is an "sfc_POINT"
bears_spatial_sf <- bears_spatial_sf %>%
  st_sfc(crs=4269) %>%
  st_transform(crs=3411) # not sure if this part worked - tried projecting in polar sterographic


buffers_sf <- st_buffer(bears_spatial_sf, dist=93900)
str(buffers_sf) # last column is an "sfc_POLYGON"
head(buffers_sf$geometry) # seems like they are in different locations?

mapview(buffers_sf[1,]) + bears_spatial_sf[1,] # this looks like it works!

mapview(buffers_sf) + bears_spatial_sf # here, though, they're all on top of each other





-----
  
# POINTS

  # below is from here: https://cran.r-project.org/web/packages/amt/vignettes/p3_rsf.html
  # I need to make a loop to do line 73 for all polygons within "buffers"
  
  available <- random_points(buffers, n=50, type="random") # this only makes one list of 50 - I need 50 within each buffer
available2 <- random_points(buffers$geometry, n=50, type="random") # this gives an error
plot(available) # this looks cool!

# attempting it using this source: https://terpconnect.umd.edu/~egurarie/teaching/SpatialModelling_AKTWS2018/6_RSF_SSF.html

available3 <- spsample(buffers, 50, "random") # error
available4 <- spsample(buffers$geometry, 50, "random") # error


# attempting loop using random_points
for(i in 1:length(bears_spatial)){
  avail <- random_points(buffers, n=50, type="random") # this gives an error
}




