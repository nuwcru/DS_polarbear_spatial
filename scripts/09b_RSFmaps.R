

# 1. Load libraries -------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(adehabitatHR) # for RSFs
library(TMB)
library(glmmTMB) # RSFs according to Muff et al. (2019)
library(jtools) # for effect_plot() (section 3)

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

# 2. Import data and format -------

# bear data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

# create squared conc column then scale
used_avail_RSF_freezeup_FINAL$CONC_2 = '^'(used_avail_RSF_freezeup_FINAL$CONC,2)
used_avail_RSF_freezeup_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_freezeup_FINAL$CONC_2, scale=TRUE, center=TRUE)

used_avail_RSF_breakup_FINAL$CONC_2 = '^'(used_avail_RSF_breakup_FINAL$CONC,2)
used_avail_RSF_breakup_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_breakup_FINAL$CONC_2, scale=TRUE, center=TRUE)

used_avail_RSF_winter_FINAL$CONC_2 = '^'(used_avail_RSF_winter_FINAL$CONC,2)
used_avail_RSF_winter_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_winter_FINAL$CONC_2, scale=TRUE, center=TRUE)

# create squared water column then scale
used_avail_RSF_freezeup_FINAL$DIST_WATER_2 = '^'(used_avail_RSF_freezeup_FINAL$DIST_WATER,2)
used_avail_RSF_freezeup_FINAL$DIST_WATER_2_SCALED <- scale(used_avail_RSF_freezeup_FINAL$DIST_WATER_2, scale=TRUE, center=TRUE)

used_avail_RSF_breakup_FINAL$DIST_WATER_2 = '^'(used_avail_RSF_breakup_FINAL$DIST_WATER,2)
used_avail_RSF_breakup_FINAL$DIST_WATER_2_SCALED <- scale(used_avail_RSF_breakup_FINAL$DIST_WATER_2, scale=TRUE, center=TRUE)

used_avail_RSF_winter_FINAL$DIST_WATER_2 = '^'(used_avail_RSF_winter_FINAL$DIST_WATER,2)
used_avail_RSF_winter_FINAL$DIST_WATER_2_SCALED <- scale(used_avail_RSF_winter_FINAL$DIST_WATER_2, scale=TRUE, center=TRUE)


# 3. Run top RSF models -------

# freeze-up, break-up, and winter for bears
# freeze-up and break-up only for seal

###

# bears
      # break-up: Model 2b (CONC + CONC_2)
bears_breakup_m2b <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m2b
summary(bears_breakup_m2b)

      # freeze-up: Model 5a (BATH + CONC + CONC_2)
bears_freezeup_m5b <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5b
summary(bears_freezeup_m5b)

      # winter: Model 5a (BATH + CONC + CONC_2)
bears_winter_m5b <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m5b
summary(bears_winter_m5b)

###



# 4. Create predictive plots (bears) -------
# 4a.      Winter ---------

# winter: Model 5a (BATH + CONC + CONC_2)


# BATH
median_CONC = median(used_avail_RSF_winter_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_winter_model5 = coef(bears_winter_m5a)
coefs_winter_model5 = coefs_winter_model5$cond$ID
rest_of_prediction = median_CONC * coefs_winter_model5$CONC_SCALED[1] + median_CONC_2 * coefs_winter_model5$CONC_2_SCALED[1]+ coefs_winter_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$BATH)) / sd(used_avail_RSF_winter_FINAL$BATH) * 
                       coefs_winter_model5$BATH_SCALED[1] + rest_of_prediction))), 
      xlim = range(used_avail_RSF_winter_FINAL$BATH), ylim = c(0,1), 
      xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


# CONC
median_BATH = median(used_avail_RSF_winter_FINAL$BATH_SCALED)
coefs_winter_model5 = coef(bears_winter_m5a)
coefs_winter_model5 = coefs_winter_model5$cond$ID
rest_of_prediction = median_BATH * coefs_winter_model5$BATH_SCALED[1] + coefs_winter_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_winter_FINAL$CONC)) / sd(used_avail_RSF_winter_FINAL$CONC) * 
                       coefs_winter_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_winter_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_winter_FINAL$CONC_2) * coefs_winter_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction))), xlim = range(used_avail_RSF_winter_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")




# 4b.      Freeze-up --------

# top model for freeze-up: Model 5a (BATH + CONC + CONC_2)

###

# BATH
median_CONC = median(used_avail_RSF_freezeup_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_freezeup_model5a = coef(bears_freezeup_m5a)
coefs_freezeup_model5a = coefs_freezeup_model5a$cond$ID
rest_of_prediction = median_CONC * coefs_freezeup_model5a$CONC_SCALED[1] + median_CONC_2 * coefs_freezeup_model5a$CONC_2_SCALED[1] + coefs_freezeup_model5a$`(Intercept)`[1]
curve(1 / (0.02361453*(1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction)))), xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")

summary(used_avail_RSF_freezeup_FINAL$BATH)

# next line just gets value for after "(1/__)
#1 / (1 + exp(-((0 - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                             #coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction)))
     
# CONC
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model5 = coef(bears_freezeup_m5a)
coefs_freezeup_model5 = coefs_freezeup_model5$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model5$BATH_SCALED[1] + coefs_freezeup_model5$`(Intercept)`[1]
curve(1 / (0.1535558*(1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * 
                       coefs_freezeup_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_freezeup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_freezeup_FINAL$CONC_2) * coefs_freezeup_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction)))), xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")

# next line just gets value for after "(1/__)
#1 / (1 + exp(-((0 - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * 
                 #coefs_freezeup_model5$CONC_SCALED[1] + (0^2 - mean(used_avail_RSF_freezeup_FINAL$CONC_2)) / 
                 #sd(used_avail_RSF_freezeup_FINAL$CONC_2) * coefs_freezeup_model5$CONC_2_SCALED[1] + 
                 #rest_of_prediction)))
###

# 4c.      Break-up --------

# break-up: Model 2a (CONC + CONC_2)

###

# CONC
#median_BATH = median(used_avail_RSF_breakup_FINAL$BATH_SCALED)
coefs_breakup_model2a = coef(bears_breakup_m2a)
coefs_breakup_model2a = coefs_breakup_model2a$cond$ID
#rest_of_prediction = median_BATH * used_avail_RSF_breakup_FINAL$BATH_SCALED[1] + used_avail_RSF_breakup_FINAL$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_breakup_FINAL$CONC)) / sd(used_avail_RSF_breakup_FINAL$CONC) * 
                       coefs_breakup_model2a$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_breakup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_breakup_FINAL$CONC_2) * coefs_breakup_model2a$CONC_2_SCALED[1]))), 
      xlim = range(used_avail_RSF_breakup_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")


###

# 5. Create predictive maps (bears) -------

# 5a.      Freeze-up Prep data --------------

# run sections 2-3 first
# freezeup top model (5b) has: BATH + ICE + ICE_2

# load polygon for cropping
polygon <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Sea ice boundary/Nov2020work/seaiceboundary_finalbeardataset.shp")
proj4string(polygon) # lat/long
polygon_proj <- spTransform(polygon, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(polygon_proj) # lat/long
plot(polygon_proj) 

# Load sea ice and land mask
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20.rds")
#tail(names(raster_list))
#plot(raster_list$`20200102`)
#raster_values <- values(raster_list$`19781026`)
#head(raster_values)
land_mask <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/Ocean_Clipped_RSFboundary.shp")
land_mask_proj <- spTransform(land_mask, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(land_mask_proj) 
plot(land_mask_proj) 
plot(polygon_proj, add=TRUE)

      # create one day of sea ice, crop, and mask land: Dec 2 2005
Dec02_2005 <- raster_list$`20051202`
#proj4string(Dec02_2005)
#plot(Dec02_2005)
#plot(polygon_proj, add=TRUE) # looks right
Dec02_2005_crop <- crop(Dec02_2005, polygon_proj, snap='out') # crop
Dec02_2005_crop <- mask(Dec02_2005_crop, polygon_proj, snap='out') # mask outside pixels
#plot(Dec02_2005_crop)
#plot(polygon_proj, add=TRUE) # looks right
Dec02_2005_crop2 <- crop(Dec02_2005_crop, land_mask_proj, snap='out')
Dec02_2005_crop2 <- mask(Dec02_2005_crop2, land_mask_proj, snap='out')
Dec02_2005_crop_resam <- resample(Dec02_2005_crop2, bathymetry_crop) # make it finer scale
#plot(Dec02_2005_crop_resam)
#plot(polygon_proj, add=TRUE)
#plot(land_mask, add=TRUE) # looks good!

### 

# Load bathymetry
bathymetry <- raster("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Bathymetry data/GEBCO/gebco_2020_n85.82123637199403_s35.03197789192201_w-96.98521256446841_e-15.826191902160673.tif")
#plot(bathymetry)
#proj4string(bathymetry)
bathymetry_proj <- projectRaster(bathymetry, crs=crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
#proj4string(bathymetry_proj)
#plot(bathymetry_proj)
bathymetry_crop <- crop(bathymetry_proj, polygon_proj, snap='out') # crop
bathymetry_crop <- mask(bathymetry_crop, polygon_proj, snap='out') # mask outside pixels
plot(bathymetry_crop)
plot(polygon_proj, add=TRUE) # looks right


#


# 5b.      Freeze-up map -----

# combine 1 sea ice raster and bathymetry and get values
# using raster_list_uncropped version
freeze_covariates_brick <- brick(Dec02_2005_crop_resam, bathymetry_crop)
names(freeze_covariates_brick) <- c("CONC", "BATH")
head(freeze_covariates_brick)

bath_unscaled <- freeze_covariates_brick$BATH
bath_unscaled[bath_unscaled>0]=NA

bath_scaled_raster <- (bath_unscaled-mean(used_avail_RSF_freezeup_FINAL$BATH))/sd(used_avail_RSF_freezeup_FINAL$BATH)
conc_scaled_raster <- (freeze_covariates_brick$CONC-mean(used_avail_RSF_freezeup_FINAL$CONC))/sd(used_avail_RSF_freezeup_FINAL$CONC)
conc2_scaled_raster <- (freeze_covariates_brick$CONC^2-mean(used_avail_RSF_freezeup_FINAL$CONC_2))/sd(used_avail_RSF_freezeup_FINAL$CONC_2)

#plot(bath_scaled_raster) # these take awhile to plot
#plot(conc_scaled_raster)
#plot(conc2_scaled_raster)

# add rasters together with coefficients from model; linear model
model_raster <- coef(bears_freezeup_m5b)
freeze_map <- model_raster$cond$ID$BATH_SCALED[1]*bath_scaled_raster+model_raster$cond$ID$CONC_SCALED[1]*conc_scaled_raster+model_raster$cond$ID$CONC_2_SCALED[1]*conc2_scaled_raster+model_raster$cond$ID$`(Intercept)`[1]
plot(freeze_map)

# transform with link function and plot
freeze_map <- 1/(1+exp(-freeze_map))
plot(freeze_map)
writeRaster(freeze_map, filename="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/RSF_maps/bears_freeze_RSF_Apr2022.tif", overwrite=TRUE)

# get values so I can summarize and put into bins in QGIS
summary(freeze_map)
freeze_map_df <- as.data.frame(values(freeze_map$layer))

###

# 5c.      Break-up prep data ------


# run sections 2-3 first

# breakup top model (2b) has: ICE + ICE_2

# Load bathymetry
bathymetry <- raster("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Bathymetry data/GEBCO/gebco_2020_n85.82123637199403_s35.03197789192201_w-96.98521256446841_e-15.826191902160673.tif")
bathymetry_proj <- projectRaster(bathymetry, crs=crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
bathymetry_crop <- crop(bathymetry_proj, polygon_proj, snap='out') # crop
bathymetry_crop <- mask(bathymetry_crop, polygon_proj, snap='out') # mask outside pixels

# load polygon for cropping
polygon <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Sea ice boundary/Nov2020work/seaiceboundary_finalbeardataset.shp")
proj4string(polygon) # lat/long
polygon_proj <- spTransform(polygon, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(polygon_proj) # lat/long
plot(polygon_proj) 

# Load sea ice and land mask
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20.rds")
land_mask <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/Ocean_Clipped_RSFboundary.shp")
land_mask_proj <- spTransform(land_mask, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(land_mask_proj) 
plot(land_mask_proj) 
plot(polygon_proj, add=TRUE)

# create one day of sea ice and crop: June 16
Jun16_1990 <- raster_list$`19900616`
proj4string(Jun16_1990)
Jun16_1990_crop <- crop(Jun16_1990, polygon_proj, snap='out') # crop
Jun16_1990_crop <- mask(Jun16_1990_crop, polygon_proj, snap='out') # mask outside pixels
Jun16_1990_crop_resam <- resample(Jun16_1990_crop, bathymetry_crop) # make it finer scale
Jun16_1990_crop_resam_noland <- crop(Jun16_1990_crop_resam, land_mask_proj, snap='out') 
Jun16_1990_crop_resam_noland <- mask(Jun16_1990_crop_resam, land_mask_proj, snap='out')
plot(Jun16_1990_crop_resam_noland)
plot(polygon_proj, add=TRUE) # it worked!

#

# 5d.      Breakup map ------

conc_scaled_raster <- (Jun16_1990_crop_resam_noland-mean(used_avail_RSF_breakup_FINAL$CONC))/sd(used_avail_RSF_breakup_FINAL$CONC)
conc2_scaled_raster <- (Jun16_1990_crop_resam_noland^2-mean(used_avail_RSF_breakup_FINAL$CONC_2))/sd(used_avail_RSF_breakup_FINAL$CONC_2)

plot(conc_scaled_raster)
plot(conc2_scaled_raster)

# add rasters together with coefficients from model; linear model
model_raster <- coef(bears_breakup_m2b)
break_map <- model_raster$cond$ID$CONC_SCALED[1]*conc_scaled_raster+model_raster$cond$ID$CONC_2_SCALED[1]*conc2_scaled_raster+model_raster$cond$ID$`(Intercept)`[1]
plot(break_map)

# transform with link function and plot
break_map <- 1/(1+exp(-break_map))
plot(break_map)
writeRaster(break_map, filename="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/RSF_maps/bears_break_RSF_Apr2022.tif", overwrite=TRUE)

# get values so I can summarize and put into bins in QGIS
summary(break_map)
break_map_df <- as.data.frame(values(break_map$layer))

###




# 5e.      Winter Prep data --------------

# run sections 2-3 first

# winter top model (5b) has: BATH + ICE + ICE_2

# Load bathymetry
bathymetry <- raster("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Bathymetry data/GEBCO/gebco_2020_n85.82123637199403_s35.03197789192201_w-96.98521256446841_e-15.826191902160673.tif")
bathymetry_proj <- projectRaster(bathymetry, crs=crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
bathymetry_crop <- crop(bathymetry_proj, polygon_proj, snap='out') # crop
bathymetry_crop <- mask(bathymetry_crop, polygon_proj, snap='out') # mask outside pixels

# load polygon for cropping
polygon <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Sea ice boundary/Nov2020work/seaiceboundary_finalbeardataset.shp")
proj4string(polygon) # lat/long
polygon_proj <- spTransform(polygon, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(polygon_proj) # lat/long
plot(polygon_proj) 

# Load sea ice and land mask
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20.rds")
land_mask <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/Ocean_Clipped_RSFboundary.shp")
land_mask_proj <- spTransform(land_mask, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(land_mask_proj) 
plot(land_mask_proj) 
plot(polygon_proj, add=TRUE)

# create one day of sea ice and crop: Mar 1 2005
Mar10_2010 <- raster_list$`20100310`
#proj4string(Mar10_2010)
#plot(Mar10_2010)
#plot(polygon_proj, add=TRUE) # looks right
Mar10_2010_crop <- crop(Mar10_2010, polygon_proj, snap='out') # crop
Mar10_2010_crop <- mask(Mar10_2010_crop, polygon_proj, snap='out') # mask outside pixels
#plot(Mar10_2010_crop) 
#plot(polygon_proj, add=TRUE) # looks right
Mar10_2010_crop_resam <- resample(Mar10_2010_crop, bathymetry_crop) # make it finer scale
#plot(Mar10_2010_crop_resam)
#plot(polygon_proj, add=TRUE)
#plot(land_mask_proj, add=TRUE) # looks good!
Mar10_2010_crop_resam_noland <- crop(Mar10_2010_crop_resam, land_mask_proj, snap='out') 
Mar10_2010_crop_resam_noland <- mask(Mar10_2010_crop_resam, land_mask_proj, snap='out')
plot(Mar10_2010_crop_resam_noland)
plot(polygon_proj, add=TRUE) # it worked!

#


# 5f.      Winter map -----

# combine 1 sea ice raster and bathymetry and get values
# using raster_list_uncropped version
winter_covariates_brick <- brick(Mar10_2010_crop_resam_noland, bathymetry_crop)
names(winter_covariates_brick) <- c("CONC", "BATH")
head(winter_covariates_brick)

bath_unscaled <- winter_covariates_brick$BATH
bath_unscaled[bath_unscaled>0]=NA

bath_scaled_raster <- (bath_unscaled-mean(used_avail_RSF_winter_FINAL$BATH))/sd(used_avail_RSF_winter_FINAL$BATH)
conc_scaled_raster <- (freeze_covariates_brick$CONC-mean(used_avail_RSF_winter_FINAL$CONC))/sd(used_avail_RSF_winter_FINAL$CONC)
conc2_scaled_raster <- (freeze_covariates_brick$CONC^2-mean(used_avail_RSF_winter_FINAL$CONC_2))/sd(used_avail_RSF_winter_FINAL$CONC_2)

#plot(bath_scaled_raster) # these take awhile to plot
#plot(conc_scaled_raster)
#plot(conc2_scaled_raster)

# add rasters together with coefficients from model; linear model
model_raster <- coef(bears_winter_m5b)
winter_map <- model_raster$cond$ID$BATH_SCALED[1]*bath_scaled_raster+model_raster$cond$ID$CONC_SCALED[1]*conc_scaled_raster+model_raster$cond$ID$CONC_2_SCALED[1]*conc2_scaled_raster+model_raster$cond$ID$`(Intercept)`[1]
plot(winter_map)

# transform with link function and plot
winter_map <- 1/(1+exp(-winter_map))
plot(winter_map)
writeRaster(winter_map, filename="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/RSF_maps/bears_winter_RSF_Apr2022.tif", overwrite=TRUE)

# get values so I can summarize and put into bins in QGIS
summary(winter_map)
winter_map_df <- as.data.frame(values(winter_map$layer))

###


# 6. K-fold cross validations ---------
# 6a.      Break-up --------

# run section 2 first

# sort of based on this, but manually: https://www.statology.org/k-fold-cross-validation-in-r/

# create dataframes for each bear
unique(used_avail_RSF_breakup_FINAL$ID)
breakup_11975 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="11975")
breakup_13284 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="13284")
breakup_13289 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="13289")
breakup_10700 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="10700")
breakup_10695 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="10695")
breakup_10703 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="10703")
breakup_10709 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="10709")
breakup_10707 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="10707")
breakup_13292 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="13292")
breakup_12080 <- used_avail_RSF_breakup_FINAL %>% filter(ID=="12080")

# create dataframes where I drop one bear at a time
break_sin11975 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="11975"),]
break_sin13284 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="13284"),]
break_sin13289 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="13289"),]
break_sin10700 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="10700"),]
break_sin10695 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="10695"),]
break_sin10703 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="10703"),]
break_sin10709 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="10709"),]
break_sin10707 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="10707"),]
break_sin13292 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="13292"),]
break_sin12080 <- used_avail_RSF_breakup_FINAL[!(used_avail_RSF_breakup_FINAL$ID=="12080"),]

# run top model (all points) 
bears_breakup_m2b <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)

# run top model (for each subset)
break_sin11975model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin11975)
break_sin13284model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin13284)
break_sin13289model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin13289)
break_sin10700model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin10700)
break_sin10695model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin10695)
break_sin10703model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin10703)
break_sin10709model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin10709)
break_sin10707model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin10707)
break_sin13292model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin13292)
break_sin12080model <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=break_sin12080)

# calculate mean squared error of each
mean(resid(break_sin11975model)^2) # 0.01919363
mean(resid(break_sin13284model)^2) # 0.01908194
mean(resid(break_sin13289model)^2) # 0.01906623
mean(resid(break_sin10700model)^2) # 0.01907415
mean(resid(break_sin10695model)^2) # 0.01907424
mean(resid(break_sin10703model)^2) # 0.01911169
mean(resid(break_sin10709model)^2) # 0.01905029
mean(resid(break_sin10707model)^2) # 0.01906896
mean(resid(break_sin13292model)^2) # 0.01909267
mean(resid(break_sin12080model)^2) # 0.01906077


# get mean of all mse = 0.1908746
mean(0.01919363+0.01908194+0.01906623+0.01907415+0.01907424+0.01911169+0.01905029+0.01906896+0.01909267+0.01906077)

# now that I'm here, I don't know what to do
# apparently a lower mse is better, but what is considered "low"?





###



# 6b.      Winter ------


### IGNORE - incorrect code from break-up

# get mean of predictions for each model
break_sin11975_pred <- predict(break_sin11975model, breakup_11975, type="response") 
summary(break_sin11975_pred) # mean = 0.02101
break_sin13284_pred <- predict(break_sin13284model, breakup_13284, type="response") 
summary(break_sin13284_pred) # mean = 0.01758
break_sin13289_pred <- predict(break_sin13289model, breakup_13289, type="response") 
summary(break_sin13289_pred) # mean = 0.01882
break_sin10700_pred <- predict(break_sin10700model, breakup_10700, type="response") 
summary(break_sin10700_pred) # mean = 0.01774
break_sin10695_pred <- predict(break_sin10695model, breakup_10695, type="response") 
summary(break_sin10695_pred) # mean = 0.02086
break_sin10703_pred <- predict(break_sin10703model, breakup_10703, type="response") 
summary(break_sin10703_pred) # mean = 0.02056
break_sin10709_pred <- predict(break_sin10709model, breakup_10709, type="response") 
summary(break_sin10709_pred) # mean = 0.02392
break_sin10707_pred <- predict(break_sin10707model, breakup_10707, type="response") 
summary(break_sin10707_pred) # mean = 0.01935
break_sin13292_pred <- predict(break_sin13292model, breakup_13292, type="response") 
summary(break_sin13292_pred) # mean = 0.01876
break_sin12080_pred <- predict(break_sin12080model, breakup_12080, type="response") 
summary(break_sin12080_pred) # mean = 0.01864

# get mean of all those means
mean(0.02101+0.01758+0.01882+0.01774+0.02086+0.02056+0.02392+0.01935+0.01876+0.01864)
# 0.19724

# get prediction of original model
bears_breakup_m2b_pred <- predict(bears_breakup_m2b, type="response")
summary(bears_breakup_m2b_pred) # mean = 0.01961


# 6c.      Freeze-up --------







