

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


# 3. Run top RSF models -------

# freeze-up, break-up, and winter for bears
# freeze-up and break-up only for seal

###

# bears
      # break-up: Model 2a (CONC + CONC_2)
bears_breakup_m2a <- glmmTMB(USED_AVAIL~CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_breakup_FINAL)
bears_breakup_m2a
summary(bears_breakup_m2a)

      # freeze-up: Model 5a (BATH + CONC + CONC_2)
bears_freezeup_m5a <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5a
summary(bears_freezeup_m5a)

      # winter: Model 5a (BATH + CONC + CONC_2)
bears_winter_m5a <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_winter_FINAL)
bears_winter_m5a
summary(bears_winter_m5a)

###

# seals
      # freeze-up: Model 12 (BATH + CONC + DIST_WATER)
#seals_freezeup_m12 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=freezeup_RSF)
#seals_freezeup_m12
#summary(seals_freezeup_m12)

      # break-up: Model 14 (BATH + CONC + DIST_LAND + DIST_WATER)
#seals_breakup_m14 <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+DIST_SCALED+DIST_WATER_SCALED+(1|ID), family=binomial(), data=breakup_RSF)
#seals_breakup_m14
#summary(seals_breakup_m14)

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
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction))), xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")

summary(used_avail_RSF_freezeup_FINAL$BATH)

# CONC
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model5 = coef(bears_freezeup_m5a)
coefs_freezeup_model5 = coefs_freezeup_model5$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model5$BATH_SCALED[1] + coefs_freezeup_model5$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * 
                       coefs_freezeup_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_freezeup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_freezeup_FINAL$CONC_2) * coefs_freezeup_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction))), xlim = range(used_avail_RSF_freezeup_FINAL$CONC), ylim = c(0,1), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")


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



# 5a.       Prep data --------------

# load polygon for cropping
polygon <- readOGR("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Shapefiles/RSF_map_boundary.shp")
proj4string(polygon) # lat/long
polygon_proj <- spTransform(polygon, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(polygon_proj) # lat/long
plot(polygon_proj) 

# Load sea ice
raster_list <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78.rds")
plot(raster_list$`20101010`)
raster_values <- values(raster_list$`19781026`)
head(raster_values)

# create one day of sea ice and crop
Oct10_2010 <- raster_list$`20101010`
proj4string(Oct10_2010)
plot(Oct10_2010)
plot(polygon_proj, add=TRUE) # looks right
Oct10_2010_crop <- crop(Oct10_2010, polygon_proj, snap='out') # crop
Oct10_2010_crop <- mask(Oct10_2010_crop, polygon_proj, snap='out') # mask outside pixels
plot(Oct10_2010_crop)
plot(polygon_proj, add=TRUE) # looks right
      # make it finer scale
Oct10_2010_crop_resam <- resample(Oct10_2010_crop,bathymetry_crop) #,method='bilinear')

# Load bathymetry
bathymetry <- raster("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/Bathymetry data/GEBCO/gebco_2020_n85.82123637199403_s35.03197789192201_w-96.98521256446841_e-15.826191902160673.tif")
plot(bathymetry)
proj4string(bathymetry)
bathymetry_proj <- projectRaster(bathymetry, crs=crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(bathymetry_proj)
plot(bathymetry_proj)
bathymetry_crop <- crop(bathymetry_proj, polygon_proj, snap='out') # crop
bathymetry_crop <- mask(bathymetry_crop, polygon_proj, snap='out') # mask outside pixels
plot(bathymetry_crop)
plot(polygon_proj, add=TRUE) # looks right

# 5b.      Freeze-up map -----

# freeze-up: Model 5a (BATH + CONC + CONC_2)

# combine 1 sea ice raster and bathymetry and get values
freeze_covariates_brick <- brick(Oct10_2010_crop_resam, bathymetry_crop)
freeze_covariates_values <- values(freeze_covariates_brick)
head(freeze_covariates_values)
freeze_covariates_values <- names(freeze_covariates_values) <- c("CONC", "BATH")
summary(freeze_covariates_values)
freeze_covariates_values_df <- data.frame(freeze_covariates_values)

# predict raster
system.time(bears_freezeup_prediction <- predict(bears_freezeup_m5a, newdata=freeze_covariates_values, allow.new.levels=TRUE))



#


# 6. ------