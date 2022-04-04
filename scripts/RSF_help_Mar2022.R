
# 1. load libraries ---------


library(ggplot2)
library(dplyr)
library(lattice) # section 3
library(raster) # section 4


# 2. Import data and run top model (run time: <2 min) -------

used_avail_RSF_freezeup_FINAL <- read.csv("used_avail_RSF_freezeup_FINAL_Apr2021.csv")
head(used_avail_RSF_freezeup_FINAL)
# create squared conc column then scale
used_avail_RSF_freezeup_FINAL$CONC_2 = '^'(used_avail_RSF_freezeup_FINAL$CONC,2)
used_avail_RSF_freezeup_FINAL$CONC_2_SCALED <- scale(used_avail_RSF_freezeup_FINAL$CONC_2, scale=TRUE, center=TRUE)

# separate used from avail
used <- used_avail_RSF_freezeup_FINAL %>% filter(USED_AVAIL==1)
avail <- used_avail_RSF_freezeup_FINAL %>% filter(USED_AVAIL==0)

# freeze-up: Model 5a (BATH + CONC + CONC_2)
bears_freezeup_m5a <- glmmTMB(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
bears_freezeup_m5a
summary(bears_freezeup_m5a) #AIC:1599.7

bears_freezeup_null <- glmmTMB(USED_AVAIL~+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
summary(bears_freezeup_null) #AIC: 1628.2

# 3. Create plots (run time: <2 min) -------

# histograms
# relative freq histograms of data: https://www.statology.org/relative-frequency-histogram-r/

histogram(used$CONC, col="grey", breaks=10)
histogram(avail$CONC, col="grey", breaks=10)


# relative probability of selection 
      # BATH
median_CONC = median(used_avail_RSF_freezeup_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_freezeup_model5a = coef(bears_freezeup_m5a)
coefs_freezeup_model5a = coefs_freezeup_model5a$cond$ID
rest_of_prediction = median_CONC * coefs_freezeup_model5a$CONC_SCALED[1] + median_CONC_2 * coefs_freezeup_model5a$CONC_2_SCALED[1] + coefs_freezeup_model5a$`(Intercept)`[1]
curve(1 / (0.02361453*(1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction)))), xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


1 / (1 + exp(-((0 - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                      coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction)))

summary(used_avail_RSF_freezeup_FINAL$BATH)

      # CONC
median_BATH = median(used_avail_RSF_freezeup_FINAL$BATH_SCALED)
coefs_freezeup_model5 = coef(bears_freezeup_m5a)
coefs_freezeup_model5 = coefs_freezeup_model5$cond$ID
rest_of_prediction = median_BATH * coefs_freezeup_model5$BATH_SCALED[1] + coefs_freezeup_model5$`(Intercept)`[1]
curve(1 / (0.1535558*(1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$CONC)) / sd(used_avail_RSF_freezeup_FINAL$CONC) * 
                       coefs_freezeup_model5$CONC_SCALED[1] + (x^2 - mean(used_avail_RSF_freezeup_FINAL$CONC_2)) / 
                       sd(used_avail_RSF_freezeup_FINAL$CONC_2) * coefs_freezeup_model5$CONC_2_SCALED[1] + 
                       rest_of_prediction)))), xlim = range(used_avail_RSF_freezeup_FINAL$CONC), 
      xlab = "Sea ice concentration", ylab = "Relative probability of selection")












# 3.2 Create plots: UNSCALED using Evie's 571 code & Amy's suggestions (run time: <2 min) -------

# using code from Evie's lab #6

# BATH

#use the following code to get the unscaled coefficient for hrbj197
unsc.BATH <- subset(used_avail_RSF_freezeup_FINAL,select=c(BATH))
sc.BATH <- subset(used_avail_RSF_freezeup_FINAL,select=c(BATH_SCALED))

colMeans(sc.BATH)
apply(sc.BATH,2,sd)
cm <- colMeans(unsc.BATH)
csd <- apply(unsc.BATH,2,sd)

rescale.coefs <- function(beta,mu,sigma) {
  beta2 <- beta ## inherit names etc.
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1] <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1])
  beta2
}

X <- model.matrix(USED_AVAIL~BATH_SCALED+CONC_SCALED+CONC_2_SCALED+(1|ID), family=binomial(), data=used_avail_RSF_freezeup_FINAL)
X <- getME(bears_freezeup_m5a,name=("X"))
cm2 <- colMeans(X)[-1]
csd2 <- apply(X,2,sd)[-1]
fixef(bears_freezeup_m5a)
coef(bears_freezeup_m5a) # these last 2 lines should match

#this next line unscales the coefficients -
#ignore the values for the variables we never scaled in the first place!
#You are looking for the coefficient of the variable we scaled earlier
(cc <- rescale.coefs(fixef(bears_freezeup_m5a),mu=c(0,cm),sigma=c(1,csd))) # this line doesn't work

###

# graphs

# original
median_CONC = median(used_avail_RSF_freezeup_FINAL$CONC_SCALED)
median_CONC_2 = median_CONC^2
coefs_freezeup_model5a = coef(bears_freezeup_m5a)
coefs_freezeup_model5a = coefs_freezeup_model5a$cond$ID
rest_of_prediction = median_CONC * coefs_freezeup_model5a$CONC_SCALED[1] + median_CONC_2 * coefs_freezeup_model5a$CONC_2_SCALED[1] + coefs_freezeup_model5a$`(Intercept)`[1]
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction))), xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")


# Amy's suggestion
      #(RSF – minimum_value) / (maximum_value – minimum_value)"

coefs_freezeup_model5a

                                                
curve(1 / (1 + exp(-((x - mean(used_avail_RSF_freezeup_FINAL$BATH)) / 
                       sd(used_avail_RSF_freezeup_FINAL$BATH) * 
                       coefs_freezeup_model5a$BATH_SCALED[1] + rest_of_prediction))), 
      xlim = range(used_avail_RSF_freezeup_FINAL$BATH), 
      ylim = c(0,1), xlab = "Ocean depth (m)", ylab = "Relative probability of selection")

curve(1/(coefs_freezeup_model5a-min(used_avail_RSF_freezeup_FINAL$BATH)/
           (max(used_avail_RSF_freezeup_FINAL$BATH))-
           min(used_avail_RSF_freezeup_FINAL$BATH)+rest_of_prediction))

curve(coefs_freezeup_model5a-min(used_avail_RSF_freezeup_FINAL$BATH)/
                 (max(used_avail_RSF_freezeup_FINAL$BATH))-
                 min(used_avail_RSF_freezeup_FINAL$BATH))


plot(coefs_freezeup_model5a-min(used_avail_RSF_freezeup_FINAL$BATH)/
       (max(used_avail_RSF_freezeup_FINAL$BATH))-
       min(used_avail_RSF_freezeup_FINAL$BATH))

plot(coefs_freezeup_model5a)



#

# 4. Create maps -------

#     4a. Prep data (run time: 40 min)------


# load polygon for cropping
#polygon <- readOGR("/1. Coding/Shapefiles/RSF_map_boundary.shp")
polygon <- readOGR("seaiceboundary_finalbeardataset.shp")
proj4string(polygon) # lat/long
polygon_proj <- spTransform(polygon, crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(polygon_proj) # lat/long
plot(polygon_proj) 

# Load sea ice
#raster_list <- readRDS("/1. Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list_78-20_final.rds")
raster_list_uncropped <- readRDS("raster_list_78-20.rds")
plot(raster_list_uncropped$`20101010`)


# Load bathymetry
bathymetry <- raster("gebco_2020_n85.82123637199403_s35.03197789192201_w-96.98521256446841_e-15.826191902160673.tif")
plot(bathymetry)
proj4string(bathymetry)
bathymetry_proj <- projectRaster(bathymetry, crs=crs('+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs'))
proj4string(bathymetry_proj)
plot(bathymetry_proj)
bathymetry_crop <- crop(bathymetry_proj, polygon_proj, snap='out') # crop
bathymetry_crop <- mask(bathymetry_crop, polygon_proj, snap='out') # mask outside pixels
plot(bathymetry_crop)
plot(polygon_proj, add=TRUE) # looks right


# create one day of sea ice and crop - raster_list_uncropped
Mar10_2011 <- raster_list_uncropped$`20110310`
proj4string(Mar10_2011)
plot(Mar10_2011)
plot(polygon_proj, add=TRUE) # looks right
Mar10_2011_crop <- crop(Mar10_2011, polygon_proj, snap='out') # crop
Mar10_2011_crop <- mask(Mar10_2011_crop, polygon_proj, snap='out') # mask outside pixels
plot(Mar10_2011_crop)
plot(polygon_proj, add=TRUE) # looks right
proj4string(Mar10_2011_crop)
# make it finer scale
Mar10_2011_crop_resam <- resample(Mar10_2011_crop,bathymetry_crop) #,method='bilinear')

#

#     4b. Make maps (run time: <2min) ------


# combine 1 sea ice raster and bathymetry and get values
# using raster_list_uncropped version
freeze_covariates_brick <- brick(Mar10_2011_crop_resam, bathymetry_crop)
names(freeze_covariates_brick) <- c("CONC", "BATH")
head(freeze_covariates_brick)

bath_unscaled <- freeze_covariates_brick$BATH
bath_unscaled[bath_unscaled>0]=NA

bath_scaled_raster <- (bath_unscaled-mean(used_avail_RSF_freezeup_FINAL$BATH))/sd(used_avail_RSF_freezeup_FINAL$BATH)
conc_scaled_raster <- (freeze_covariates_brick$CONC-mean(used_avail_RSF_freezeup_FINAL$CONC))/sd(used_avail_RSF_freezeup_FINAL$CONC)
conc2_scaled_raster <- (freeze_covariates_brick$CONC^2-mean(used_avail_RSF_freezeup_FINAL$CONC_2))/sd(used_avail_RSF_freezeup_FINAL$CONC_2)

#plot(bath_scaled_raster)
#plot(polygon_proj, add=TRUE)
#plot(conc_scaled_raster)
#plot(polygon_proj, add=TRUE)
#plot(conc2_scaled_raster)
#plot(polygon_proj, add=TRUE)


# add rasters together with coefficients from model; linear model

model_raster <- coef(bears_freezeup_m5a)
freeze_map <- model_raster$cond$ID$BATH_SCALED[1]*bath_scaled_raster+model_raster$cond$ID$CONC_SCALED[1]*conc_scaled_raster+model_raster$cond$ID$CONC_2_SCALED[1]*conc2_scaled_raster+model_raster$cond$ID$`(Intercept)`[1]

# transform with link function and plot

freeze_map <- 1/(1+exp(-freeze_map))

plot(freeze_map)
plot(polygon_proj, add=TRUE)

