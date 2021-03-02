
# Load libraries and NUWCRU theme ------------------

library(rgdal)
library(sp)
library(raster)
library(ggplot2)
library(dplyr)
library(lubridate)


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

setwd("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/Coding/PB_DataExploration/")

# Import updated bear_FINAL with season column added and plot -------------------------------
    # Note: The seasons were added in excel based on the sea ice analysis
    # Note: ggplot package used

bear_FINAL_seasons <- read.csv("DS_polarbear_spatial/data/bear_FINAL_seasons.csv")

# plot number of observations per season
ggplot(bear_FINAL_seasons, aes(Season)) +
  geom_bar() +
  ylab("Count") +
  xlab("Sea ice season")

# summarize data
bear_FINAL_seasons_summary <- bear_FINAL_seasons %>% 
  group_by(Season) %>% 
  summarize(count=n())
      # Note: there are 602 winter observations, 473 break-up, 543 ice-free (summer), and 240 freeze-up


# Create separate dfs for each season and export -----------------------------------
  # Note: summer is not needed
  # Note: dplyr package used

winter <- bear_FINAL_seasons %>%
  filter(Season=="winter")
write.csv(winter, "DS_polarbear_spatial/data/winter.csv")
break_up <- bear_FINAL_seasons %>%
  filter(Season=="break")
write.csv(break_up, "DS_polarbear_spatial/data/break_up.csv")
freeze_up <- bear_FINAL_seasons %>%
  filter(Season=="freeze")
write.csv(freeze_up, "DS_polarbear_spatial/data/freeze_up.csv")


# Import buffers for each season, then generate random points within - keep for thesis -----------------
  # NOTE: created buffers in QGIS (43 km around each used point)

install.packages("sf")
library(sf)


      # NOTE: I didn't end up using this code; made random points in QGIS instead
      # NOTE: Come back to this point for thesis



      # Import shapefiles, check type of data and projection, and plot
            # Got help with this from here: https://datacarpentry.org/r-raster-vector-geospatial/06-vector-open-shapefile-in-r/
winter_buffer <- st_read("QGIS_buffer/winter_buffer.shp")
plot(winter_buffer$LONG, winter_buffer$LAT)
st_geometry_type(winter_buffer) #polygons
st_crs(winter_buffer) #polar stereographic
ggplot() + 
  geom_sf(data = winter_buffer, size = 0.5, color = "black", fill="blue") + 
  coord_sf()


break_up_buffer <- st_read("QGIS_buffer/break_up_buffer.shp")
plot(break_up_buffer$LONG, break_up_buffer$LAT)
st_geometry_type(break_up_buffer) #polygons
st_crs(break_up_buffer) #polar stereographic
ggplot() + 
  geom_sf(data = break_up_buffer, size = 0.5, color = "black", fill="red") + 
  coord_sf()

freeze_up_buffer <- st_read("QGIS_buffer/freeze_up_buffer.shp")
plot(freeze_up_buffer$LONG, freeze_up_buffer$LAT)
st_geometry_type(freeze_up_buffer) #polygons
st_crs(freeze_up_buffer) #polar stereographic
ggplot() + 
  geom_sf(data = freeze_up_buffer, size = 0.5, color = "black", fill="orange") + 
  coord_sf()

      # Plot them all together
ggplot() + 
  geom_sf(data = freeze_up_buffer, size = 0.5, color = "black", fill="orange") + 
  geom_sf(data = winter_buffer, size = 0.5, color = "black", fill="blue") + 
  geom_sf(data = break_up_buffer, size = 0.5, color = "black", fill="red") + 
  coord_sf()





# Generating random points - code from Phil; this did not work but keep for thesis ----------------



      # NOTE: I didn't end up using this code; made random points in QGIS instead
      # NOTE: Come back to this point for thesis



      # Winter
#for 126 individuals (number of individual polygons that you have)
winter_buffer_df$ID_number<- seq(1,602, by = 1)

fixes_winter<-dplyr::select(winter_buffer_df, ID_number)

s <- sapply(slot(winter_buffer_df, 'geometry'), function(i) spsample(i, n=fixes_winter[fixes_winter$ID_number==slot(i, 'ID'),c(2)]*10, type='stratified'))

s.merged <- do.call('rbind', s)

ids <- sapply(slot(uds, 'polygons'), function(i) slot(i, 'ID'))

npts <- sapply(s, function(i) nrow(i@coords))

pt_id <- rep(ids, npts)

s.final <- SpatialPointsDataFrame(s.merged, data=data.frame(poly_id=pt_id))

random_pts<- as.data.frame(s.final)

random_pts$ID_number<- random_pts$poly_id














      # Create random points
          # first, need shapefiles as spatial objects

winter_buffer_spatial <- readOGR("QGIS_buffer/winter_buffer.shp")
break_buffer_spatial <- readOGR("QGIS_buffer/break_up_buffer.shp")
freeze_buffer_spatial <- readOGR("QGIS_buffer/freeze_up_buffer.shp")

          # generate points and attach information from polygon to point
install.packages("spatialEco")
library(spatialEco)
                   

winter_samples <- sample.poly(winter_buffer_spatial, n = 5, type = "random")
plot(winter_samples)
class(winter_samples)
summary(winter_samples)

    # Convert from SpatialPoints to SpatialPointsDataFrame
winter_samples_spatial <- SpatialPointsDataFrame(winter_samples, data.frame(matrix(ncol=1, nrow=3010)), proj4string = CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))
 #Note 3010 is the number of points - get from summary()
proj4string(winter_samples_spatial)


    # Combine points and polygon information
point.in.poly(winter_samples_spatial, winter_buffer_spatial, duplicate=FALSE)


?point.in.poly()












plot(winter_buffer_spatial)
plot(winter_samples, add=TRUE)

?as.data.frame















# Organize dataframes for bathymetry and distance to land values ----------
    # NOTE: The values for these two habitat covariates were generated in QGIS

    # Import data
used_distland_bath <- read.csv("DS_polarbears/attribute tables from QGIS/bear_FINAL_Dist_land_bath.csv")
avail_distland_bath <- read.csv("DS_polarbears/attribute tables from QGIS/allrandompoints_Dist_land_bath.csv")

    # Remove unnecessary columns and change column names to match each other
    # Also add new column with available as 0 and used as 1
    # Needed to reorganize used df so that it aligns with the other

avail_distland_bath2 = subset(avail_distland_bath, select=-c(RANDOM_PT_, POLY_ID, X, BEAR_LAT, BEAR_LONG, ROWID))
avail_distland_bath3 <- setNames(avail_distland_bath2, c("LONG", "LAT", "ID", "DATE", "DAY", "MONTH", "YEAR", "TIME", "TEST_DATE", "SEASON", "DIST_LAND", "BATH"))
avail_distland_bath3$USED_VS_AVAIL <- rep(0, nrow(avail_distland_bath3))
avail_distland_bath4 <- as.data.frame(avail_distland_bath3)

    # The coordinates above are in UTM, but the ones below are in lat/long
    # Yet, both are in polar stereographic - need to figure this out for thesis


used_distland_bath2 <- subset(used_distland_bath, select=-c(ROWID))
used_distland_bath3 <- setNames(used_distland_bath2, c("ID", "DATE", "DAY", "MONTH", "YEAR", "TIME", "LAT", "LONG", "TEST_DATE", "SEASON", "DIST_LAND", "BATH"))
used_distland_bath3$USED_VS_AVAIL <- rep(1, nrow(used_distland_bath3))
used_distland_bath4 <- as.data.frame(used_distland_bath3)

    # Combine them both 

used_vs_avail <- rbind(avail_distland_bath4, used_distland_bath5)
write.csv(used_vs_avail, "DS_polarbear_spatial/data/used_vs_avail.csv")



# Final dataframes ------------------

# used_distland_bath4 = final used dataframe
# avail_distland_bath4 = final available dataframe
# used_vs_avail = final dataframe with both used and available points



# Analyzing bathymetry data  --------------------------------

    # used points
ggplot(used_distland_bath4, aes(BATH)) +
  geom_histogram() +
  ylab("Count") +
  xlab("Selected bathymetric values")

    # available points
ggplot(avail_distland_bath4, aes(BATH)) +
  geom_histogram() +
  ylab("Count") +
  xlab("Bathymetric values associated with available data points")

    # both together
ggplot(data=used_vs_avail) +
  geom_histogram(aes(x=BATH)) +
  facet_grid(USED_VS_AVAIL~.)

    # both together separated into seasons
used_vs_avail_iceonly <- used_vs_avail %>%
  filter(SEASON != "summer")

ggplot(data=used_vs_avail_iceonly) +
  geom_histogram(aes(x=BATH)) +
  facet_grid(USED_VS_AVAIL~SEASON)

    # summarize info
bathymetry_summary <- used_vs_avail_iceonly %>%
  group_by(USED_VS_AVAIL, SEASON) %>%
  summarize(COUNT=n(), BATH.AVG=mean(BATH))


# Analyzing distance to land data  --------------------------------

options(scipen = 999) #Change from scientific notation so x-axis values look correct

    # used points
ggplot(used_distland_bath4, aes(DIST_LAND)) +
  geom_histogram() +
  ylab("Count") +
  xlab("Selected distance to land values")

# available points
ggplot(avail_distland_bath4, aes(DIST_LAND)) +
  geom_histogram() +
  ylab("Count") +
  xlab("Distance to land values associated with available data points") # Note: this is in the Google Drive folder

# both together
ggplot(data=used_vs_avail) +
  geom_histogram(aes(x=DIST_LAND)) +
  facet_grid(USED_VS_AVAIL~.)

# both together separated into seasons
ggplot(data=used_vs_avail_iceonly) +
  geom_histogram(aes(x=DIST_LAND)) +
  facet_grid(USED_VS_AVAIL~SEASON)

# summarize info
dist_land_summary <- used_vs_avail_iceonly %>%
  group_by(USED_VS_AVAIL, SEASON) %>%
  summarize(COUNT=n(), DIST.LAND.AVG=mean(DIST_LAND))


# testing colinearity for bathymetry and distance to land variables --------------------





#### NEED TO DO THIS AGAIN WITH ONLY THE USED RECORDS
#### MAKE NEW DF WITH ONLY USED AND WITHOUT SUMMER

used_distland_final_iceonly <- used_distland_bath4%>%
  filter(SEASON != "summer")




# plot a scatterplot of bathymetry vs distance to land
ggplot(used_distland_final_iceonly, aes(BATH, DIST_LAND)) +
  geom_smooth(method=lm, se=FALSE) +
  ylab("Selected distance to land values") +
  xlab("Selected bathymetric values") +
  geom_point(size=.5)

# create a function for making a boxplot
panel.bxp <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 2, usr[3:4]))
  boxplot(x, add=TRUE, col="grey")
}

pairs(~BATH+DIST_LAND, data=used_distland_final_iceonly, diag.panel=panel.bxp, panel=panel.smooth)

#gives us a smooth line, but we want a least squares line also
twolines = function(x,y)
{ 
  points(x,y)
  lines(loess.smooth(x,y), col="red")
  abline(lsfit(x,y),col="blue")
}
#try again with our boxplot function
pairs(~BATH+DIST_LAND, data=used_distland_final_iceonly, diag.panel=panel.bxp, panel=twolines)


# Pearson correlation test
res <- cor.test(used_distland_final_iceonly$BATH, used_distland_final_iceonly$DIST_LAND, method = "pearson")
print(res)






# Building a basic RSF with just BATH and DIST_LAND (for 571) --------------------------

# install packages
install.packages("lme4")
install.packages("Hmisc")
install.packages("survival")
library(lme4)
library(Hmisc)
library(survival)

summary(used_vs_avail_iceonly_unscaled)

used_avail_summary <- used_vs_avail_iceonly_unscaled %>%
  group_by(USED_VS_AVAIL, SEASON) %>%
  summarize(COUNT=n())

bear_summary <- used_vs_avail_iceonly_unscaled %>%
  filter(USED_VS_AVAIL==1) %>%
  group_by(ID) %>%
  summarize(COUNT=n())

summary(bear_summary)

selected_summary <- used_vs_avail_iceonly_unscaled %>%
  filter(USED_VS_AVAIL==1)

summary(selected_summary)

seasonal_selected_summary <- used_vs_avail_iceonly_unscaled %>%
  filter(USED_VS_AVAIL==1) %>%
  group_by(SEASON) %>%
  summarize(AVG.BATH=mean(BATH), AVG.DIST.LAND=mean(DIST_LAND))



# create models

  # Null: mixed effect (i.e. random effect)
null=glmer(USED_VS_AVAIL~1+(1|ID), family=binomial, data=used_vs_avail_iceonly)
summary(null)
ranef(null) # difference between the pop. average and each individual
coef(null) # coefficients for each individual

        # Calculate AIC using deviance value of 6682 and k value of 2 (0 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
6682 + (2*2) # AIC for null model = 6686

  # Scale and center bath and dist_land variables
used_vs_avail_iceonly_unscaled <- used_vs_avail
used_vs_avail_iceonly$BATH <- scale(used_vs_avail_iceonly$BATH, scale=TRUE, center=TRUE)
used_vs_avail_iceonly$DIST_LAND <- scale(used_vs_avail_iceonly$DIST_LAND, scale=TRUE, center=TRUE)

  # model1: bathymetry and mixed effects
model1=glmer(USED_VS_AVAIL~BATH+(1|ID), family=binomial, data=used_vs_avail_iceonly)
summary(model1)
ranef(model1)
coef(model1)

        # Calculate AIC using deviance value of 6675.2 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
6675.2 + (2*3) # AIC for model1 = 6681.2

  # model2: distance to land and mixed effects
model2=glmer(USED_VS_AVAIL~DIST_LAND+(1|ID), family=binomial, data=used_vs_avail_iceonly)
summary(model2)
ranef(model2)
coef(model2)

# Calculate AIC using deviance value of 6648.9 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
6648.9 + (2*3) # AIC for model2 = 6654.9



# Building seasonal RSFs with just the BATH and DIST_LAND variables ------------------------

# Separate data into seasons

winter <- used_vs_avail_iceonly %>%
  filter(SEASON=="winter")
freeze_up <- used_vs_avail_iceonly %>%
  filter(SEASON=="freeze")
break_up <- used_vs_avail_iceonly %>%
  filter(SEASON=="break")

# create winter models

    # Null: mixed effect (i.e. random effect)
winter_null=glmer(USED_VS_AVAIL~1+(1|ID), family=binomial, data=winter)
summary(winter_null)
ranef(winter_null) # difference between the pop. average and each individual
coef(winter_null) # coefficients for each individual

          # Calculate AIC using deviance value of 2934.1 and k value of 2 (0 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2934.1 + (2*2) # AIC for null model = 2938.1

    # model1: bathymetry and mixed effects
winter_model1=glmer(USED_VS_AVAIL~BATH+(1|ID), family=binomial, data=winter)
summary(winter_model1)
ranef(winter_model1)
coef(winter_model1)

          # Calculate AIC using deviance value of 2929.5 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2929.5 + (2*3) # AIC for model1 = 2935.5

    # model2: distance to land and mixed effects
winter_model2=glmer(USED_VS_AVAIL~DIST_LAND+(1|ID), family=binomial, data=winter)
summary(winter_model2)
ranef(winter_model2)
coef(winter_model2)

          # Calculate AIC using deviance value of 2910.6 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2910.6 + (2*3) # AIC for model2 = 2916.6


# create break-up models

  # Null: mixed effect (i.e. random effect)
break_null=glmer(USED_VS_AVAIL~1+(1|ID), family=binomial, data=break_up)
summary(break_null)
ranef(break_null) # difference between the pop. average and each individual
coef(break_null) # coefficients for each individual

          # Calculate AIC using deviance value of 2399.3 and k value of 2 (0 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2399.3 + (2*2) # AIC for null model = 2403.3

  # model1: bathymetry and mixed effects
break_model1=glmer(USED_VS_AVAIL~BATH+(1|ID), family=binomial, data=break_up)
summary(break_model1)
ranef(break_model1)
coef(break_model1)

          # Calculate AIC using deviance value of 2399.1 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2399.1 + (2*3) # AIC for model1 = 2405.1

  # model2: distance to land and mixed effects
break_model2=glmer(USED_VS_AVAIL~DIST_LAND+(1|ID), family=binomial, data=break_up)
summary(break_model2)
ranef(break_model2)
coef(break_model2)

          # Calculate AIC using deviance value of 2387.2 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
2387.2 + (2*3) # AIC for model2 = 2393.2



# create freeze-up models

  # Null: mixed effect (i.e. random effect)
freeze_null=glmer(USED_VS_AVAIL~1+(1|ID), family=binomial, data=freeze_up)
summary(freeze_null)
ranef(freeze_null) # difference between the pop. average and each individual
coef(freeze_null) # coefficients for each individual

          # Calculate AIC using deviance value of 1233.1 and k value of 2 (0 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
1233.1 + (2*2) # AIC for null model = 1237.1

  # model1: bathymetry and mixed effects
freeze_model1=glmer(USED_VS_AVAIL~BATH+(1|ID), family=binomial, data=freeze_up)
summary(freeze_model1)
ranef(freeze_model1)
coef(freeze_model1)

          # Calculate AIC using deviance value of 1220.4 and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
1220.4 + (2*3) # AIC for model1 = 1226.4

  # model2: distance to land and mixed effects
freeze_model2=glmer(USED_VS_AVAIL~DIST_LAND+(1|ID), family=binomial, data=freeze_up)
summary(freeze_model2)
ranef(freeze_model2)
coef(freeze_model2)

          # Calculate AIC using deviance value of 1229.8  and k value of 3 (1 predictor variables + 1 for the intercept + 0 for the random slope + 1 for the random intercept)
1229.8 + (2*3) # AIC for model2 = 1235.8




# Getting coefficients for top models ----------------

# RSF that is averaged across sea ice seasons
    # top model = model2 (DIST_LAND + (1|ID))

# unscale the coefficient for DIST_LAND
unsc.dist.land <- subset(used_vs_avail_iceonly_unscaled,select=c(DIST_LAND)) sc.dist.land <-
  subset(used_vs_avail_iceonly,select=c(DIST_LAND))
colMeans(sc.dist.land)
apply(sc.dist.land,2,sd) cm <-
  colMeans(unsc.dist.land) csd <-
  apply(unsc.dist.land,2,sd)
rescale.coefs <- 
  function(beta,mu,sigma) { ## inherit names etc.
  beta2 <- beta
  beta2[-1] <- sigma[1]*beta[-1]/sigma[-1]
  beta2[1] <- sigma[1]*beta[1]+mu[1]-sum(beta2[-1]*mu[-1]) beta2
}
X <- model.matrix(USED_VS_AVAIL~DIST_LAND+(1|ID), #this is our top model
                  family=binomial,data=used_vs_avail_iceonly)
X <- getME(model2,"X")
cm2 <- colMeans(X)[-1]
csd2 <- apply(X,2,sd)[-1]
fixef(model2)

#these are the uncorrected coefficients; #double-check that they match what you got above
#this next line unscales the coefficients -
#ignore the values for the variables we never scaled in the first place! #You are looking for the coefficient of the variable we scaled earlier
(cc <- rescale.coefs(fixef(model2),mu=c(0,cm),sigma=c(1,csd)))







# Seasonal RSFs














## CANNOT EXTRACT SEA ICE VALUES WITHOUT FIXING COORDINATE ISSUE -----------

# Extracting sea ice concentration values ------------------
# The code below is from Phil
    # Make new df
used_vs_avail_seaice <- used_vs_avail
head(used_vs_avail_seaice)

    # Make a new date column that matches the sea ice raster_list file names
used_vs_avail_seaice$TEST_DATE <- ymd(used_vs_avail_seaice$TEST_DATE)
used_vs_avail_seaice$year <- year(used_vs_avail_seaice$TEST_DATE)
used_vs_avail_seaice$month <- month(used_vs_avail_seaice$TEST_DATE)
used_vs_avail_seaice$day <- day(used_vs_avail_seaice$TEST_DATE)
used_vs_avail_seaice$raster_date <- sprintf("%04d%02d%02d", used_vs_avail_seaice$year, used_vs_avail_seaice$month, used_vs_avail_seaice$day) #Note: code from Phil for this line wasn't working; got this from Stack Exchange

head(used_vs_avail_seaice)

      # import sea ice rasters
sea_ice <- readRDS("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/Coding/SeaIce_DataExploration/DS_seaice_rasterlistrds/raster_list.rds")

head(sea_ice)

    #make a list of all the unique dates
list_dates<-unique(used_vs_avail_seaice$raster_date)

y<-data()

for (i in list_dates) {
  
  i= `19910402`
  
  data_1<-dplyr::filter(used_vs_avail_seaice, raster_date == i) %>% droplevels
  
  points<-data_1
  
  points_DF<-points
  
  coordinates(points) <- ~LONG+LAT
  
  proj4string(points) <- CRS("+proj=longlat +ellps=WGS84")
  
  points_polar <- spTransform(points, CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs"))

  points_DF$sea_ice_values <- extract(sea_ice$i, points_polar, df= TRUE)
  
  y<-rbind(y, points_DF)
  
  #or...
  #write.csv(points_DF, paste0(i, "_sea_ice_values", ".csv"), row.names = FALSE)
  
}





plot(sea_ice_extract)

write.csv(y, "GPS_sea_ice_values.csv")


#now combine them all to one dataframe
# Get the files names

#files = list.files(pattern="*_sea_ice_values.csv")

# First apply read.csv, then rbind
#data = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))






