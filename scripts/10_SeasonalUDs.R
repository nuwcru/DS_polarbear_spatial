
# 1. Load libraries -------

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)
library(ks)
library(maptools)
library(rgdal)

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


# 2. Load data and format --------


# bear data
used_avail_RSF_winter_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_winter_FINAL_Apr2021.csv")
used_avail_RSF_breakup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_breakup_FINAL_Apr2021.csv")
used_avail_RSF_freezeup_FINAL <- read.csv("data/Oct2020work/FINAL DATASET/used_avail_RSF_freezeup_FINAL_Apr2021.csv")

head(used_avail_RSF_winter_FINAL)

used_winter <- used_avail_RSF_winter_FINAL %>% filter(USED_AVAIL==1)
used_freezeup <- used_avail_RSF_freezeup_FINAL %>% filter(USED_AVAIL==1)
used_breakup <- used_avail_RSF_breakup_FINAL %>% filter(USED_AVAIL==1)



# Make lat/long UDs (HPI) so that I can import into QGIS
# Calculate areas in QGIS!


# 3. Freeze-up HPIs -------

# run section 2 first
# use used_freezeup

head(used_freezeup)
colnames(used_freezeup)

# create pilot first
freezeup_pilot <- Hpi(used_freezeup[, 11:12], pilot="amse", binned=T) 

# plot
freezeup_kernel <- kde(used_freezeup[, 11:12], H=freezeup_pilot)
freezeup_contour95 <- contourLevels(freezeup_kernel, cont=95) # 95%
freezeup_contour50 <- contourLevels(freezeup_kernel, cont=50) # 50%
freezeup_line95 = contourLines(x = freezeup_kernel$eval.points[[1]], y = freezeup_kernel$eval.points[[2]], z = freezeup_kernel$estimate, level = freezeup_contour95)
freezeup_line50 = contourLines(x = freezeup_kernel$eval.points[[1]], y = freezeup_kernel$eval.points[[2]], z = freezeup_kernel$estimate, level = freezeup_contour50)
#freezeup_sldf95 = ContourLines2SLDF(freezeup_line95, proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75"))
#freezeup_sldf50 = ContourLines2SLDF(freezeup_line50, proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75"))
freezeup_sldf95 = ContourLines2SLDF(freezeup_line95, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
freezeup_sldf50 = ContourLines2SLDF(freezeup_line50, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(freezeup_sldf95)
plot(freezeup_sldf50, add=TRUE, col="blue")

writeOGR(freezeup_sldf50, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_freezeup50.shp", layer="freezeup_sldf50", driver="ESRI Shapefile")
writeOGR(freezeup_sldf95, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_freezeup95.shp", layer="freezeup_sldf95", driver="ESRI Shapefile")


###

# 4. Break-up HPIs -------

# run section 2 first
# use used_freezeup

head(used_breakup)
colnames(used_breakup)


# run section 2 first
# use used_freezeup

head(used_freezeup)
colnames(used_freezeup)

# create pilot first
breakup_pilot <- Hpi(used_breakup[, 11:12], pilot="amse", binned=T) 

# plot
breakup_kernel <- kde(used_breakup[, 11:12], H=breakup_pilot)
breakup_contour95 <- contourLevels(breakup_kernel, cont=95) # 95%
breakup_contour50 <- contourLevels(breakup_kernel, cont=50) # 50%
breakup_line95 = contourLines(x = breakup_kernel$eval.points[[1]], y = breakup_kernel$eval.points[[2]], z = breakup_kernel$estimate, level = breakup_contour95)
breakup_line50 = contourLines(x = breakup_kernel$eval.points[[1]], y = breakup_kernel$eval.points[[2]], z = breakup_kernel$estimate, level = breakup_contour50)
breakup_sldf95 = ContourLines2SLDF(breakup_line95, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
breakup_sldf50 = ContourLines2SLDF(breakup_line50, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(breakup_sldf95)
plot(breakup_sldf50, add=TRUE, col="blue")

writeOGR(breakup_sldf95, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_breakup95.shp", layer="breakup_sldf95", driver="ESRI Shapefile")
writeOGR(breakup_sldf50, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_breakeup50.shp", layer="breakup_sldf50", driver="ESRI Shapefile")



# 5. Winter HPIs -------

# run section 2 first
# use used_freezeup

head(used_winter)
colnames(used_winter)

# create pilot first
winter_pilot <- Hpi(used_winter[, 11:12], pilot="amse", binned=T) 

# plot
winter_kernel <- kde(used_winter[, 11:12], H=winter_pilot)
winter_contour95 <- contourLevels(winter_kernel, cont=95) # 95%
winter_contour50 <- contourLevels(winter_kernel, cont=50) # 50%
winter_line95 = contourLines(x = winter_kernel$eval.points[[1]], y = winter_kernel$eval.points[[2]], z = winter_kernel$estimate, level = winter_contour95)
winter_line50 = contourLines(x = winter_kernel$eval.points[[1]], y = winter_kernel$eval.points[[2]], z = winter_kernel$estimate, level = winter_contour50)
winter_sldf95 = ContourLines2SLDF(winter_line95, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
winter_sldf50 = ContourLines2SLDF(winter_line50, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(winter_sldf95)
plot(winter_sldf50, add=TRUE, col="blue")

writeOGR(winter_sldf95, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_winter95.shp", layer="winter_sldf95", driver="ESRI Shapefile")
writeOGR(winter_sldf50, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/UDs_2022/bears_winter50.shp", layer="winter_sldf50", driver="ESRI Shapefile")


