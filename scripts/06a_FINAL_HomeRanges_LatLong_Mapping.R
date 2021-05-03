


# 1. Import libraries  --------------

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
library(lme4) # for step #12 - mixed model

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

# 2. Pooled HRs: Import data ---------

# import data
bears_final_Nov2020_UTM_LATLONG <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")

# separate each bear from the rest of the dataset 
unique(bears_final_Nov2020_UTM_LATLONG$ID)

X10695 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10695")
X10703 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10703")
X10709 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10709")
X10707 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10707")
X10700 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10700")
X13284 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13284")
X13292 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13292")
X13289 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13289")
X11974 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11974")
X11975 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X11975")
X13428 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13428")
X13437 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13437")
X10393 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10393")
X12080 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12080")
X12078 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12078")
X12081 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12081")
X12082 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12082")
X12083 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12083")
X12086 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12086")
X03956 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X03956")
X10374 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X10374")
X12092 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X12092")
X13746 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X13746")
X30129 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30129")
X30126 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30126")
X30131 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30131")
X30140 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30140")
X30135 <- bears_final_Nov2020_UTM_LATLONG %>% filter(bears_final_Nov2020_UTM_LATLONG$ID=="X30135")

# check which bears have >100 fixes
str(X10695) # 110
str(X10709) # 86 
str(X12083) # 5 
str(X12092) # 51
str(X11975) # 88 
str(X30140) # 42
str(X30135) # 71 
str(X30129) # 79 
str(X30131) # 79 
str(X30126) # 87
str(X10703) # 100
str(X10707) # 75
str(X10700) # 97
str(X13284) # 90
str(X13292) # 125
str(X13289) # 139
str(X11974) # 53
str(X13428) # 75
str(X13437) # 76
str(X12086) # 54
str(X03956) # 12
str(X10374) # 27
str(X12080) # 93
str(X12078) # 5
str(X13746) # 108 
str(X10393) # 8
str(X12081) # 6
str(X12082) # 9



# 4. Pooled HRs: MCPs ---------

# Need only bears with >=100 fixes: X10695, X10703, X13292, X13289, X13746
# get projections here: https://proj.org/operations/projections/stere.html#id1

# make a new list for bears with >100 fixes
bear_list_greater100 <- list(X10695, X10703, X13292, X13289, X13746)

# name items in list
names(bear_list_greater100) <- c("X10695_mcp_utm", "X10703_mcp_utm", "X13292_mcp_utm", "X13289_mcp_utm", "X13746_mcp_utm")
head(bear_list_greater100$X10695_mcp_utm) # check that it worked
names(bear_list_greater100)

# create loop to make each a spatial points dataframe (and check projections)
for(i in 1:length(bear_list_greater100)){
  coordinates(bear_list_greater100[[i]]) <- c("LONG", "LAT") 
  proj4string(bear_list_greater100[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=75") 
  temp <- bear_list_greater100[[i]]       
  temp[["ID"]] <- as.factor(temp[["ID"]]) 
  bear_list_greater100[[i]] <- temp         
}


# rename items in list
names(bear_list_greater100) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
names(bear_list_greater100)
head(bear_list_greater100$'X10695_mcp') # note that easting and northing columns are gone because they're used to make it a spatial object

# Check projections
proj4string(bear_list_greater100[[1]])
proj4string(bear_list_greater100$'X13289_mcp')

# create loop for the MCPs
# Make an empty list for the MCPs
bear_list_greater100_mcps <- list()

# create 95% MCP for each (Step 5)
for(i in 1: length(bear_list_greater100)){
  temp_dat <- bear_list_greater100[[i]] # subset the the bear_list first, and then use that subset in the mcp function
  bear_list_greater100_mcps[[i]] <- mcp(temp_dat[, "ID"], percent=95, unin="m", unout="km2")  
}

# Name items in the list
names(bear_list_greater100_mcps) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
head(bear_list_greater100_mcps$X13289_mcp) # check that it worked


# test plot - it worked! 
plot(bear_list_greater100_mcps$X13289_mcp)

# unlist and rename them
bear_list_greater100_mcps <- unlist(bear_list_greater100_mcps)
names(bear_list_greater100_mcps) <- c("X10695_mcp", "X10703_mcp", "X13292_mcp", "X13289_mcp", "X13746_mcp")
head(bear_list_greater100_mcps$X10695_mcp)

# test plot
plot(bear_list_greater100_mcps$X10695_mcp)
plot(bear_list_greater100$X10695_mcp, add=T)

# make shapefiles and export
X10695_mcp_shp <- bear_list_greater100_mcps$X10695_mcp
writeOGR(X10695_mcp_shp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10695_mcp_shp.shp", layer="X10695_mcp_shp", driver="ESRI Shapefile")
X10703_mcp_shp <- bear_list_greater100_mcps$X10703_mcp
writeOGR(X10703_mcp_shp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10703_mcp_shp.shp", layer="X10703_mcp_shp", driver="ESRI Shapefile")
X13292_mcp_shp <- bear_list_greater100_mcps$X13292_mcp
writeOGR(X13292_mcp_shp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13292_mcp_shp.shp", layer="X13292_mcp_shp", driver="ESRI Shapefile")
X13289_mcp_shp <- bear_list_greater100_mcps$X13289_mcp
writeOGR(X13289_mcp_shp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13289_mcp_shp.shp", layer="X13289_mcp_shp", driver="ESRI Shapefile")
X13746_mcp_shp <- bear_list_greater100_mcps$X13746_mcp
writeOGR(X13746_mcp_shp, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13746_mcp_shp.shp", layer="X13746_mcp_shp", driver="ESRI Shapefile")


# 5. Pooled HRs: Hrefs ------------

# make a list of only the bears with >30 fixes (across all years) - Seaman et al. 1999
bear_href_list <- list(X10695, X10700, X10703, X10707, X10709, X11974, X11975, X12080, X12086, X12092, X13284, X13289, X13292, X13428, X13437, X13746, X30126, X30129, X30131, X30135, X30140)

# name items in list
names(bear_href_list) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_list$X13289) # check that it worked
names(bear_href_list) 
head(bear_href_list$X10695)

# Make empty list for the kernel and then extract it with loop
bear_href <- list()

for(i in 1:length(bear_href_list)){
  temp_dat <- bear_href_list[[i]]
  bear_href[[i]] <- kernelUD(SpatialPoints(temp_dat[, 15:14]), h="href")
} # this worked! no warnings!


plot(bear_href[[1]]) # test plot

# Name items in the list  
names(bear_href) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href$X13289) 

# Make empty list for the vertices and then extract with loop (for plotting)
bear_href_ver <- list()

for(i in 1:length(bear_href)){ 
  temp_dat2 <- bear_href[[i]]
  bear_href_ver[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2') 
} # this worked!

plot(bear_href_ver[[1]]) 

# Name items in the list
names(bear_href_ver) <- c("X10695_href", "X10700_href", "X10703_href", "X10707_href", "X10709_href", "X11974_href", "X11975_href", "X12080_href", "X12086_href", "X12092_href", "X13284_href", "X13289_href", "X13292_href", "X13428_href", "X13437_href", "X13746_href", "X30126_href", "X30129_href", "X30131_href", "X30135_href", "X30140_href")
head(bear_href_ver$X10695_href) # this worked!


# unlist and rename them
bear_href_ver <- unlist(bear_href_ver)
names(bear_href_ver) <- c("X10695_href", "X10700_href", "X10703_href", "X10707_href", "X10709_href", "X11974_href", "X11975_href", "X12080_href", "X12086_href", "X12092_href", "X13284_href", "X13289_href", "X13292_href", "X13428_href", "X13437_href", "X13746_href", "X30126_href", "X30129_href", "X30131_href", "X30135_href", "X30140_href")
head(bear_href_ver$X10695_href)


# make shapefiles and export
X10695_href <- bear_href_ver$X10695_href
writeOGR(X10695_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10695_href.shp", layer="X10695_href", driver="ESRI Shapefile")
X10700_href <- bear_href_ver$X10700_href
writeOGR(X10700_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10700_href.shp", layer="X10700_href", driver="ESRI Shapefile")
X10703_href <- bear_href_ver$X10703_href
writeOGR(X10703_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10703_href.shp", layer="X10703_href", driver="ESRI Shapefile")
X10707_href <- bear_href_ver$X10707_href
writeOGR(X10707_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10707_href.shp", layer="X10707_href", driver="ESRI Shapefile")
X10709_href <- bear_href_ver$X10709_href
writeOGR(X10709_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10709_href.shp", layer="X10709_href", driver="ESRI Shapefile")
X11974_href <- bear_href_ver$X11974_href
writeOGR(X11974_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X11974_href.shp", layer="X11974_href", driver="ESRI Shapefile")
X11975_href <- bear_href_ver$X11975_href
writeOGR(X11975_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X11975_href.shp", layer="X11975_href", driver="ESRI Shapefile")
X12080_href <- bear_href_ver$X12080_href
writeOGR(X12080_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12080_href.shp", layer="X12080_href", driver="ESRI Shapefile")
X12086_href <- bear_href_ver$X12086_href
writeOGR(X12086_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12086_href.shp", layer="X12086_href", driver="ESRI Shapefile")
X12092_href <- bear_href_ver$X12092_href
writeOGR(X12092_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12092_href.shp", layer="X12092_href", driver="ESRI Shapefile")
X13284_href <- bear_href_ver$X13284_href
writeOGR(X13284_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13284_href.shp", layer="X13284_href", driver="ESRI Shapefile")
X13289_href <- bear_href_ver$X13289_href
writeOGR(X13289_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13289_href.shp", layer="X13289_href", driver="ESRI Shapefile")
X13292_href <- bear_href_ver$X13292_href
writeOGR(X13292_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13292_href.shp", layer="X13292_href", driver="ESRI Shapefile")
X13428_href <- bear_href_ver$X13428_href
writeOGR(X13428_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13428_href.shp", layer="X13428_href", driver="ESRI Shapefile")
X13437_href <- bear_href_ver$X13437_href
writeOGR(X13437_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13437_href.shp", layer="X13437_href", driver="ESRI Shapefile")
X13746_href <- bear_href_ver$X13746_href
writeOGR(X13746_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13746_href.shp", layer="X13746_href", driver="ESRI Shapefile")
X30126_href <- bear_href_ver$X30126_href
writeOGR(X30126_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30126_href.shp", layer="X30126_href", driver="ESRI Shapefile")
X30129_href <- bear_href_ver$X30129_href
writeOGR(X30129_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30129_href.shp", layer="X30129_href", driver="ESRI Shapefile")
X30131_href <- bear_href_ver$X30131_href
writeOGR(X30131_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30131_href.shp", layer="X30131_href", driver="ESRI Shapefile")
X30135_href <- bear_href_ver$X30135_href
writeOGR(X30135_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30135_href.shp", layer="X30135_href", driver="ESRI Shapefile")
X30140_href <- bear_href_ver$X30140_href
writeOGR(X30140_href, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30140_href.shp", layer="X30140_href", driver="ESRI Shapefile")



# 6. Pooled HRs: Hpi -----------



# use same list we used for href kernels at the beginning (bear_href_list) - both need >= 30 fixes

# Make empty list and then create samse pilot with loop
bear_hpi_amsepilot <- list()

for(i in 1:length(bear_href_list)){
  temp_dat <- bear_href_list[[i]]
  bear_hpi_amsepilot[[i]] <- Hpi(temp_dat[, 15:14], pilot="amse", binned=T) 
} 

# Make empty list and then make samse contours
bear_polygon <- list()

for(i in 1:length(bear_hpi_amsepilot)){
  bear_hpi_amse_contour <- kde(bear_href_list[[i]][15:14], H=bear_hpi_amsepilot[[i]]) 
  bear_contourlevels <- contourLevels(bear_hpi_amse_contour, cont = 95)               
  bear_lines <- contourLines(x=bear_hpi_amse_contour$eval.points[[1]], y=bear_hpi_amse_contour$eval.points[[2]], 
                                 z=bear_hpi_amse_contour$estimate, level=bear_contourlevels) 
  bear_sldf = ContourLines2SLDF(bear_lines)  
  bear_polyset = SpatialLines2PolySet(bear_sldf) 
  bear_polygon[[i]] = PolySet2SpatialPolygons(bear_polyset) 
} 

# name items in list (same order as bear_KDE_list)
names(bear_polygon) <- c("X10695_hpi", "X10700_hpi", "X10703_hpi", "X10707_hpi", "X10709_hpi", "X11974_hpi", "X11975_hpi", "X12080_hpi", "X12086_hpi", "X12092_hpi", "X13284_hpi", "X13289_hpi", "X13292_hpi", "X13428_hpi", "X13437_hpi", "X13746_hpi", "X30126_hpi", "X30129_hpi", "X30131_hpi", "X30135_hpi", "X30140_hpi")
head(bear_polygon$X13289_hpi) # check that it worked
names(bear_polygon) 

# test plot - looks like it worked
plot(bear_polygon$X13289_hpi)

# unlist and rename them
bear_polygon <- unlist(bear_polygon)
names(bear_polygon) <- c("X10695_hpi", "X10700_hpi", "X10703_hpi", "X10707_hpi", "X10709_hpi", "X11974_hpi", "X11975_hpi", "X12080_hpi", "X12086_hpi", "X12092_hpi", "X13284_hpi", "X13289_hpi", "X13292_hpi", "X13428_hpi", "X13437_hpi", "X13746_hpi", "X30126_hpi", "X30129_hpi", "X30131_hpi", "X30135_hpi", "X30140_hpi")
head(bear_polygon$X10695_hpi)

# make spatialpolygonsdataframes and export
X10695_hpi <- bear_polygon$X10695_hpi
X10695_hpi <- SpatialPolygonsDataFrame(X10695_hpi, data=as.data.frame("X10695_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X10695_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10695_hpi.shp", layer="X10695_hpi", driver="ESRI Shapefile")

X10700_hpi <- bear_polygon$X10700_hpi
X10700_hpi <- SpatialPolygonsDataFrame(X10700_hpi, data=as.data.frame("X10700_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X10700_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10700_hpi.shp", layer="X10700_hpi", driver="ESRI Shapefile")

X10703_hpi <- bear_polygon$X10703_hpi
X10703_hpi <- SpatialPolygonsDataFrame(X10703_hpi, data=as.data.frame("X10703_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X10703_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10703_hpi.shp", layer="X10703_hpi", driver="ESRI Shapefile")

X10707_hpi <- bear_polygon$X10707_hpi
X10707_hpi <- SpatialPolygonsDataFrame(X10707_hpi, data=as.data.frame("X10707_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X10707_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10707_hpi.shp", layer="X10707_hpi", driver="ESRI Shapefile")

X10709_hpi <- bear_polygon$X10709_hpi
X10709_hpi <- SpatialPolygonsDataFrame(X10709_hpi, data=as.data.frame("X10709_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X10709_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X10709_hpi.shp", layer="X10709_hpi", driver="ESRI Shapefile")

X11974_hpi <- bear_polygon$X11974_hpi
X11974_hpi <- SpatialPolygonsDataFrame(X11974_hpi, data=as.data.frame("X11974_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X11974_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X11974_hpi.shp", layer="X11974_hpi", driver="ESRI Shapefile")

X11975_hpi <- bear_polygon$X11975_hpi
X11975_hpi <- SpatialPolygonsDataFrame(X11975_hpi, data=as.data.frame("X11975_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X11975_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X11975_hpi.shp", layer="X11975_hpi", driver="ESRI Shapefile")

X12080_hpi <- bear_polygon$X12080_hpi
X12080_hpi <- SpatialPolygonsDataFrame(X12080_hpi, data=as.data.frame("X12080_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X12080_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12080_hpi.shp", layer="X12080_hpi", driver="ESRI Shapefile")

X12086_hpi <- bear_polygon$X12086_hpi
X12086_hpi <- SpatialPolygonsDataFrame(X12086_hpi, data=as.data.frame("X12086_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X12086_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12086_hpi.shp", layer="X12086_hpi", driver="ESRI Shapefile")

X12092_hpi <- bear_polygon$X12092_hpi
X12092_hpi <- SpatialPolygonsDataFrame(X12092_hpi, data=as.data.frame("X12092_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X12092_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X12092_hpi.shp", layer="X12092_hpi", driver="ESRI Shapefile")

X13284_hpi <- bear_polygon$X13284_hpi
X13284_hpi <- SpatialPolygonsDataFrame(X13284_hpi, data=as.data.frame("X13284_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13284_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13284_hpi.shp", layer="X13284_hpi", driver="ESRI Shapefile")

X13289_hpi <- bear_polygon$X13289_hpi
X13289_hpi <- SpatialPolygonsDataFrame(X13289_hpi, data=as.data.frame("X13289_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13289_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13289_hpi.shp", layer="X13289_hpi", driver="ESRI Shapefile")

X13292_hpi <- bear_polygon$X13292_hpi
X13292_hpi <- SpatialPolygonsDataFrame(X13292_hpi, data=as.data.frame("X13292_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13292_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13292_hpi.shp", layer="X13292_hpi", driver="ESRI Shapefile")

X13428_hpi <- bear_polygon$X13428_hpi
X13428_hpi <- SpatialPolygonsDataFrame(X13428_hpi, data=as.data.frame("X13428_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13428_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13428_hpi.shp", layer="X13428_hpi", driver="ESRI Shapefile")

X13437_hpi <- bear_polygon$X13437_hpi
X13437_hpi <- SpatialPolygonsDataFrame(X13437_hpi, data=as.data.frame("X13437_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13437_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13437_hpi.shp", layer="X13437_hpi", driver="ESRI Shapefile")

X13746_hpi <- bear_polygon$X13746_hpi
X13746_hpi <- SpatialPolygonsDataFrame(X13746_hpi, data=as.data.frame("X13746_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X13746_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X13746_hpi.shp", layer="X13746_hpi", driver="ESRI Shapefile")

X30126_hpi <- bear_polygon$X30126_hpi
X30126_hpi <- SpatialPolygonsDataFrame(X30126_hpi, data=as.data.frame("X30126_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X30126_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30126_hpi.shp", layer="X30126_hpi", driver="ESRI Shapefile")

X30129_hpi <- bear_polygon$X30129_hpi
X30129_hpi <- SpatialPolygonsDataFrame(X30129_hpi, data=as.data.frame("X30129_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X30129_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30129_hpi.shp", layer="X30129_hpi", driver="ESRI Shapefile")

X30131_hpi <- bear_polygon$X30131_hpi
X30131_hpi <- SpatialPolygonsDataFrame(X30131_hpi, data=as.data.frame("X30131_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X30131_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30131_hpi.shp", layer="X30131_hpi", driver="ESRI Shapefile")

X30135_hpi <- bear_polygon$X30135_hpi
X30135_hpi <- SpatialPolygonsDataFrame(X30135_hpi, data=as.data.frame("X30135_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X30135_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30135_hpi.shp", layer="X30135_hpi", driver="ESRI Shapefile")

X30140_hpi <- bear_polygon$X30140_hpi
X30140_hpi <- SpatialPolygonsDataFrame(X30140_hpi, data=as.data.frame("X30140_hpi", proj4string=CRS("+proj=stere +lat_0=90 +lat_ts=75")))
writeOGR(X30140_hpi, dsn="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/2. Mapping/Home Ranges/X30140_hpi.shp", layer="X30140_hpi", driver="ESRI Shapefile")







# 7. Annual HRs: Import data -------