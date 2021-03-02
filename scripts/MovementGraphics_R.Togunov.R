# ** READ FIRST: Notes from Ron Togunov -----------------
#                                                      #
#     Script to animate any number of animal tracks    #
#             over one animated raster and             #
#        any number of still rasters/shapefiles        #
#          with option to change map framing           #
#                                                      #
#   Copyright 2018, Ron Togunov, All rights resevred   #
#   Feel free to copy, modify, and share this script   #
#   If this script is used laregely in its current     #
#   state, I kindly ask to be credited.                #
#                                                      #
#                            last modified: 2019-06-25 #
########################################################.
########################################################.

# notes: 
# [] 1. you will have to modify each line with "!!" in the comments
# [X] 2. this scrpit requires each animal be an induvidual csv
# [X] 3. script also assumes a regular location fix rate
#    if fix rate is not consistent (eg. commonly with fastloc or Argos),
#    you can either: a) approximate upper range of locations/day or
#    b) regularize data (see note 4)
# [!] 4. You can either use raw location data, or you can regularize the            NOTE: I am attempting with raw first
#    location frequency using linear interpolation (eg. using 'approx()')
#    or using more complex methods such as the 'crawl' package
# [] 5. Current script downloads sea ice concentration as the raster, however,
#    you can modify the loop to import data of any raster from your computer
# [] 6. You will likely need to play around with colours of the raster, the land,  
#    and your animals. The following tools are very handy: 
#    1. for colour gradients (eg. raster): www.mycolor.space
#    2. for general pallets, (find contrasting colour): www.coolors.co
#    3. also for generating pallets: color.adobe.com
#    4. similar to adobe: www.colourco.de
# [] 7. For selecting a good projection, you can browse: www.epsg.io


# 1. Load libraries -------------- 
library(maptools)
library(rgdal)
library(rgeos)
library(lubridate)
library(raster)
library(latticeExtra)
library(grDevices)
library(sp)
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



# 2. Import data and format --------

bears <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")
unique(bears$ID)

# separate each bear
X03956 <- bears %>% filter(bears$ID=="X03956")
X10374 <- bears %>% filter(bears$ID=="X10374")
X10393 <- bears %>% filter(bears$ID=="X10393")
X10695 <- bears %>% filter(bears$ID=="X10695")
X10700 <- bears %>% filter(bears$ID=="X10700")
X10703 <- bears %>% filter(bears$ID=="X10703")
X10707 <- bears %>% filter(bears$ID=="X10707")
X10709 <- bears %>% filter(bears$ID=="X10709")
X11974 <- bears %>% filter(bears$ID=="X11974")
X11975 <- bears %>% filter(bears$ID=="X11975")
X12078 <- bears %>% filter(bears$ID=="X12078")
X12080 <- bears %>% filter(bears$ID=="X12080")
X12081 <- bears %>% filter(bears$ID=="X12081")
X12082 <- bears %>% filter(bears$ID=="X12082")
X12083 <- bears %>% filter(bears$ID=="X12083")
X12086 <- bears %>% filter(bears$ID=="X12086")
X12092 <- bears %>% filter(bears$ID=="X12092")
X13284 <- bears %>% filter(bears$ID=="X13284")
X13289 <- bears %>% filter(bears$ID=="X13289")
X13292 <- bears %>% filter(bears$ID=="X13292")
X13428 <- bears %>% filter(bears$ID=="X13428")
X13437 <- bears %>% filter(bears$ID=="X13437")
X13746 <- bears %>% filter(bears$ID=="X13746")
X30126 <- bears %>% filter(bears$ID=="X30126")
X30129 <- bears %>% filter(bears$ID=="X30129")
X30131 <- bears %>% filter(bears$ID=="X30131")
X30135 <- bears %>% filter(bears$ID=="X30135")
X30140 <- bears %>% filter(bears$ID=="X30140")

# Make dataframes for just 1994 - save csvs to a file
bears_1994 <- bears %>% filter(bears$YEAR=="1994")
unique(bears_1994$ID)

X03956_1994 <- bears_1994 %>% filter(bears_1994$ID=="X03956")
write.csv(X03956_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X03956.csv")
X10374_1994 <- bears_1994 %>% filter(bears_1994$ID=="X10374")
write.csv(X10374_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X10374.csv")
X10393_1994 <- bears_1994 %>% filter(bears_1994$ID=="X10393")
write.csv(X10393_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X10393.csv")
X10700_1994 <- bears_1994 %>% filter(bears_1994$ID=="X10700")
write.csv(X10700_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X10700.csv")
X10703_1994 <- bears_1994 %>% filter(bears_1994$ID=="X10703")
write.csv(X10703_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X10703.csv")
X10709_1994 <- bears_1994 %>% filter(bears_1994$ID=="X10709")
write.csv(X10709_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X10709.csv")
X11974_1994 <- bears_1994 %>% filter(bears_1994$ID=="X11974")
write.csv(X11974_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X11974.csv")
X11975_1994 <- bears_1994 %>% filter(bears_1994$ID=="X11975")
write.csv(X11975_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X11975.csv")
X12078_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12078")
write.csv(X12078_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12078.csv")
X12080_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12080")
write.csv(X12080_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12080.csv")
X12081_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12081")
write.csv(X12081_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12081.csv")
X12082_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12082")
write.csv(X12082_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12082.csv")
X12083_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12083")
write.csv(X12083_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12083.csv")
X12086_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12086")
write.csv(X12086_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12086.csv")
X12092_1994 <- bears_1994 %>% filter(bears_1994$ID=="X12092")
write.csv(X12092_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X12092.csv")
X13284_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13284")
write.csv(X13284_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13284.csv")
X13289_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13289")
write.csv(X13289_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13289.csv")
X13292_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13292")
write.csv(X13292_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13292.csv")
X13428_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13428")
write.csv(X13428_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13428.csv")
X13437_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13437")
write.csv(X13437_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13437.csv")
X13746_1994 <- bears_1994 %>% filter(bears_1994$ID=="X13746")
write.csv(X13746_1994, "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/X13746.csv")





# 3. TEST - Movement graphic (1994 only) -----

# Define script parameters
animalid <- c("X03956", "X10374", "X10393", "X10700",
              "X10703", "X10709", "X11974", "X11975",
              "X12078", "X12080", "X12081", "X12082",
              "X12083","X12086", "X12092", "X13284",
              "X13289", "X13292", "X13428", "X13437", "X13746")  # define IDs of animals to plot !!
locs.per.day <- 9  # the number of location fixes per day programmed into the tag (or upper approximation of irregular tags) !!
max.loc.age <- 10  # desired maximum age of a location to be plotted (in days) !!
start.date <- ymd("1994-01-01")  # date from which to start plotting !! NOTE: I REMOVED hms!
length <- 365 # number of days that will be plotted !!
track.projection <- "+proj=longlat +datum=WGS84"  # projection of your animal location data !!
working.projection <- "+proj=longlat +datum=WGS84"  # I'd say use the best UTM projection for your region. !!
loc.data.dir <- "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/CSVfiles_movementgraphic/"  # define directory location for location data !!
loc.combined <- TRUE  # specify whether dat is combined !!
loc.data <- "X03956.csv"  # specify name of location file (if separate) !!
plot.dir <- "/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/Movement graphics/" # specify the directory in which to save the file !!

# define dimentions of figure 
width = 400
height = 300

----- 
  
# load data 
# load animal tracks 
tracks <- list()  # create blank list for animal tracks
if(loc.combined){
  track <- read.csv(paste0(loc.data.dir, loc.data))  # location of animal tracks
  for (i in 1:length(animalid)){
    track.temp <- track %>% filter(Xnumber == animalid[i])  # add animal track to list
    coordinates(track.temp) = ~LONG + LAT  # define the names of horizontal and vertical coordinat columns (in that order) !!
    proj4string(track.temp) = CRS(track.projection)  # define projection
    tracks[[i]] <- spTransform(track.temp, CRS(working.projection)) # reproject track ino working projection
    # define datetime to lubridate date 
    tracks[[i]]$loc_date_time <- ymd(tracks[[i]]$loc_date_time)  # change "ymd_hms" to match your date format (see ?lubridate) !!
  }
  } else {
    for (i in 1:length(animalid)){
      track <- read.csv(paste0(loc.data.dir, animalid[i],".csv"))  # location of animal tracks !!
      coordinates(track) = ~longitude + latitude  # define the names of horizontal and vertical coordinat columns (in that order) !!
      proj4string(track) = CRS(track.projection)  # define projection
      track <- spTransform(track, CRS(working.projection))  # reproject track ino working projection
      tracks[[i]] <- track  # add animal track to list
      # define datetime to lubridate date 
      tracks[[i]]$loc_date_time <- ymd_hms(tracks[[i]]$loc_date_time)  # change "ymd_hms" to match your date format (see ?lubridate) !!
    }
}

s.bw.locs <- 86400/locs.per.day  # seconds between consecutive locations
max.loc.age.s <- 86400*max.loc.age  # maximum age of locations to plot in seconds
max.n.locs <- locs.per.day*max.loc.age  # maximum number of locations for each animal

# load world map, lakes, and gradicules
world <- readOGR("data/GIS files/world boarders.shp")    # load world map  !!
lakes <- readOGR("data/GIS files/50m lakes.shp")    # load lakes shapefile  !!
grad <- readOGR("data/GIS files/10m graticules.shp") # load gradicules (grid lines)  #  !!
extent <- extent(-170, 50, -60, 90)  # to speed processing, define max extent to plot. (xmin, ymin, xmax, ymax) !!

# download sample rastel layer #
test.day <- 180 # which day to test !! 
url <- paste0("https://seaice.uni-bremen.de/data/amsre/asi_daygrid_swath/n6250/", year(start.date+test.day*86400), 
              "/", tolower(month(start.date+test.day*86400, label = T, abbr = T)),
              "/Arctic/asi-n6250-", 
              paste0(year(start.date+test.day*86400), substr(start.date+test.day*86400, 6, 7), substr(start.date+test.day*86400, 9, 10)), 
              "-v5.4.tif")  # !!
download.file(url, "raster.tif", mode="wb")  # download raster !!
raster <- raster("raster.tif")  # load raster !!
proj4string(raster) = CRS("+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-45.2 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")  # !!
raster <- crop(raster, extent(-3000000,-450000,-4000000,-800000))  # cropping raster accelerates reprojeing >10X !!
raster2 <- projectRaster(raster, crs = working.projection)  # reproject raster into working projection  !!
raster.frame <- projectRaster(raster, crs = CRS("+init=epsg:3395"))  # reproject raster into working projection  !!

# framing and extent ####
# framing of plot # can freeze fix framing by making each initial the same as the final 
x.i <- -85  # initial centre x coordinate to plot !!
y.i <- 58  # initial centre y coordinate to plot !!
km.i <- 1300 # inital km from western boundary to eastern boundary!!
x.f <- -85  # final centre x coordinate to plot !!
y.f <- 58  # final centre y coordinate to plot !!
km.f <- 1300  # final km from western boundary to eastern boundary !!
# plot framing ###
{
  frame.i <- data.frame(longitude = c(x.i-1/(cos(y.i/180*pi)*111.321)*(km.i/2),
                                      x.i+1/(cos(y.i/180*pi)*111.321)*(km.i/2),
                                      x.i+1/(cos(y.i/180*pi)*111.321)*(km.i/2),
                                      x.i-1/(cos(y.i/180*pi)*111.321)*(km.i/2)),
                        latitude = c(y.i-1/111*km.i/2*height/width,
                                     y.i-1/111*km.i/2*height/width,
                                     y.i+1/111*km.i/2*height/width,
                                     y.i+1/111*km.i/2*height/width)) %>% 
    Polygon() %>% 
    {Polygons(srl = list(.),1)} %>% 
    {SpatialPolygons(list(.))} 
  frame.f <- data.frame(longitude = c(x.f-1/(cos(y.f/180*pi)*111.321)*(km.f/2),
                                      x.f+1/(cos(y.f/180*pi)*111.321)*(km.f/2),
                                      x.f+1/(cos(y.f/180*pi)*111.321)*(km.f/2),
                                      x.f-1/(cos(y.f/180*pi)*111.321)*(km.f/2)),
                        latitude = c(y.f-1/111*km.f/2*height/width,
                                     y.f-1/111*km.f/2*height/width,
                                     y.f+1/111*km.f/2*height/width,
                                     y.f+1/111*km.f/2*height/width)) %>% 
    Polygon() %>% {Polygons(srl = list(.),1)} %>% 
    {SpatialPolygons(list(.))} 
  
  proj4string(frame.i) <- CRS("+proj=longlat +datum=WGS84")
  proj4string(frame.f) <- CRS("+proj=longlat +datum=WGS84")
  frame.i <- spTransform(frame.i, CRS("+init=epsg:3395"))
  frame.f <- spTransform(frame.f, CRS("+init=epsg:3395"))
  
  world.frame <- gSimplify(world, tol = 0.15) %>% 
    crop(extent(-179.9,180,-85,90)) %>% 
    spTransform(CRS("+init=epsg:3395"))
  
  plot(c(extent(world.frame)@xmin, extent(world.frame)@xmax), 
       c(extent(world.frame)@ymin, extent(world.frame)@ymax), axes = F) 
  plot(raster.frame, legend = F, add = TRUE)
  plot(world.frame, col = "white", border = "grey40", add = TRUE)
  plot(world.frame, col = "white", border = "grey40", add = T)
  plot(frame.i, border = "red", add = T)
  plot(frame.f, border = "blue", add = T)
  text(x = -20000000, y = 18000000,"initial", cex=0.6, pos=4, col="red")
  text(x = -20000000, y = 17000000,"final", cex=0.6, pos=4, col="blue")
}
# crop and reproject layers
world <- crop(world, extent)  #  crop land by defined 'extent'
lakes <- crop(lakes, extent)  #  crop lakes by defined 'extent'
grad <- crop(grad, extent)  #  crop gradicules by defined 'extent'
world <- spTransform(world, CRS(working.projection)) # reproject map into working projection
lakes <- spTransform(lakes, CRS(working.projection)) # convert map ino  working projection
grad <- spTransform(grad, CRS(working.projection)) # convert grad ino  working projection
# create framing data frame #
{
  xyi <- data.frame(x.i, y.i)  # combine initial xy
  xyf <- data.frame(x.f, y.f)  # combine final xy
  
  coordinates(xyi) = ~x.i + y.i  # define coordinate names
  coordinates(xyf) = ~x.f + y.f  # define coordinate names
  
  proj4string(xyi) = CRS("+init=epsg:4326")  # define CRS
  proj4string(xyf) = CRS("+init=epsg:4326")  # define CRS
  
  xyi <- spTransform(xyi, CRS(working.projection))  # reproject to working CRS
  xyf <- spTransform(xyf, CRS(working.projection))  # reproject to working CRS
  
  # define easing modifier (to smooth tracking and zooming)
  ease.cos <- 1/2*(-cos(log(seq(1, (pi+1)*6, length.out = length*locs.per.day*{x = 0.2}))/ # !! modify x to determine when zoom is complete (0-1)
                          log((pi+1)*6)*pi
  ) + 1)  
  ease.flat <- rep(1, length*locs.per.day*(1-x))
  
  easer <- c(ease.cos, ease.flat)
  plot(easer)
  
  # plot((sin(seq(-pi/2, pi/2, length.out = length*locs.per.day)) + 1)/2)
  
  x1  <- xyi$x.i + (xyf$x.f - xyi$x.i)*easer - (km.i*1000*(1-easer) + km.f*1000*easer)/2
  y1  <- xyi$y.i + (xyf$y.f - xyi$y.i)*easer - (km.i*1000*(1-easer) + km.f*1000*easer)/width*height/2
  y2  <- y1      + (km.i*1000*(1-easer) + km.f*1000*easer)/width*height
  x2  <- x1      + (y2 - y1)/height*width
  bx1 <- x1      + (x2 - x1)*0.0488
  bx2 <- x1      + (x2 - x1)*0.95
  by1 <- y1      + (y2 - y1)*0.04
  by2 <- y1      + (y2 - y1)*0.929
  lab <- x1      + (x2 - x1)*0.13
  
  frame <- data.frame(x1,	y1,	y2, x2, bx1, bx2, by1, by2, lab)
}

# colour maps ####
breaks <- c(seq(0,100,20)) # bins for raster breaks (in my case, values range from -80 - 280 and will be divided into 30 bins)  !!
raster.col <- colorRampPalette(c("#66688E","#FFFFFF","#FFFFFF"))  # raster gradient
raster.col <- raster.col(length(breaks))
plot(raster2,  
     breaks = breaks, col = raster.col, legend = F, bg = water.col)

gramp <- colorRampPalette(c("#20116920", "#86086C20","#C62F5D20",  # good gradient of colours that look good over white raster # start value
                            "#E5614020", "#DD932C20"), alpha = T)  # the 01s represent 1% opacity
gramp2 <- colorRampPalette(c("#20116999", "#86086C99","#C62F5D99",  # good gradient of colours that look good over white raster # end value
                             "#E5614099", "#DD932C99"), alpha = T)  # the 99s represent 99% opacity 
gramp <- gramp(length(animalid))

gramp2 <- gramp2(length(animalid))
land.col <- "#D8DBDE"
water.col <- "#66688E"
border.col <- "#b0b2b5"
grad.col <- "#97999c" # colour of the gradicules 

#  define a gradient for each animal based on start and end values of gramp
animalcol <- list()
for (i in 1:length(animalid)){
  assign(paste0("animalcol", i), colorRampPalette(c(gramp[i], gramp2[i]), alpha = T))
}
# test plot of colours
{colours.df <- data.frame(name = c(paste0(rep("raster.col.",length(raster.col)), 
                                 seq(1, length(raster.col))),
                  paste0(rep("gramp2.", length(gramp2)),seq(1, length(gramp2))),
             "land.col",
             "water.col",
             "border.col", "grad.col"),
             col = c(raster.col,gramp2,land.col,water.col,border.col, grad.col))
barplot(rep(1,nrow(colours.df)), col = colours.df$col %>% as.character(), names = colours.df$name,las=2)
}
{# add environmental data limited by specidief (x1, x2, y1, y2) bounds
  plot(raster2,  
       breaks = breaks, col = raster.col, legend = F, bg = water.col)
  # add world map and lakes
  plot(world, add = T, col = land.col, border = adjustcolor(border.col, 0.2))
  plot(lakes, add = T, col = water.col, border = adjustcolor(border.col, 0.2), lwd = 0.1)
  # add gradicules
  plot(grad, add = T, col = grad.col) 
}

# plotting loop ######################################################################################### #####  
strt <- now() # start time
for (i in 1:length){ # length is number of days that will be animated (using 'days' because environmental data is once daily)
    datetime <- start.date + 86400*(i-1) # 86400s in one day # each loop, add one day
    
    # download, save, and load raster file (currently structured for AMSR2 sea ice data 
    year <- year(datetime)  # !!
    month <- tolower(month(datetime, label = T, abbr = T))
    date <- paste0(year, substr(datetime, 6, 7), substr(datetime, 9, 10))
    # define URL for raster file to download 
    url <- sprintf("https://seaice.uni-bremen.de/data/amsre/asi_daygrid_swath/n6250/%s/%s/Arctic/asi-n6250-%s-v5.4.tif",
                  year, month, date)
      
    download.file(url, "raster.tif", mode="wb")  # download raster !!
    raster <- raster("raster.tif")  # load raster !!
    proj4string(raster) = CRS("+proj=stere +lat_0=90 +lat_ts=71 +lon_0=-45.2 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")  # !!
    raster <- crop(raster, extent(-3000000,-450000,-4000000,-1800000))  # cropping raster accelerates reprojeing >10X !!
    raster2 <- projectRaster(raster, crs = working.projection)  # reproject raster into working projection  !!
    raster2[is.na(raster2)] <- 0  # set a value of 0 to NAs !!
    
    # plotting sub loop for each hour of the day with fixes 
    for(j in 1:locs.per.day){
      plot.num <- (i - 1)*locs.per.day + j
        png(filename = sprintf("%s/%06d.png",plot.dir, plot.num),  # substr(datetime + (j - 1)*s.bw.locs, 1, 13), # make sure directory exists !!
             width = width, height = height, units = "mm", res = 100)  # you can change the resolution !!
            par(mar = rep(0, 4), oma = rep(0, 4))
            # create blank plot area with specified (x1, x2, y1, y2) limits
            plot(c(frame$bx1[plot.num], frame$bx2[plot.num]), 
                 c(frame$by1[plot.num], frame$by2[plot.num]), axes = F) 
            # add environmental data limited by specidief (x1, x2, y1, y2) bounds
            plot(raster2, xlim = c(frame$x1[plot.num],frame$x2[plot.num]), 
                 ylim = c(frame$y1[plot.num],frame$y2[plot.num]), 
                 breaks = breaks, col = raster.col, legend = F, add = T, bg = water.col)
            # add world map and lakes
            plot(world, add = T, col = land.col, border = adjustcolor(border.col, 0.2))
            plot(lakes, add = T, col = water.col, border = adjustcolor(border.col, 0.2), lwd = 0.1)
            # add the locations for each animal
                for(k in 1:length(animalid)){
                    points(subset(tracks[[k]], 
                                  loc_date_time <= (datetime + (j - 1)*s.bw.locs) &  # no locs past current date
                                    loc_date_time >= (datetime + (j - 1)*s.bw.locs - max.loc.age.s)),  # no locs past max age
                         pch = 19, cex = 2, col = get(paste0("animalcol", k))(max.n.locs))  # define point size and colour
                  # why all colours are the same and not with gradientns?
                  }
            # add gradicules
            plot(grad, add = T, col = grad.col) 
            # add location date
            mtext(paste(substr(datetime, 1, 10)), 
                  side = 1, line = - 1.5, at = frame$lab[(i - 1)*locs.per.day + j], cex = 2.5, family="mono") 
        dev.off()
    }
    # progress information
    laps <- round(as.duration(now() - strt)/as.duration(seconds(60)), 2)
    perc <- round(i/as.numeric(length)*100, 2)
    print(paste("row:", i,"-", perc,"% completion - elapsed time (m):", laps ," - remaining (m):", round(100/perc*laps - laps, 1)))
}
