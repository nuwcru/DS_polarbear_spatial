# 1. load libraries ---------------------

library(adehabitatHR) # for MCP and lscv/href KDEs
library(adehabitatHS) # to get areas for above
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
library(ks) # for hpi KDE
library(maptools) # for plotting kernel
library(sp)
library(PBSmapping)
library(rgeos)
library(foreign)
library(maptools)
library(FNN)
library(grid) # to use textGrob
library(gridExtra) # to use textGrob

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

# 2. import bear dataset (made in 03_MCP_HomeRanges.R) and create lists -----------

bears_FINAL <- read.csv("data/bears_FINAL.csv")
head(bears_FINAL)

# Pooled list (i.e. individual bears pooled across years)
unique(bears_FINAL$ID)

X10695 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695")
X10703 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703")
X10709 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709")
X10707 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707")
X10700 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700")
X13284 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284")
X13292 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292")
X13289 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289")
X11974 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11974")
X11975 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975")
X13428 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428")
X13437 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437")
X10393 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10393")
X12080 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080")
X12078 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12078")
X12081 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12081")
X12082 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12082")
X12083 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12083")
X12086 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12086")
X03956 <- bears_FINAL %>% filter(bears_FINAL$ID=="X03956")
X10374 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10374")
X12092 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12092")
X13746 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746")
X30129 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129")
X30126 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126")
X30131 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131")
X30140 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30140")
X30135 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30135")


# Annual list (i.e. individual bears seprated between years)
X10695_1991 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1991")
X10695_1992 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10695" & bears_FINAL$YEAR=="1992")
X10700_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700" & bears_FINAL$YEAR=="1993") # Note: this bear has data in 1992, but not >30
X10700_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10700" & bears_FINAL$YEAR=="1994")
X10703_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703" & bears_FINAL$YEAR=="1993") # Note: same comment as above
X10703_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10703" & bears_FINAL$YEAR=="1994")
X10707_1992 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707" & bears_FINAL$YEAR=="1992")
X10707_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10707" & bears_FINAL$YEAR=="1993")
X10709_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X10709" & bears_FINAL$YEAR=="1993") # Note: this bear has data in 1992 and 1994, but not >30
X11975_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975" & bears_FINAL$YEAR=="1994")
X11975_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X11975" & bears_FINAL$YEAR=="1995") # Note: this bear has data in 1993, but not >30
X12080_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1994")
X12080_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12080" & bears_FINAL$YEAR=="1995")
X12086_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12086" & bears_FINAL$YEAR=="1995") # Note: this bear has data in 1994, but not >30
X12092_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X12092" & bears_FINAL$YEAR=="1994") # Note: this bear has data in 1995, but not >30
X13284_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13284" & bears_FINAL$YEAR=="1993") # Note: this bear has data in 1992 and 1994, but not >30
X13289_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1993") # Note: this bear has data in 1992 and 1995, but not >30
X13289_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13289" & bears_FINAL$YEAR=="1994")
X13292_1993 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1993") # Note: this bear has data in 1992 and 1995, but not >30
X13292_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13292" & bears_FINAL$YEAR=="1994")
X13428_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13428" & bears_FINAL$YEAR=="1994") # Note: this bear has data in 1993 and 1995, but not >30
X13437_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13437" & bears_FINAL$YEAR=="1995") # Note: this bear has data in 1993 and 1994, but not >30
X13746_1994 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1994") 
X13746_1995 <- bears_FINAL %>% filter(bears_FINAL$ID=="X13746" & bears_FINAL$YEAR=="1995") 
X30126_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126" & bears_FINAL$YEAR=="1998") # Note: this bear has data in 1997, but not >30
X30126_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30126" & bears_FINAL$YEAR=="1999") 
X30129_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129" & bears_FINAL$YEAR=="1998") # Note: this bear has data in 1997, but not >30
X30129_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30129" & bears_FINAL$YEAR=="1999") 
X30131_1998 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131" & bears_FINAL$YEAR=="1998") # Note: this bear has data in 1997, but not >30
X30131_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30131" & bears_FINAL$YEAR=="1999") 
X30135_1999 <- bears_FINAL %>% filter(bears_FINAL$ID=="X30135" & bears_FINAL$YEAR=="1999") # Note: this bear has data in 1997 and 1998, but not >30

# bears with no years that have >30: X03956, X10374, X10393, X11974, X12078, X12081, X12082, X12083, X30140
# i.e. these bears are not included in the annual analysis
# Note: see "DScollardata.xlsx" for how I got these years/bears

# make a list with only the years that a bear has  >30 fixes 
bear_annualKDE_list <- list(X10695_1991, X10695_1992, X10700_1993, X10700_1994, X10703_1993, X10703_1994, X10707_1992, X10707_1993, X10709_1993, X11975_1994, X11975_1995, X12080_1994, X12080_1995, X12086_1995, X12092_1994, X13284_1993, X13289_1993, X13289_1994, X13292_1993, X13292_1994, X13428_1994, X13437_1995, X13746_1994, X13746_1995, X30126_1998, X30126_1999, X30129_1998, X30129_1999, X30131_1998, X30131_1999, X30135_1999)

# name items in list
names(bear_annualKDE_list) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualKDE_list$X10695_1991) # check that it worked
names(bear_annualKDE_list) 


# 3. POOLED loops for href KDE for all bears with >30 fixes (UTM) - UTM IS REQUIRED TO GET ACCURATE AREA IN KM2 -------------

# make a list of only the bears with >30 fixes (across all years) - Seaman et al. 1999
# Note: this list does not include the 3 problem bears from below (X12078, X12082, and X12083), as well as X03956, X10374, X10393, and X12081
bear_KDE_list <- list(X10695, X10700, X10703, X10707, X10709, X11974, X11975, X12080, X12086, X12092, X13284, X13289, X13292, X13428, X13437, X13746, X30126, X30129, X30131, X30135, X30140)

unique(X30140$YEAR)

# name items in list
names(bear_KDE_list) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_KDE_list$X13289) # check that it worked
names(bear_KDE_list) 

# Make empty list for the kernel and then extract it with loop
bear_href <- list()

for(i in 1:length(bear_KDE_list)){
  temp_dat <- bear_KDE_list[[i]]
  bear_href[[i]] <- kernelUD(SpatialPoints(temp_dat[, 13:14]), h="href")
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
} # this worked!! no warnings!

plot(bear_href_ver[[1]]) 

# Name items in the list
names(bear_href_ver) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_ver$X10695) # this worked!

# calculate area and make into dataframe

# create empty dataframe to put in the areas and fill first row with IDs
bears_href_area_df <- data.frame(ID = rep(NA, length(bear_KDE_list)), area = rep(NA, length(bear_KDE_list)))
bears_href_area_df$ID <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")

# Make an empty list
bears_href_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_href_ver)){
  bears_href_area[[i]] <- gArea(bear_href_ver[[i]])
}

# flip new dataframe
bears_href_area <- t(bears_href_area)
bears_href_area <- unlist(bears_href_area)

# add values from new dataframe into bears_95mcp_summary
bears_href_area_df$area <- bears_href_area
# make a new column that is km2 (previous was m2) and make into a spreadsheet
bears_href_area_df$area_km2 <- bears_href_area_df$area*0.000001
write.csv(bears_href_area_df, "data/bears_href_area_df.csv")


# 4. POOLED loops for href KDE for all bears with >30 fixes (LAT/LONG) - LAT/LONG REQUIRED FOR MAPPING -------------

# Note: use same list as above (bear_KDE_list)

# Make empty list for the kernel and then extract it with loop
bear_href_latlong <- list()

for(i in 1:length(bear_KDE_list)){
  temp_dat <- bear_KDE_list[[i]]
  bear_href_latlong[[i]] <- kernelUD(SpatialPoints(temp_dat[, 11:10]), h="href") # note the different column numbers chosen
} # this worked! no warnings!

plot(bear_href_latlong[[1]]) # test plot

# Name items in the list  
names(bear_href_latlong) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_latlong$X13289) 

# Make empty list for the vertices and then extract with loop (for plotting)
bear_href_ver_latlong <- list()

for(i in 1:length(bear_href_latlong)){ 
  temp_dat2 <- bear_href_latlong[[i]]
  bear_href_ver_latlong[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2') 
} # this worked!! no warnings!

plot(bear_href_ver_latlong[[1]]) 

# Name items in the list
names(bear_href_ver_latlong) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_href_ver_latlong$X10695) # this worked!


# don't need to do the rest of the loops, since we're not looking for areas here - we just need to spatial polygon list (bear_href_ver_latlong) in order to map them


# 5. ANNUAL loops for href KDE for bears with >30 annual fixes (UTM)  -------------

# Make empty list for the kernel and then extract it with loop
bear_annualhref <- list()

for(i in 1:length(bear_annualKDE_list)){
  temp_dat <- bear_annualKDE_list[[i]]
  bear_annualhref[[i]] <- kernelUD(SpatialPoints(temp_dat[, 13:14]), h="href")
} # this worked! no warnings!

plot(bear_annualhref[[1]]) # test plot


# Name items in the list  
names(bear_annualhref) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref$X10695_1991) 


# Make empty list for the vertices and then extract with loop (for plotting)
bear_annualhref_ver <- list()

for(i in 1:length(bear_annualhref)){ 
  temp_dat2 <- bear_annualhref[[i]]
  bear_annualhref_ver[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2') 
} # this worked!! no warnings!

plot(bear_annualhref_ver[[1]]) 

# Name items in the list
names(bear_annualhref_ver) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref_ver$X10695_1991) # this worked!


# calculate area and make into dataframe

# create empty dataframe to put in the areas and fill first row with IDs
bear_annualhref_area_df <- data.frame(ID_YEAR=rep(NA, length(bear_annualKDE_list)), ID = rep(NA, length(bear_annualKDE_list)), YEAR = rep(NA, length(bear_annualKDE_list)), AREA = rep(NA, length(bear_annualKDE_list)))
bear_annualhref_area_df$ID_YEAR <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bear_annualhref_area_df$ID <- c("X10695", "X10695", "X10700", "X10700", "X10703", "X10703", "X10707", "X10707", "X10709", "X11975", "X11975", "X12080", "X12080", "X12086", "X12092", "X13284", "X13289", "X13289", "X13292", "X13292", "X13428", "X13437", "X13746", "X13746", "X30126", "X30126", "X30129", "X30129", "X30131", "X30131", "X30135")
bear_annualhref_area_df$YEAR <- c("1991", "1992", "1993", "1994", "1993", "1994", "1992", "1993", "1993", "1994", "1995", "1994", "1995", "1995", "1994", "1993", "1993", "1994", "1993", "1994", "1994", "1995", "1994", "1995", "1998", "1999", "1998", "1999", "1998", "1999", "1999")

# Make an empty list
bears_annualhref_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_annualhref_ver)){
  bears_annualhref_area[[i]] <- gArea(bear_annualhref_ver[[i]])
}

# flip new dataframe
bears_annualhref_area <- t(bears_annualhref_area)
bears_annualhref_area <- unlist(bears_annualhref_area)

# add values from new dataframe into bears_95mcp_summary
bear_annualhref_area_df$AREA <- bears_annualhref_area

# make a new column that is km2 (previous was m2) and make into a spreadsheet
bear_annualhref_area_df$AREA_KM2 <- bear_annualhref_area_df$AREA*0.000001
summary(bear_annualhref_area_df)

write.csv(bear_annualhref_area_df, "data/bear_annualhref_area_df.csv")

# number of bear years = number of names in annual list (since its already separated by bear and year) 
names(bear_annualKDE_list)

# yearly stats
unique(bear_annualhref_area_df$YEAR)

annualhref_1991 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1991")
summary(annualhref_1991)

annualhref_1992 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1992")
summary(annualhref_1992)

annualhref_1993 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1993")
summary(annualhref_1993)

annualhref_1994 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1994")
summary(annualhref_1994)

annualhref_1995 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1995")
summary(annualhref_1995)

annualhref_1998 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1998")
summary(annualhref_1998)

annualhref_1999 <- bear_annualhref_area_df %>%
  filter(bear_annualhref_area_df$YEAR=="1999")
summary(annualhref_1999)


# 6. ANNUAL loops for href KDE for bears with >30 annual fixes (LAT/LONG)  -------------

# Note: use same list as above (bear_annualKDE_list)

# I kept getting a grid size error (too small) for the second loop below
# help for grid errors from: https://stackoverflow.com/questions/41683905/grid-too-small-for-kernelud-getverticeshr-adehabitathr-home-range-estimation

# first set Domain
x <- seq(0, 100, by=1.)
y <- seq(0, 100, by=1.)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

# then increase Domain - for error messages that kept popping up in second loop below
# note: increasing the domain makes lines below run slower
x <- seq(-3500, 3600, by=3)
y <- seq(-3500, 3600, by=3)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)


# Make empty list for the kernel and then extract it with loop
bear_annualhref_latlong <- list()

for(i in 1:length(bear_annualKDE_list)){
  temp_dat <- bear_annualKDE_list[[i]]
  bear_annualhref_latlong[[i]] <- kernelUD(SpatialPoints(temp_dat[, 11:10]), h="href", grid=xy) # note different column numbers
} # this worked! no warnings!

plot(bear_annualhref_latlong[[2]]) # test plotted all of them (1-31) and none seem to be larger than the grid

# Name items in the list  
names(bear_annualhref_latlong) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref_latlong$X10695_1991) 

# Make empty list for the vertices and then extract with loop (for plotting)
bear_annualhref_ver_latlong <- list()

#for(i in 1:length(bear_annualhref_latlong)){ 
  i=9 # did this separately for i=1 to i=31
  # before I changed the grid size, the error only popped up for i=31 (X30135_1999)
  # after I changed the grid size to -200, 250, the error popped up for i=9 (X10709_1993), i=11 (X11975_1995), i=14 (X12086_1995), i=15 (X12092_1994)
  # after I chnaged the grid size to -300, -350, the error popped up for the same as above: i=9 (X10709_1993), i=11 (X11975_1995), i=14 (X12086_1995), i=15 (X12092_1994) 
  # after I changed the grid to -400 to 450, errors for same as above: i=9, i=11, i=14, i=15
  # ran into error again for i=9 when grid=-600, 650 so incresed it again right away
  # error again for i=9 when grid was -1000, 1050 - at this point I realized that I was setting the limits wrong (should be a difference of 100 not 50)
  # error message: "the grid is too small to allow the estimation of home-range
  #
  # Cannot get a big enough grid size! -3000, 3100 is still too small!
  #
  temp_dat2 <- bear_annualhref_latlong[[i]]
  bear_annualhref_ver_latlong[[i]] <- getverticeshr(temp_dat2, percent=95, unin='m', unout='km2', grid=xy)
#} 

names(bear_annualhref_latlong)

str(bear_annualhref_latlong$X30135_1999)
plot(x=X30135_1999$LONG, y=X30135_1999$LAT) # error before changing grid size - lat range = approx. 59 to 62; long range = approx. -62 to -71
plot(x=X10700_1994$LONG, y=X10700_1994$LAT) # no error - this has a larger range than above (lat: 58 to 67; long: -56 to -64), yet it works fine
plot(x=X12092_1994$LONG, y=X12092_1994$LAT) # error after grid size - lat: 59 to 62; long: -63 to -67


plot(bear_annualhref_ver_latlong[[10]]) # I can plot all numbers without errors from loop above
# if they did have an error, I get an error for this line: "error in plot.window(...) : need finite 'xlim' values..."
# the ones that do work all look different, which is a good sign
# before changing the grid size #10, #23, #25, #28 plots discontinuous HRs (I thought only hpi did that)
# after changing the grid size, just #23, #25, and #28 were discontinous (#10 no longer was)







# Name items in the list
str(bear_annualhref_ver_latlong)
names(bear_annualhref_ver_latlong) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualhref_ver_latlong$X10695_1991) # this worked!








# 7. ANNUAL href KDE for bears with >30 annual fixes - all bears individually (LAT/LONG)  -------------

    # Note: don't calculate area here (only do that with utm)

# X10695_1991
X10695_1991_href <- kernelUD(SpatialPoints(X10695_1991[, 11:10]), h="href") 
image(X10695_1991_href) 
X10695_1991_href_ver <- getverticeshr(X10695_1991_href, percent=95, unin='m', unout='km2') 

# X10695_1992
X10695_1992_href <- kernelUD(SpatialPoints(X10695_1992[, 11:10]), h="href") 
image(X10695_1992_href) 
X10695_1992_href_ver <- getverticeshr(X10695_1992_href, percent=95, unin='m', unout='km2') 

# X10700_1993
X10700_1993_href <- kernelUD(SpatialPoints(X10700_1993[, 11:10]), h="href") 
image(X10700_1993_href) 
X10700_1993_href_ver <- getverticeshr(X10700_1993_href, percent=95, unin='m', unout='km2')

# X10700_1994
X10700_1994_href <- kernelUD(SpatialPoints(X10700_1994[, 11:10]), h="href") 
image(X10700_1994_href) 
X10700_1994_href_ver <- getverticeshr(X10700_1994_href, percent=95, unin='m', unout='km2')

# X10703_1993
X10703_1993_href <- kernelUD(SpatialPoints(X10703_1993[, 11:10]), h="href") 
image(X10703_1993_href) 
X10703_1993_href_ver <- getverticeshr(X10703_1993_href, percent=95, unin='m', unout='km2')

# X10703_1994
X10703_1994_href <- kernelUD(SpatialPoints(X10703_1994[, 11:10]), h="href") 
image(X10703_1994_href) 
X10703_1994_href_ver <- getverticeshr(X10703_1994_href, percent=95, unin='m', unout='km2')

# X10707_1992
X10707_1992_href <- kernelUD(SpatialPoints(X10707_1992[, 11:10]), h="href") 
image(X10707_1992_href) 
X10707_1992_href_ver <- getverticeshr(X10707_1992_href, percent=95, unin='m', unout='km2')

# X10707_1993
X10707_1993_href <- kernelUD(SpatialPoints(X10707_1993[, 11:10]), h="href") 
image(X10707_1993_href) 
X10707_1993_href_ver <- getverticeshr(X10707_1993_href, percent=95, unin='m', unout='km2')

# X10709_1993
X10709_1993_href <- kernelUD(SpatialPoints(X10709_1993[, 11:10]), h="href") 
image(X10709_1993_href) 
X10709_1993_href_ver <- getverticeshr(X10709_1993_href, percent=95, unin='m', unout='km2')

# X11975_1994
X11975_1994_href <- kernelUD(SpatialPoints(X11975_1994[, 11:10]), h="href") 
image(X11975_1994_href) 
X11975_1994_href_ver <- getverticeshr(X11975_1994_href, percent=95, unin='m', unout='km2')

# X11975_1995
X11975_1995_href <- kernelUD(SpatialPoints(X11975_1995[, 11:10]), h="href") 
image(X11975_1995_href) 
X11975_1995_href_ver <- getverticeshr(X11975_1995_href, percent=95, unin='m', unout='km2')

# X12080_1994
X12080_1994_href <- kernelUD(SpatialPoints(X12080_1994[, 11:10]), h="href") 
image(X12080_1994_href) 
X12080_1994_href_ver <- getverticeshr(X12080_1994_href, percent=95, unin='m', unout='km2')

# X12080_1995
X12080_1995_href <- kernelUD(SpatialPoints(X12080_1995[, 11:10]), h="href") 
image(X12080_1995_href) 
X12080_1995_href_ver <- getverticeshr(X12080_1995_href, percent=95, unin='m', unout='km2')

# X12086_1995
X12086_1995_href <- kernelUD(SpatialPoints(X12086_1995[, 11:10]), h="href") 
image(X12086_1995_href) 
X12086_1995_href_ver <- getverticeshr(X12086_1995_href, percent=95, unin='m', unout='km2')

# X12092_1994
X12092_1994_href <- kernelUD(SpatialPoints(X12092_1994[, 11:10]), h="href") 
image(X12092_1994_href) 
X12092_1994_href_ver <- getverticeshr(X12092_1994_href, percent=95, unin='m', unout='km2')

# X13284_1993
X13284_1993_href <- kernelUD(SpatialPoints(X13284_1993[, 11:10]), h="href") 
image(X13284_1993_href) 
X13284_1993_href_ver <- getverticeshr(X13284_1993_href, percent=95, unin='m', unout='km2')

# X13289_1993
X13289_1993_href <- kernelUD(SpatialPoints(X13289_1993[, 11:10]), h="href") 
image(X13289_1993_href) 
X13289_1993_href_ver <- getverticeshr(X13289_1993_href, percent=95, unin='m', unout='km2')

# X13289_1994
X13289_1994_href <- kernelUD(SpatialPoints(X13289_1994[, 11:10]), h="href") 
image(X13289_1994_href) 
X13289_1994_href_ver <- getverticeshr(X13289_1994_href, percent=95, unin='m', unout='km2')

# X13292_1993
X13292_1993_href <- kernelUD(SpatialPoints(X13292_1993[, 11:10]), h="href") 
image(X13292_1993_href) 
X13292_1993_href_ver <- getverticeshr(X13292_1993_href, percent=95, unin='m', unout='km2')

# X13292_1994
X13292_1994_href <- kernelUD(SpatialPoints(X13292_1994[, 11:10]), h="href") 
image(X13292_1994_href) 
X13292_1994_href_ver <- getverticeshr(X13292_1994_href, percent=95, unin='m', unout='km2')

# X13428_1994 
X13428_1994_href <- kernelUD(SpatialPoints(X13428_1994[, 11:10]), h="href") 
image(X13428_1994_href) 
X13428_1994_href_ver <- getverticeshr(X13428_1994_href, percent=95, unin='m', unout='km2')

# X13437_1995
X13437_1995_href <- kernelUD(SpatialPoints(X13437_1995[, 11:10]), h="href") 
image(X13437_1995_href) 
X13437_1995_href_ver <- getverticeshr(X13437_1995_href, percent=95, unin='m', unout='km2')

# X13746_1994
X13746_1994_href <- kernelUD(SpatialPoints(X13746_1994[, 11:10]), h="href") 
image(X13746_1994_href) 
X13746_1994_href_ver <- getverticeshr(X13746_1994_href, percent=95, unin='m', unout='km2')

# X13746_1995
X13746_1995_href <- kernelUD(SpatialPoints(X13746_1995[, 11:10]), h="href") 
image(X13746_1995_href) 
X13746_1995_href_ver <- getverticeshr(X13746_1995_href, percent=95, unin='m', unout='km2')

# X30126_1998
X30126_1998_href <- kernelUD(SpatialPoints(X30126_1998[, 11:10]), h="href") 
image(X30126_1998_href) 
X30126_1998_href_ver <- getverticeshr(X30126_1998_href, percent=95, unin='m', unout='km2')

# X30126_1999
X30126_1999_href <- kernelUD(SpatialPoints(X30126_1999[, 11:10]), h="href") 
image(X30126_1999_href) 
X30126_1999_href_ver <- getverticeshr(X30126_1999_href, percent=95, unin='m', unout='km2')

# X30129_1998
X30129_1998_href <- kernelUD(SpatialPoints(X30129_1998[, 11:10]), h="href") 
image(X30129_1998_href) 
X30129_1998_href_ver <- getverticeshr(X30129_1998_href, percent=95, unin='m', unout='km2')

# X30129_1999
X30129_1999_href <- kernelUD(SpatialPoints(X30129_1999[, 11:10]), h="href") 
image(X30129_1999_href) 
X30129_1999_href_ver <- getverticeshr(X30129_1999_href, percent=95, unin='m', unout='km2')

# X30131_1998
X30131_1998_href <- kernelUD(SpatialPoints(X30131_1998[, 11:10]), h="href") 
image(X30131_1998_href) 
X30131_1998_href_ver <- getverticeshr(X30131_1998_href, percent=95, unin='m', unout='km2')

# X30131_1999
X30131_1999_href <- kernelUD(SpatialPoints(X30131_1999[, 11:10]), h="href") 
image(X30131_1999_href) 
X30131_1999_href_ver <- getverticeshr(X30131_1999_href, percent=95, unin='m', unout='km2')

# X30135_1999 - this is the only one that says the grid size is too small!
X30135_1999_href <- kernelUD(SpatialPoints(X30135_1999[, 11:10]), h="href") 
image(X30135_1999_href) 
X30135_1999_href_ver <- getverticeshr(X30135_1999_href, percent=95, unin='m', unout='km2')






# 8. POOLED Plotting 95% href areas for all bears with >30 fixes (UTM) ------------------------

summary(bears_href_area_df)

# plot individual bears
ggplot(data=bears_href_area_df) +
  geom_point(aes(y=ID, x=area_km2)) +
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

ggplot(data=bears_href_area_df) +
  geom_col(aes(y=ID, x=area_km2)) +
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# reordered based on area
unique(bears_href_area_df$ID)

bears_href_area_df_reordered <- bears_href_area_df %>%
  mutate(ID2=factor(ID, levels=c("X12080", "X13292","X30131", "X10700", "X13437", "X10695", "X13284", "X10707", "X13428", "X10709", "X13289", "X13746", "X10703", "X30126", "X30135", "X30129", "X30140", "X12092", "X11974", "X11975", "X12086")))

ggplot(data=bears_href_area_df_reordered) +
  geom_col(aes(y=ID2, x=area_km2)) +
  scale_x_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()


# boxplot of summary 
summary(bears_href_area_df)

bears_95href_boxplot <- ggplot(data=bears_href_area_df, aes(y=area_km2)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()

bears_95href_boxplot
bears_95href_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# 9. ANNUAL Plotting 95% href areas for all bears with >30 fixes (UTM) ------------------------

summary(bear_annualhref_area_df)


# plot individual bears
ggplot(data=bear_annualhref_area_df) +
  geom_point(aes(y=AREA_KM2, x=ID, color=YEAR)) +
  scale_y_continuous(breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000)) +
  labs(title="Annual 95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

ggplot(data=bear_annualhref_area_df) +
  geom_col(aes(y=AREA_KM2, x=ID, fill=YEAR), color="black", position="dodge") +
  scale_y_continuous(breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000)) +
  labs(title="Annual 95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Bear ID", y="Area (Km2)") +
  theme_nuwcru()

# boxplot of summary 
summary(bear_annualhref_area_df)

bear_annualhref_boxplot <- ggplot(data=bear_annualhref_area_df, aes(y=AREA_KM2)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000)) +
  labs(title="Annual 95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()

bear_annualhref_boxplot
bear_annualhref_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# average of each year
    # create summarized dataframe (avg area/year and # bears/year)
bear_annualhref_area_summarized <- bear_annualhref_area_df %>%
  group_by(YEAR) %>%
  summarize(AVG_AREA_KM2=mean(AREA_KM2), COUNT=n())

summary(bear_annualhref_area_summarized)

    # plot
ggplot(data=bear_annualhref_area_summarized, aes(x=YEAR, y=AVG_AREA_KM2)) +
  geom_line(group=1) +
  geom_point(size=2, colour="red") +
  geom_text(aes(label=COUNT), vjust=-.75) +
  scale_y_continuous(limits=c(0, 600000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000")) +
  labs(title="Annual average 95% href KDE home range areas of\nDavis Strait female polar bears", x="Year", y="Area (Km2)") +
  theme_nuwcru()

    # boxplot separated by year 
 ggplot(data=bear_annualhref_area_df) +
  geom_boxplot(aes(x=YEAR, y=AREA_KM2), varwidth = T, alpha=0.2) +
   scale_y_continuous(limits=c(0, 1700000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000", "1000000", "1100000", "1200000", "1300000", "1400000", "1500000", "1600000", "1700000")) +
   labs(title="Yearly 95% href KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()







# 10. POOLED Prepping data for mapping polygons (LAT/LONG) ------------------------

# select only points for the bears that have >30 fixes (used in bear_KDE_list above)
names(bear_KDE_list)

bears_kdepoints_greater30 <- bears_FINAL %>%
  filter(ID=="X10695" | ID=="X10700" | ID=="X10703" | ID=="X10707" | ID=="X10709" | ID=="X11974" |
           ID=="X11975" | ID=="X12080" | ID=="X12086" | ID=="X12092" | ID=="X13284" | ID=="X13289" | ID=="X13292" | 
           ID=="X13428" | ID=="X13437" | ID=="X13746" | ID=="X30126" | ID=="X30129" | ID=="X30131" | ID=="X30135" | ID=="X30140")
unique(bears_kdepoints_greater30$ID)

# converting points to correct format
bears_kdepoints_greater30$LAT <- gsub(",","",bears_kdepoints_greater30$LAT)  
bears_kdepoints_greater30$LAT <- as.numeric(bears_kdepoints_greater30$LAT)
bears_kdepoints_greater30$LONG <- as.numeric(as.character((bears_kdepoints_greater30$LONG)))

# converting polygon to correct format
# split list into items
lapply(names(bear_href_ver_latlong), function(x) assign(x, bear_href_ver_latlong[[x]], envir=.GlobalEnv))
names(bear_href_ver_latlong)
# these should all now be SpatialPolygonsDataframes that are separated from the list; check environment 

# fortify each separately (create dataframes)
X10695_fortify <- fortify(X10695)
X10700_fortify <- fortify(X10700)
X10703_fortify <- fortify(X10703)
X10707_fortify <- fortify(X10707)
X10709_fortify <- fortify(X10709)
X11974_fortify <- fortify(X11974)
X11975_fortify <- fortify(X11975)
X12080_fortify <- fortify(X12080)
X12086_fortify <- fortify(X12086)
X12092_fortify <- fortify(X12092)
X13284_fortify <- fortify(X13284)
X13289_fortify <- fortify(X13289)
X13292_fortify <- fortify(X13292)
X13428_fortify <- fortify(X13428)
X13437_fortify <- fortify(X13437)
X13746_fortify <- fortify(X13746)
X30126_fortify <- fortify(X30126)
X30129_fortify <- fortify(X30129)
X30131_fortify <- fortify(X30131)
X30135_fortify <- fortify(X30135)
X30140_fortify <- fortify(X30140)

# formatted new dfs to prep for mapping using this help pdf (see p. 15-16): https://cran.r-project.org/doc/contrib/intro-spatial-rl.pdf
head(X10695_fortify, n=2)
X10695_fortify$id <- row.names(X10695_fortify)
head(X10695_fortify@data, n=2)
X10695_fortify <- left_join(X10695_fortify, X10695@data)

head(X10700_fortify, n=2)
X10700_fortify$id <- row.names(X10700_fortify)
head(X10700_fortify@data, n=2)
X10700_fortify <- left_join(X10700_fortify, X10700@data)

head(X10703_fortify, n=2)
X10703_fortify$id <- row.names(X10703_fortify)
head(X10703_fortify@data, n=2)
X10703_fortify <- left_join(X10703_fortify, X10703@data)

head(X10707_fortify, n=2)
X10707_fortify$id <- row.names(X10707_fortify)
head(X10707_fortify@data, n=2)
X10707_fortify <- left_join(X10707_fortify, X10707@data)

head(X10709_fortify, n=2)
X10709_fortify$id <- row.names(X10709_fortify)
head(X10709_fortify@data, n=2)
X10709_fortify <- left_join(X10709_fortify, X10709@data)

head(X11974_fortify, n=2)
X11974_fortify$id <- row.names(X11974_fortify)
head(X11974_fortify@data, n=2)
X11974_fortify <- left_join(X11974_fortify, X11974@data)

head(X11975_fortify, n=2)
X11975_fortify$id <- row.names(X11975_fortify)
head(X11975_fortify@data, n=2)
X11975_fortify <- left_join(X11975_fortify, X11975@data)

head(X12080_fortify, n=2)
X12080_fortify$id <- row.names(X12080_fortify)
head(X12080_fortify@data, n=2)
X12080_fortify <- left_join(X12080_fortify, X12080@data)

head(X12086_fortify, n=2)
X12086_fortify$id <- row.names(X12086_fortify)
head(X12086_fortify@data, n=2)
X12086_fortify <- left_join(X12086_fortify, X12086@data)

head(X12092_fortify, n=2)
X12092_fortify$id <- row.names(X12092_fortify)
head(X12092_fortify@data, n=2)
X12092_fortify <- left_join(X12092_fortify, X12092@data)

head(X13284_fortify, n=2)
X13284_fortify$id <- row.names(X13284_fortify)
head(X13284_fortify@data, n=2)
X13284_fortify <- left_join(X13284_fortify, X13284@data)

head(X13289_fortify, n=2)
X13289_fortify$id <- row.names(X13289_fortify)
head(X13289_fortify@data, n=2)
X13289_fortify <- left_join(X13289_fortify, X13289@data)

head(X13292_fortify, n=2)
X13292_fortify$id <- row.names(X13292_fortify)
head(X13292_fortify@data, n=2)
X13292_fortify <- left_join(X13292_fortify, X13292@data)

head(X13428_fortify, n=2)
X13428_fortify$id <- row.names(X13428_fortify)
head(X13428_fortify@data, n=2)
X13428_fortify <- left_join(X13428_fortify, X13428@data)

head(X13437_fortify, n=2)
X13437_fortify$id <- row.names(X13437_fortify)
head(X13437_fortify@data, n=2)
X13437_fortify <- left_join(X13437_fortify, X13437@data)

head(X13746_fortify, n=2)
X13746_fortify$id <- row.names(X13746_fortify)
head(X13746_fortify@data, n=2)
X13746_fortify <- left_join(X13746_fortify, X13746@data)

head(X30126_fortify, n=2)
X30126_fortify$id <- row.names(X30126_fortify)
head(X30126_fortify@data, n=2)
X30126_fortify <- left_join(X30126_fortify, X30126@data)

head(X30129_fortify, n=2)
X30129_fortify$id <- row.names(X30129_fortify)
head(X30129_fortify@data, n=2)
X30129_fortify <- left_join(X30129_fortify, X30129@data)

head(X30131_fortify, n=2)
X30131_fortify$id <- row.names(X30131_fortify)
head(X30131_fortify@data, n=2)
X30131_fortify <- left_join(X30131_fortify, X30131@data)

head(X30135_fortify, n=2)
X30135_fortify$id <- row.names(X30135_fortify)
head(X30135_fortify@data, n=2)
X30135_fortify <- left_join(X30135_fortify, X30135@data)

head(X30140_fortify, n=2)
X30140_fortify$id <- row.names(X30140_fortify)
head(X30140_fortify@data, n=2)
X30140_fortify <- left_join(X30140_fortify, X30140@data)



# 11. POOLED Mapping 95% href KDEs for bears with >30 fixes (LAT/LONG) ------------------------
      # Note: map each bear individually (altogether is too messy with 21 polygons)
      # Note: need to first get each bears points, then plot - below sections are separated into each bear with >30 fixes (pooled across all years)

# X10695
X10695_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X10695")
X10695_points$MONTH2 <- NULL
X10695_points$MONTH2 <- factor(X10695_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X10695_points$MONTH2)

ggplot() +
  geom_point(data=X10695_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10695", x="Longitude", y="Latitude", color="Year", shape="Month")

# X10700
X10700_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X10700")
X10700_points$MONTH2 <- NULL
X10700_points$MONTH2 <- factor(X10700_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X10700_points$MONTH2)

ggplot() +
  geom_point(data=X10700_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10700_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10700", x="Longitude", y="Latitude", color="Year", shape="Month")

# X10703
X10703_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X10703")
X10703_points$MONTH2 <- NULL
X10703_points$MONTH2 <- factor(X10703_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X10703_points$MONTH2)

ggplot() +
  geom_point(data=X10703_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10703_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10703", x="Longitude", y="Latitude", color="Year", shape="Month")

# X10707
X10707_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X10707")
X10707_points$MONTH2 <- NULL
X10707_points$MONTH2 <- factor(X10707_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X10707_points$MONTH2)

ggplot() +
  geom_point(data=X10707_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10707_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10707", x="Longitude", y="Latitude", color="Year", shape="Month")
 
# X10709
X10709_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X10709")
X10709_points$MONTH2 <- NULL
X10709_points$MONTH2 <- factor(X10709_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X10709_points$MONTH2)

ggplot() +
  geom_point(data=X10709_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10709_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10709", x="Longitude", y="Latitude", color="Year", shape="Month")
 
# X11974
X11974_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X11974")
X11974_points$MONTH2 <- NULL
X11974_points$MONTH2 <- factor(X11974_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X11974_points$MONTH2)

ggplot() +
  geom_point(data=X11974_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X11974_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X11974", x="Longitude", y="Latitude", color="Year", shape="Month")
 
# X11975
X11975_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X11975")
X11975_points$MONTH2 <- NULL
X11975_points$MONTH2 <- factor(X11975_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X11975_points$MONTH2)

ggplot() +
  geom_point(data=X11975_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X11975_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X11975", x="Longitude", y="Latitude", color="Year", shape="Month")

# X12080
X12080_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X12080")
X12080_points$MONTH2 <- NULL
X12080_points$MONTH2 <- factor(X12080_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X12080_points$MONTH2)

ggplot() +
  geom_point(data=X12080_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12080_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12080", x="Longitude", y="Latitude", color="Year", shape="Month")

# X12086
X12086_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X12086")
X12086_points$MONTH2 <- NULL
X12086_points$MONTH2 <- factor(X12086_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X12086_points$MONTH2)

ggplot() +
  geom_point(data=X12086_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12086_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12086", x="Longitude", y="Latitude", color="Year", shape="Month")

# X12092
X12092_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X12092")
X12092_points$MONTH2 <- NULL
X12092_points$MONTH2 <- factor(X12092_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X12092_points$MONTH2)

ggplot() +
  geom_point(data=X12092_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12092_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12092", x="Longitude", y="Latitude", color="Year", shape="Month")

# X13284
X13284_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13284")
X13284_points$MONTH2 <- NULL
X13284_points$MONTH2 <- factor(X13284_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13284_points$MONTH2)

ggplot() +
  geom_point(data=X13284_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13284_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13284", x="Longitude", y="Latitude", color="Year", shape="Month")

# X13289
X13289_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13289")
X13289_points$MONTH2 <- NULL
X13289_points$MONTH2 <- factor(X13289_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13289_points$MONTH2)

ggplot() +
  geom_point(data=X13289_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13289", x="Longitude", y="Latitude", color="Year", shape="Month")

# X13292
X13292_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13292")
X13292_points$MONTH2 <- NULL
X13292_points$MONTH2 <- factor(X13292_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13292_points$MONTH2)

ggplot() +
  geom_point(data=X13292_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13292_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13292", x="Longitude", y="Latitude", color="Year", shape="Month")

# X13428
X13428_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13428")
X13428_points$MONTH2 <- NULL
X13428_points$MONTH2 <- factor(X13428_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13428_points$MONTH2)

ggplot() +
  geom_point(data=X13428_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13428_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13428", x="Longitude", y="Latitude", color="Year", shape="Month")

# X13437
X13437_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13437")
X13437_points$MONTH2 <- NULL
X13437_points$MONTH2 <- factor(X13437_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13437_points$MONTH2)

ggplot() +
  geom_point(data=X13437_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13437_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13437", x="Longitude", y="Latitude", color="Year", shape="Month")
 
# X13746
X13746_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X13746")
X13746_points$MONTH2 <- NULL
X13746_points$MONTH2 <- factor(X13746_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X13746_points$MONTH2)

ggplot() +
  geom_point(data=X13746_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13746", x="Longitude", y="Latitude", color="Year", shape="Month")
 
# X30126
X30126_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X30126")
X30126_points$MONTH2 <- NULL
X30126_points$MONTH2 <- factor(X30126_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X30126_points$MONTH2)

ggplot() +
  geom_point(data=X30126_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30126_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0)  + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30126", x="Longitude", y="Latitude", color="Year", shape="Month")

# X30129
X30129_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X30129")
X30129_points$MONTH2 <- NULL
X30129_points$MONTH2 <- factor(X30129_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X30129_points$MONTH2)

ggplot() +
  geom_point(data=X30129_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30129_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30129", x="Longitude", y="Latitude", color="Year", shape="Month")

# X30131
X30131_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X30131")
X30131_points$MONTH2 <- NULL
X30131_points$MONTH2 <- factor(X30131_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X30131_points$MONTH2)

ggplot() +
  geom_point(data=X30131_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30131_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30131", x="Longitude", y="Latitude", color="Year", shape="Month")

# X30135
X30135_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X30135")
X30135_points$MONTH2 <- NULL
X30135_points$MONTH2 <- factor(X30135_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X30135_points$MONTH2)

ggplot() +
  geom_point(data=X30135_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30135_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30135", x="Longitude", y="Latitude", color="Year", shape="Month")

# X30140
X30140_points <- bears_FINAL %>%
  filter(bears_FINAL$ID=="X30140")
X30140_points$MONTH2 <- NULL
X30140_points$MONTH2 <- factor(X30140_points$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(X30140_points$MONTH2)

ggplot() +
  geom_point(data=X30140_points, mapping=aes(x=LONG, y=LAT, colour=factor(YEAR), shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30140_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0)+ 
  scale_x_continuous(limits=c(-90, -50), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50")) +
  scale_y_continuous(limits=c(50, 75), breaks=c(50, 55, 60, 65, 70, 75), labels=c("50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30140", x="Longitude", y="Latitude", color="Year", shape="Month")


# 12. ANNUAL Prepping data for mapping polygons (LAT/LONG) --------------

# do all bears separately (see section 7)
names(bear_annualKDE_list)

# converte points to correct format
# create new month column
# fortify polygon into dataframe, then format new dataframe for mapping

# X10695_1991
X10695_1991$LAT <- gsub(",","",X10695_1991$LAT)  
X10695_1991$LAT <- as.numeric(X10695_1991$LAT)
X10695_1991$LONG <- as.numeric(as.character((X10695_1991$LONG)))

X10695_1991$MONTH2 <- NULL
X10695_1991$MONTH2 <- factor(X10695_1991$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10695_1991_fortify <- fortify(X10695_1991_href_ver)

# X10695_1992
X10695_1992$LAT <- gsub(",","",X10695_1992$LAT)  
X10695_1992$LAT <- as.numeric(X10695_1992$LAT)
X10695_1992$LONG <- as.numeric(as.character((X10695_1992$LONG)))

X10695_1992$MONTH2 <- NULL
X10695_1992$MONTH2 <- factor(X10695_1992$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10695_1992_fortify <- fortify(X10695_1992_href_ver)

# X10700_1993
X10700_1993$LAT <- gsub(",","",X10700_1993$LAT)  
X10700_1993$LAT <- as.numeric(X10700_1993$LAT)
X10700_1993$LONG <- as.numeric(as.character((X10700_1993$LONG)))

X10700_1993$MONTH2 <- NULL
X10700_1993$MONTH2 <- factor(X10700_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10700_1993_fortify <- fortify(X10700_1993_href_ver)

# X10700_1994
X10700_1994$LAT <- gsub(",","",X10700_1994$LAT)  
X10700_1994$LAT <- as.numeric(X10700_1994$LAT)
X10700_1994$LONG <- as.numeric(as.character((X10700_1994$LONG)))

X10700_1994$MONTH2 <- NULL
X10700_1994$MONTH2 <- factor(X10700_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10700_1994_fortify <- fortify(X10700_1994_href_ver)

# X10703_1993
X10703_1993$LAT <- gsub(",","",X10703_1993$LAT)  
X10703_1993$LAT <- as.numeric(X10703_1993$LAT)
X10703_1993$LONG <- as.numeric(as.character((X10703_1993$LONG)))

X10703_1993$MONTH2 <- NULL
X10703_1993$MONTH2 <- factor(X10703_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10703_1993_fortify <- fortify(X10703_1993_href_ver)

# X10703_1994
X10703_1994$LAT <- gsub(",","",X10703_1994$LAT)  
X10703_1994$LAT <- as.numeric(X10703_1994$LAT)
X10703_1994$LONG <- as.numeric(as.character((X10703_1994$LONG)))

X10703_1994$MONTH2 <- NULL
X10703_1994$MONTH2 <- factor(X10703_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10703_1994_fortify <- fortify(X10703_1994_href_ver)

# X10707_1992
X10707_1992$LAT <- gsub(",","",X10707_1992$LAT)  
X10707_1992$LAT <- as.numeric(X10707_1992$LAT)
X10707_1992$LONG <- as.numeric(as.character((X10707_1992$LONG)))

X10707_1992$MONTH2 <- NULL
X10707_1992$MONTH2 <- factor(X10707_1992$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10707_1992_fortify <- fortify(X10707_1992_href_ver)

# X10707_1993
X10707_1993$LAT <- gsub(",","",X10707_1993$LAT)  
X10707_1993$LAT <- as.numeric(X10707_1993$LAT)
X10707_1993$LONG <- as.numeric(as.character((X10707_1993$LONG)))

X10707_1993$MONTH2 <- NULL
X10707_1993$MONTH2 <- factor(X10707_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10707_1993_fortify <- fortify(X10707_1993_href_ver)

# X10709_1993
X10709_1993$LAT <- gsub(",","",X10709_1993$LAT)  
X10709_1993$LAT <- as.numeric(X10709_1993$LAT)
X10709_1993$LONG <- as.numeric(as.character((X10709_1993$LONG)))

X10709_1993$MONTH2 <- NULL
X10709_1993$MONTH2 <- factor(X10709_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10709_1993_fortify <- fortify(X10709_1993_href_ver)

# X11975_1994
X11975_1994$LAT <- gsub(",","",X11975_1994$LAT)  
X11975_1994$LAT <- as.numeric(X11975_1994$LAT)
X11975_1994$LONG <- as.numeric(as.character((X11975_1994$LONG)))

X11975_1994$MONTH2 <- NULL
X11975_1994$MONTH2 <- factor(X11975_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X11975_1994_fortify <- fortify(X11975_1994_href_ver)

# X11975_1995
X11975_1995$LAT <- gsub(",","",X11975_1995$LAT)  
X11975_1995$LAT <- as.numeric(X11975_1995$LAT)
X11975_1995$LONG <- as.numeric(as.character((X11975_1995$LONG)))

X11975_1995$MONTH2 <- NULL
X11975_1995$MONTH2 <- factor(X11975_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X11975_1995_fortify <- fortify(X11975_1995_href_ver)

# X12080_1994
X12080_1994$LAT <- gsub(",","",X12080_1994$LAT)  
X12080_1994$LAT <- as.numeric(X12080_1994$LAT)
X12080_1994$LONG <- as.numeric(as.character((X12080_1994$LONG)))

X12080_1994$MONTH2 <- NULL
X12080_1994$MONTH2 <- factor(X12080_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12080_1994_fortify <- fortify(X12080_1994_href_ver)

# X12080_1995
X12080_1995$LAT <- gsub(",","",X12080_1995$LAT)  
X12080_1995$LAT <- as.numeric(X12080_1995$LAT)
X12080_1995$LONG <- as.numeric(as.character((X12080_1995$LONG)))

X12080_1995$MONTH2 <- NULL
X12080_1995$MONTH2 <- factor(X12080_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12080_1995_fortify <- fortify(X12080_1995_href_ver)

# X12086_1995
X12086_1995$LAT <- gsub(",","",X12086_1995$LAT)  
X12086_1995$LAT <- as.numeric(X12086_1995$LAT)
X12086_1995$LONG <- as.numeric(as.character((X12086_1995$LONG)))

X12086_1995$MONTH2 <- NULL
X12086_1995$MONTH2 <- factor(X12086_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12086_1995_fortify <- fortify(X12086_1995_href_ver)

# X12092_1994
X12092_1994$LAT <- gsub(",","",X12092_1994$LAT)  
X12092_1994$LAT <- as.numeric(X12092_1994$LAT)
X12092_1994$LONG <- as.numeric(as.character((X12092_1994$LONG)))

X12092_1994$MONTH2 <- NULL
X12092_1994$MONTH2 <- factor(X12092_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12092_1994_fortify <- fortify(X12092_1994_href_ver)

# X13284_1993
X13284_1993$LAT <- gsub(",","",X13284_1993$LAT)  
X13284_1993$LAT <- as.numeric(X13284_1993$LAT)
X13284_1993$LONG <- as.numeric(as.character((X13284_1993$LONG)))

X13284_1993$MONTH2 <- NULL
X13284_1993$MONTH2 <- factor(X13284_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13284_1993_fortify <- fortify(X13284_1993_href_ver)

# X13289_1993
X13289_1993$LAT <- gsub(",","",X13289_1993$LAT)  
X13289_1993$LAT <- as.numeric(X13289_1993$LAT)
X13289_1993$LONG <- as.numeric(as.character((X13289_1993$LONG)))

X13289_1993$MONTH2 <- NULL
X13289_1993$MONTH2 <- factor(X13289_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13289_1993_fortify <- fortify(X13289_1993_href_ver)

# X13289_1994
X13289_1994$LAT <- gsub(",","",X13289_1994$LAT)  
X13289_1994$LAT <- as.numeric(X13289_1994$LAT)
X13289_1994$LONG <- as.numeric(as.character((X13289_1994$LONG)))

X13289_1994$MONTH2 <- NULL
X13289_1994$MONTH2 <- factor(X13289_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13289_1994_fortify <- fortify(X13289_1994_href_ver)

# X13292_1993
X13292_1993$LAT <- gsub(",","",X13292_1993$LAT)  
X13292_1993$LAT <- as.numeric(X13292_1993$LAT)
X13292_1993$LONG <- as.numeric(as.character((X13292_1993$LONG)))

X13292_1993$MONTH2 <- NULL
X13292_1993$MONTH2 <- factor(X13292_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13292_1993_fortify <- fortify(X13292_1993_href_ver)

# X13292_1994
X13292_1994$LAT <- gsub(",","",X13292_1994$LAT)  
X13292_1994$LAT <- as.numeric(X13292_1994$LAT)
X13292_1994$LONG <- as.numeric(as.character((X13292_1994$LONG)))

X13292_1994$MONTH2 <- NULL
X13292_1994$MONTH2 <- factor(X13292_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13292_1994_fortify <- fortify(X13292_1994_href_ver)

# X13428_1994
X13428_1994$LAT <- gsub(",","",X13428_1994$LAT)  
X13428_1994$LAT <- as.numeric(X13428_1994$LAT)
X13428_1994$LONG <- as.numeric(as.character((X13428_1994$LONG)))

X13428_1994$MONTH2 <- NULL
X13428_1994$MONTH2 <- factor(X13428_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13428_1994_fortify <- fortify(X13428_1994_href_ver)

# X13437_1995
X13437_1995$LAT <- gsub(",","",X13437_1995$LAT)  
X13437_1995$LAT <- as.numeric(X13437_1995$LAT)
X13437_1995$LONG <- as.numeric(as.character((X13437_1995$LONG)))

X13437_1995$MONTH2 <- NULL
X13437_1995$MONTH2 <- factor(X13437_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13437_1995_fortify <- fortify(X13437_1995_href_ver)

# X13746_1994
X13746_1994$LAT <- gsub(",","",X13746_1994$LAT)  
X13746_1994$LAT <- as.numeric(X13746_1994$LAT)
X13746_1994$LONG <- as.numeric(as.character((X13746_1994$LONG)))

X13746_1994$MONTH2 <- NULL
X13746_1994$MONTH2 <- factor(X13746_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13746_1994_fortify <- fortify(X13746_1994_href_ver)

# X13746_1995
X13746_1995$LAT <- gsub(",","",X13746_1995$LAT)  
X13746_1995$LAT <- as.numeric(X13746_1995$LAT)
X13746_1995$LONG <- as.numeric(as.character((X13746_1995$LONG)))

X13746_1995$MONTH2 <- NULL
X13746_1995$MONTH2 <- factor(X13746_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13746_1995_fortify <- fortify(X13746_1995_href_ver)

# X30126_1998
X30126_1998$LAT <- gsub(",","",X30126_1998$LAT)  
X30126_1998$LAT <- as.numeric(X30126_1998$LAT)
X30126_1998$LONG <- as.numeric(as.character((X30126_1998$LONG)))

X30126_1998$MONTH2 <- NULL
X30126_1998$MONTH2 <- factor(X30126_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30126_1998_fortify <- fortify(X30126_1998_href_ver)

# X30126_1999
X30126_1999$LAT <- gsub(",","",X30126_1999$LAT)  
X30126_1999$LAT <- as.numeric(X30126_1999$LAT)
X30126_1999$LONG <- as.numeric(as.character((X30126_1999$LONG)))

X30126_1999$MONTH2 <- NULL
X30126_1999$MONTH2 <- factor(X30126_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30126_1999_fortify <- fortify(X30126_1999_href_ver)

# X30129_1998
X30129_1998$LAT <- gsub(",","",X30129_1998$LAT)  
X30129_1998$LAT <- as.numeric(X30129_1998$LAT)
X30129_1998$LONG <- as.numeric(as.character((X30129_1998$LONG)))

X30129_1998$MONTH2 <- NULL
X30129_1998$MONTH2 <- factor(X30129_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30129_1998_fortify <- fortify(X30129_1998_href_ver)

# X30129_1999
X30129_1999$LAT <- gsub(",","",X30129_1999$LAT)  
X30129_1999$LAT <- as.numeric(X30129_1999$LAT)
X30129_1999$LONG <- as.numeric(as.character((X30129_1999$LONG)))

X30129_1999$MONTH2 <- NULL
X30129_1999$MONTH2 <- factor(X30129_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30129_1999_fortify <- fortify(X30129_1999_href_ver)

# X30131_1998
X30131_1998$LAT <- gsub(",","",X30131_1998$LAT)  
X30131_1998$LAT <- as.numeric(X30131_1998$LAT)
X30131_1998$LONG <- as.numeric(as.character((X30131_1998$LONG)))

X30131_1998$MONTH2 <- NULL
X30131_1998$MONTH2 <- factor(X30131_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30131_1998_fortify <- fortify(X30131_1998_href_ver)

# X30131_1999
X30131_1999$LAT <- gsub(",","",X30131_1999$LAT)  
X30131_1999$LAT <- as.numeric(X30131_1999$LAT)
X30131_1999$LONG <- as.numeric(as.character((X30131_1999$LONG)))

X30131_1999$MONTH2 <- NULL
X30131_1999$MONTH2 <- factor(X30131_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30131_1999_fortify <- fortify(X30131_1999_href_ver)

# X30135_1999
      # this one won't work because of the grid error above


# 13. ANNUAL Mapping 95% href KDEs (LAT/LONG) ----------------

# maps per bear_year

# X10695_1991
ggplot() +
  geom_point(data=X10695_1991, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_1991_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10695 in 1991", x="Longitude", y="Latitude", color="Year", shape="Month")

# X10695_1992
ggplot() +
  geom_point(data=X10695_1992, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_1992_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10695 in 1992", x="Longitude", y="Latitude", shape="Month")

# X10700_1993
ggplot() +
  geom_point(data=X10700_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10700_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10700 in 1993", x="Longitude", y="Latitude", shape="Month")

# X10700_1994
ggplot() +
  geom_point(data=X10700_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10700_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10700 in 1994", x="Longitude", y="Latitude", shape="Month")

# X10703_1993
ggplot() +
  geom_point(data=X10703_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10703_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10703 in 1993", x="Longitude", y="Latitude", shape="Month")

# X10703_1994
ggplot() +
  geom_point(data=X10703_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10703_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10703 in 1994", x="Longitude", y="Latitude", shape="Month")

# X10707_1992
ggplot() +
  geom_point(data=X10707_1992, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10707_1992_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10707 in 1992", x="Longitude", y="Latitude", shape="Month")

# X10707_1993
ggplot() +
  geom_point(data=X10707_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10707_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10707 in 1993", x="Longitude", y="Latitude", shape="Month")

# X10709_1993
ggplot() +
  geom_point(data=X10709_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10709_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X10709 in 1993", x="Longitude", y="Latitude", shape="Month")

# X11975_1994
ggplot() +
  geom_point(data=X11975_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X11975_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X11975 in 1994", x="Longitude", y="Latitude", shape="Month")

# X11975_1995
ggplot() +
  geom_point(data=X11975_1995, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X11975_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X11975 in 1995", x="Longitude", y="Latitude", shape="Month")

# X12080_1994
ggplot() +
  geom_point(data=X12080_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12080_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12080 in 1994", x="Longitude", y="Latitude", shape="Month")

# X12080_1995
ggplot() +
  geom_point(data=X12080_1995, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12080_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12080 in 1995", x="Longitude", y="Latitude", shape="Month")

# X12086_1995
ggplot() +
  geom_point(data=X12086_1995, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12086_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12086 in 1995", x="Longitude", y="Latitude", shape="Month")

# X12092_1994
ggplot() +
  geom_point(data=X12092_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12092_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X12092 in 1994", x="Longitude", y="Latitude", shape="Month")

# X13284_1993
ggplot() +
  geom_point(data=X13284_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13284_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13284 in 1993", x="Longitude", y="Latitude", shape="Month")

# X13289_1993
ggplot() +
  geom_point(data=X13289_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13289 in 1993", x="Longitude", y="Latitude", shape="Month")

# X13289_1994
ggplot() +
  geom_point(data=X13289_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13289 in 1994", x="Longitude", y="Latitude", shape="Month")

# X13292_1993
ggplot() +
  geom_point(data=X13292_1993, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13292_1993_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13292 in 1993", x="Longitude", y="Latitude", shape="Month")

# X13292_1994
ggplot() +
  geom_point(data=X13292_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13292_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13292 in 1994", x="Longitude", y="Latitude", shape="Month")

# X13428_1994
ggplot() +
  geom_point(data=X13428_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13428_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13428 in 1994", x="Longitude", y="Latitude", shape="Month")

# X13437_1995
ggplot() +
  geom_point(data=X13437_1995, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13437_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13437 in 1995", x="Longitude", y="Latitude", shape="Month")

# X13746_1994
ggplot() +
  geom_point(data=X13746_1994, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_1994_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13746 in 1994", x="Longitude", y="Latitude", shape="Month")

# X13746_1995
ggplot() +
  geom_point(data=X13746_1995, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_1995_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X13746 in 1995", x="Longitude", y="Latitude", shape="Month")

# X30126_1998
ggplot() +
  geom_point(data=X30126_1998, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30126_1998_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30126 in 1998", x="Longitude", y="Latitude", shape="Month")

# X30126_1999
ggplot() +
  geom_point(data=X30126_1999, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30126_1999_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30126 in 1999", x="Longitude", y="Latitude", shape="Month")

# X30129_1998
ggplot() +
  geom_point(data=X30129_1998, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30129_1998_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30129 in 1998", x="Longitude", y="Latitude", shape="Month")

# X30129_1999
ggplot() +
  geom_point(data=X30129_1999, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30129_1999_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30129 in 1999", x="Longitude", y="Latitude", shape="Month")

# X30131_1998
ggplot() +
  geom_point(data=X30131_1998, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30131_1998_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30131 in 1998", x="Longitude", y="Latitude", shape="Month")

# X30131_1999
ggplot() +
  geom_point(data=X30131_1999, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30131_1999_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30131 in 1999", x="Longitude", y="Latitude", shape="Month")

# X30135_1999
    # no map for this one because of the grid error above



# maps per year

names(bear_annualKDE_list)

# 1991
    # just one bear: X10695_1991
ggplot() +
  geom_point(data=X10695_1991, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_1991_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1991", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")
  
# 1992: X10695_1992, X10707_1992
      # combine points together first
bears_1992 <- rbind(X10695_1992, X10707_1992)
ggplot() +
  geom_point(data=bears_1992, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000", "#3399FF")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10695_1992_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X10707_1992_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1992", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

# 1993: X10700_1993, X10703_1993, X10707_1993, X10709_1993, X13284_1993, X13289_1993, X13292_1993
bears_1993_1 <- rbind(X10707_1993, X13284_1993, X13292_1993)
bears_1993_2 <- rbind(X10703_1993, X13284_1993, X10709_1993)
bears_1993_3 <- rbind(X10700_1993, X13289_1993)

    #1
ggplot() +
  geom_point(data=bears_1993_1, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000", "#3399FF", "#339900")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10707_1993_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X13284_1993_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X13292_1993_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1993", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

    #2
ggplot() +
  geom_point(data=bears_1993_2, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10703_1993_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X13284_1993_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X10709_1993_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1993", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

    # 3
ggplot() +
  geom_point(data=bears_1993_3, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000", "#3399FF")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10700_1993_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X13289_1993_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) +
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1993", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")


# 1994: X10700_1994, X10703_1994, X11975_1994, X12080_1994, X12092_1994, X13289_1994, X13292_1994, X13428_1994, X13746_1994
bears_1994_1 <- rbind(X13428_1994, X11975_1994, X12080_1994)
bears_1994_2 <- rbind(X10700_1994, X12092_1994, X13746_1994)
bears_1994_3 <- rbind(X13289_1994, X13292_1994, X10703_1994)

#1
ggplot() +
  geom_point(data=bears_1994_1, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000", "#3399FF", "#339900")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13428_1994_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X11975_1994_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X12080_1994_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1994", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

#2
ggplot() +
  geom_point(data=bears_1994_2, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X10700_1994_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X12092_1994_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X13746_1994_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1994", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

# 3
ggplot() +
  geom_point(data=bears_1994_3, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13289_1994_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X13292_1994_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X10703_1994_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1994", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")


# 1995: X11975_1995, X12080_1995, X12086_1995, X13437_1995, X13746_1995

bears_1995_1 <- rbind(X12080_1995, X12086_1995)
bears_1995_2 <- rbind(X11975_1995, X13437_1995, X13746_1995)

#1
ggplot() +
  geom_point(data=bears_1995_1, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#FF0000", "#3399FF")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X12080_1995_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  geom_polygon(data=X12086_1995_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1995", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")

#2
ggplot() +
  geom_point(data=bears_1995_2, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X11975_1995_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X13437_1995_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X13746_1995_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1995", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")


# 1998: X30126_1998, X30129_1998, X30131_1998
bears_1998 <- rbind(X30126_1998, X30129_1998, X30131_1998)

#1
ggplot() +
  geom_point(data=bears_1998, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30126_1998_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X30129_1998_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X30131_1998_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1998", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")


# 1999: X30126_1999, X30129_1999, X30131_1999, X30135_1999 (except it didn't work above, so don't use this last one)
bears_1999 <- rbind(X30126_1999, X30129_1999, X30131_1999)

#1
ggplot() +
  geom_point(data=bears_1999, mapping=aes(x=LONG, y=LAT, colour=ID, shape=factor(MONTH2))) +
  scale_color_manual(values=c("#3399FF", "#339900", "#FF0000")) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30126_1999_fortify, aes(long, lat, group=group), size=.2, color="#3399FF", alpha=0) + 
  geom_polygon(data=X30129_1999_fortify, aes(long, lat, group=group), size=.2, color="#339900", alpha=0) + 
  geom_polygon(data=X30131_1999_fortify, aes(long, lat, group=group), size=.2, color="#FF0000", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="href KDE home ranges for\nfemale collared polar bears in 1999", x="Longitude", y="Latitude", colour="Bear ID", shape="Month")



# 14. Testing kernels with single bears -------------
# Used Tetreault & Franke (2017)


# Using hlscv (package: adehabitatHR)
X13292_hlscv <- kernelUD(SpatialPoints(X13292[, 12:13]), h="LSCV") 
plotLSCV(X13292_hlscv)
# No error for bears X03956, X10393, X10703, X10707, X11974, X11975, X12082, X12086, X13284, X13437, X30140, X12081, X30135 (total=13)
# Convergence errors for bears X10374, X12078, X30129, X10695, X10700, X10709, X12080, X12083, X12092, X13289, X13428, X13746, X30126, X30131, X13292 (total=15)

# ASK ALASTAIR IF I SHOULD STILL INCLUDE THE BEARS THAT IT DID WORK FOR

# Using href (package: adehabitatHR)

# first needed to expand the grid size because of the error message I kept getting (grid size was too small) for bears X12083, X12082, and X12078
# set Domain
x <- seq(0, 100, by=1.)
y <- seq(0, 100, by=1.)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

# increase Domain
x <- seq(-3000, 3100, by=1.)
y <- seq(-3000, 3100, by=1.)
xy <- expand.grid(x=x, y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

X12083_href <- kernelUD(SpatialPoints(X12083[, 12:13]), h="href", grid=xy) # if "grid=xy" is not included here, up to 70% in line 221 works
image(X12083_href) # this is the image I don't understand (included this because of the forum)
X12083_href_ver <- getverticeshr(X12083_href, percent=95, unin='m', unout='m2') # this is the line that causes error if grid size is too small
gArea(X12083_href_ver) # in m2 - doesn't work when line above causes error

# test plot
plot(X12083_href_ver) # this just shows the HR outline (no contours yet)
points(x=X12083$EASTING, y=X12083$NORTHING)




# No error for 25 bears X03956, X10393, X10703, X10707, X11974, X10695, X10709, X12092, X11975, X30140, X30135, X30129, X30131, X30126, X10700, X13284, X13292, X13289, X12081, X13428, X13437, X12086, X10374, X12080, X13746
# Errors for X12083, X12082, and X12078: "The grid is too small to allow the estimation of home-range"
# No error for X12081, but line 207 gives actual values for ud column, and it doesn't for the rest

# test plot
plot(X13746_href_ver)



# test plot using ggplot
X13746_href_df <- fortify(X13746_href_ver) # lat/long columns were correctly labelled this time

X13746_href_plot <- ggplot() +
  geom_point(data=X13746, mapping=aes(x=EASTING, y=NORTHING, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_href_df, aes(long, lat), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% reference kernel density)\nof bear X13289 in Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month")
X13746_href_plot 


# plot using ggmap

ds_bbox <- make_bbox(X13746$EASTING, X13746$NORTHING, f=0.05)
ds_basemap <- get_map(location=ds_bbox, maptype="satellite", source="google", color="color") # this line won't work

X13746_href_map <- ggmap(ds_basemap) +
  geom_point(data=X13746, mapping=aes(x=EASTING, y=NORTHING, colour=factor(YEAR), shape=MONTH2)) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X13746_href_df, aes(long, lat), size=.2, color="black", alpha=0) + 
  labs(title="Home range (95% reference kernel density)\nof bear X13289 in Davis Strait", x="Longitude",
       y="Latitude", color="Year", shape="Month")
X13746_href_map









