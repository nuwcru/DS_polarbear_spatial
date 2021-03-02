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

# 2. import bear dataset (made in 03_MCP_HomeRanges.R) and create lists  -----------

bears_FINAL <- read.csv("data/bears_FINAL.csv") 
head(bears_FINAL)

# create the same pooled and annual lists as for the other KDEs (see 04_Kernel_LSCV_HREF_HomeRange.R)

# pooled list (i.e. individual bears pooled across years)
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

    # only use the bears with >30 fixes across all years
bear_KDE_list <- list(X10695, X10700, X10703, X10707, X10709, X11974, X11975, X12080, X12086, X12092, X13284, X13289, X13292, X13428, X13437, X13746, X30126, X30129, X30131, X30135, X30140)

    # name items in list
names(bear_KDE_list) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_KDE_list$X13289) # check that it worked
names(bear_KDE_list) 




# annual list
    # Note: see "DScollardata.xlsx" for how I got these years/bears
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

    # make a list with only the years that a bear has >30 fixes 
bear_annualKDE_list <- list(X10695_1991, X10695_1992, X10700_1993, X10700_1994, X10703_1993, X10703_1994, X10707_1992, X10707_1993, X10709_1993, X11975_1994, X11975_1995, X12080_1994, X12080_1995, X12086_1995, X12092_1994, X13284_1993, X13289_1993, X13289_1994, X13292_1993, X13292_1994, X13428_1994, X13437_1995, X13746_1994, X13746_1995, X30126_1998, X30126_1999, X30129_1998, X30129_1999, X30131_1998, X30131_1999, X30135_1999)

    # name items in list
names(bear_annualKDE_list) <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
head(bear_annualKDE_list$X10695_1991) # check that it worked
names(bear_annualKDE_list) 


# 3. POOLED YEARS - loops for hpi KDE (amse pilot only) for bears with >30 fixes (LAT/LONG) --------------

head(bear_KDE_list$X10695)

# Make empty list and then create samse pilot with loop
bear_hpi_amsepilot <- list()

for(i in 1:length(bear_KDE_list)){
  temp_dat <- bear_KDE_list[[i]]
  bear_hpi_amsepilot[[i]] <- Hpi(temp_dat[, 11:10], pilot="amse", binned=T) # this is where I specify which pilot I'm using (i.e. amse)
} # I think this worked


# Make empty list and then make samse contours
bear_polygon <- list()

for(i in 1:length(bear_hpi_amsepilot)){
#i=10
    bear_hpi_amse_contour <- kde(bear_KDE_list[[i]][11:10], H=bear_hpi_amsepilot[[i]]) 
    bear_contourlevels <- contourLevels(bear_hpi_amse_contour, cont = 95)               
    bear_lines <- contourLines(x=bear_hpi_amse_contour$eval.points[[1]], y=bear_hpi_amse_contour$eval.points[[2]], 
                               z=bear_hpi_amse_contour$estimate, level=bear_contourlevels) 
    bear_sldf = ContourLines2SLDF(bear_lines)  
    bear_polyset = SpatialLines2PolySet(bear_sldf) 
    bear_polygon[[i]] = PolySet2SpatialPolygons(bear_polyset) 
    proj4string(bear_polygon[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
} 

plot(bear_polygon[[21]]) # use [[1]] and X10695 below and it looks correct - if you use what's here, though, the points are all to the R of the polygon
points(x=X30140$LONG, y=X30140$LAT)  

# name items in list (same order as bear_KDE_list)
names(bear_polygon) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_polygon$X13289) # check that it worked
names(bear_polygon) 

# test plot pilot
plot(bear_polygon$X10700)
points(x=X10700$LONG, y=X10700$LAT) # it works!


# 4. POOLED YEARS - loops for hpi KDE (amse pilot only) for bears with >30 fixes (UTM) --------------
head(bear_KDE_list$X10695)

# Make empty list and then create samse pilot with loop
bear_hpi_amsepilot_utm <- list()

for(i in 1:length(bear_KDE_list)){
  temp_dat <- bear_KDE_list[[i]]
  bear_hpi_amsepilot_utm[[i]] <- Hpi(temp_dat[, 13:14], pilot="amse", binned=T) 
} 

# Make empty list and then make samse contours
bear_polygon_utm <- list()

for(i in 1:length(bear_hpi_amsepilot_utm)){
  #i=10
  bear_hpi_amse_contour_utm <- kde(bear_KDE_list[[i]][13:14], H=bear_hpi_amsepilot_utm[[i]]) 
  bear_contourlevels_utm <- contourLevels(bear_hpi_amse_contour_utm, cont = 95)               
  bear_lines_utm <- contourLines(x=bear_hpi_amse_contour_utm$eval.points[[1]], y=bear_hpi_amse_contour_utm$eval.points[[2]], 
                             z=bear_hpi_amse_contour_utm$estimate, level=bear_contourlevels_utm) 
  bear_sldf_utm = ContourLines2SLDF(bear_lines_utm)  
  bear_polyset_utm = SpatialLines2PolySet(bear_sldf_utm) 
  bear_polygon_utm[[i]] = PolySet2SpatialPolygons(bear_polyset_utm) # note: I tested this loop w/ and w/out the CRS line and it comes up with the same areas regardless - I removed it because the CRS I used above isn't correct with UTM coordinates
} 

# name items in list (same order as bear_KDE_list)
names(bear_polygon_utm) <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")
head(bear_polygon_utm$X13289) # check that it worked
names(bear_polygon_utm) 

# test area
gArea(bear_polygon_utm[[21]])
gArea(bear_polygon_utm$X30140)

# create empty dataframe to put in the areas and fill first row with IDs
bear_hpi_area_df <- data.frame(ID = rep(NA, length(bear_KDE_list)))
bear_hpi_area_df$ID <- c("X10695", "X10700", "X10703", "X10707", "X10709", "X11974", "X11975", "X12080", "X12086", "X12092", "X13284", "X13289", "X13292", "X13428", "X13437", "X13746", "X30126", "X30129", "X30131", "X30135", "X30140")

# Make an empty list
bear_hpi_area <- list()

# create loop to calculate the areas
for(i in 1:length(bear_polygon_utm)){
  bear_hpi_area[[i]] <- gArea(bear_polygon_utm[[i]])
}

# add values into new dataframe
bear_hpi_area_df$AREA <- bear_hpi_area
str(bear_hpi_area_df)
bear_hpi_area_df$AREA <- as.numeric(bear_hpi_area_df$AREA)

# make a new column that is km2 (previous was m2) and make into a spreadsheet
bear_hpi_area_df$AREA_KM2 <- bear_hpi_area_df$AREA*0.000001
summary(bear_hpi_area_df)

write.csv(bear_hpi_area_df, "data/bear_hpi_area_df.csv")


# 5. POOLED YEARS - Plotting 95% hpi areas for all bears with >30 fixes (UTM) ------------------------

summary(bear_hpi_area_df)

# plot individual bears
ggplot(data=bear_hpi_area_df) +
  geom_point(aes(y=ID, x=AREA_KM2)) +
  scale_x_continuous(breaks=c(100000, 150000, 200000, 250000, 300000, 350000, 400000), labels=c("100000", "150000", "200000", "250000", "300000", "350000", "400000")) +
  labs(title="95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

ggplot(data=bear_hpi_area_df) +
  geom_col(aes(y=ID, x=AREA_KM2)) +
  scale_x_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# reordered based on area
unique(bear_hpi_area_df$ID)

bear_hpi_area_df_reordered <- bear_hpi_area_df %>%
  mutate(ID2=factor(ID, levels=c("X12080", "X13292", "X10700", "X10695", "X13284", "X30131", "X13437", "X13289", "X10707", "X10703", "X10709", "X30135", "X13428", "X13746", "X30140", "X30129", "X30126", "X11974", "X12092", "X11975", "X12086")))

ggplot(data=bear_hpi_area_df_reordered) +
  geom_col(aes(y=ID2, x=AREA_KM2)) +
  scale_x_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

# boxplot of summary 
summary(bear_hpi_area_df)

bear_hpi_boxplot <- ggplot(data=bear_hpi_area_df, aes(y=AREA_KM2)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1050000), breaks=c(0, 50000, 100000, 150000, 200000, 250000, 300000, 350000, 400000, 450000, 500000, 550000, 600000, 650000, 700000, 750000, 800000, 850000, 900000, 950000, 1000000, 1050000)) +
  labs(title="95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()

bear_hpi_boxplot
bear_hpi_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

# 6. POOLED YEARS - Prepping 95% hpi data for mapping polygons (LAT/LONG) ------------------------

# select only points for the bears that have >30 fixes (used in bear_KDE_list above)
names(bear_KDE_list)

bears_kdepoints_greater30 <- bears_FINAL %>%
  filter(ID=="X10695" | ID=="X10700" | ID=="X10703" | ID=="X10707" | ID=="X10709" | ID=="X11974" |
           ID=="X11975" | ID=="X12080" | ID=="X12086" | ID=="X12092" | ID=="X13284" | ID=="X13289" | ID=="X13292" | 
           ID=="X13428" | ID=="X13437" | ID=="X13746" | ID=="X30126" | ID=="X30129" | ID=="X30131" | ID=="X30135" | ID=="X30140")
unique(bears_kdepoints_greater30$ID) # these should match what's in bear_KDE_list

# converting points to correct format
bears_kdepoints_greater30$LAT <- gsub(",","",bears_kdepoints_greater30$LAT)  
bears_kdepoints_greater30$LAT <- as.numeric(bears_kdepoints_greater30$LAT)
bears_kdepoints_greater30$LONG <- as.numeric(as.character((bears_kdepoints_greater30$LONG)))

# converting polygon to correct format (use polygon made in lat/long, not utm)
# split list into items
lapply(names(bear_polygon), function(x) assign(x, bear_polygon[[x]], envir=.GlobalEnv))
names(bear_polygon)
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




# 7. POOLED YEARS - Mapping 95% hpi KDEs for bears with >30 fixes (LAT/LONG; grid background) ------------------------
# Note: map each bear individually (altogether is too messy with 21 polygons)
# Note: need to first get each bears points, then plot - below sections are separated into each bear with >30 fixes (pooled across all years)

# X10695
X10695_points <- bears_FINAL %>%
  filter(bears_kdepoints_greater30$ID=="X10695")
bears_kdepoints_greater30$MONTH2 <- NULL
bears_kdepoints_greater30$MONTH2 <- factor(bears_kdepoints_greater30$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
unique(bears_kdepoints_greater30$MONTH2)

plot(bear_polygon$X10695)
points(x=X10695_points$LONG, y=X10695_points$LAT)

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

plot(bear_polygon$X10700)
points(x=X10700_points$LONG, y=X10700_points$LAT)

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

plot(bear_polygon$X10703)
points(x=X10703_points$LONG, y=X10703_points $LAT) 

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



# 8. ANNUAL - loops for hpi KDE (amse pilot only) for bears with >30 fixes (LAT/LONG) --------------

head(bear_annualKDE_list$X10695_1991)

# Make empty list and then create samse pilot with loop
bear_annualhpi_amsepilot <- list()

for(i in 1:length(bear_annualKDE_list)){
  temp_dat <- bear_annualKDE_list[[i]]
  bear_annualhpi_amsepilot[[i]] <- Hpi(temp_dat[, 11:10], pilot="amse", binned=T) 
} 

# Make empty list and then make samse contours
bear_annualpolygon <- list()

for(i in 1:length(bear_annualhpi_amsepilot)){
  bear_annualhpi_amse_contour <- kde(bear_annualKDE_list[[i]][11:10], H=bear_annualhpi_amsepilot[[i]]) 
  bear_annualcontourlevels <- contourLevels(bear_annualhpi_amse_contour, cont = 95)               
  bear_annuallines <- contourLines(x=bear_annualhpi_amse_contour$eval.points[[1]], y=bear_annualhpi_amse_contour$eval.points[[2]], 
                             z=bear_annualhpi_amse_contour$estimate, level=bear_annualcontourlevels) 
  bear_annualsldf = ContourLines2SLDF(bear_annuallines)  
  bear_annualpolyset = SpatialLines2PolySet(bear_annualsldf) 
  bear_annualpolygon[[i]] = PolySet2SpatialPolygons(bear_annualpolyset) 
  proj4string(bear_annualpolygon[[i]]) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
} 

# name items in list (same order as bear_annualKDE_list)
names(bear_annualpolygon) <- c("X10695_1991_hpi", "X10695_1992_hpi", "X10700_1993_hpi", "X10700_1994_hpi", "X10703_1993_hpi", "X10703_1994_hpi", "X10707_1992_hpi", "X10707_1993_hpi", "X10709_1993_hpi", "X11975_1994_hpi", "X11975_1995_hpi", "X12080_1994_hpi", "X12080_1995_hpi", "X12086_1995_hpi", "X12092_1994_hpi", "X13284_1993_hpi", "X13289_1993_hpi", "X13289_1994_hpi", "X13292_1993_hpi", "X13292_1994_hpi", "X13428_1994_hpi", "X13437_1995_hpi", "X13746_1994_hpi", "X13746_1995_hpi", "X30126_1998_hpi", "X30126_1999_hpi", "X30129_1998_hpi", "X30129_1999_hpi", "X30131_1998_hpi", "X30131_1999_hpi", "X30135_1999_hpi")
    # added "_hpi" at the end to differenciate between the polygons and points dataframes for mapping later
head(bear_annualpolygon$X10695_1991) # check that it worked
names(bear_annualpolygon) 

# test plot
plot(bear_annualpolygon[[10]]) 
points(x=X11975_1994$LONG, y=X11975_1994$LAT) # this works!



# 9. ANNUAL - loops for hpi KDE (amse pilot only) for bears with >30 fixes (UTM) --------------

head(bear_annualKDE_list$X10695_1991)

# Make empty list and then create amse pilot with loop
bear_annualhpi_amsepilot_utm <- list()

for(i in 1:length(bear_annualKDE_list)){
  temp_dat <- bear_annualKDE_list[[i]]
  bear_annualhpi_amsepilot_utm[[i]] <- Hpi(temp_dat[, 13:14], pilot="amse", binned=T) 
} 

# Make empty list and then make amse contours
bear_annualpolygon_utm <- list()

for(i in 1:length(bear_annualhpi_amsepilot_utm)){
  bear_annualhpi_amse_contour_utm <- kde(bear_annualKDE_list[[i]][13:14], H=bear_annualhpi_amsepilot_utm[[i]]) 
  bear_annualcontourlevels_utm <- contourLevels(bear_annualhpi_amse_contour_utm, cont = 95)               
  bear_annuallines_utm <- contourLines(x=bear_annualhpi_amse_contour_utm$eval.points[[1]], y=bear_annualhpi_amse_contour_utm$eval.points[[2]], 
                                   z=bear_annualhpi_amse_contour_utm$estimate, level=bear_annualcontourlevels_utm) 
  bear_annualsldf_utm = ContourLines2SLDF(bear_annuallines_utm)  
  bear_annualpolyset_utm = SpatialLines2PolySet(bear_annualsldf_utm) 
  bear_annualpolygon_utm[[i]] = PolySet2SpatialPolygons(bear_annualpolyset_utm) # note: CRS isn't required here, and the error message below pops up regardless of if I set the CRS or not
} 

# name items in list (same order as bear_annualKDE_list)
names(bear_annualpolygon_utm) <- c("X10695_1991_poly", "X10695_1992_poly", "X10700_1993_poly", "X10700_1994_poly", "X10703_1993_poly", "X10703_1994_poly", "X10707_1992_poly", "X10707_1993_poly", "X10709_1993_poly", "X11975_1994_poly", "X11975_1995_poly", "X12080_1994_poly", "X12080_1995_poly", "X12086_1995_poly", "X12092_1994_poly", "X13284_1993_poly", "X13289_1993_poly", "X13289_1994_poly", "X13292_1993_poly", "X13292_1994_poly", "X13428_1994_poly", "X13437_1995_poly", "X13746_1994_poly", "X13746_1995_poly", "X30126_1998_poly", "X30126_1999_poly", "X30129_1998_poly", "X30129_1999_poly", "X30131_1998_poly", "X30131_1999_poly", "X30135_1999_poly")
head(bear_annualpolygon_utm$X10695_1991_poly) # check that it worked - should be SpatialPolygons class
names(bear_annualpolygon_utm) 

# test area
gArea(bear_annualpolygon_utm[[6]]) # tested #1-31, error message for number 6 only (X10703_1994_poly)!
      # error for #6: "error in createpolygonscomment(p): rgeos_polycreatecomment: orphaned hole, cannot find containing polygon for hole at index 3"

# because of the error message above, I could not get the loop below to work
# I redid this whole section below without #6 - see section 10 below

# create empty dataframe to put in the areas and fill first row with IDs
bear_hpi_annualarea_df <- data.frame(ID_YEAR=rep(NA, length(bear_annualKDE_list)), ID = rep(NA, length(bear_annualKDE_list)), YEAR = rep(NA, length(bear_annualKDE_list)), AREA = rep(NA, length(bear_annualKDE_list)))
bear_hpi_annualarea_df$ID_YEAR <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bear_hpi_annualarea_df$ID <- c("X10695", "X10695", "X10700", "X10700", "X10703", "X10703", "X10707", "X10707", "X10709", "X11975", "X11975", "X12080", "X12080", "X12086", "X12092", "X13284", "X13289", "X13289", "X13292", "X13292", "X13428", "X13437", "X13746", "X13746", "X30126", "X30126", "X30129", "X30129", "X30131", "X30131", "X30135")
bear_hpi_annualarea_df$YEAR <- c("1991", "1992", "1993", "1994", "1993", "1994", "1992", "1993", "1993", "1994", "1995", "1994", "1995", "1995", "1994", "1993", "1993", "1994", "1993", "1994", "1994", "1995", "1994", "1995", "1998", "1999", "1998", "1999", "1998", "1999", "1999")

# Make an empty list
bear_hpi_annualarea <- list()

# create loop to calculate the areas
#for(i in 1:length(bear_annualpolygon_utm)){
  i=6 # tested the loop using this for i=1 to i=31 and only i=6 gives the error (i=6 is X10703_1994)
  bear_hpi_annualarea[[i]] <- gArea(bear_annualpolygon_utm[[i]]) # error message: "rgeos_PolyCreateComment: orphaned hole, cannot find containing polygon for hole at index 3"
#}

  # test gArea with only bear #6
gArea(bear_annualpolygon_utm[[6]]) # gives same error message as above
gArea(bear_annualpolygon_utm$X10703_1994) # gives error message
  # test plotting only bear #6
plot(bear_annualpolygon_utm[[6]]) # seems to plot fine
points(x=X10703_1994$EASTING, y=X10703_1994$NORTHING)

checkPolygonsHoles(bear_annualpolygon_utm$X10703_1994) # this says that it is not a polygons object, but the line below classifies it as a "Spatial Polygons"
str(bear_annualpolygon_utm$X10703_1994)


# Below I would put the area values into a dataframe, but it doesn't work because of the above loop

# add values into new dataframe
bear_hpi_annualarea_df$AREA <- bear_hpi_annualarea
str(bear_hpi_annualarea_df)
bear_hpi_annualarea_df$AREA <- as.numeric(bear_hpi_annualarea_df$AREA)

# make a new column that is km2 (previous was m2) and make into a spreadsheet
bear_hpi_annualarea_df$AREA_KM2 <- bear_hpi_annualarea_df$AREA*0.000001
summary(bear_hpi_annualarea_df)

write.csv(bear_hpi_annualarea_df, "data/bear_hpi_annualarea_df.csv")




# Getting stats

# number of bear years = number of names in annual list (since its already separated by bear and year) 
names(bear_annualKDE_list)

# yearly stats
unique(bear_hpi_annualarea_df$YEAR)

annualhpi_1991 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1991")
summary(annualhpi_1991)

annualhpi_1992 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1992")
summary(annualhpi_1992)

annualhpi_1993 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1993")
summary(annualhpi_1993)

annualhpi_1994 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1994")
summary(annualhpi_1994)

annualhpi_1995 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1995")
summary(annualhpi_1995)

annualhpi_1999 <- bear_hpi_annualarea_df %>%
  filter(bear_hpi_annualarea_df$YEAR=="1999")
summary(annualhpi_1999)


# 10. ANNUAL SUBLIST (not including #6, #8) - loops for hpi KDE (amse pilot only) for bears with >30 fixes (UTM) --------------

# Even though there was only an error message for #6 above, I ran through this again with the sublist (i.e. #6 removed) and suddenly #8 was also giving me the same error
# So I removed #8 as well and ran through it again
# This time, I got an error message for a new bear (#12), so I stopped doing the sublist and did each bear separately below
# See section 11


names(bear_annualKDE_list)
    # remove problem bears
bear_annualKDE_sublist <- bear_annualKDE_list
names(bear_annualKDE_sublist)

bear_annualKDE_sublist[[6]] <- NULL # remove X10703_1994 (or #6)
names(bear_annualKDE_sublist)

bear_annualKDE_sublist[[8]] <- NULL # remove X10709_1993 (or #8)
names(bear_annualKDE_sublist) # check that X10703_1994 and X10709_1993 are not in there - it worked

# Make empty list and then create amse pilot with loop
bear_annualhpi_amsepilot_utm_sublist <- list()

for(i in 1:length(bear_annualKDE_sublist)){
  temp_dat <- bear_annualKDE_sublist[[i]]
  bear_annualhpi_amsepilot_utm_sublist[[i]] <- Hpi(temp_dat[, 13:14], pilot="amse", binned=T) 
} 

# Make empty list and then make amse contours
bear_annualpolygon_utm_sublist <- list()

for(i in 1:length(bear_annualhpi_amsepilot_utm_sublist)){
  bear_annualhpi_amse_contour_utm_sublist <- kde(bear_annualKDE_list[[i]][13:14], H=bear_annualhpi_amsepilot_utm_sublist[[i]]) 
  bear_annualcontourlevels_utm_sublist <- contourLevels(bear_annualhpi_amse_contour_utm_sublist, cont = 95)               
  bear_annuallines_utm_sublsit <- contourLines(x=bear_annualhpi_amse_contour_utm_sublist$eval.points[[1]], y=bear_annualhpi_amse_contour_utm_sublist$eval.points[[2]], 
                                       z=bear_annualhpi_amse_contour_utm_sublist$estimate, level=bear_annualcontourlevels_utm_sublist) 
  bear_annualsldf_utm_sublist = ContourLines2SLDF(bear_annuallines_utm_sublsit)  
  bear_annualpolyset_utm_sublist = SpatialLines2PolySet(bear_annualsldf_utm_sublist) 
  bear_annualpolygon_utm_sublist[[i]] = PolySet2SpatialPolygons(bear_annualpolyset_utm_sublist) # note: CRS isn't required here, and the error message below pops up regardless of if I set the CRS or not
} 

# name items in list (same order as bear_annualKDE_sublist)
names(bear_annualKDE_sublist)
names(bear_annualpolygon_utm_sublist) <- c("X10695_1991_poly", "X10695_1992_poly", "X10700_1993_poly", "X10700_1994_poly", "X10703_1993_poly", "X10707_1992_poly", "X10707_1993_poly", "X11975_1994_poly", "X11975_1995_poly", "X12080_1994_poly", "X12080_1995_poly", "X12086_1995_poly", "X12092_1994_poly", "X13284_1993_poly", "X13289_1993_poly", "X13289_1994_poly", "X13292_1993_poly", "X13292_1994_poly", "X13428_1994_poly", "X13437_1995_poly", "X13746_1994_poly", "X13746_1995_poly", "X30126_1998_poly", "X30126_1999_poly", "X30129_1998_poly", "X30129_1999_poly", "X30131_1998_poly", "X30131_1999_poly", "X30135_1999_poly")
head(bear_annualpolygon_utm_sublist$X10695_1991_poly) # check that it worked - should be SpatialPolygons class
names(bear_annualpolygon_utm_sublist) 

# test area
gArea(bear_annualpolygon_utm_sublist[[1]]) # tested #1-29, now there's an error for #12 (X12086_1995_poly)



# 11. ANNUAL - hpi KDE (amse pilot only) for bears with >30 annual fixes - all bears individually (UTM)  -------------

# go back to section 2 and re-create the dataframes and list first
    # the bear_year dataframes had been altered during lat/long mapping
names(bear_annualKDE_list)

# X10695_1991
head(X10695_1991)
X10695_1991_amse <- Hpi(X10695_1991[, 13:14], pilot="amse", binned=T) # make the pilot

X10695_1991_contour <- kde(X10695_1991[13:14], H=X10695_1991_amse) # make the polygon
X10695_1991_contourlevels <- contourLevels(X10695_1991_contour, cont = 95)               
X10695_1991_lines <- contourLines(x=X10695_1991_contour$eval.points[[1]], y=X10695_1991_contour$eval.points[[2]], z=X10695_1991_contour$estimate, level=X10695_1991_contourlevels) 
X10695_1991_sldf = ContourLines2SLDF(X10695_1991_lines)  
X10695_1991_polyset = SpatialLines2PolySet(X10695_1991_sldf) 
X10695_1991_polygon = PolySet2SpatialPolygons(X10695_1991_polyset) # note: CRS isn't required here, and the error message below pops up regardless of if I set the CRS or not

plot(X10695_1991_polygon)
gArea(X10695_1991_polygon) # get the area (in m2)
      # note, as I go through each one, copy and paste the area into the new dataframe being made at the end of this section

# X10695_1992
X10695_1992_amse <- Hpi(X10695_1992[, 13:14], pilot="amse", binned=T) 
X10695_1992_contour <- kde(X10695_1992[13:14], H=X10695_1992_amse) 
X10695_1992_contourlevels <- contourLevels(X10695_1992_contour, cont = 95)               
X10695_1992_lines <- contourLines(x=X10695_1992_contour$eval.points[[1]], y=X10695_1992_contour$eval.points[[2]], z=X10695_1992_contour$estimate, level=X10695_1992_contourlevels) 
X10695_1992_sldf = ContourLines2SLDF(X10695_1992_lines)  
X10695_1992_polyset = SpatialLines2PolySet(X10695_1992_sldf) 
X10695_1992_polygon = PolySet2SpatialPolygons(X10695_1992_polyset) 
plot(X10695_1992_polygon)
gArea(X10695_1992_polygon) 

# X10700_1993
X10700_1993_amse <- Hpi(X10700_1993[, 13:14], pilot="amse", binned=T) 
X10700_1993_contour <- kde(X10700_1993[13:14], H=X10700_1993_amse) 
X10700_1993_contourlevels <- contourLevels(X10700_1993_contour, cont = 95)               
X10700_1993_lines <- contourLines(x=X10700_1993_contour$eval.points[[1]], y=X10700_1993_contour$eval.points[[2]], z=X10700_1993_contour$estimate, level=X10700_1993_contourlevels) 
X10700_1993_sldf = ContourLines2SLDF(X10700_1993_lines)  
X10700_1993_polyset = SpatialLines2PolySet(X10700_1993_sldf) 
X10700_1993_polygon = PolySet2SpatialPolygons(X10700_1993_polyset) 
plot(X10700_1993_polygon)
gArea(X10700_1993_polygon) 

# X10700_1994
X10700_1994_amse <- Hpi(X10700_1994[, 13:14], pilot="amse", binned=T) 
X10700_1994_contour <- kde(X10700_1994[13:14], H=X10700_1994_amse) 
X10700_1994_contourlevels <- contourLevels(X10700_1994_contour, cont = 95)               
X10700_1994_lines <- contourLines(x=X10700_1994_contour$eval.points[[1]], y=X10700_1994_contour$eval.points[[2]], z=X10700_1994_contour$estimate, level=X10700_1994_contourlevels) 
X10700_1994_sldf = ContourLines2SLDF(X10700_1994_lines)  
X10700_1994_polyset = SpatialLines2PolySet(X10700_1994_sldf) 
X10700_1994_polygon = PolySet2SpatialPolygons(X10700_1994_polyset) 
plot(X10700_1994_polygon)
gArea(X10700_1994_polygon) 

# X10703_1993
X10703_1993_amse <- Hpi(X10703_1993[, 13:14], pilot="amse", binned=T) 
X10703_1993_contour <- kde(X10703_1993[13:14], H=X10703_1993_amse) 
X10703_1993_contourlevels <- contourLevels(X10703_1993_contour, cont = 95)               
X10703_1993_lines <- contourLines(x=X10703_1993_contour$eval.points[[1]], y=X10703_1993_contour$eval.points[[2]], z=X10703_1993_contour$estimate, level=X10703_1993_contourlevels) 
X10703_1993_sldf = ContourLines2SLDF(X10703_1993_lines)  
X10703_1993_polyset = SpatialLines2PolySet(X10703_1993_sldf) 
X10703_1993_polygon = PolySet2SpatialPolygons(X10703_1993_polyset) 
plot(X10703_1993_polygon)
gArea(X10703_1993_polygon) 

# X10703_1994 - error message (cannot do gArea line)
X10703_1994_amse <- Hpi(X10703_1994[, 13:14], pilot="amse", binned=T) 
X10703_1994_contour <- kde(X10703_1994[13:14], H=X10703_1994_amse) 
X10703_1994_contourlevels <- contourLevels(X10703_1994_contour, cont = 95)               
X10703_1994_lines <- contourLines(x=X10703_1994_contour$eval.points[[1]], y=X10703_1994_contour$eval.points[[2]], z=X10703_1994_contour$estimate, level=X10703_1994_contourlevels) 
X10703_1994_sldf = ContourLines2SLDF(X10703_1994_lines)  
X10703_1994_polyset = SpatialLines2PolySet(X10703_1994_sldf) 
X10703_1994_polygon = PolySet2SpatialPolygons(X10703_1994_polyset) 
plot(X10703_1994_polygon)
gArea(X10703_1994_polygon) # causes orphaned hole error - cannot calculate the area

# X10707_1992
X10707_1992_amse <- Hpi(X10707_1992[, 13:14], pilot="amse", binned=T) 
X10707_1992_contour <- kde(X10707_1992[13:14], H=X10707_1992_amse) 
X10707_1992_contourlevels <- contourLevels(X10707_1992_contour, cont = 95)               
X10707_1992_lines <- contourLines(x=X10707_1992_contour$eval.points[[1]], y=X10707_1992_contour$eval.points[[2]], z=X10707_1992_contour$estimate, level=X10707_1992_contourlevels) 
X10707_1992_sldf = ContourLines2SLDF(X10707_1992_lines)  
X10707_1992_polyset = SpatialLines2PolySet(X10707_1992_sldf) 
X10707_1992_polygon = PolySet2SpatialPolygons(X10707_1992_polyset) 
plot(X10707_1992_polygon)
gArea(X10707_1992_polygon)

# X10707_1993
X10707_1993_amse <- Hpi(X10707_1993[, 13:14], pilot="amse", binned=T) 
X10707_1993_contour <- kde(X10707_1993[13:14], H=X10707_1993_amse) 
X10707_1993_contourlevels <- contourLevels(X10707_1993_contour, cont = 95)               
X10707_1993_lines <- contourLines(x=X10707_1993_contour$eval.points[[1]], y=X10707_1993_contour$eval.points[[2]], z=X10707_1993_contour$estimate, level=X10707_1993_contourlevels) 
X10707_1993_sldf = ContourLines2SLDF(X10707_1993_lines)  
X10707_1993_polyset = SpatialLines2PolySet(X10707_1993_sldf) 
X10707_1993_polygon = PolySet2SpatialPolygons(X10707_1993_polyset) 
plot(X10707_1993_polygon)
gArea(X10707_1993_polygon)

# X10709_1993
X10709_1993_amse <- Hpi(X10709_1993[, 13:14], pilot="amse", binned=T) 
X10709_1993_contour <- kde(X10709_1993[13:14], H=X10709_1993_amse) 
X10709_1993_contourlevels <- contourLevels(X10709_1993_contour, cont = 95)               
X10709_1993_lines <- contourLines(x=X10709_1993_contour$eval.points[[1]], y=X10709_1993_contour$eval.points[[2]], z=X10709_1993_contour$estimate, level=X10709_1993_contourlevels) 
X10709_1993_sldf = ContourLines2SLDF(X10709_1993_lines)  
X10709_1993_polyset = SpatialLines2PolySet(X10709_1993_sldf) 
X10709_1993_polygon = PolySet2SpatialPolygons(X10709_1993_polyset) 
plot(X10709_1993_polygon)
gArea(X10709_1993_polygon)

# X11975_1994
X11975_1994_amse <- Hpi(X11975_1994[, 13:14], pilot="amse", binned=T) 
X11975_1994_contour <- kde(X11975_1994[13:14], H=X11975_1994_amse) 
X11975_1994_contourlevels <- contourLevels(X11975_1994_contour, cont = 95)               
X11975_1994_lines <- contourLines(x=X11975_1994_contour$eval.points[[1]], y=X11975_1994_contour$eval.points[[2]], z=X11975_1994_contour$estimate, level=X11975_1994_contourlevels) 
X11975_1994_sldf = ContourLines2SLDF(X11975_1994_lines)  
X11975_1994_polyset = SpatialLines2PolySet(X11975_1994_sldf) 
X11975_1994_polygon = PolySet2SpatialPolygons(X11975_1994_polyset) 
plot(X11975_1994_polygon)
gArea(X11975_1994_polygon)

# X11975_1995
X11975_1995_amse <- Hpi(X11975_1995[, 13:14], pilot="amse", binned=T) 
X11975_1995_contour <- kde(X11975_1995[13:14], H=X11975_1995_amse) 
X11975_1995_contourlevels <- contourLevels(X11975_1995_contour, cont = 95)               
X11975_1995_lines <- contourLines(x=X11975_1995_contour$eval.points[[1]], y=X11975_1995_contour$eval.points[[2]], z=X11975_1995_contour$estimate, level=X11975_1995_contourlevels) 
X11975_1995_sldf = ContourLines2SLDF(X11975_1995_lines)  
X11975_1995_polyset = SpatialLines2PolySet(X11975_1995_sldf) 
X11975_1995_polygon = PolySet2SpatialPolygons(X11975_1995_polyset) 
plot(X11975_1995_polygon)
gArea(X11975_1995_polygon)

# X12080_1994
X12080_1994_amse <- Hpi(X12080_1994[, 13:14], pilot="amse", binned=T) 
X12080_1994_contour <- kde(X12080_1994[13:14], H=X12080_1994_amse) 
X12080_1994_contourlevels <- contourLevels(X12080_1994_contour, cont = 95)               
X12080_1994_lines <- contourLines(x=X12080_1994_contour$eval.points[[1]], y=X12080_1994_contour$eval.points[[2]], z=X12080_1994_contour$estimate, level=X12080_1994_contourlevels) 
X12080_1994_sldf = ContourLines2SLDF(X12080_1994_lines)  
X12080_1994_polyset = SpatialLines2PolySet(X12080_1994_sldf) 
X12080_1994_polygon = PolySet2SpatialPolygons(X12080_1994_polyset) 
plot(X12080_1994_polygon)
gArea(X12080_1994_polygon)

# X12080_1995
X12080_1995_amse <- Hpi(X12080_1995[, 13:14], pilot="amse", binned=T) 
X12080_1995_contour <- kde(X12080_1995[13:14], H=X12080_1995_amse) 
X12080_1995_contourlevels <- contourLevels(X12080_1995_contour, cont = 95)               
X12080_1995_lines <- contourLines(x=X12080_1995_contour$eval.points[[1]], y=X12080_1995_contour$eval.points[[2]], z=X12080_1995_contour$estimate, level=X12080_1995_contourlevels) 
X12080_1995_sldf = ContourLines2SLDF(X12080_1995_lines)  
X12080_1995_polyset = SpatialLines2PolySet(X12080_1995_sldf) 
X12080_1995_polygon = PolySet2SpatialPolygons(X12080_1995_polyset) 
plot(X12080_1995_polygon)
gArea(X12080_1995_polygon)

# X12086_1995
X12086_1995_amse <- Hpi(X12086_1995[, 13:14], pilot="amse", binned=T) 
X12086_1995_contour <- kde(X12086_1995[13:14], H=X12086_1995_amse) 
X12086_1995_contourlevels <- contourLevels(X12086_1995_contour, cont = 95)               
X12086_1995_lines <- contourLines(x=X12086_1995_contour$eval.points[[1]], y=X12086_1995_contour$eval.points[[2]], z=X12086_1995_contour$estimate, level=X12086_1995_contourlevels) 
X12086_1995_sldf = ContourLines2SLDF(X12086_1995_lines)  
X12086_1995_polyset = SpatialLines2PolySet(X12086_1995_sldf) 
X12086_1995_polygon = PolySet2SpatialPolygons(X12086_1995_polyset) 
plot(X12086_1995_polygon)
gArea(X12086_1995_polygon)

# X12092_1994
X12092_1994_amse <- Hpi(X12092_1994[, 13:14], pilot="amse", binned=T) 
X12092_1994_contour <- kde(X12092_1994[13:14], H=X12092_1994_amse) 
X12092_1994_contourlevels <- contourLevels(X12092_1994_contour, cont = 95)               
X12092_1994_lines <- contourLines(x=X12092_1994_contour$eval.points[[1]], y=X12092_1994_contour$eval.points[[2]], z=X12092_1994_contour$estimate, level=X12092_1994_contourlevels) 
X12092_1994_sldf = ContourLines2SLDF(X12092_1994_lines)  
X12092_1994_polyset = SpatialLines2PolySet(X12092_1994_sldf) 
X12092_1994_polygon = PolySet2SpatialPolygons(X12092_1994_polyset) 
plot(X12092_1994_polygon)
gArea(X12092_1994_polygon)

# X13284_1993 - oddly this one clearly has a hole in it, yet it doesn't give the warning error
X13284_1993_amse <- Hpi(X13284_1993[, 13:14], pilot="amse", binned=T) 
X13284_1993_contour <- kde(X13284_1993[13:14], H=X13284_1993_amse) 
X13284_1993_contourlevels <- contourLevels(X13284_1993_contour, cont = 95)               
X13284_1993_lines <- contourLines(x=X13284_1993_contour$eval.points[[1]], y=X13284_1993_contour$eval.points[[2]], z=X13284_1993_contour$estimate, level=X13284_1993_contourlevels) 
X13284_1993_sldf = ContourLines2SLDF(X13284_1993_lines)  
X13284_1993_polyset = SpatialLines2PolySet(X13284_1993_sldf) 
X13284_1993_polygon = PolySet2SpatialPolygons(X13284_1993_polyset) 
plot(X13284_1993_polygon)
gArea(X13284_1993_polygon)

# X13289_1993
X13289_1993_amse <- Hpi(X13289_1993[, 13:14], pilot="amse", binned=T) 
X13289_1993_contour <- kde(X13289_1993[13:14], H=X13289_1993_amse) 
X13289_1993_contourlevels <- contourLevels(X13289_1993_contour, cont = 95)               
X13289_1993_lines <- contourLines(x=X13289_1993_contour$eval.points[[1]], y=X13289_1993_contour$eval.points[[2]], z=X13289_1993_contour$estimate, level=X13289_1993_contourlevels) 
X13289_1993_sldf = ContourLines2SLDF(X13289_1993_lines)  
X13289_1993_polyset = SpatialLines2PolySet(X13289_1993_sldf) 
X13289_1993_polygon = PolySet2SpatialPolygons(X13289_1993_polyset) 
plot(X13289_1993_polygon)
gArea(X13289_1993_polygon)

# X13289_1994
X13289_1994_amse <- Hpi(X13289_1994[, 13:14], pilot="amse", binned=T) 
X13289_1994_contour <- kde(X13289_1994[13:14], H=X13289_1994_amse) 
X13289_1994_contourlevels <- contourLevels(X13289_1994_contour, cont = 95)               
X13289_1994_lines <- contourLines(x=X13289_1994_contour$eval.points[[1]], y=X13289_1994_contour$eval.points[[2]], z=X13289_1994_contour$estimate, level=X13289_1994_contourlevels) 
X13289_1994_sldf = ContourLines2SLDF(X13289_1994_lines)  
X13289_1994_polyset = SpatialLines2PolySet(X13289_1994_sldf) 
X13289_1994_polygon = PolySet2SpatialPolygons(X13289_1994_polyset) 
plot(X13289_1994_polygon)
gArea(X13289_1994_polygon)

# X13292_1993
X13292_1993_amse <- Hpi(X13292_1993[, 13:14], pilot="amse", binned=T) 
X13292_1993_contour <- kde(X13292_1993[13:14], H=X13292_1993_amse) 
X13292_1993_contourlevels <- contourLevels(X13292_1993_contour, cont = 95)               
X13292_1993_lines <- contourLines(x=X13292_1993_contour$eval.points[[1]], y=X13292_1993_contour$eval.points[[2]], z=X13292_1993_contour$estimate, level=X13292_1993_contourlevels) 
X13292_1993_sldf = ContourLines2SLDF(X13292_1993_lines)  
X13292_1993_polyset = SpatialLines2PolySet(X13292_1993_sldf) 
X13292_1993_polygon = PolySet2SpatialPolygons(X13292_1993_polyset) 
plot(X13292_1993_polygon)
gArea(X13292_1993_polygon)

# X13292_1994
X13292_1994_amse <- Hpi(X13292_1994[, 13:14], pilot="amse", binned=T) 
X13292_1994_contour <- kde(X13292_1994[13:14], H=X13292_1994_amse) 
X13292_1994_contourlevels <- contourLevels(X13292_1994_contour, cont = 95)               
X13292_1994_lines <- contourLines(x=X13292_1994_contour$eval.points[[1]], y=X13292_1994_contour$eval.points[[2]], z=X13292_1994_contour$estimate, level=X13292_1994_contourlevels) 
X13292_1994_sldf = ContourLines2SLDF(X13292_1994_lines)  
X13292_1994_polyset = SpatialLines2PolySet(X13292_1994_sldf) 
X13292_1994_polygon = PolySet2SpatialPolygons(X13292_1994_polyset) 
plot(X13292_1994_polygon)
gArea(X13292_1994_polygon)

# X13428_1994
X13428_1994_amse <- Hpi(X13428_1994[, 13:14], pilot="amse", binned=T) 
X13428_1994_contour <- kde(X13428_1994[13:14], H=X13428_1994_amse) 
X13428_1994_contourlevels <- contourLevels(X13428_1994_contour, cont = 95)               
X13428_1994_lines <- contourLines(x=X13428_1994_contour$eval.points[[1]], y=X13428_1994_contour$eval.points[[2]], z=X13428_1994_contour$estimate, level=X13428_1994_contourlevels) 
X13428_1994_sldf = ContourLines2SLDF(X13428_1994_lines)  
X13428_1994_polyset = SpatialLines2PolySet(X13428_1994_sldf) 
X13428_1994_polygon = PolySet2SpatialPolygons(X13428_1994_polyset) 
plot(X13428_1994_polygon)
gArea(X13428_1994_polygon)

# X13437_1995
X13437_1995_amse <- Hpi(X13437_1995[, 13:14], pilot="amse", binned=T) 
X13437_1995_contour <- kde(X13437_1995[13:14], H=X13437_1995_amse) 
X13437_1995_contourlevels <- contourLevels(X13437_1995_contour, cont = 95)               
X13437_1995_lines <- contourLines(x=X13437_1995_contour$eval.points[[1]], y=X13437_1995_contour$eval.points[[2]], z=X13437_1995_contour$estimate, level=X13437_1995_contourlevels) 
X13437_1995_sldf = ContourLines2SLDF(X13437_1995_lines)  
X13437_1995_polyset = SpatialLines2PolySet(X13437_1995_sldf) 
X13437_1995_polygon = PolySet2SpatialPolygons(X13437_1995_polyset) 
plot(X13437_1995_polygon)
gArea(X13437_1995_polygon)

# X13746_1994
X13746_1994_amse <- Hpi(X13746_1994[, 13:14], pilot="amse", binned=T) 
X13746_1994_contour <- kde(X13746_1994[13:14], H=X13746_1994_amse) 
X13746_1994_contourlevels <- contourLevels(X13746_1994_contour, cont = 95)               
X13746_1994_lines <- contourLines(x=X13746_1994_contour$eval.points[[1]], y=X13746_1994_contour$eval.points[[2]], z=X13746_1994_contour$estimate, level=X13746_1994_contourlevels) 
X13746_1994_sldf = ContourLines2SLDF(X13746_1994_lines)  
X13746_1994_polyset = SpatialLines2PolySet(X13746_1994_sldf) 
X13746_1994_polygon = PolySet2SpatialPolygons(X13746_1994_polyset) 
plot(X13746_1994_polygon)
gArea(X13746_1994_polygon)

# X13746_1995
X13746_1995_amse <- Hpi(X13746_1995[, 13:14], pilot="amse", binned=T) 
X13746_1995_contour <- kde(X13746_1995[13:14], H=X13746_1995_amse) 
X13746_1995_contourlevels <- contourLevels(X13746_1995_contour, cont = 95)               
X13746_1995_lines <- contourLines(x=X13746_1995_contour$eval.points[[1]], y=X13746_1995_contour$eval.points[[2]], z=X13746_1995_contour$estimate, level=X13746_1995_contourlevels) 
X13746_1995_sldf = ContourLines2SLDF(X13746_1995_lines)  
X13746_1995_polyset = SpatialLines2PolySet(X13746_1995_sldf) 
X13746_1995_polygon = PolySet2SpatialPolygons(X13746_1995_polyset) 
plot(X13746_1995_polygon)
gArea(X13746_1995_polygon)

# X30126_1998
X30126_1998_amse <- Hpi(X30126_1998[, 13:14], pilot="amse", binned=T) 
X30126_1998_contour <- kde(X30126_1998[13:14], H=X30126_1998_amse) 
X30126_1998_contourlevels <- contourLevels(X30126_1998_contour, cont = 95)               
X30126_1998_lines <- contourLines(x=X30126_1998_contour$eval.points[[1]], y=X30126_1998_contour$eval.points[[2]], z=X30126_1998_contour$estimate, level=X30126_1998_contourlevels) 
X30126_1998_sldf = ContourLines2SLDF(X30126_1998_lines)  
X30126_1998_polyset = SpatialLines2PolySet(X30126_1998_sldf) 
X30126_1998_polygon = PolySet2SpatialPolygons(X30126_1998_polyset) 
plot(X30126_1998_polygon)
gArea(X30126_1998_polygon)

# X30126_1999
X30126_1999_amse <- Hpi(X30126_1999[, 13:14], pilot="amse", binned=T) 
X30126_1999_contour <- kde(X30126_1999[13:14], H=X30126_1999_amse) 
X30126_1999_contourlevels <- contourLevels(X30126_1999_contour, cont = 95)               
X30126_1999_lines <- contourLines(x=X30126_1999_contour$eval.points[[1]], y=X30126_1999_contour$eval.points[[2]], z=X30126_1999_contour$estimate, level=X30126_1999_contourlevels) 
X30126_1999_sldf = ContourLines2SLDF(X30126_1999_lines)  
X30126_1999_polyset = SpatialLines2PolySet(X30126_1999_sldf) 
X30126_1999_polygon = PolySet2SpatialPolygons(X30126_1999_polyset) 
plot(X30126_1999_polygon)
gArea(X30126_1999_polygon)

# X30129_1998
X30129_1998_amse <- Hpi(X30129_1998[, 13:14], pilot="amse", binned=T) 
X30129_1998_contour <- kde(X30129_1998[13:14], H=X30129_1998_amse) 
X30129_1998_contourlevels <- contourLevels(X30129_1998_contour, cont = 95)               
X30129_1998_lines <- contourLines(x=X30129_1998_contour$eval.points[[1]], y=X30129_1998_contour$eval.points[[2]], z=X30129_1998_contour$estimate, level=X30129_1998_contourlevels) 
X30129_1998_sldf = ContourLines2SLDF(X30129_1998_lines)  
X30129_1998_polyset = SpatialLines2PolySet(X30129_1998_sldf) 
X30129_1998_polygon = PolySet2SpatialPolygons(X30129_1998_polyset) 
plot(X30129_1998_polygon)
gArea(X30129_1998_polygon)

# X30129_1999
X30129_1999_amse <- Hpi(X30129_1999[, 13:14], pilot="amse", binned=T) 
X30129_1999_contour <- kde(X30129_1999[13:14], H=X30129_1999_amse) 
X30129_1999_contourlevels <- contourLevels(X30129_1999_contour, cont = 95)               
X30129_1999_lines <- contourLines(x=X30129_1999_contour$eval.points[[1]], y=X30129_1999_contour$eval.points[[2]], z=X30129_1999_contour$estimate, level=X30129_1999_contourlevels) 
X30129_1999_sldf = ContourLines2SLDF(X30129_1999_lines)  
X30129_1999_polyset = SpatialLines2PolySet(X30129_1999_sldf) 
X30129_1999_polygon = PolySet2SpatialPolygons(X30129_1999_polyset) 
plot(X30129_1999_polygon)
gArea(X30129_1999_polygon)

# X30131_1998
X30131_1998_amse <- Hpi(X30131_1998[, 13:14], pilot="amse", binned=T) 
X30131_1998_contour <- kde(X30131_1998[13:14], H=X30131_1998_amse) 
X30131_1998_contourlevels <- contourLevels(X30131_1998_contour, cont = 95)               
X30131_1998_lines <- contourLines(x=X30131_1998_contour$eval.points[[1]], y=X30131_1998_contour$eval.points[[2]], z=X30131_1998_contour$estimate, level=X30131_1998_contourlevels) 
X30131_1998_sldf = ContourLines2SLDF(X30131_1998_lines)  
X30131_1998_polyset = SpatialLines2PolySet(X30131_1998_sldf) 
X30131_1998_polygon = PolySet2SpatialPolygons(X30131_1998_polyset) 
plot(X30131_1998_polygon)
gArea(X30131_1998_polygon)

# X30131_1999
X30131_1999_amse <- Hpi(X30131_1999[, 13:14], pilot="amse", binned=T) 
X30131_1999_contour <- kde(X30131_1999[13:14], H=X30131_1999_amse) 
X30131_1999_contourlevels <- contourLevels(X30131_1999_contour, cont = 95)               
X30131_1999_lines <- contourLines(x=X30131_1999_contour$eval.points[[1]], y=X30131_1999_contour$eval.points[[2]], z=X30131_1999_contour$estimate, level=X30131_1999_contourlevels) 
X30131_1999_sldf = ContourLines2SLDF(X30131_1999_lines)  
X30131_1999_polyset = SpatialLines2PolySet(X30131_1999_sldf) 
X30131_1999_polygon = PolySet2SpatialPolygons(X30131_1999_polyset) 
plot(X30131_1999_polygon)
gArea(X30131_1999_polygon)

# X30135_1999
X30135_1999_amse <- Hpi(X30135_1999[, 13:14], pilot="amse", binned=T) 
X30135_1999_contour <- kde(X30135_1999[13:14], H=X30135_1999_amse) 
X30135_1999_contourlevels <- contourLevels(X30135_1999_contour, cont = 95)               
X30135_1999_lines <- contourLines(x=X30135_1999_contour$eval.points[[1]], y=X30135_1999_contour$eval.points[[2]], z=X30135_1999_contour$estimate, level=X30135_1999_contourlevels) 
X30135_1999_sldf = ContourLines2SLDF(X30135_1999_lines)  
X30135_1999_polyset = SpatialLines2PolySet(X30135_1999_sldf) 
X30135_1999_polygon = PolySet2SpatialPolygons(X30135_1999_polyset) 
plot(X30135_1999_polygon)
gArea(X30135_1999_polygon)

# make new dataframe of all the areas made above
bears_annualhpi_area <- data.frame(ID_YEAR=rep(NA, length(bear_annualKDE_list)), ID = rep(NA, length(bear_annualKDE_list)), YEAR = rep(NA, length(bear_annualKDE_list)), AREA = rep(NA, length(bear_annualKDE_list)))
bears_annualhpi_area$ID_YEAR <- c("X10695_1991", "X10695_1992", "X10700_1993", "X10700_1994", "X10703_1993", "X10703_1994", "X10707_1992", "X10707_1993", "X10709_1993", "X11975_1994", "X11975_1995", "X12080_1994", "X12080_1995", "X12086_1995", "X12092_1994", "X13284_1993", "X13289_1993", "X13289_1994", "X13292_1993", "X13292_1994", "X13428_1994", "X13437_1995", "X13746_1994", "X13746_1995", "X30126_1998", "X30126_1999", "X30129_1998", "X30129_1999", "X30131_1998", "X30131_1999", "X30135_1999")
bears_annualhpi_area$ID <- c("X10695", "X10695", "X10700", "X10700", "X10703", "X10703", "X10707", "X10707", "X10709", "X11975", "X11975", "X12080", "X12080", "X12086", "X12092", "X13284", "X13289", "X13289", "X13292", "X13292", "X13428", "X134375", "X13746", "X13746", "X30126", "X30126", "X30129", "X30129", "X30131", "X30131", "X30135")
bears_annualhpi_area$YEAR <- c("1991", "1992", "1993", "1994", "1993", "1994", "1992", "1993", "1993", "1994", "1995", "1994", "1995", "1995", "1994", "1993", "1993", "1994", "1993", "1994", "1994", "1995", "1994", "1995", "1998", "1999", "1998", "1999", "1998", "1999", "1999")
bears_annualhpi_area$AREA <- c("77731179691", "183554465067", "59846985299", "199846106237", "43495088657", "NA", "101302199883", "74103247873", "8836836026", "1780306863", "1348884101", "153486113919", "176635253804", "697823514", "7638894645", "167955173893", "108277765386", "51206448693", "75168578862", "276115389329", "47729464617", "94662044680", "4289042973", "88354507181", "29150328003", "5453831685", "13824347104", "25254224086", "158470432142", "64608909151", "30549809032")
      # Note: NA was added to the area column for every error message that occurred below

# remove NA record (i.e. remove X10703_1994, which was giving me the error orginally)
bears_annualhpi_area_df <- bears_annualhpi_area[bears_annualhpi_area$ID_YEAR != 'X10703_1994', ] # check data frame - #6 has been removed

# add new column for km2
str(bears_annualhpi_area_df)
bears_annualhpi_area_df$AREA <- as.numeric(bears_annualhpi_area_df$AREA)
bears_annualhpi_area_df$AREA_KM2 <- bears_annualhpi_area_df$AREA*0.000001

summary(bears_annualhpi_area_df)
unique(bears_annualhpi_area_df$ID_YEAR)
unique(bears_annualhpi_area_df$ID)

head(bears_annualhpi_area_df)

# yearly stats
unique(bears_annualhpi_area_df$YEAR)

# 1991
annualhpi_1991 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1991")
summary(annualhpi_1991)

# 1992
annualhpi_1992 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1992")
summary(annualhpi_1992)

# 1993
annualhpi_1993 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1993")
summary(annualhpi_1993)

# 1994
annualhpi_1994 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1994")
summary(annualhpi_1994)

# 1995
annualhpi_1995 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1995")
summary(annualhpi_1995)

# 1998
annualhpi_1998 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1998")
summary(annualhpi_1998)

# 1999
annualhpi_1999 <- bears_annualhpi_area_df %>%
  filter(bears_annualhpi_area_df$YEAR=="1999")
summary(annualhpi_1999)


# 12. ANNUAL - Plotting 95% hpi areas for all bears with >30 fixes (UTM) ------------------------

summary(bears_annualhpi_area_df)

# plot individual bears
      # point plot
ggplot(data=bears_annualhpi_area_df) +
  geom_point(aes(y=ID_YEAR, x=AREA_KM2)) +
  scale_x_continuous(breaks=c(100000, 150000, 200000, 250000, 300000, 350000, 400000), labels=c("100000", "150000", "200000", "250000", "300000", "350000", "400000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()
      # coloured lineplot
ggplot(data=bears_annualhpi_area_df) +
  geom_col(aes(y=AREA_KM2, x=ID, fill=YEAR), color="black", position="dodge") +
  scale_y_continuous(breaks=c(100000, 150000, 200000, 250000, 300000, 350000, 400000), labels=c("100000", "150000", "200000", "250000", "300000", "350000", "400000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Bear ID", y="Area (Km2)") +
  theme_nuwcru()

# boxplot of summary 
summary(bears_annualhpi_area_df)

bear_annualhpi_boxplot <- ggplot(data=bears_annualhpi_area_df, aes(y=AREA_KM2)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1700000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000", "1000000", "1100000", "1200000", "1300000", "1400000", "1500000", "1600000", "1700000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()

bear_annualhpi_boxplot
bear_annualhpi_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# average of each year
# create summarized dataframe (avg area/year and # bears/year)
bears_annualhpi_area_summarized <- bears_annualhpi_area_df %>%
  group_by(YEAR) %>%
  summarize(AVG_AREA_KM2=mean(AREA_KM2), COUNT=n())

summary(bears_annualhpi_area_summarized)
head(bears_annualhpi_area_summarized)

# lineplot
ggplot(data=bears_annualhpi_area_summarized, aes(x=YEAR, y=AVG_AREA_KM2)) +
  scale_y_continuous(limits=c(0, 600000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000")) +
  geom_line(group=1) +
  geom_point(size=2, colour="red") +
  geom_text(aes(label=COUNT), vjust=-.75) +
  labs(title="Annual average 95% hpi KDE home range areas of\nDavis Strait female polar bears", x="Year", y="Area (Km2)") +
  theme_nuwcru()

# boxplot separated by year
ggplot(data=bears_annualhpi_area_df) +
  geom_boxplot(aes(x=YEAR, y=AREA_KM2), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 1700000), breaks=c(0, 100000, 200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, 1000000, 1100000, 1200000, 1300000, 1400000, 1500000, 1600000, 1700000), labels=c("0", "100000", "200000", "300000", "400000", "500000", "600000", "700000", "800000", "900000", "1000000", "1100000", "1200000", "1300000", "1400000", "1500000", "1600000", "1700000")) +
  labs(title="Yearly 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()




# 13. ANNUAL - Plotting 95% hpi areas for all bears with >30 fixes (UTM) - from when area was calculated for each bear individually -----

summary(bears_annualhpi_area_df)

# plot individual bears
ggplot(data=bears_annualhpi_area_df) +
  geom_point(aes(y=ID_YEAR, x=AREA_KM2)) +
  scale_x_continuous(limits=c(0, 300000), breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000, 275000, 300000), labels=c("0", "25000", "50000", "75000", "100000", "125000", "150000", "175000", "200000", "225000", "250000", "275000", "300000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Area (Km2)", y="Bear ID") +
  theme_nuwcru()

ggplot(data=bears_annualhpi_area_df) +
  geom_col(aes(y=AREA_KM2, x=ID, fill=YEAR), color="black", position="dodge") +
  scale_y_continuous(limits=c(0, 300000), breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000, 275000, 300000), labels=c("0", "25000", "50000", "75000", "100000", "125000", "150000", "175000", "200000", "225000", "250000", "275000", "300000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", x="Bear ID", y="Area (Km2)") +
  theme_nuwcru()

# boxplot of summary 
summary(bears_annualhpi_area_df)

bear_annualhpi_boxplot <- ggplot(data=bears_annualhpi_area_df, aes(y=AREA_KM2)) +
  geom_boxplot(varwidth = T, alpha=0.2) +
  scale_y_continuous(breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000, 250000, 275000, 300000), labels=c("0", "25000", "50000", "75000", "100000", "125000", "150000", "175000", "200000", "225000", "250000", "275000", "300000")) +
  labs(title="Annual 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()

bear_annualhpi_boxplot
bear_annualhpi_boxplot + theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())


# average of each year
# create summarized dataframe (avg area/year and # bears/year)
bear_hpi_annualarea_summarized <- bears_annualhpi_area_df %>%
  group_by(YEAR) %>%
  summarize(AVG_AREA_KM2=mean(AREA_KM2), COUNT=n())

summary(bear_hpi_annualarea_summarized)

# plot
ggplot(data=bear_hpi_annualarea_summarized, aes(x=YEAR, y=AVG_AREA_KM2)) +
  geom_line(group=1) +
  geom_point(size=2, colour="red") +
  geom_text(aes(label=COUNT), vjust=-.75) +
  scale_y_continuous(limits=c(0, 175000), breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000), labels=c("0", "25000", "50000", "75000", "100000", "125000", "150000", "175000", "200000", "225000")) +
  labs(title="Annual average 95% hpi KDE home range areas of\nDavis Strait female polar bears", x="Year", y="Area (Km2)") +
  theme_nuwcru()

# boxplots
ggplot(data=bears_annualhpi_area_df) +
  geom_boxplot(aes(x=YEAR, y=AREA_KM2), varwidth = T, alpha=0.2) +
  scale_y_continuous(limits=c(0, 175000), breaks=c(0, 25000, 50000, 75000, 100000, 125000, 150000, 175000, 200000, 225000), labels=c("0", "25000", "50000", "75000", "100000", "125000", "150000", "175000", "200000", "225000")) +
  labs(title="Yearly 95% hpi KDE home range areas of\nDavis Strait female polar bears (1991-2001)", y="Area (Km2)") +
  theme_nuwcru()




# 13. ANNUAL - Prepping 95% hpi data for mapping polygons (LAT/LONG) ------------------------

# select only points for the bears that have >30 fixes (used in bear_KDE_list above)
names(bear_annualKDE_list)

# split polygon list into items
lapply(names(bear_annualpolygon), function(x) assign(x, bear_annualpolygon[[x]], envir=.GlobalEnv))
names(bear_annualpolygon)
# these should all now be SpatialPolygonsDataframes that are separated from the list; check environment 


# converte points to correct format
# create new month column
# fortify polygon into dataframe, then format new dataframe for mapping

# X10695_1991
X10695_1991$LAT <- gsub(",","",X10695_1991$LAT)  
X10695_1991$LAT <- as.numeric(X10695_1991$LAT)
X10695_1991$LONG <- as.numeric(as.character((X10695_1991$LONG)))

X10695_1991$MONTH2 <- NULL
X10695_1991$MONTH2 <- factor(X10695_1991$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10695_1991_fortify <- fortify(X10695_1991_hpi)

# X10695_1992
X10695_1992$LAT <- gsub(",","",X10695_1992$LAT)  
X10695_1992$LAT <- as.numeric(X10695_1992$LAT)
X10695_1992$LONG <- as.numeric(as.character((X10695_1992$LONG)))

X10695_1992$MONTH2 <- NULL
X10695_1992$MONTH2 <- factor(X10695_1992$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10695_1992_fortify <- fortify(X10695_1992_hpi)

# X10700_1993
X10700_1993$LAT <- gsub(",","",X10700_1993$LAT)  
X10700_1993$LAT <- as.numeric(X10700_1993$LAT)
X10700_1993$LONG <- as.numeric(as.character((X10700_1993$LONG)))

X10700_1993$MONTH2 <- NULL
X10700_1993$MONTH2 <- factor(X10700_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10700_1993_fortify <- fortify(X10700_1993_hpi)

# X10700_1994
X10700_1994$LAT <- gsub(",","",X10700_1994$LAT)  
X10700_1994$LAT <- as.numeric(X10700_1994$LAT)
X10700_1994$LONG <- as.numeric(as.character((X10700_1994$LONG)))

X10700_1994$MONTH2 <- NULL
X10700_1994$MONTH2 <- factor(X10700_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10700_1994_fortify <- fortify(X10700_1994_hpi)

# X10703_1993
X10703_1993$LAT <- gsub(",","",X10703_1993$LAT)  
X10703_1993$LAT <- as.numeric(X10703_1993$LAT)
X10703_1993$LONG <- as.numeric(as.character((X10703_1993$LONG)))

X10703_1993$MONTH2 <- NULL
X10703_1993$MONTH2 <- factor(X10703_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10703_1993_fortify <- fortify(X10703_1993_hpi)

# X10703_1994
X10703_1994$LAT <- gsub(",","",X10703_1994$LAT)  
X10703_1994$LAT <- as.numeric(X10703_1994$LAT)
X10703_1994$LONG <- as.numeric(as.character((X10703_1994$LONG)))

X10703_1994$MONTH2 <- NULL
X10703_1994$MONTH2 <- factor(X10703_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10703_1994_fortify <- fortify(X10703_1994_hpi)

# X10707_1992
X10707_1992$LAT <- gsub(",","",X10707_1992$LAT)  
X10707_1992$LAT <- as.numeric(X10707_1992$LAT)
X10707_1992$LONG <- as.numeric(as.character((X10707_1992$LONG)))

X10707_1992$MONTH2 <- NULL
X10707_1992$MONTH2 <- factor(X10707_1992$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10707_1992_fortify <- fortify(X10707_1992_hpi)

# X10707_1993
X10707_1993$LAT <- gsub(",","",X10707_1993$LAT)  
X10707_1993$LAT <- as.numeric(X10707_1993$LAT)
X10707_1993$LONG <- as.numeric(as.character((X10707_1993$LONG)))

X10707_1993$MONTH2 <- NULL
X10707_1993$MONTH2 <- factor(X10707_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10707_1993_fortify <- fortify(X10707_1993_hpi)

# X10709_1993
X10709_1993$LAT <- gsub(",","",X10709_1993$LAT)  
X10709_1993$LAT <- as.numeric(X10709_1993$LAT)
X10709_1993$LONG <- as.numeric(as.character((X10709_1993$LONG)))

X10709_1993$MONTH2 <- NULL
X10709_1993$MONTH2 <- factor(X10709_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X10709_1993_fortify <- fortify(X10709_1993_hpi)

# X11975_1994
X11975_1994$LAT <- gsub(",","",X11975_1994$LAT)  
X11975_1994$LAT <- as.numeric(X11975_1994$LAT)
X11975_1994$LONG <- as.numeric(as.character((X11975_1994$LONG)))

X11975_1994$MONTH2 <- NULL
X11975_1994$MONTH2 <- factor(X11975_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X11975_1994_fortify <- fortify(X11975_1994_hpi)

# X11975_1995
X11975_1995$LAT <- gsub(",","",X11975_1995$LAT)  
X11975_1995$LAT <- as.numeric(X11975_1995$LAT)
X11975_1995$LONG <- as.numeric(as.character((X11975_1995$LONG)))

X11975_1995$MONTH2 <- NULL
X11975_1995$MONTH2 <- factor(X11975_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X11975_1995_fortify <- fortify(X11975_1995_hpi)

# X12080_1994
X12080_1994$LAT <- gsub(",","",X12080_1994$LAT)  
X12080_1994$LAT <- as.numeric(X12080_1994$LAT)
X12080_1994$LONG <- as.numeric(as.character((X12080_1994$LONG)))

X12080_1994$MONTH2 <- NULL
X12080_1994$MONTH2 <- factor(X12080_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12080_1994_fortify <- fortify(X12080_1994_hpi)

# X12080_1995
X12080_1995$LAT <- gsub(",","",X12080_1995$LAT)  
X12080_1995$LAT <- as.numeric(X12080_1995$LAT)
X12080_1995$LONG <- as.numeric(as.character((X12080_1995$LONG)))

X12080_1995$MONTH2 <- NULL
X12080_1995$MONTH2 <- factor(X12080_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12080_1995_fortify <- fortify(X12080_1995_hpi)

# X12086_1995
X12086_1995$LAT <- gsub(",","",X12086_1995$LAT)  
X12086_1995$LAT <- as.numeric(X12086_1995$LAT)
X12086_1995$LONG <- as.numeric(as.character((X12086_1995$LONG)))

X12086_1995$MONTH2 <- NULL
X12086_1995$MONTH2 <- factor(X12086_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12086_1995_fortify <- fortify(X12086_1995_hpi)

# X12092_1994
X12092_1994$LAT <- gsub(",","",X12092_1994$LAT)  
X12092_1994$LAT <- as.numeric(X12092_1994$LAT)
X12092_1994$LONG <- as.numeric(as.character((X12092_1994$LONG)))

X12092_1994$MONTH2 <- NULL
X12092_1994$MONTH2 <- factor(X12092_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X12092_1994_fortify <- fortify(X12092_1994_hpi)

# X13284_1993
X13284_1993$LAT <- gsub(",","",X13284_1993$LAT)  
X13284_1993$LAT <- as.numeric(X13284_1993$LAT)
X13284_1993$LONG <- as.numeric(as.character((X13284_1993$LONG)))

X13284_1993$MONTH2 <- NULL
X13284_1993$MONTH2 <- factor(X13284_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13284_1993_fortify <- fortify(X13284_1993_hpi)

# X13289_1993
X13289_1993$LAT <- gsub(",","",X13289_1993$LAT)  
X13289_1993$LAT <- as.numeric(X13289_1993$LAT)
X13289_1993$LONG <- as.numeric(as.character((X13289_1993$LONG)))

X13289_1993$MONTH2 <- NULL
X13289_1993$MONTH2 <- factor(X13289_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13289_1993_fortify <- fortify(X13289_1993_hpi)

# X13289_1994
X13289_1994$LAT <- gsub(",","",X13289_1994$LAT)  
X13289_1994$LAT <- as.numeric(X13289_1994$LAT)
X13289_1994$LONG <- as.numeric(as.character((X13289_1994$LONG)))

X13289_1994$MONTH2 <- NULL
X13289_1994$MONTH2 <- factor(X13289_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13289_1994_fortify <- fortify(X13289_1994_hpi)

# X13292_1993
X13292_1993$LAT <- gsub(",","",X13292_1993$LAT)  
X13292_1993$LAT <- as.numeric(X13292_1993$LAT)
X13292_1993$LONG <- as.numeric(as.character((X13292_1993$LONG)))

X13292_1993$MONTH2 <- NULL
X13292_1993$MONTH2 <- factor(X13292_1993$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13292_1993_fortify <- fortify(X13292_1993_hpi)

# X13292_1994
X13292_1994$LAT <- gsub(",","",X13292_1994$LAT)  
X13292_1994$LAT <- as.numeric(X13292_1994$LAT)
X13292_1994$LONG <- as.numeric(as.character((X13292_1994$LONG)))

X13292_1994$MONTH2 <- NULL
X13292_1994$MONTH2 <- factor(X13292_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13292_1994_fortify <- fortify(X13292_1994_hpi)

# X13428_1994
X13428_1994$LAT <- gsub(",","",X13428_1994$LAT)  
X13428_1994$LAT <- as.numeric(X13428_1994$LAT)
X13428_1994$LONG <- as.numeric(as.character((X13428_1994$LONG)))

X13428_1994$MONTH2 <- NULL
X13428_1994$MONTH2 <- factor(X13428_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13428_1994_fortify <- fortify(X13428_1994_hpi)

# X13437_1995
X13437_1995$LAT <- gsub(",","",X13437_1995$LAT)  
X13437_1995$LAT <- as.numeric(X13437_1995$LAT)
X13437_1995$LONG <- as.numeric(as.character((X13437_1995$LONG)))

X13437_1995$MONTH2 <- NULL
X13437_1995$MONTH2 <- factor(X13437_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13437_1995_fortify <- fortify(X13437_1995_hpi)

# X13746_1994
X13746_1994$LAT <- gsub(",","",X13746_1994$LAT)  
X13746_1994$LAT <- as.numeric(X13746_1994$LAT)
X13746_1994$LONG <- as.numeric(as.character((X13746_1994$LONG)))

X13746_1994$MONTH2 <- NULL
X13746_1994$MONTH2 <- factor(X13746_1994$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13746_1994_fortify <- fortify(X13746_1994_hpi)

# X13746_1995
X13746_1995$LAT <- gsub(",","",X13746_1995$LAT)  
X13746_1995$LAT <- as.numeric(X13746_1995$LAT)
X13746_1995$LONG <- as.numeric(as.character((X13746_1995$LONG)))

X13746_1995$MONTH2 <- NULL
X13746_1995$MONTH2 <- factor(X13746_1995$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X13746_1995_fortify <- fortify(X13746_1995_hpi)

# X30126_1998
X30126_1998$LAT <- gsub(",","",X30126_1998$LAT)  
X30126_1998$LAT <- as.numeric(X30126_1998$LAT)
X30126_1998$LONG <- as.numeric(as.character((X30126_1998$LONG)))

X30126_1998$MONTH2 <- NULL
X30126_1998$MONTH2 <- factor(X30126_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30126_1998_fortify <- fortify(X30126_1998_hpi)

# X30126_1999
X30126_1999$LAT <- gsub(",","",X30126_1999$LAT)  
X30126_1999$LAT <- as.numeric(X30126_1999$LAT)
X30126_1999$LONG <- as.numeric(as.character((X30126_1999$LONG)))

X30126_1999$MONTH2 <- NULL
X30126_1999$MONTH2 <- factor(X30126_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30126_1999_fortify <- fortify(X30126_1999_hpi)

# X30129_1998
X30129_1998$LAT <- gsub(",","",X30129_1998$LAT)  
X30129_1998$LAT <- as.numeric(X30129_1998$LAT)
X30129_1998$LONG <- as.numeric(as.character((X30129_1998$LONG)))

X30129_1998$MONTH2 <- NULL
X30129_1998$MONTH2 <- factor(X30129_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30129_1998_fortify <- fortify(X30129_1998_hpi)

# X30129_1999
X30129_1999$LAT <- gsub(",","",X30129_1999$LAT)  
X30129_1999$LAT <- as.numeric(X30129_1999$LAT)
X30129_1999$LONG <- as.numeric(as.character((X30129_1999$LONG)))

X30129_1999$MONTH2 <- NULL
X30129_1999$MONTH2 <- factor(X30129_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30129_1999_fortify <- fortify(X30129_1999_hpi)

# X30131_1998
X30131_1998$LAT <- gsub(",","",X30131_1998$LAT)  
X30131_1998$LAT <- as.numeric(X30131_1998$LAT)
X30131_1998$LONG <- as.numeric(as.character((X30131_1998$LONG)))

X30131_1998$MONTH2 <- NULL
X30131_1998$MONTH2 <- factor(X30131_1998$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30131_1998_fortify <- fortify(X30131_1998_hpi)

# X30131_1999
X30131_1999$LAT <- gsub(",","",X30131_1999$LAT)  
X30131_1999$LAT <- as.numeric(X30131_1999$LAT)
X30131_1999$LONG <- as.numeric(as.character((X30131_1999$LONG)))

X30131_1999$MONTH2 <- NULL
X30131_1999$MONTH2 <- factor(X30131_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30131_1999_fortify <- fortify(X30131_1999_hpi)

# X30135_1999
X30135_1999$LAT <- gsub(",","",X30135_1999$LAT)  
X30135_1999$LAT <- as.numeric(X30135_1999$LAT)
X30135_1999$LONG <- as.numeric(as.character((X30135_1999$LONG)))

X30135_1999$MONTH2 <- NULL
X30135_1999$MONTH2 <- factor(X30135_1999$MONTH, levels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

X30135_1999_fortify <- fortify(X30135_1999_hpi)


# 14. ANNUAL Mapping 95% hpi KDEs (LAT/LONG) ----------------

names(bear_annualKDE_list)

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
ggplot() +
  geom_point(data=X30135_1999, mapping=aes(x=LONG, y=LAT, shape=factor(MONTH2))) +
  scale_shape_manual(values=c(0, 15, 1, 16, 2, 17, 5, 18, 6, 4, 3, 8)) +
  geom_polygon(data=X30135_1999_fortify, aes(long, lat, group=group), size=.2, color="black", alpha=0) + 
  scale_x_continuous(limits=c(-90, -45), breaks=c(-90, -85, -80, -75, -70, -65, -60, -55, -50, -45), labels=c("-90", "-85", "-80", "-75", "-70", "-65", "-60", "-55", "-50", "-45")) +
  scale_y_continuous(limits=c(45, 75), breaks=c(45, 50, 55, 60, 65, 70, 75), labels=c("45", "50", "55", "60", "65", "70", "75")) +
  labs(title="Bear X30135 in 1999", x="Longitude", y="Latitude", shape="Month")



# 15. testing orphaned hole issue with sublist of problem bear/year + one other bear/year ------------------

sublist <- list(X10703_1993, X10703_1994)
names(sublist) <- c("X10703_1993", "X10703_1994")
names(sublist)

# Make empty list and then create amse pilot with loop
amse_sublisttest <- list()

for(i in 1:length(sublist)){
  temp_dat <- sublist[[i]]
  amse_sublisttest[[i]] <- Hpi(temp_dat[, 13:14], pilot="amse", binned=T) 
} 

# Make empty list and then make amse contours
sublist_polygon <- list()

for(i in 1:length(amse_sublisttest)){
  sublist_contour <- kde(sublist[[i]][13:14], H=amse_sublisttest[[i]]) 
  sublist_contourlevels <- contourLevels(sublist_contour, cont = 95)               
  sublist_lines <- contourLines(x=sublist_contour$eval.points[[1]], y=sublist_contour$eval.points[[2]], 
                                       z=sublist_contour$estimate, level=sublist_contourlevels) 
  sublist_sldf = ContourLines2SLDF(sublist_lines)  
  sublist_polyset = SpatialLines2PolySet(sublist_sldf) 
  sublist_polygon[[i]] = PolySet2SpatialPolygons(sublist_polyset) # note: CRS isn't required here, and the error message below pops up regardless
} 

# name items in list (same order as bear_annualKDE_list)
names(sublist_polygon) <- c("X10703_1993", "X10703_1994")
head(sublist_polygon$X10703_1993) # check that it worked
names(sublist_polygon) 

# test area
gArea(sublist_polygon[[2]])
gArea(sublist_polygon$X10703_1994) # get the same error message as earlier

# create empty dataframe to put in the areas and fill first row with IDs
hpi_area_df <- data.frame(ID_YEAR=rep(NA, length(sublist)), ID = rep(NA, length(sublist)), YEAR = rep(NA, length(sublist)), AREA = rep(NA, length(sublist)))
hpi_area_df$ID_YEAR <- c("X10703_1993", "X10703_1994")
hpi_area_df$ID <- c("X10703", "X10703")
hpi_area_df$YEAR <- c("1993", "1994")

# Make an empty list
hpi_area <- list()

# create loop to calculate the areas
#for(i in 1:length(sublist_polygon)){
  i=2 # only i=2 (not i=1) gives the error message - so the same bear as above is causing issues
  hpi_area[[i]] <- gArea(sublist_polygon[[i]])
#} # same error message as before

# test gArea 
gArea(sublist_polygon[[2]]) # error message
gArea(sublist_polygon$X10703_1994) # error message
# test plotting 
plot(sublist_polygon[[2]]) # seems to plot fine
points(x=X10703_1994$EASTING, y=X10703_1994$NORTHING)







# 16. Testing kernels with single bears -------------

# "X10695" "X10709" "X12083" "X12092" "X11975" "X30140" "X30135" "X30129" "X30131" "X30126" 
# "X10700" "X13284" "X13292" "X13289" "X11974" "X10703" "X10707" "X12081" "X12082"
# "X13428" "X13437" "X12086" "X03956" "X10374" "X12080" "X12078" "X13746" "X10393" 


# Using different KDEs (package: ks)

# making pilots - get different values for each
X13289_h1 = Hpi(X13289[, 11:10], pilot="samse", binned=T)
X13289_h2 = Hpi(X13289[, 11:10], pilot='unconstr', binned=T)
X13289_h3 = Hpi(X13289[, 11:10], pilot='dunconstr', binned=T)
X13289_h4 = Hpi(X13289[, 11:10], pilot='amse', binned=T)

# plotting pilots
X13289_kernh1 <- kde(X13289[,11:10], H=X13289_h1)
X13289_kernh1_cont = contourLevels(X13289_kernh1, cont = 95)
X13289_kernh1_line = contourLines(x = X13289_kernh1$eval.points[[1]], y = X13289_kernh1$eval.points[[2]], z = X13289_kernh1$estimate, level = X13289_kernh1_cont)
X13289_kernh1_sldf = ContourLines2SLDF(X13289_kernh1_ligne)
plot(X13289_kernh1_sldf)

X13289_kernh2 <- kde(X13289[,11:10], H=X13289_h2)
X13289_kernh2_cont = contourLevels(X13289_kernh2, cont = 95)
X13289_kernh2_line = contourLines(x = X13289_kernh2$eval.points[[1]], y = X13289_kernh2$eval.points[[2]], z = X13289_kernh2$estimate, level = X13289_kernh2_cont)
X13289_kernh2_sldf = ContourLines2SLDF(X13289_kernh2_ligne)
plot(X13289_kernh2_sldf)

X13289_kernh3 <- kde(X13289[,11:10], H=X13289_h3)
X13289_kernh3_cont = contourLevels(X13289_kernh3, cont = 95)
X13289_kernh3_line = contourLines(x = X13289_kernh3$eval.points[[1]], y = X13289_kernh3$eval.points[[2]], z = X13289_kernh3$estimate, level = X13289_kernh3_cont)
X13289_kernh3_sldf = ContourLines2SLDF(X13289_kernh3_ligne)
plot(X13289_kernh3_sldf)

X13289_kernh4 <- kde(X13289[,11:10], H=X13289_h4)
X13289_kernh4_cont = contourLevels(X13289_kernh4, cont = 95)
X13289_kernh4_line = contourLines(x = X13289_kernh4$eval.points[[1]], y = X13289_kernh4$eval.points[[2]], z = X13289_kernh4$estimate, level = X13289_kernh4_cont)
X13289_kernh4_sldf = ContourLines2SLDF(X13289_kernh4_line)
plot(X13289_kernh4_sldf)
points(x=X13289$LONG, y=X13289$LAT)


# Plotting isopleths for samse pilot

#Create list of polygons of different isopleth from 50% to 95% 
SPDF=list()
levels=seq(95,50,by=-2.5)
for (j in 1 : length(levels)){
  cat(j,"\n")
  # Extract every isopleth (contour lines) of level j
  cont = contourLevels(X13289_kernh1, cont = levels[j])
  line = contourLines(x = X13289_kernh1$eval.points[[1]], y = X13289_kernh1$eval.points[[2]], z = X13289_kernh1$estimate, level = cont)
  sldf = ContourLines2SLDF(line)
  
  # Set projection (UTM) for polygons
  proj4string(sldf) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  # Convert contour lines into polygons
  sldf = SpatialLines2PolySet(sldf)
  sldf = PolySet2SpatialPolygons(sldf)
  
  #list of every polygons made with each contour level
  SPDF[[j]]=sldf
  names(SPDF)[j]=levels[j]
  
}

#Set shading colours for every contour lines (i.e. gradient from yellow to red)
cols = colorRampPalette(c("yellow","red"))
cols = cols(length(SPDF))
#borders of every isopleths
bord = "NA"

#Export results on a single plot by overlaping all polygons of different colors/isopleth 
plot(SPDF[[1]],col=cols[1],border=bord, axes = F, main = "Bear X13289 samse kernel density estimate")

##
for (i in 2 : length(SPDF)){
  plot(SPDF[[i]],col=cols[i],border=bord, add=T)
}
points(X13289[, 11:10],pch=4,cex=0.75)
scalebar(d=100,type="bar", label=c("0","","10km"))






# Plotting isopleths for unconstr pilot

#Create list of polygons of different isopleth from 50% to 95% 
SPDF=list()
levels=seq(95,50,by=-2.5)
for (j in 1 : length(levels)){
  cat(j,"\n")
  # Extract every isopleth (contour lines) of level j
  cont = contourLevels(X13289_kernh2, cont = levels[j])
  line = contourLines(x = X13289_kernh2$eval.points[[1]], y = X13289_kernh2$eval.points[[2]], z = X13289_kernh2$estimate, level = cont)
  sldf = ContourLines2SLDF(line)
  
  # Set projection (UTM) for polygons
  proj4string(sldf) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  # Convert contour lines into polygons
  sldf = SpatialLines2PolySet(sldf)
  sldf = PolySet2SpatialPolygons(sldf)
  
  #list of every polygons made with each contour level
  SPDF[[j]]=sldf
  names(SPDF)[j]=levels[j]
  
}

#Set shading colours for every contour lines (i.e. gradient from yellow to red)
cols = colorRampPalette(c("yellow","red"))
cols = cols(length(SPDF))
#borders of every isopleths
bord = "NA"

#Export results on a single plot by overlaping all polygons of different colors/isopleth 
plot(SPDF[[1]],col=cols[1],border=bord, axes = F, main = "Bear X13289 unconstr kernel density estimate")

##
for (i in 2 : length(SPDF)){
  plot(SPDF[[i]],col=cols[i],border=bord, add=T)
}
points(X13289_locations[, 9:8],pch=4,cex=0.75)
scalebar(d=100,type="bar", label=c("0","","10km"))




# Plotting isopleths for dunconstr pilot

#Create list of polygons of different isopleth from 50% to 95% 
SPDF=list()
levels=seq(95,50,by=-2.5)
for (j in 1 : length(levels)){
  cat(j,"\n")
  # Extract every isopleth (contour lines) of level j
  cont = contourLevels(X13289_kernh3, cont = levels[j])
  line = contourLines(x = X13289_kernh3$eval.points[[1]], y = X13289_kernh3$eval.points[[2]], z = X13289_kernh3$estimate, level = cont)
  sldf = ContourLines2SLDF(line)
  
  # Set projection (UTM) for polygons
  proj4string(sldf) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  # Convert contour lines into polygons
  sldf = SpatialLines2PolySet(sldf)
  sldf = PolySet2SpatialPolygons(sldf)
  
  #list of every polygons made with each contour level
  SPDF[[j]]=sldf
  names(SPDF)[j]=levels[j]
  
}

#Set shading colours for every contour lines (i.e. gradient from yellow to red)
cols = colorRampPalette(c("yellow","red"))
cols = cols(length(SPDF))
#borders of every isopleths
bord = "NA"

#Export results on a single plot by overlaping all polygons of different colors/isopleth 
plot(SPDF[[1]],col=cols[1],border=bord, axes = F, main = "Bear X13289 dunconstr kernel density estimate")

##
for (i in 2 : length(SPDF)){
  plot(SPDF[[i]],col=cols[i],border=bord, add=T)
}
points(X13289_locations[, 9:8],pch=4,cex=0.75)
scalebar(d=100,type="bar", label=c("0","","10km"))





# Plotting isopleths for amse pilot

#Create list of polygons of different isopleth from 50% to 95% 
SPDF=list()
levels=seq(95,50,by=-2.5)
for (j in 1 : length(levels)){
  cat(j,"\n")
  # Extract every isopleth (contour lines) of level j
  cont = contourLevels(X13289_kernh4, cont = levels[j])
  line = contourLines(x = X13289_kernh4$eval.points[[1]], y = X13289_kernh4$eval.points[[2]], z = X13289_kernh4$estimate, level = cont)
  sldf = ContourLines2SLDF(line)
  
  # Set projection (UTM) for polygons
  proj4string(sldf) <- CRS("+proj=stere +lat_0=90 +lat_ts=70 +lon_0=-45 +k=1 +x_0=0 +y_0=0 +a=6378273 +b=6356889.449 +units=m +no_defs")
  
  # Convert contour lines into polygons
  sldf = SpatialLines2PolySet(sldf)
  sldf = PolySet2SpatialPolygons(sldf)
  
  #list of every polygons made with each contour level
  SPDF[[j]]=sldf
  names(SPDF)[j]=levels[j]
  
}

#Set shading colours for every contour lines (i.e. gradient from yellow to red)
cols = colorRampPalette(c("yellow","red"))
cols = cols(length(SPDF))
#borders of every isopleths
bord = "NA"

#Export results on a single plot by overlaping all polygons of different colors/isopleth 
plot(SPDF[[1]],col=cols[1],border=bord, axes = F, main = "Bear X13289 amse kernel density estimate")

##
for (i in 2 : length(SPDF)){
  plot(SPDF[[i]],col=cols[i],border=bord, add=T)
}
points(X13289_locations[, 9:8],pch=4,cex=0.75)
scalebar(d=100,type="bar", label=c("0","","10km"))