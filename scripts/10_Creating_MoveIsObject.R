
# 1. load libraries -------

library(moveVis)
library(move)
library(chron)
library(raster)
library(dplyr)
library(lubridate)
library(data.table)

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


# 2. Import data and format for moveVis ------

# import
bears <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")
head(bears)
str(bears)

# create timestamp column - must be as.POSIXct
bears$DATE <- as.POSIXct(bears$DATE, format="%Y-%m-%d", tz="EST") # added random timezone to deal with error I was getting in move_vis
bears$TIMESTAMP <- paste(bears$DATE, bears$TIME)
head(bears)
str(bears)
bears$TIMESTAMP <- as.POSIXct(bears$TIMESTAMP, format="%Y-%m-%d %H:%M")

# http://movevis.org/index.html


###

# WHB data

HB_bears <- read.csv("/Volumes/Larissa G-drive/UAlberta MSc/Thesis/3. Data/WHB_data_movementgraphics/1991_1999_EC_Collar_Data.csv")
head(HB_bears)

HB_bears <- subset(HB_bears, select=-c(DATE2, COLLARYEAR, LAT, LONG, SATELLITE, PCODE, HITS, SEN_MSG, LOC_CODE, LAT_ARG, LONGE_ARG, LONGW_ARG, X, COMPFREQ, REMARKS))
names(HB_bears) <- c("ID", "LAT", "LONG", "YEAR", "MONTH", "YEAR2", "MONTH2", "DATE", "TIME")

# create timestamp column
HB_bears$DATE <- as.POSIXct(HB_bears$DATE, format="%Y-%m-%d", tz="EST")
HB_bears$TIMESTAMP <- paste(HB_bears$DATE, HB_bears$TIME)
head(HB_bears)
str(HB_bears)
HB_bears$TIMESTAMP <- as.POSIXct(HB_bears$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

HB_unique <- unique(setDT(HB_bears), by=c("ID", "DATE")) # remove all duplicates


# 3. TEST Create moveVis gifs (one bear) -------

# test with one bear
X03956 <- bears %>% filter(bears$ID=="X03956")

X03956_move <- df2move(X03956, proj='+proj=longlat +datum=WGS84', x="LONG", y="LAT", time="DATE", track_id="ID")
# +proj=longlat +datum=WGS84
# no warnings

X03956_move_align <- align_move(X03956_move, res="min", unit="mins")
# CRS warning

X03956_frames <- frames_spatial(X03956_move_align, map_service="osm", map_type="watercolor", alpha=0.5) %>%
  add_labels(x="Longitude", y="Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(X03956_move_align, type="label") %>% 
  add_progress()
# works way faster

# view one frame
length(X03956_frames) # there are 13
X03956_frames[[12]] # this looks cool!!

# animate frames
animate_frames(X03956_frames, out_file="moveVis.gif")
#animate_frames(X03956_frames, out_file="X03956.mp4")

suggest_formats()



# 4. TEST Create moveVis gifs (one year, all bears) -------

bears_1994 <- bears %>% filter(bears$YEAR=="1994")

bears_1994_move <- df2move(bears_1994, proj='+proj=longlat +datum=WGS84', x="LONG", y="LAT", time="DATE", track_id="ID")
# 28 CRS warnings

bears_1994_align <- align_move(bears_1994_move, res="min", unit="mins")
# CRS warning


bears_1994_frames <- frames_spatial(bears_1994_align, map_service="osm", map_type="watercolor", alpha=0.5) %>%
  add_labels(x="Longitude", y="Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(bears_1994_align, type="label") %>% 
  add_progress()
# works way faster

# view one frame
length(bears_1994_frames) # 364
bears_1994_frames[[300]] 

# animate frames - this takes awhile
animate_frames(bears_1994_frames, out_file="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/Movement graphics/bears_1994.mp4")



# 5. Create moveVis gifs (all bears, all years) -------

# NOTE: This ended up taking way too long to run, so I stopped it - will try doing per year instead

# convert to move object
bears_move <- df2move(bears, proj="+proj=longlat +datum=WGS84", x="LONG", y="LAT", time="DATE", track_id="ID")
      # because so many don't have times associated with them, tried using the DATE column instead

# align dates
bears_move_align <- align_move(bears_move, res=4, unit="mins")


# create spatial frames
frames <- frames_spatial(bears_move_align, map_service="osm", map_type="watercolor", alpha=0.5) %>%
  add_labels(x="Longitude", y="Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(bears_move_align, type="label") %>% 
  add_progress()

# view one frame
frames[[100]]

# animate frames
animate_frames(frames, out_file="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/Movement graphics/allbears.mp4")


# path_colours=c("red","green", "blue"),

# MoveVis with both DS and WH -----

###

# RUN SECTION 2 FIRST

###

# Organize DS and WHB data first
bear_year_total <- bears %>% group_by(YEAR) %>% summarize(n()) 
ungroup(bears)

HBbear_year_total <- HB_bears %>% group_by(YEAR) %>% summarize(n()) 
ungroup(HB_bears)

HBbear_unique_year_total <- HB_unique %>% group_by(YEAR) %>% summarize(n()) 
ungroup(HB_unique)


# use 1993 as there's lots for both in this year (373 for bears and 353 for HB_unique)
DS_1993 <- bears %>% filter(YEAR=="1993")
head(DS_1993)
DS_1993 = subset(DS_1993, select=-c(X.2, ROWID, ZONE, EASTING, NORTHING, X.1, X, ICE_LAND, SEASON, ANGLE, DIST_KM, DIFF_DATE, KM_PER_DAY, KM_PER_HR, M_PER_HR, M_PER_S, TIMESTAMP))

HB_1993 <- HB_unique %>% filter(YEAR=="1993")
head(HB_1993)
HB_1993 = subset(HB_1993, select=-c(YEAR2, MONTH2, TIMESTAMP))

# make subpopulation columns
DS_1993$SUBPOP <- rep("DS")
HB_1993$SUBPOP <- rep("HB")

# running the movement graphic for all of them wouldn't work (I let it run for ~2 hours)
# try using even less - choose 2 individuals from each
DS_bear_total <- DS_1993 %>% group_by(ID) %>% summarize(n()) 
HB_bear_total <- HB_1993 %>% group_by(ID) %>% summarize(n()) 

      # for HB, the 2 with the most fixs are 01003B (36) and 01004B (34)
      # 2 in DS with similar amounts of points = X10707	(39) and X10703 (38)
            # tried with just one bear as well
DS_1993_subset <- DS_1993 %>% filter(ID=="X10703")
HB_1993_subset <- HB_1993 %>% filter(ID=="01003B")
str(DS_1993_subset)
head(DS_1993_subset)

str(HB_1993_subset)
HB_1993_subset$MONTH <- month.name[HB_1993_subset$MONTH]  
head(HB_1993_subset)

# combine into one df
allbears_1993 <- merge(DS_1993_subset, HB_1993_subset, by=c('ID', 'YEAR', 'DATE', 'LAT', 'LONG', 'SUBPOP', 'MONTH', 'TIME'), all=TRUE)
head(allbears_1993)
str(allbears_1993)
unique(allbears_1993$ID)
unique(allbears_1993$MONTH)

allbears_1993_months <- allbears_1993 %>% group_by(MONTH) %>% summarize(n()) 
# use March, April, May, June
allbears_1993_subset <- allbears_1993 %>% filter(MONTH=="March" | MONTH=="April" | MONTH=="May")
head(allbears_1993_subset)
unique(allbears_1993_subset$ID)
# change IDs so they look better: 01003B, 01004B, X10703, X10707

allbears_1993_subset$ID[allbears_1993_subset$ID == "01004B"] <- "Hudson Bay Polar Bear"
allbears_1993_subset$ID[allbears_1993_subset$ID == "X10703"] <- "Davis Strait Polar Bear"

###

# convert to move object
allbears_1993_move <- df2move(allbears_1993, proj="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0", x="LONG", y="LAT", time="DATE", track_id="ID")

# align dates
allbears_1993_move_align <- align_move(allbears_1993_move, res=4, unit="mins")

# create spatial frames
allbears_1993_frames <- frames_spatial(allbears_1993_move_align, map_service="osm", map_type="watercolor", alpha=0.5) %>%
  add_labels(x="Longitude", y="Latitude") %>% 
  add_northarrow() %>% 
  #add_scalebar() %>% 
  add_timestamps(allbears_1993_move_align, type="label") %>% 
  add_progress()

# view one frame
allbears_1993_frames[[25]]

# animate frames
animate_frames(allbears_1993_frames, out_file="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/Movement graphics/bears_1993_frames.mp4")




# Graphic for Earth Rangers ------

# look at just freeze-up to winter season to show how they move off land
      # find a year with lots of points
bear_year_total <- bears %>% group_by(YEAR) %>% summarize(n()) 
ungroup(bears)
      # most are in 1994, so let's use that year
bears_1994 <- bears %>% filter(YEAR==1994)
bear_month_total <- bears_1994 %>% group_by(MONTH) %>% summarize(n()) 
ungroup(bears_1994)
      # good, there are lots during that time of year!
head(bears_1994)
bears_1994_subset <- bears_1994 %>% filter(MONTH=="September" | MONTH=="October" | MONTH=="November" | MONTH=="December" | MONTH=="January" | MONTH=="February" | MONTH=="March" | MONTH=="April")


###


# convert to move object
bears_1994_subset_move <- df2move(bears_1994_subset, proj="+proj=longlat +datum=WGS84", x="LONG", y="LAT", time="DATE", track_id="ID")

# align dates
bears_1994_subset_move_align <- align_move(bears_1994_subset_move, res=4, unit="mins")

# create spatial frames
EarthRangers_frames <- frames_spatial(bears_1994_subset_move_align, map_service="osm", map_type="watercolor", alpha=0.5) %>%
  add_labels(x="Longitude", y="Latitude") %>% 
  add_northarrow() %>% 
  add_scalebar() %>% 
  add_timestamps(bears_1994_subset_move_align, type="label") %>% 
  add_progress()

# view one frame
EarthRangers_frames[[100]]

# animate frames
animate_frames(EarthRangers_frames, out_file="/Volumes/Larissa G-drive/UAlberta MSc/Thesis/1. Coding/PB_DataExploration/Movement graphics/EarthRangers_frames.mp4")





