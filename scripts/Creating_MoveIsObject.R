
# 1. load libraries -------

library(moveVis)
library(move)
library(chron)
library(raster)
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


# 2. Import data and format for moveVis ------

# import
bears <- read.csv("data/Oct2020work/FINAL DATASET/bears_final_Nov2020_UTM_LATLONG.csv")
head(bears)
str(bears)

# create timestamp column - must be as.POSIXct
bears$DATE <- as.POSIXct(bears$DATE, format="%Y-%m-%d")
bears$TIMESTAMP <- paste(bears$DATE, bears$TIME)
head(bears)
str(bears)
bears$TIMESTAMP <- as.POSIXct(bears$TIMESTAMP, format="%Y-%m-%d %H:%M")

# http://movevis.org/index.html

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
animate_frames(X03956_frames, out_file="X03956.mp4")

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
animate_frames(frames, out_file="moveVis.gif")


# path_colours=c("red","green", "blue"),



