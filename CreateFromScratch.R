###########################################
######## Explore Garmin FIT tracks
######## Create data from scratch
###########################################

# load packages ----

library(lubridate)
library(sf)


# SOURCE FUNCTIONS ----

# convert fit files into gpx

Fit2Gpx <- function(file){
  system(command = paste0("fit2tcx -i ", file, ".FIT -o temp.tcx"))
  system(command = paste0("gpsbabel -i gtrnctr -f temp.tcx -o gpx -F ", file, ".gpx"))
  system(command = "rm temp.tcx")
}

# extract data and compute summaries

RunSum <- function(gpxname){
  trackPts <- read_sf(paste0("DATA/", gpxname, ".gpx"), layer = "track_points", stringsAsFactors = FALSE)
  trackPts$speed <- trackPts$speed / 1000 * 3600
  trackPts$time <- ymd_hms(paste(substr(trackPts$time, 1, 10), substr(trackPts$time, 12, 19), sep = " "), tz = "Europe/Paris")
  trackPts <- trackPts[, c("track_seg_point_id", "ele", "time", "speed", "geometry")]
  oneLineStr <- st_linestring(x = do.call(rbind, trackPts$geometry), dim = "XY")
  oneLine <- st_sf(ID = 1,
                   geometry = st_sfc(oneLineStr),
                   crs = 4326)
  totalDistance <- as.numeric(round(st_length(oneLine) / 1000, digits = 1))
  totalTime <- as.numeric(difftime(time1 = trackPts$time[nrow(trackPts)], time2 = trackPts$time[1], units = "sec"))
  difTime <- difftime(trackPts$time[2:nrow(trackPts)], trackPts$time[1:nrow(trackPts) - 1], units = "sec")
  pauseTime <- as.numeric(sum(difTime[difTime > 20]))
  totalTimeCorr <- round((totalTime - pauseTime) / 60)
  avgSpeed <- round(60 * totalDistance / totalTimeCorr, digits = 1)
  trackDate <- substr(trackPts$time[1], 1, 10)
  trackSummary <- list(
    PTS = trackPts,
    LINE = oneLine,
    DIST = totalDistance,
    TIME = totalTimeCorr,
    SPEED = avgSpeed,
    DATE = trackDate
  )
  return(trackSummary)
}


# APPLY FUNCTIONS ----

# get list of FIT files (here in a directory called "DATA/")
vecFit <- unique(gsub(list.files("DATA/"), pattern = ".gpx|.FIT", replacement = ""))

# create gpx files in the same directory
lapply(vecFit, Fit2Gpx)

# create geoinformation from gpx files
listActivities <- lapply(vecFit, RunSum)
names(listActivities) <- vecFit

# save list of activites in R native format
saveRDS(object = listActivities, file = "listactivities.Rds")
