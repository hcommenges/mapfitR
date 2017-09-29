###########################################
######## Explore Garmin FIT tracks
######## Functions
###########################################


# sync files from watch to local ----

SyncFiles <- function(source = "PathToGarminWatch/GARMIN/ACTIVITY/", destination = "PathToDataDirectory/DATA"){
  system(command = paste0("rsync -avh ", source, " ", destination))
}


# convert fit files into gpx ----

Fit2Gpx <- function(dsn, file){
  oriWd <- getwd()
  setwd(paste0(oriWd, "/", dsn))
  system(command = paste0("fit2tcx -i ", file, " -o temp.tcx"))
  system(command = paste0("gpsbabel -i gtrnctr -f temp.tcx -o gpx -F ", substr(file, 1 , nchar(file) - 4), ".gpx"))
  system(command = "rm temp.tcx")
  setwd(oriWd)
}


# add new run to history ----

AddNewrun <- function(listact){
  lengthOri <- length(listact)
  namesOri <- names(listact)
  vecFit <- unique(gsub(list.files("DATA/"), pattern = ".gpx|.FIT", replacement = ""))
  fitCheck <- vecFit %in% names(listact)
  fitToSync <- vecFit[!fitCheck]
  if(length(fitToSync) > 0){
    for(i in fitToSync){
      tempTracks <- Fit2Gpx(dsn = "DATA/", file = paste0(i, ".FIT"))
      tempSummary <- RunSum(dsn = "DATA/", file = paste0(i, ".gpx"))
      listact[[length(listact) + 1]] <- tempSummary
    }
  }
  if(lengthOri < length(listact)){
    names(listact) <- c(namesOri, fitToSync)
    saveRDS(object = listact, file = "listactivities.Rds")
  }

  return(listact)
}


# extract data and compute summaries ----

RunSum <- function(dsn, file){
  trackPts <- read_sf(paste0(dsn, file), layer = "track_points", stringsAsFactors = FALSE)
  trackPts$speed <- trackPts$speed / 1000 * 3600
  trackPts$time <- ymd_hms(paste(substr(trackPts$time, 1, 10), substr(trackPts$time, 12, 19), sep = " "), tz = "Europe/Paris")
  trackPts <- trackPts[, c("track_seg_point_id", "ele", "time", "speed", "geometry")]
  oneLineStr <- st_linestring(x = st_coordinates(trackPts), dim = "XY")
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


# draw speed plot ----

SpeedPlot <- function(onerun, span = 0.1, fixedaxis = TRUE){
  medSpeed <- median(onerun$speed, na.rm = TRUE)
  onerun$speed <- ifelse(onerun$speed < 3, medSpeed, onerun$speed)
  speedPlot <- ggplot(onerun, aes(x = time, y = speed)) +
    geom_smooth(method = "loess", span = span, se = FALSE, color = "firebrick") +
    theme_bw()

  if(isTRUE(fixedaxis)){
    speedPlot <- speedPlot + scale_x_datetime("Heure") + scale_y_continuous("Vitesse (km/h)", limits = c(8, 15), breaks = seq(8, 15, 1))
  } else {
    speedPlot <- speedPlot + scale_x_datetime("Heure") + scale_y_continuous("Vitesse (km/h)")
  }
  return(speedPlot)
}


# map run ----

MapTracks <- function(onerun){
  leafMap <- leaflet() %>% addTiles() %>% addPolylines(color = "firebrick", weight = 3, opacity = 0.8, data = onerun$geometry)
  return(leafMap)
}


# view run summaries and plots ----

ViewRun <- function(listact, date){
  oneRun <- listact[[which(sapply(listact, function(x) x$DATE == date) == TRUE)]]
  print(paste0("Date : ", oneRun$DATE))
  print(paste0("Distance : ", oneRun$DIST, " km"))
  print(paste0("Temps : ", oneRun$TIME, " mn"))
  print(paste0("Vitesse : ", oneRun$SPEED, " km/h"))
  print(SpeedPlot(oneRun$PTS))
  MapTracks(oneRun$LINE)
}


# view Vincennes run bunch ----

ViewRunOverlaid <- function(listact, date, mybox = NULL){
  oneRun <- listact[[which(sapply(listact, function(x) x$DATE == date) == TRUE)]]
  listactRef <- listact[- which(sapply(listact, function(x) x$DATE == date) == TRUE)]
  if(is.null(mybox)){
    listLines <- lapply(listactRef, function(x) x$LINE)
    allLines <- do.call(rbind, listLines)
  } else {
    idBox <- sapply(listactRef, function(x) length(unlist(st_within(x = x$PTS, y = mybox))) > 0)
    listLines <- lapply(listactRef[idBox], function(x) x$LINE)
    allLines <- do.call(rbind, listLines)
  }

  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolylines(color = "black", weight = 6, opacity = 0.2, data = allLines$geometry) %>%
    addPolylines(color = "firebrick", weight = 3, opacity = 0.8, data = oneRun$LINE$geometry)
}


# view totals ----

ViewTotals <- function(listact){
  getRuns <- data.frame(DATE = sapply(listact, function(x) x$DATE),
                        DIST = sapply(listact, function(x) x$DIST),
                        TIME = sapply(listact, function(x) x$TIME),
                        stringsAsFactors = FALSE)

  avgWeekDist <- 7 * sum(getRuns$DIST) / as.integer(max(ymd(getRuns$DATE)) - min(ymd(getRuns$DATE)))
  getRuns$WEEK <- isoweek(ymd(getRuns$DATE))
  idWeek <- data.frame(WEEK = seq(min(getRuns$WEEK), max(getRuns$WEEK), 1))
  weekSummary <- getRuns %>% group_by(WEEK) %>% summarise(DISTANCE = sum(DIST), TIME = sum(TIME))
  weekSummary <- idWeek %>%
    left_join(y = weekSummary, by = "WEEK") %>%
    mutate(DISTANCE = ifelse(is.na(DISTANCE), 0, DISTANCE),
           TIME = ifelse(is.na(TIME), 0, TIME))

  weekSummary$DAY1 <- substr(date_in_week(year = 2017, week = weekSummary$WEEK, weekday = 1), 6, 10)
  weekSummary$DAY7 <- substr(date_in_week(year = 2017, week = weekSummary$WEEK, weekday = 7), 6, 10)

  sumPlot <- ggplot(weekSummary) +
    geom_bar(aes(x = factor(WEEK), y = DISTANCE), fill = "firebrick", stat = "identity") +
    geom_hline(yintercept = avgWeekDist, color = "grey30", linetype = 2) +
    geom_smooth(aes(x = WEEK-1, y = DISTANCE), method = "loess", span = 0.7, se = FALSE, color = "grey30") +
    scale_x_discrete("SEMAINES (lundi)", breaks = weekSummary$WEEK, labels = name_day(weekSummary$DAY1)) +
    scale_y_continuous("DISTANCE (km)", breaks = seq(0, 50, 5)) +
    theme_bw()

  print(sumPlot)

  return(weekSummary)
}


# other functions ----

date_in_week <- function(year, week, weekday){
  w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
  ISOweek2date(w)
}

name_day <- function(daynumber){
  monthNumber <- as.integer(substr(daynumber, 1, 2))
  monthNames <- c("Jan", "Fev", "Mar", "Avr", "Mai", "Jun", "Jui", "Aou", "Sep", "Oct", "Nov", "Dec")
  monthName <- monthNames[monthNumber]
  dayNumber <- as.integer(substr(daynumber, 4, 5))
  nameDay <- paste(dayNumber, monthName, sep = "-")
  return(nameDay)
}

theme_empty <- theme_bw() +
  theme(axis.line = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")
