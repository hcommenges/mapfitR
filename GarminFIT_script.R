###########################################
######## Explore Garmin FIT tracks
######## Script
###########################################


# load packages ----

library(lubridate)
library(ISOweek)
library(ggplot2)
library(leaflet)
library(sf)
library(dplyr)

# load functions ----
source(file = "GarminFIT_fct.R")

# sync raw files ----
SyncFiles()

# load data ----
listActivities <- readRDS(file = "listactivities.Rds")

# update data ----
listActivities <- AddNewrun(listActivities)

# view one run ----
ViewRun(listActivities, date = "2017-09-03")

# view one run among a set of superimposed tracks ----
ViewRunOverlaid(listActivities, date = "2017-09-03")

# view totals ----
ViewTotals(listActivities)
