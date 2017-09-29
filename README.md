# mapfitR

R functions for exploring GPS run tracks.

The app relies on 3 external libraries:
- [rsync](https://rsync.samba.org) for syncronizing data between Garmin watch and local directory
- [fit2tcx](http://www.andreas-diesner.de/garminplugin/doku.php?id=fit2tcx) for converting Garmin FIT files into Garmin tcx files (training center)
- [gpsbabel](https://www.gpsbabel.org/) multi-format conversion tool for GPS data

The app relies on the [R software](https://cran.r-project.org) and 6 packages:
- `dplyr` for data manipulation
- `lubridate` and `ISOweek` for temporal formats
- `ggplot2` for graphical outputs
- `sf` for spatial data (note that you need the external library gdal > 2.0)
- `leaflet` for interactive mapping

The [RStudio IDE](https://www.rstudio.com) eases graphic and cartographic visualization.
