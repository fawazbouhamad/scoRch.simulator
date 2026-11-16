# Small data files for the browser test (guided.cjs). They are written with
# ncdf4 alone, so they can be made before scoRch.simulator is installed and
# before its window opens:
#   Rscript software/tests/browser/fixtures.R <demo NetCDF file> <folder>
# The test finds them through SCORCH_UI_FIXTURES, the folder given here.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !file.exists(args[1L])) {
  stop("Use: Rscript software/tests/browser/fixtures.R ",
       "demo/input/north_america_processed_tmax.nc <folder for the small files>", call. = FALSE)
}
folder <- args[2L]
dir.create(folder, recursive = TRUE, showWarnings = FALSE)

# One variable of daily values on a longitude-latitude grid. `values` is
# longitude x latitude x day. `missing` NULL declares no missing value.
write_daily <- function(name, variable, units, values, lon, lat, days, since, attributes = list(), missing = 1e30) {
  axes <- list(ncdf4::ncdim_def("lon", "degrees_east", lon), ncdf4::ncdim_def("lat", "degrees_north", lat),
               ncdf4::ncdim_def("time", paste("days since", since), days, calendar = "standard"))
  defined <- ncdf4::ncvar_def(variable, units, axes, missval = missing, prec = "double")
  file <- file.path(folder, name)
  nc <- ncdf4::nc_create(file, defined)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  ncdf4::ncvar_put(nc, defined, values)
  for (key in names(attributes)) ncdf4::ncatt_put(nc, variable, key, attributes[[key]])
  cat(sprintf("Wrote %s (%s, %d days, %d by %d cells)\n", file, variable, length(days), length(lon), length(lat)))
}

# 1. July and August 2012 of the bundled example, unchanged: a prepared file
# that holds only part of the year. Chosen first in a session, the window
# must offer July-August, not the April-September of the study.
demo <- ncdf4::nc_open(args[1L])
since <- sub("^days since ", "", demo$dim$time$units)
dates <- as.Date(since) + demo$dim$time$vals
keep <- which(dates >= as.Date("2012-07-01") & dates <= as.Date("2012-08-31"))
stopifnot(length(keep) == 62L, all(diff(keep) == 1L))
lon <- as.numeric(demo$dim$lon$vals)
lat <- as.numeric(demo$dim$lat$vals)
summer <- ncdf4::ncvar_get(demo, "tmax", start = c(1L, 1L, keep[1L]), count = c(-1L, -1L, length(keep)))
ncdf4::nc_close(demo)
stopifnot(identical(dim(summer), c(length(lon), length(lat), 62L)), all(is.finite(summer)))
write_daily("july_august.nc", "tmax", "degC", summer, lon, lat, demo$dim$time$vals[keep], since,
            list(standard_name = "air_temperature", cell_methods = "time: maximum"))

# 2. A daily minimum. SCORCH analyzes daily maxima, so the window must ask
# before it takes this variable, and must not call it a maximum afterwards.
small_lon <- c(10, 11, 12, 13)
small_lat <- c(40, 41, 42)
june <- 0:29
cool <- array(285 + (seq_len(4L * 3L * 30L) %% 9), c(4L, 3L, 30L))
write_daily("daily_minimum.nc", "tasmin", "K", cool, small_lon, small_lat, june, "2001-06-01",
            list(standard_name = "air_temperature", long_name = "Daily Minimum Near-Surface Air Temperature",
                 cell_methods = "time: minimum"))

# 3. Daily maxima in which two cells hold 1e20, a number that marks missing
# data but that this file does not declare as missing. The window must stop
# such a selection instead of analyzing 1e20 as a temperature.
holes <- array(20 + (seq_len(4L * 3L * 30L) %% 15), c(4L, 3L, 30L))
holes[1L, 1L, ] <- 1e20
holes[3L, 2L, ] <- 1e20
write_daily("undeclared_missing.nc", "tmax", "degC", holes, small_lon, small_lat, june, "2001-06-01",
            list(standard_name = "air_temperature", cell_methods = "time: maximum"), missing = NULL)
