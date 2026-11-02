# Offline check of prepared input and locally supplied ERA5 preprocessing.
# The shipped CDS sample is a real daily-max ERA5 file at 0.25 degrees.
# It checks local-file handling and aggregation; hourly maxima are tested
# separately in tests/testthat/test-era5.R with independently known values.
library(scoRch.simulator)

sample <- system.file("extdata", "cds_daily_tmax_2012_great_plains.nc", package = "scoRch.simulator")
whole <- read_tmax(system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator"),
                   start = "2012-01-01", end = "2012-12-31")
processed <- read_era5(sample, months = 4:9, cell_size = 1)
columns <- match(paste(processed$cells$lon, processed$cells$lat), paste(whole$cells$lon, whole$cells$lat))
stopifnot(!anyNA(columns), identical(processed$dates, whole$dates))
maximum_difference <- max(abs(processed$tmax - whole$tmax[, columns]))
stopifnot(maximum_difference <= 0.0065)

copy <- write_tmax(processed, tempfile(fileext = ".nc"), precision = "float64")
back <- read_tmax(copy)
stopifnot(identical(back$dates, processed$dates),
          identical(back$tmax, processed$tmax))
cat(sprintf("Local ERA5 preprocessing and prepared-file round trip passed; reference difference %.6f C.\n",
            maximum_difference))
