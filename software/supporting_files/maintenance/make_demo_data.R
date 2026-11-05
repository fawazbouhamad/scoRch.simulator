# Build demo/input/north_america_processed_tmax.nc from 1996-2025 yearly ERA5 Tmax files.
# Validate the 0.25-degree inputs, average to one-degree cells and record source checksums.
# From the repository root, with the package installed:
# Rscript software/supporting_files/maintenance/make_demo_data.R path/to/raw_daily_nc
suppressPackageStartupMessages({library(ncdf4); library(scoRch.simulator)})
dir <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(dir) || !dir.exists(dir)) stop("give the folder of the yearly <year>_daily_Tmax_NA.nc files")
years <- 1996:2025
files <- file.path(dir, sprintf("%d_daily_Tmax_NA.nc", years))
target <- file.path("demo", "input", "north_america_processed_tmax.nc")

# 1. Every yearly file: structure, metadata, dates, units, missing values.
manifest <- character()
for (k in seq_along(files)) {
  f <- files[k]; year <- years[k]
  if (!file.exists(f)) stop("missing: ", f)
  nc <- nc_open(f)
  dims <- vapply(nc$dim, function(d) d$len, 1)
  stopifnot(identical(unname(dims[c("longitude", "latitude", "time")]), c(289, 101, 183)))
  stopifnot(identical(names(nc$var), "Tmax_C"), ncatt_get(nc, "Tmax_C", "units")$value == "degC")
  stopifnot(as.integer(ncatt_get(nc, 0, "year")$value) == year,
            ncatt_get(nc, 0, "source")$value == "ARCO ERA5 (Google Cloud, Zarr)",
            ncatt_get(nc, 0, "latitude_bounds")$value == "25 to 50",
            ncatt_get(nc, 0, "longitude_bounds")$value == "-130 to -58")
  lon <- nc$dim$longitude$vals; lat <- nc$dim$latitude$vals
  stopifnot(all(abs(lon - seq(230, 302, by = 0.25)) < 1e-9), all(abs(lat - seq(50, 25, by = -0.25)) < 1e-9))
  stopifnot(nc$dim$time$units == sprintf("days since %d-04-01 00:00:00", year), all(nc$dim$time$vals == 0:182))
  x <- ncvar_get(nc, "Tmax_C")
  stopifnot(!anyNA(x), min(x) > -40, max(x) < 60)
  nc_close(nc)
  manifest <- c(manifest, sprintf("%s %d %s", unname(tools::md5sum(f)), file.size(f), basename(f)))
  cat(sprintf("%d: ok (%.1f to %.1f degC)\n", year, min(x), max(x)))
}
writeLines(c("# Source files for demo/input/north_america_processed_tmax.nc (ARCO-ERA5 daily Tmax, 0.25 degrees).",
             "# Columns: md5 size_bytes filename",
             manifest), file.path("software", "supporting_files", "data", "source_checksums.txt"))

# 2. The package's reduction to 1-degree cells, checked.
tmax <- read_era5(files, variable = "Tmax_C", months = 4:9, cell_size = 1)
expected <- seq(as.Date("1996-04-01"), as.Date("2025-09-30"), by = "day")
expected <- expected[as.integer(format(expected, "%m")) %in% 4:9]
stopifnot(nrow(tmax$cells) == 1800, length(tmax$dates) == 5490, all(tmax$dates == expected), !anyNA(tmax$tmax),
          all(abs(tmax$grid$lon - seq(-129.5, -58.5)) < 1e-9), all(abs(tmax$grid$lat - seq(25.5, 49.5)) < 1e-9))
set.seed(20261012)
for (i in 1:8) {                       # the 4 x 4 mean by hand, from the raw file
  cell <- sample(nrow(tmax$cells), 1); date <- sample(tmax$dates, 1)
  nc <- nc_open(files[match(as.integer(format(date, "%Y")), years)])
  lon <- nc$dim$longitude$vals - 360; lat <- nc$dim$latitude$vals
  rows <- which(lon >= tmax$cells$lon[cell] - 0.5 - 1e-9 & lon < tmax$cells$lon[cell] + 0.5 - 1e-9)
  cols <- which(lat >= tmax$cells$lat[cell] - 0.5 - 1e-9 & lat < tmax$cells$lat[cell] + 0.5 - 1e-9)
  t <- which(as.Date(sub(".*since ", "", nc$dim$time$units)) + nc$dim$time$vals == date)
  block <- ncvar_get(nc, "Tmax_C", start = c(min(rows), min(cols), t), count = c(4, 4, 1))
  nc_close(nc)
  stopifnot(length(rows) == 4, length(cols) == 4, abs(mean(block) - tmax$tmax[match(date, tmax$dates), cell]) < 1e-5)
  cat(sprintf("hand check %d: cell %g N %g W on %s: mean of 16 points %.4f = package %.4f\n", i, tmax$cells$lat[cell],
              -tmax$cells$lon[cell], format(date), mean(block), tmax$tmax[match(date, tmax$dates), cell]))
}

# 3. The file.
write_tmax(tmax, target, precision = "int16", overwrite = TRUE,
           title = "scoRch.simulator demonstration data: daily maximum 2-m temperature, 25-50 N and 130-58 W, April-September 1996-2025",
           source = paste("ERA5 (Hersbach et al., 2020) hourly 2-m temperature from the ARCO-ERA5 archive on Google Cloud",
                          "(Carver and Merose, 2023), reduced by the authors to the daily maximum of each UTC calendar day",
                          "at 0.25 degrees (files <year>_daily_Tmax_NA.nc), averaged to 1-degree cells by",
                          "scoRch.simulator::read_era5() and rounded to 0.01 degC"),
           licence = "CC BY 4.0 for the authors' processing; contains modified Copernicus Climate Change Service information 2026 (ERA5)",
           history = sprintf("written on %s by software/supporting_files/maintenance/make_demo_data.R of scoRch.simulator %s from 30 yearly files (software/supporting_files/data/source_checksums.txt)",
                             format(Sys.Date()), as.character(packageVersion("scoRch.simulator"))))
back <- read_tmax(target)
stopifnot(nrow(back$cells) == 1800, length(back$dates) == 5490, !anyNA(back$tmax), max(abs(back$tmax - tmax$tmax)) <= 0.005 + 1e-9)
cat(sprintf("wrote %s: %d cells, %d dates, %.1f MB, md5 %s; Tmax %.2f to %.2f degC, round trip within %.4f degC\n",
            target, nrow(back$cells), length(back$dates), file.size(target) / 1e6, unname(tools::md5sum(target)),
            min(tmax$tmax), max(tmax$tmax), max(abs(back$tmax - tmax$tmax))))
