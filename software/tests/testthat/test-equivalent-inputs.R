# The same temperature offered as a prepared file, a Kelvin copy, a ZIP, a
# folder and raw hourly files must reach the analysis as the same numbers.

# Read a selection as the setup window would: detect it, then follow the
# configuration that a guided run saves.
read_selection <- function(path, months = 6:7) {
  info <- detect_input(path)
  expect_length(info$questions, 0L)
  cfg <- read_config(list(input = list(file = path, format = info$format),
                          period = list(warm_season_months = months)))
  list(info = info, tmax = suppressMessages(read_configured_input(cfg)))
}

test_that("equivalent selections give identical daily maximum temperature and events", {
  local_figure_test_dpi()
  window <- example_window()
  folder <- withr::local_tempdir()
  n <- length(window$dates)
  reference <- tmax_from_array(window$celsius, window$dates, window$lon, window$lat, months = 6:7)

  # 1. Prepared daily Tmax in degrees Celsius, and the same file inside a ZIP and alone in a folder.
  prepared <- file.path(folder, "prepared.nc")
  write_tmax(reference, prepared, precision = "float64")
  zipped <- file.path(folder, "prepared.zip")
  zip::zipr(zipped, prepared)
  alone <- file.path(folder, "alone")
  dir.create(alone)
  file.copy(prepared, alone)

  # 2. The same values stored in Kelvin.
  kelvin <- write_grid(file.path(folder, "kelvin.nc"), window$kelvin, as.numeric(window$dates - as.Date("2000-01-01")),
                       window$lon, window$lat, units = "K")

  # 3. Raw hourly 2 m temperature in Kelvin, one point per cell and latitudes
  # from north to south as ERA5 stores them: the daily maximum is reached at
  # 14 UTC and every other hour is cooler.
  hourly <- function(days) {
    cooler <- ((0:23) - 14)^2 / 20
    values <- window$kelvin[rep(days, each = 24), rev(seq_along(window$lat)), , drop = FALSE] - rep(cooler, length(days))
    seconds <- as.numeric(rep(window$dates[days], each = 24)) * 86400 + rep(0:23, length(days)) * 3600
    list(values = values, seconds = seconds)
  }
  june <- which(format(window$dates, "%m") == "06")
  raw <- file.path(folder, "raw")
  dir.create(raw)
  for (part in list(list("era5_2012_06.nc", june), list("era5_2012_07.nc", setdiff(seq_len(n), june)))) {
    hours <- hourly(part[[2]])
    write_grid(file.path(raw, part[[1]]), list(t2m = hours$values), hours$seconds, window$lon - 0.5, rev(window$lat - 0.5),
               units = "K", time_units = "seconds since 1970-01-01", dims = c("longitude", "latitude", "valid_time"))
  }
  raw_zip <- file.path(folder, "raw.zip")
  zip::zipr(raw_zip, raw)

  routes <- list(prepared = prepared, zip = zipped, kelvin = kelvin, raw_folder = raw, raw_zip = raw_zip)
  read <- lapply(routes, read_selection)
  expect_identical(vapply(read, function(r) r$info$format, ""),
                   c(prepared = "daily_tmax", zip = "daily_tmax", kelvin = "daily_tmax", raw_folder = "era5", raw_zip = "era5"))
  expect_identical(unname(vapply(read, function(r) paste(r$info$units, collapse = ","), "")),
                   c("degC", "degC", "K", "K", "K"))
  for (name in names(read)) {
    expect_identical(read[[name]]$tmax[c("dates", "cells", "tmax", "grid")], reference[c("dates", "cells", "tmax", "grid")],
                     info = name)
    expect_equal(read[[name]]$info$dates, window$dates, info = name)
  }
  # Kelvin is converted exactly once on every route: the values stay summer temperatures in Celsius.
  expect_true(all(vapply(read, function(r) max(r$tmax$tmax) < 60 && min(r$tmax$tmax) > 0, logical(1))))
  expect_equal(read$kelvin$info$sample_c, range(window$celsius[1, , ]))

  # A folder that holds one NetCDF file is treated as that file.
  shiny::testServer(scorch_server, {
    choose("data", alone)
    expect_identical(state$path, normalizePath(file.path(alone, "prepared.nc"), winslash = "/"))
    expect_identical(header()$format, "daily_tmax")
    expect_equal(header()$dates, window$dates)
  })

  # The catalogs that follow are therefore the same: compare a prepared and a raw run in full.
  settings <- function(path) list(input = list(file = path), period = list(warm_season_months = 6:7),
                                  power_law = list(bootstrap_repetitions = 10))
  run <- function(path, out) suppressWarnings(suppressMessages(
    run_scorch(c(settings(path), list(output = list(directory = file.path(folder, out)))))))
  from_prepared <- run(prepared, "out-prepared")
  from_raw <- run(raw_zip, "out-raw")
  expect_gt(nrow(from_prepared$events$events), 0L)
  for (stage in c("heatwaves", "regional", "clusters", "ellipses", "events", "power_law")) {
    expect_identical(from_raw[[stage]], from_prepared[[stage]], info = stage)
  }
  books <- c(file.path("catalogs", c("01_heatwave_events.xlsx", "02_regionally_extensive_heatwaves.xlsx",
                                     "03_extreme_heatwave_clusters.xlsx", "04_extreme_heatwave_ellipses.xlsx",
                                     "05_compound_heatwave_events.xlsx")), "Heatwave_Statistics.xlsx")
  for (book in books) {
    expect_identical(workbook_tables(file.path(folder, "out-raw", book)),
                     workbook_tables(file.path(folder, "out-prepared", book)), info = book)
  }
})
