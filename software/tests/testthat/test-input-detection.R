# Small grids: 4 days, 3 latitudes, 4 longitudes, clearly Celsius or Kelvin.
grid_lon <- c(10, 11, 12, 13)
grid_lat <- c(40, 41, 42)
grid_days <- 151:154                                   # 31 May to 3 June 2000
celsius <- array(20 + seq_len(4 * 3 * 4) / 4, c(4, 3, 4))
grid_dates <- as.Date("2000-01-01") + grid_days

# The window's settings for a detected file, with everything else at its default.
plan_for <- function(path, info, answers = list()) {
  base <- read_config(input_required = FALSE)
  form <- c(list(month_from = "1", month_to = "12", native_grid = FALSE, cell_size = 1),
            unlist(lapply(c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp"), function(section) {
              stats::setNames(lapply(names(base[[section]]), function(key) {
                value <- base[[section]][[key]]
                if (section == "clustering") paste(value, collapse = ",") else value
              }), paste(section, names(base[[section]]), sep = "_"))
            }), recursive = FALSE))
  guided_settings(base, form, path, info, tempfile("results"), answers)
}

test_that("standard files are recognized without a question and keep automatic names", {
  folder <- withr::local_tempdir()
  prepared <- write_grid(file.path(folder, "prepared.nc"), celsius, grid_days, grid_lon, grid_lat)
  info <- detect_input(prepared)
  expect_length(info$questions, 0L)
  expect_identical(info[c("format", "variable", "longitude", "latitude", "time", "units")],
                   list(format = "daily_tmax", variable = "tmax", longitude = "lon", latitude = "lat",
                        time = "time", units = "degC"))
  expect_equal(info$dates, grid_dates)
  expect_identical(info$per_day, 1L)
  expect_equal(info$sample_c, range(celsius[1, , ]))
  expect_identical(plan_for(prepared, info)$config$input[c("variable", "longitude", "latitude", "time", "units")],
                   list(variable = "auto", longitude = "auto", latitude = "auto", time = "auto", units = "auto"))

  # Hourly 2 m temperature as the Climate Data Store writes it.
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- array(295 + seq_len(48 * 3 * 4) / 100, c(48, 3, 4))
  raw <- write_grid(file.path(folder, "hourly.nc"), list(t2m = hourly), hours, grid_lon, rev(grid_lat), units = "K",
                    time_units = "seconds since 1970-01-01", dims = c("longitude", "latitude", "valid_time"))
  info <- detect_input(raw)
  expect_identical(info$format, "era5")
  expect_identical(info$per_day, 24L)
  expect_identical(info$units, "K")
  expect_equal(info$dates, as.Date(c("2000-07-01", "2000-07-02")))
  expect_identical(plan_for(raw, info)$config$input$variable, "auto")
})

test_that("other names are found from CF metadata and written into the configuration", {
  folder <- withr::local_tempdir()
  named <- write_grid(file.path(folder, "tasmax.nc"), list(tasmax = celsius + 273.15, tasmin = celsius + 263.15),
                      grid_days * 86400, grid_lon, grid_lat, units = "K", time_units = "seconds since 2000-01-01",
                      dims = c("x", "y", "t"),
                      attributes = list(tasmax = list(standard_name = "air_temperature", cell_methods = "time: maximum"),
                                        tasmin = list(standard_name = "air_temperature", cell_methods = "time: minimum")))
  info <- detect_input(named)
  expect_length(info$questions, 0L)
  expect_identical(info[c("format", "variable", "longitude", "latitude", "time", "units")],
                   list(format = "daily_tmax", variable = "tasmax", longitude = "x", latitude = "y", time = "t", units = "K"))
  expect_equal(info$sample_c, range(celsius[1, , ]))
  settings <- plan_for(named, info)
  expect_identical(settings$config$input[c("format", "variable", "longitude", "latitude", "time")],
                   list(format = "daily_tmax", variable = "tasmax", longitude = "x", latitude = "y", time = "t"))
  # The saved settings replay through the scripted reader, converting Kelvin once.
  write_guided_config(settings$config, folder)
  replay <- suppressMessages(read_configured_input(read_config(file.path(folder, "config.yaml"))))
  expect_equal(replay$dates, grid_dates)
  expect_equal(replay$tmax, matrix(aperm(celsius, c(1, 3, 2)), nrow = 4), tolerance = 1e-12)
  expect_gt(min(replay$tmax), 0)
  expect_lt(max(replay$tmax), 60)
})

test_that("one focused question is asked when the file leaves a decision open", {
  folder <- withr::local_tempdir()
  silent <- write_grid(file.path(folder, "no-units.nc"), celsius + 273.15, grid_days, grid_lon, grid_lat, units = "")
  info <- detect_input(silent)
  expect_named(info$questions, "units")
  expect_identical(info$questions$units$recommended, "K")
  expect_match(info$questions$units$detail, "does not record the units")
  answered <- detect_input(silent, units = "K")
  expect_length(answered$questions, 0L)
  expect_equal(answered$sample_c, range(celsius[1, , ]))
  expect_identical(plan_for(silent, detect_input(silent, units = "K"), answers = list(units = "K"))$config$input$units, "K")
  expect_identical(detect_input(write_grid(file.path(folder, "no-units-c.nc"), celsius, grid_days, grid_lon, grid_lat,
                                           units = ""))$questions$units$recommended, "degC")

  # Units that contradict the numbers are asked about, not trusted.
  wrong <- write_grid(file.path(folder, "says-kelvin.nc"), celsius, grid_days, grid_lon, grid_lat, units = "K")
  info <- detect_input(wrong)
  expect_named(info$questions, "units")
  expect_identical(info$questions$units$recommended, "degC")
  expect_match(info$questions$units$detail, "look like degrees Celsius")
  expect_equal(detect_input(wrong, units = "degC")$sample_c, range(celsius[1, , ]))

  both <- write_grid(file.path(folder, "both.nc"), list(tmax = celsius, t2m = celsius + 273.15), grid_days,
                     grid_lon, grid_lat, units = c("degC", "K"))
  info <- detect_input(both)
  expect_named(info$questions, "variable")
  expect_setequal(unname(info$questions$variable$choices), c("tmax", "t2m"))
  expect_identical(detect_input(both, variable = "tmax")$format, "daily_tmax")
  expect_identical(detect_input(both, variable = "t2m")$format, "era5")

  several <- write_grid(file.path(folder, "several.nc"), list(skin = celsius + 273.15, dew = celsius + 263.15),
                        grid_days, grid_lon, grid_lat, units = "K",
                        attributes = list(skin = list(long_name = "Skin temperature"),
                                          dew = list(long_name = "Dew point temperature")))
  info <- detect_input(several)
  expect_named(info$questions, "variable")
  expect_named(info$questions$variable$choices, c("skin (Skin temperature)", "dew (Dew point temperature)"))

  unlabelled <- write_grid(file.path(folder, "row-col.nc"), celsius, grid_days, grid_lon, grid_lat,
                           dims = c("col", "row", "time"), dim_units = c("", ""))
  info <- detect_input(unlabelled)
  expect_named(info$questions, "longitude")
  expect_setequal(unname(info$questions$longitude$choices), c("col", "row"))
  answered <- detect_input(unlabelled, longitude = "col")
  expect_identical(answered[c("longitude", "latitude")], list(longitude = "col", latitude = "row"))
  expect_identical(plan_for(unlabelled, answered, answers = list(longitude = "col"))$config$input[c("longitude", "latitude")],
                   list(longitude = "col", latitude = "row"))
})

test_that("files SCORCH cannot analyze are refused with the reason", {
  folder <- withr::local_tempdir()
  expect_error(detect_input(write_grid(file.path(folder, "fahrenheit.nc"), celsius * 1.8 + 32, grid_days, grid_lon,
                                       grid_lat, units = "degF")), "Fahrenheit")
  expect_error(detect_input(write_grid(file.path(folder, "rain.nc"), list(precip = celsius), grid_days, grid_lon,
                                       grid_lat, units = "mm/day")), "does not appear to hold temperature")
  expect_error(detect_input(write_grid(file.path(folder, "projected.nc"), celsius, grid_days, grid_lon * 1e5,
                                       grid_lat * 1e5, dims = c("x", "y", "time"), dim_units = c("m", "m"))),
               "not on a regular longitude-latitude grid")
  expect_error(detect_input(write_grid(file.path(folder, "monthly.nc"), celsius, c(0, 31, 60, 91), grid_lon, grid_lat)),
               "needs daily or hourly temperature")
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- write_grid(file.path(folder, "hourly-tmax.nc"), array(300, c(48, 3, 4)), hours, grid_lon, grid_lat,
                       units = "K", time_units = "seconds since 1970-01-01")
  expect_identical(detect_input(hourly)$format, "era5")
  expect_error(detect_input(hourly, format = "daily_tmax"), "24 values per day")
  expect_error(detect_input(hourly, variable = "t2m"), "no temperature variable called 't2m'")
  expect_error(detect_input(hourly, longitude = "x"), "no dimension called 'x'")

  # A folder or ZIP must hold one dataset.
  mixed <- file.path(folder, "mixed")
  dir.create(mixed)
  write_grid(file.path(mixed, "a.nc"), list(t2m = array(300, c(48, 3, 4))), hours, grid_lon, grid_lat, units = "K",
             time_units = "seconds since 1970-01-01")
  write_grid(file.path(mixed, "b.nc"), list(tp = array(0.001, c(48, 3, 4))), hours, grid_lon, grid_lat, units = "m",
             time_units = "seconds since 1970-01-01")
  expect_error(detect_input(mixed), "b.nc does not hold temperature in the same way as a.nc")
})

test_that("time origins written with single digits are read", {
  decode_time <- scoRch.simulator:::decode_time
  expect_equal(decode_time(c(0, 24), "hours since 1900-01-01 00:00:0.0"), as.Date(c("1900-01-01", "1900-01-02")))
  expect_equal(decode_time(1, "days since 1915-1-1 0:0:0"), as.Date("1915-01-02"))
  expect_error(decode_time(0, "days since 2000-01-01 12:61:00"), "valid time")
})
