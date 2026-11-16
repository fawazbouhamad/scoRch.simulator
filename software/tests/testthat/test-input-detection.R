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
  expect_true(info$identified)
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
  expect_length(info$questions, 0L)
  expect_identical(info$format, "era5")
  expect_identical(info$per_day, 24L)
  expect_identical(info$units, "K")
  expect_true(info$identified)
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
  expect_true(info$identified)
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

  # Up to 70 the numbers can be degrees Celsius, but above 60 they may as well
  # be Fahrenheit: both answers are offered and neither is suggested.
  heated <- function(name, by, units) {
    write_grid(file.path(folder, paste0(name, ".nc")), celsius + by, grid_days, grid_lon, grid_lat, units = units)
  }
  expect_identical(detect_input(heated("no-units-59", 28, ""))$questions$units$recommended, "degC")
  hot <- heated("no-units-66", 35, "")
  question <- detect_input(hot)$questions$units
  expect_true(is.na(question$recommended))
  expect_identical(unname(question$choices), c("K", "degC"))
  expect_equal(detect_input(hot, units = "degC")$sample_c, range(celsius[1, , ]) + 35)
  expect_true(is.na(detect_input(heated("unknown-66", 35, "degrees"))$questions$units$recommended))
  question <- detect_input(heated("kelvin-66", 35, "K"))$questions$units
  expect_true(is.na(question$recommended))
  expect_match(question$detail, "records 'K' for 'tmax', but the numbers cannot be air temperature in Kelvin",
               fixed = TRUE)
  expect_false(grepl("look like", question$detail, fixed = TRUE))

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
  # The chosen variable is read, and still not described as a daily maximum.
  expect_identical(detect_input(several, variable = "skin")[c("variable", "identified")],
                   list(variable = "skin", identified = FALSE))

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

test_that("a lone temperature that the file does not identify is confirmed, not assumed", {
  folder <- withr::local_tempdir()
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- array(295 + seq_len(48 * 3 * 4) / 100, c(48, 3, 4))
  daily <- function(name, attributes = list(), units = "K") {
    write_grid(file.path(folder, paste0(name, "-day.nc")),
               stats::setNames(list(celsius + if (units == "K") 273.15 else 0), name), grid_days, grid_lon, grid_lat,
               units = units, attributes = stats::setNames(list(attributes), name))
  }
  subdaily <- function(name, attributes = list()) {
    write_grid(file.path(folder, paste0(name, "-1hr.nc")), stats::setNames(list(hourly), name), hours, grid_lon,
               rev(grid_lat), units = "K", time_units = "seconds since 1970-01-01",
               dims = c("longitude", "latitude", "valid_time"), attributes = stats::setNames(list(attributes), name))
  }
  cf <- function(long, method) list(standard_name = "air_temperature", long_name = long, cell_methods = method)

  # A minimum, a mean, a dew point or a sea temperature is never taken for the daily maximum.
  asked <- list(
    tasmin = daily("tasmin", cf("Daily Minimum Near-Surface Air Temperature", "area: mean time: minimum")),
    tas = daily("tas", cf("Near-Surface Air Temperature", "area: time: mean")),
    tn = daily("tn", list(long_name = "minimum temperature"), units = "Celsius"),
    tg = daily("tg", list(long_name = "mean temperature"), units = "Celsius"),
    tmin = daily("tmin", units = "degC"),
    d2m = subdaily("d2m", list(long_name = "2 metre dewpoint temperature")),
    sst = subdaily("sst", list(long_name = "Sea surface temperature")),
    mn2t = subdaily("mn2t", list(long_name = "Minimum temperature at 2 metres since previous post-processing")))
  for (name in names(asked)) {
    info <- detect_input(asked[[name]])
    expect_named(info$questions, "variable", info = name)
    question <- info$questions$variable
    expect_identical(question$id, "variable", info = name)
    expect_identical(unname(question$choices), name, info = name)
    expect_true(is.na(question$recommended), info = name)
    expect_match(question$detail, sprintf("holds the variable '%s'", name), fixed = TRUE, info = name)
  }
  question <- detect_input(asked$tasmin)$questions$variable
  expect_identical(question$title, "Is this the temperature to analyze?")
  expect_named(question$choices, "Analyze 'tasmin' as the daily temperature")
  expect_match(question$detail, "long name 'Daily Minimum Near-Surface Air Temperature'", fixed = TRUE)
  expect_match(question$detail, "cell methods 'area: mean time: minimum'", fixed = TRUE)
  expect_match(question$detail, "does not identify it as daily maximum temperature", fixed = TRUE)
  question <- detect_input(asked$d2m)$questions$variable
  expect_named(question$choices, "Analyze the daily maximum of 'd2m'")
  expect_match(question$detail, "with 24 values per day", fixed = TRUE)
  expect_match(question$detail, "does not identify it as 2 m air temperature", fixed = TRUE)

  # What the file identifies is read without a question, as before.
  known <- list(
    tasmax = daily("tasmax", cf("Daily Maximum Near-Surface Air Temperature", "area: mean time: maximum")),
    tmax = daily("tmax", units = "degC"), mx2t = daily("mx2t"), t2m = subdaily("t2m"),
    tas = subdaily("tas", cf("Near-Surface Air Temperature", "area: mean time: point")))
  for (name in names(known)) {
    info <- detect_input(known[[name]])
    expect_identical(length(info$questions), 0L, info = name)
    expect_identical(info$variable, name, info = name)
    expect_true(info$identified, info = name)
  }

  # Air temperature on one pressure level is not the 2 m temperature; the
  # 2 m height that some files carry as a dimension is.
  axes <- list(longitude = list(units = "degrees_east", values = grid_lon),
               latitude = list(units = "degrees_north", values = grid_lat),
               pressure_level = list(units = "hPa", values = 850), height = list(units = "m", values = 2),
               valid_time = list(units = "seconds since 1970-01-01", values = hours))
  on_level <- function(name, level) {
    dims <- c("longitude", "latitude", level, "valid_time")
    write_layout(file.path(folder, paste0(name, "-level.nc")), axes[dims], stats::setNames(list(list(
      dims = dims, units = "K", values = array(aperm(hourly, c(3, 2, 1)), c(4, 3, 1, 48)),
      attributes = list(standard_name = "air_temperature"))), name))
  }
  expect_named(detect_input(on_level("t", "pressure_level"))$questions, "variable")
  expect_true(detect_input(on_level("tas", "height"))$identified)

  # The answer names the variable, as a configuration would. It is then read
  # without another question, and still not described as a daily maximum.
  answered <- detect_input(asked$tasmin, variable = "tasmin")
  expect_length(answered$questions, 0L)
  expect_identical(answered[c("format", "variable", "identified")],
                   list(format = "daily_tmax", variable = "tasmin", identified = FALSE))
  settings <- plan_for(asked$tasmin, answered, answers = list(variable = "tasmin"))
  expect_identical(settings$config$input$variable, "tasmin")
  write_guided_config(settings$config, folder)
  replay <- suppressMessages(read_configured_input(read_config(file.path(folder, "config.yaml"))))
  expect_equal(replay$tmax, matrix(aperm(celsius, c(1, 3, 2)), nrow = 4), tolerance = 1e-12)
  expect_identical(detect_input(asked$d2m, variable = "d2m")[c("format", "variable", "identified")],
                   list(format = "era5", variable = "d2m", identified = FALSE))
})

test_that("sub-daily air temperature counts as 2 m temperature only when the file says so", {
  folder <- withr::local_tempdir()
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- array(295 + seq_len(48 * 3 * 4) / 100, c(48, 3, 4))
  subdaily <- function(file, name, long = NULL, standard = "air_temperature") {
    attributes <- c(list(standard_name = standard), if (!is.null(long)) list(long_name = long))
    write_grid(file.path(folder, paste0(file, ".nc")), stats::setNames(list(hourly), name), hours, grid_lon,
               rev(grid_lat), units = "K", time_units = "seconds since 1970-01-01",
               dims = c("longitude", "latitude", "valid_time"), attributes = stats::setNames(list(attributes), name))
  }

  # CF calls the air at any height air_temperature. A level that is not a
  # dimension leaves no other trace, so that name alone does not say 2 m.
  upper <- list(subdaily("t", "t", "Temperature"), subdaily("ta", "ta", "Air Temperature"), subdaily("ta850", "ta850"),
                subdaily("sigma", "air", "4xDaily Air temperature at sigma level 995"),
                subdaily("two-mb", "temp", "Temperature at 2 mb"),
                # Two metres of water or of ground are not the air at 2 m.
                subdaily("below", "T", "Temperature 2 m below surface"),
                subdaily("water", "T", "Water temperature 2 m"))
  for (file in upper) {
    question <- detect_input(file)$questions$variable
    expect_identical(question$title, "Is this the temperature to analyze?", info = basename(file))
    expect_match(question$detail, "does not identify it as 2 m air temperature", fixed = TRUE, info = basename(file))
  }
  expect_identical(detect_input(upper[[1]], variable = "t")[c("format", "variable", "identified")],
                   list(format = "era5", variable = "t", identified = FALSE))

  # Names and descriptions of the 2 m temperature are read without a question.
  near <- list(subdaily("t2m", "t2m"), subdaily("2m_temperature", "2m_temperature"), subdaily("capital", "T2M"),
               subdaily("tas", "tas", "Near-Surface Air Temperature"), subdaily("mx2t", "mx2t"),
               subdaily("at-2m", "air", "air temperature at 2 m"),
               subdaily("near-surface", "Tair", "Near-Surface Air Temperature"),
               subdaily("above-ground", "Tair", "2-m above ground temperature"),
               subdaily("in-brackets", "air_temperature_2m", "Air temperature (2m)"),
               subdaily("square-brackets", "TMP", "Temperature [2 m]"))
  for (file in near) {
    info <- detect_input(file)
    expect_identical(length(info$questions), 0L, info = basename(file))
    expect_true(info$identified, info = basename(file))
  }
})

test_that("numbers that cannot be air temperature stop the selection", {
  folder <- withr::local_tempdir()
  stored <- function(name, values, ...) {
    write_grid(file.path(folder, paste0(name, ".nc")), values, grid_days, grid_lon, grid_lat, ...)
  }
  fahrenheit <- celsius * 1.8 + 32

  # A missing-value code that the file does not declare is a number like any
  # other. The message prints the range (20.25 to 31.25 without the code).
  expect_error(detect_input(stored("fill-1e20", replace(celsius, 5, 1e20))), "ranges from 20[.][23] to 1e[+]20 ")
  expect_error(detect_input(stored("fill-9999", replace(celsius, 5, 9999))), "to 9999 .*does not declare as missing")
  expect_error(detect_input(stored("fill-minus", replace(celsius, 5, -9999))), "ranges from -9999 to 31[.][23] ")
  expect_error(detect_input(stored("zero-kelvin", replace(celsius + 273.15, 5, 0), units = "K")),
               "ranges from 0 to 304[.]4 K .*cannot be air temperature")

  # Other units under a Celsius or Kelvin label, and Fahrenheit however it is spelled.
  expect_error(detect_input(stored("fahrenheit-as-celsius", fahrenheit)), "cannot be air temperature.*Fahrenheit")
  expect_error(detect_input(stored("tenths", celsius * 10, units = "0.1 degC")), "not whole degrees")
  expect_error(detect_input(stored("times-ten", celsius * 10, units = "degC*10")), "not whole degrees")
  # A number is a factor where one stands: leading the units, or after *, x
  # or /, whether or not the rest spells a unit that the readers know.
  factors <- c("10 K", "1e-1 degC", "K/10", "degrees Celsius x 10", "degC x 10", "degC \u00d7 10", "tenths of degrees C")
  for (i in seq_along(factors)) {
    expect_error(detect_input(stored(paste0("factor-", i), celsius * 10, units = factors[i])),
                 "not whole degrees.*choose the temperature units under Advanced settings", info = factors[i])
  }
  # Any other number is none, and the units are read as they were before.
  harmless <- c("degrees Celsius at 2 m", "Celsius (2m)", "degree_Celsius (ITS-90)", "1 degC")
  for (i in seq_along(harmless)) {
    expect_identical(detect_input(stored(paste0("harmless-", i), celsius, units = harmless[i]))$units, "degC",
                     info = harmless[i])
  }
  expect_identical(detect_input(stored("harmless-kelvin", celsius + 273.15, units = "Kelvin (2 m)"))$units, "K")
  # The way out that the refusal names, for numbers that are whole degrees after all.
  expect_identical(detect_input(stored("whole-after-all", celsius, units = "degC*10"), units = "degC")$units, "degC")
  spellings <- c("\u00b0F", "Farenheit", "deg. F", "degrees F.", "degree_F")
  for (i in seq_along(spellings)) {
    expect_error(detect_input(stored(paste0("spelling-", i), fahrenheit, units = spellings[i])), "Fahrenheit",
                 info = spellings[i])
  }

  # Without units the question is asked only when one of its answers could be
  # right, and that answer is the one suggested.
  expect_error(detect_input(stored("no-units-fahrenheit", fahrenheit, units = "")),
               "records no units.*cannot be air temperature")
  expect_error(detect_input(stored("unknown-units", fahrenheit, units = "imperial")), "records the units as 'imperial'")
  plain <- stored("no-units-celsius", celsius, units = "")
  expect_identical(detect_input(plain)$questions$units$recommended, "degC")
  expect_error(detect_input(plain, units = "K"), "ranges from 20[.][23] to 31[.][23] K [(]-252[.]9 to -241[.]9 ")
  gap <- celsius + 273.15
  gap[1, , ] <- NA
  expect_true(is.na(detect_input(stored("first-step-missing", gap, units = ""))$questions$units$recommended))
  expect_true(anyNA(detect_input(stored("first-step-missing-kelvin", gap, units = "K"))$sample_c))

  # Real extremes pass: an Antarctic winter and a desert afternoon.
  expect_equal(detect_input(stored("cold", celsius - 110))$sample_c, range(celsius[1, , ]) - 110)
  expect_equal(detect_input(stored("hot", celsius + 303.15, units = "K"))$sample_c, range(celsius[1, , ]) + 30)
})

test_that("attributes written in Latin-1 are read, and units in such text are confirmed", {
  folder <- withr::local_tempdir()
  latin1 <- function(...) rawToChar(as.raw(c(...)))
  stored <- function(name, values, ...) {
    write_grid(file.path(folder, paste0(name, ".nc")), values, grid_days, grid_lon, grid_lat,
               attributes = list(tmax = list(...)))
  }
  # An accented long name does not stop the reading.
  accented <- stored("accented", celsius, long_name = latin1(0x54, 0x65, 0x6d, 0x70, 0xe9, 0x72, 0x61, 0x74, 0x75, 0x72, 0x65))
  expect_identical(detect_input(accented)[c("variable", "units")], list(variable = "tmax", units = "degC"))

  # A degree sign in Latin-1 is legible here but not to the readers, so the
  # unit is asked and the answer travels with the configuration.
  degree <- stored("degree", celsius, units = latin1(0xb0, 0x43))
  question <- detect_input(degree)$questions$units
  expect_identical(question$recommended, "degC")
  expect_match(question$detail, "text encoding")
  answered <- detect_input(degree, units = "degC")
  expect_identical(answered$units, "degC")
  write_guided_config(plan_for(degree, answered, answers = list(units = "degC"))$config, folder)
  replay <- suppressMessages(read_configured_input(read_config(file.path(folder, "config.yaml"))))
  expect_equal(replay$dates, grid_dates)
  expect_error(detect_input(stored("fahrenheit", celsius * 1.8 + 32, units = latin1(0xb0, 0x46))), "Fahrenheit")

  # The units of a coordinate are read the same way. The readers do not look
  # at them, so nothing is asked.
  compass <- write_grid(file.path(folder, "compass.nc"), celsius, grid_days, grid_lon, grid_lat,
                        attributes = list(lon = list(units = latin1(0xb0, 0x45)),
                                          lat = list(units = latin1(0xb0, 0x4e))))
  info <- detect_input(compass)
  expect_length(info$questions, 0L)
  expect_identical(info[c("longitude", "latitude", "units")], list(longitude = "lon", latitude = "lat", units = "degC"))
  expect_equal(suppressMessages(read_tmax(compass))$dates, grid_dates)
})

test_that("selections that the readers could not read stop before the run", {
  folder <- withr::local_tempdir()
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- array(295 + seq_len(48 * 3 * 4) / 100, c(48, 3, 4))
  era5 <- function(path, name = "t2m", day = 0, lon = grid_lon, dims = c("longitude", "latitude", "valid_time")) {
    write_grid(path, stats::setNames(list(hourly), name), hours + day * 86400, lon, grid_lat, units = "K",
               time_units = "seconds since 1970-01-01", dims = dims)
  }

  # Prepared daily Tmax is read with exactly three dimensions.
  axes <- list(lon = list(units = "degrees_east", values = grid_lon), lat = list(units = "degrees_north", values = grid_lat),
               height = list(units = "m", values = 2), expver = list(values = c(1, 5)),
               time = list(units = "days since 2000-01-01", values = grid_days))
  tall <- write_layout(file.path(folder, "height.nc"), axes[c("lon", "lat", "height", "time")], list(tasmax = list(
    dims = c("lon", "lat", "height", "time"), units = "degC", values = array(aperm(celsius, c(3, 2, 1)), c(4, 3, 1, 4)),
    attributes = list(cell_methods = "time: maximum"))))
  expect_error(detect_input(tall), "has 4 dimensions (lon, lat, height, time)", fixed = TRUE)
  expect_error(detect_input(tall), "exactly three: time, latitude and longitude.*extra dimension 'height'")
  expect_identical(detect_input(tall, format = "era5")$format, "era5")
  versions <- write_layout(file.path(folder, "expver.nc"), axes[c("lon", "lat", "expver", "time")], list(mx2t = list(
    dims = c("lon", "lat", "expver", "time"), units = "K", values = array(300, c(4, 3, 2, 4)))))
  expect_error(detect_input(versions), "extra dimension 'expver'.*data format to ERA5 temperature")

  # Both readers need constant spacing. Longitudes are judged in the order the
  # ERA5 reader gives them, so a region across 180 degrees stays regular.
  gaussian <- write_grid(file.path(folder, "gaussian.nc"), celsius, grid_days, grid_lon, c(44.7611, 46.6658, 48.57))
  expect_error(detect_input(gaussian), "not on a regular longitude-latitude grid.*latitude values are not evenly spaced")
  expect_error(detect_input(era5(file.path(folder, "uneven.nc"), lon = c(10, 11, 12, 14))),
               "longitude values are not evenly spaced")
  dateline <- era5(file.path(folder, "dateline.nc"), lon = c(179, 180, 181, 182))
  expect_length(detect_input(dateline)$questions, 0L)
  expect_s3_class(suppressMessages(read_era5(dateline, months = 7, cell_size = NULL)), "scorch_tmax")

  # One selection is read with one set of names. Names that differ are fine
  # only when `auto` finds each of them by itself.
  found <- file.path(folder, "found-by-auto")
  dir.create(found)
  era5(file.path(found, "a.nc"))
  era5(file.path(found, "b.nc"), day = 2, dims = c("lon", "lat", "time"))
  info <- detect_input(found)
  expect_identical(info$files, 2L)
  expect_setequal(info$longitude, c("longitude", "lon"))
  expect_identical(plan_for(found, info)$config$input[c("variable", "longitude", "latitude", "time")],
                   list(variable = "auto", longitude = "auto", latitude = "auto", time = "auto"))
  both <- read_config(list(input = list(file = found, format = "era5"), period = list(warm_season_months = 7)))
  expect_length(suppressMessages(read_configured_input(both))$dates, 4L)

  variables <- file.path(folder, "two-variables")
  dir.create(variables)
  era5(file.path(variables, "a.nc"))
  era5(file.path(variables, "b.nc"), name = "mx2t", day = 2)
  expect_error(detect_input(variables), "a.nc and b.nc name the temperature variable differently ('t2m' and 'mx2t')",
               fixed = TRUE)
  expect_error(detect_input(variables, variable = "t2m"), "b.nc has no temperature variable called 't2m'")

  coordinates <- file.path(folder, "two-coordinates")
  dir.create(coordinates)
  era5(file.path(coordinates, "a.nc"), dims = c("lon", "lat", "time"))
  era5(file.path(coordinates, "b.nc"), day = 2, dims = c("x", "y", "time"))
  expect_error(detect_input(coordinates), "a.nc and b.nc name the longitude dimension differently ('lon' and 'x')",
               fixed = TRUE)
})

test_that("unevenly spaced coordinates are refused with the remedy for their cause", {
  folder <- withr::local_tempdir()
  refusal <- function(file) tryCatch(detect_input(file), error = conditionMessage)
  daily <- function(name, lon, lat = grid_lat, ...) {
    write_grid(file.path(folder, name), celsius, grid_days, lon, lat, ...)
  }
  hours <- (as.numeric(as.Date("2000-07-01")) * 24 + 0:47) * 3600
  hourly <- function(name, lon, ...) {
    write_grid(file.path(folder, name), list(t2m = array(295 + seq_len(48 * 3 * 4) / 100, c(48, 3, 4))), hours, lon,
               grid_lat, units = "K", time_units = "seconds since 1970-01-01",
               dims = c("longitude", "latitude", "valid_time"), ...)
  }
  kept <- function(file, name) {
    nc <- ncdf4::nc_open(file)
    on.exit(ncdf4::nc_close(nc))
    as.numeric(nc$dim[[name]]$vals)
  }

  # A regular 0.1 degree grid at 130 E loses its even spacing in single
  # precision, for the readers too. Rounded and saved in double precision, as
  # the refusal says, the coordinates that the file holds are read.
  advice <- "regular only to single precision.*longitude values are not evenly spaced.*in double precision, rounded"
  lon <- 130 + 0:3 / 10
  lat <- -30 + 0:2 / 10
  single <- daily("single.nc", lon, lat, coordinates = "float")
  expect_match(refusal(single), advice)
  expect_error(read_tmax(single), "longitude values are not evenly spaced")
  expect_gt(max(abs(kept(single, "lon") - lon)), 1e-6)
  double <- daily("double.nc", round(kept(single, "lon"), 1), round(kept(single, "lat"), 1))
  info <- detect_input(double)
  expect_length(info$questions, 0L)
  expect_equal(as.numeric(info$lon), lon, tolerance = 1e-12)
  expect_equal(suppressMessages(read_tmax(double))$grid$lon_step, 0.1, tolerance = 1e-9)
  # The rounding grows with the longitude, not with the step: at 300 E the
  # steps of a 0.01 degree grid differ by three thousandths of a step.
  expect_match(refusal(daily("single-fine.nc", 300 + 0:3 / 100, lat, coordinates = "float")), advice)
  # The reader of hourly data counts 355 E as 5 W, with the rounding of 355.
  single <- hourly("era5-single.nc", 355 + 0:3 / 10, coordinates = "float")
  expect_match(refusal(single), advice)
  double <- hourly("era5-double.nc", round(kept(single, "longitude"), 1))
  expect_length(detect_input(double)$questions, 0L)
  expect_s3_class(suppressMessages(read_era5(double, months = 7, cell_size = NULL)), "scorch_tmax")

  # A region across 180 degrees with longitudes from -180 to 180 is regular
  # once they are stored as 0 to 360, for prepared and for hourly data.
  across <- c(178.5, 179.5, -179.5, -178.5)
  advice <- "across 180 degrees longitude.*store them as 0 to 360 in ascending order [(]178[.]5 to 181[.]5[)]"
  expect_match(refusal(daily("across.nc", across)), advice)
  expect_error(read_tmax(daily("across.nc", across)), "longitude values are not evenly spaced")
  expect_match(refusal(daily("across-sorted.nc", sort(across))), advice)
  counted <- daily("across-360.nc", across %% 360)
  expect_length(detect_input(counted)$questions, 0L)
  expect_equal(suppressMessages(read_tmax(counted))$grid$lon, c(178.5, 179.5, 180.5, 181.5))
  expect_match(refusal(hourly("era5-across.nc", across)), advice)
  counted <- hourly("era5-across-360.nc", across %% 360)
  expect_length(detect_input(counted)$questions, 0L)
  expect_s3_class(suppressMessages(read_era5(counted, months = 7, cell_size = NULL)), "scorch_tmax")

  # A Gaussian grid is uneven however its coordinates are stored, and a
  # stretched axis is no region across 180 degrees: these are to be regridded.
  for (precision in c("double", "float")) {
    gaussian <- write_grid(file.path(folder, paste0("gaussian-", precision, ".nc")), celsius, grid_days, grid_lon,
                           c(44.7611, 46.6658, 48.57), coordinates = precision)
    expect_match(refusal(gaussian), "not on a regular longitude-latitude grid.*regrid a Gaussian", info = precision)
    expect_false(grepl("single precision|180 degrees", refusal(gaussian)), info = precision)
  }
  stretched <- write_grid(file.path(folder, "stretched.nc"), celsius, grid_days, c(-1, 0, 1, 3), grid_lat)
  expect_match(refusal(stretched),
               "longitude values are not evenly spaced.*regrid a Gaussian or otherwise uneven grid first")
})

test_that("coordinates that are not longitudes and latitudes in degrees are not taken for them", {
  folder <- withr::local_tempdir()
  no_grid <- "no temperature variable on a time, latitude and longitude grid"
  days <- list(units = "days since 2000-01-01", values = grid_days)
  field <- aperm(celsius, c(3, 2, 1))

  # The dimension left beside a labelled coordinate must itself pass for the
  # other one: a pressure level or a numbered level is not a longitude.
  levels <- write_grid(file.path(folder, "levels.nc"), list(ta = celsius + 273.15), grid_days, c(1000, 850, 700, 500) * 100,
                       grid_lat, units = "K", dims = c("plev", "lat", "time"), dim_units = c("Pa", "degrees_north"))
  expect_error(detect_input(levels), no_grid)
  numbered <- write_grid(file.path(folder, "numbered.nc"), list(ta = celsius + 273.15), grid_days, 1:4, grid_lat,
                         units = "K", dims = c("level", "lat", "time"), dim_units = c("", "degrees_north"),
                         attributes = list(level = list(axis = "Z", positive = "down")))
  expect_error(detect_input(numbered), no_grid)
  beyond <- write_grid(file.path(folder, "beyond.nc"), celsius, grid_days, grid_lon, c(100.5, 101.5, 102.5),
                       dim_units = c("", ""))
  expect_error(detect_input(beyond), no_grid)
  # Units that are not degrees say so whatever the values are: pressures of
  # 50 to 200 hPa, levels or potential temperatures would pass for longitudes.
  beside <- function(name, unit) {
    write_grid(file.path(folder, paste0(name, ".nc")), list(ta = celsius + 273.15), grid_days, c(50, 100, 150, 200),
               grid_lat, units = "K", dims = c("level", "lat", "time"), dim_units = c(unit, "degrees_north"))
  }
  others <- c("hPa", "millibar", "Pa", "K", "degK", "level", "1", "sigma", "km")
  for (i in seq_along(others)) {
    expect_error(detect_input(beside(paste0("beside-", i), others[i])), no_grid, info = others[i])
  }
  # Degrees, a compass direction or no units leave the values to decide.
  angles <- c("", "degrees", "deg", "E", "east", "\u00b0E")
  for (i in seq_along(angles)) {
    expect_identical(detect_input(beside(paste0("angle-", i), angles[i]), variable = "ta")$longitude, "level",
                     info = angles[i])
  }
  # A dimension that is called longitude or latitude is one whatever text
  # stands for its units, unless that text says a length or radians.
  called <- function(name, unit) {
    write_grid(file.path(folder, paste0(name, ".nc")), celsius, grid_days, grid_lon, grid_lat, dim_units = c(unit, unit))
  }
  loose <- c("none", "unitless", "1", "dd", "Grad")
  for (i in seq_along(loose)) {
    expect_identical(detect_input(called(paste0("called-", i), loose[i]))[c("longitude", "latitude")],
                     list(longitude = "lon", latitude = "lat"), info = loose[i])
  }
  for (unit in c("radians", "m", "km", "kilometers", "metres_east")) {
    expect_error(detect_input(called(paste0("called-", unit), unit)), "projected or rotated", info = unit)
  }

  # A rotated pole and a curvilinear grid say so in the variable, not in the dimensions.
  rotated <- write_layout(file.path(folder, "rotated.nc"),
    list(rlon = list(units = "degrees", values = c(-4, -3, -2, -1), attributes = list(axis = "X")),
         rlat = list(units = "degrees", values = c(-2, -1, 0), attributes = list(axis = "Y")), time = days),
    list(tasmax = list(dims = c("rlon", "rlat", "time"), units = "degC", values = field,
                       attributes = list(grid_mapping = "rotated_pole", coordinates = "lat lon")),
         lat = list(dims = c("rlon", "rlat"), units = "degrees_north", values = matrix(45, 4, 3)),
         lon = list(dims = c("rlon", "rlat"), units = "degrees_east", values = matrix(5, 4, 3)),
         rotated_pole = list(attributes = list(grid_mapping_name = "rotated_latitude_longitude"))))
  expect_error(detect_input(rotated), "projected or rotated")
  curved <- write_layout(file.path(folder, "curved.nc"),
    list(i = list(values = 0:3), j = list(values = 0:2), time = days),
    list(tasmax = list(dims = c("i", "j", "time"), units = "degC", values = field,
                       attributes = list(coordinates = "latitude longitude")),
         latitude = list(dims = c("i", "j"), units = "degrees_north", values = matrix(45, 4, 3)),
         longitude = list(dims = c("i", "j"), units = "degrees_east", values = matrix(5, 4, 3))))
  expect_error(detect_input(curved), "projected or rotated")

  # Plain grids are read as before: one that names a latitude-longitude
  # mapping, and one completed from the coordinate that is labelled.
  mapped <- write_layout(file.path(folder, "mapped.nc"),
    list(lon = list(values = grid_lon), lat = list(values = grid_lat), time = days),
    list(tmax = list(dims = c("lon", "lat", "time"), units = "degC", values = field,
                     attributes = list(grid_mapping = "crs")),
         crs = list(attributes = list(grid_mapping_name = "latitude_longitude"))))
  expect_identical(detect_input(mapped)[c("longitude", "latitude")], list(longitude = "lon", latitude = "lat"))
  completed <- write_grid(file.path(folder, "completed.nc"), celsius, grid_days, grid_lon, grid_lat,
                          dims = c("lon", "nlat", "time"), dim_units = c("degrees_east", ""))
  expect_identical(detect_input(completed)[c("longitude", "latitude")], list(longitude = "lon", latitude = "nlat"))
})

test_that("two unlabelled dimensions are told apart where only one can be the latitude", {
  folder <- withr::local_tempdir()
  unlabelled <- function(name, first, second) {
    write_grid(file.path(folder, paste0(name, ".nc")), celsius, grid_days, first, second,
               dims = c("col", "row", "time"), dim_units = c("", ""))
  }
  east <- c(100.5, 101.5, 102.5, 103.5)

  # A latitude stays within 90 degrees, so the other dimension is the
  # longitude. Nothing is asked, and the names found replay through the reader.
  wide <- unlabelled("col-is-longitude", east, grid_lat)
  info <- detect_input(wide)
  expect_length(info$questions, 0L)
  expect_identical(info[c("longitude", "latitude")], list(longitude = "col", latitude = "row"))
  settings <- plan_for(wide, info)
  expect_identical(settings$config$input[c("longitude", "latitude")], list(longitude = "col", latitude = "row"))
  write_guided_config(settings$config, folder)
  replay <- suppressMessages(read_configured_input(read_config(file.path(folder, "config.yaml"))))
  expect_equal(replay$tmax, matrix(aperm(celsius, c(1, 3, 2)), nrow = 4), tolerance = 1e-12)
  expect_identical(detect_input(unlabelled("row-is-longitude", grid_lon, east[1:3]))[c("longitude", "latitude")],
                   list(longitude = "row", latitude = "col"))

  # Where both stay within 90 the file cannot tell and the question is asked;
  # where neither does, no answer could be right.
  expect_named(detect_input(unlabelled("either", grid_lon, grid_lat))$questions, "longitude")
  expect_error(detect_input(unlabelled("neither", east, c(95, 96, 97))),
               "uses the dimensions col and row, and both have values beyond 90, so that neither can be the latitude")

  # Axes that only count their cells, or a "longitude" wider than 360 degrees,
  # are not degrees for certain: the question is asked, and it offers the one
  # dimension that can be the longitude.
  long <- array(20 + seq_len(4 * 3 * 100) / 100, c(4, 3, 100))
  counted <- write_grid(file.path(folder, "counted.nc"), long, grid_days, 1:100, 1:3,
                        dims = c("col", "row", "time"), dim_units = c("", ""))
  asked <- detect_input(counted)$questions$longitude
  expect_identical(asked$title, "Which dimension is the longitude?")
  expect_identical(unname(asked$choices), "col")
  expect_identical(detect_input(counted, longitude = "col")[c("longitude", "latitude")],
                   list(longitude = "col", latitude = "row"))
  expect_identical(unname(detect_input(unlabelled("too-wide", c(-300, -100, 100, 300), grid_lat))$questions$longitude$choices),
                   "col")
})

test_that("a name identifies nothing that the file calls otherwise, or does not call temperature", {
  folder <- withr::local_tempdir()
  stored <- function(name, values, ...) {
    write_grid(file.path(folder, paste0(name, ".nc")), values, grid_days, grid_lon, grid_lat,
               attributes = list(tmax = list(...)))
  }

  # tmax is the name that the scripted workflow reads, but not what the file
  # calls a minimum, an anomaly, or the temperature of the sea or the dew point.
  objected <- list(stored("minimum", celsius, cell_methods = "time: minimum"),
                   stored("anomaly", celsius - 25, long_name = "Daily maximum temperature anomaly"),
                   stored("sea", celsius, standard_name = "sea_surface_temperature"),
                   stored("dew", celsius, long_name = "Daily maximum dew point temperature"))
  for (file in objected) {
    question <- detect_input(file)$questions$variable
    expect_identical(question$title, "Is this the temperature to analyze?", info = basename(file))
    expect_identical(unname(question$choices), "tmax", info = basename(file))
    expect_false(detect_input(file, variable = "tmax")$identified, info = basename(file))
  }

  # A variable that nothing calls temperature is confirmed before its units
  # are asked, even when its name says maximum.
  gusts <- write_grid(file.path(folder, "wsmax.nc"), list(wsmax = celsius), grid_days, grid_lon, grid_lat, units = "")
  questions <- detect_input(gusts)$questions
  expect_named(questions, "variable")
  expect_identical(unname(questions$variable$choices), "wsmax")
  expect_named(detect_input(gusts, variable = "wsmax")$questions, "units")
  expect_identical(detect_input(gusts, variable = "wsmax", units = "degC")[c("variable", "identified")],
                   list(variable = "wsmax", identified = FALSE))

  # The bundled example and the files that write_tmax() writes say what they
  # hold, and are read without a question.
  example <- detect_input(system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator"))
  expect_length(example$questions, 0L)
  expect_identical(example[c("format", "variable", "identified")],
                   list(format = "daily_tmax", variable = "tmax", identified = TRUE))
  prepared <- suppressMessages(read_tmax(stored("prepared", celsius)))
  for (precision in c("int16", "float32", "float64")) {
    written <- detect_input(write_tmax(prepared, file.path(folder, paste0("written-", precision, ".nc")),
                                       precision = precision))
    expect_identical(length(written$questions), 0L, info = precision)
    expect_identical(written[c("format", "variable", "units", "identified")],
                     list(format = "daily_tmax", variable = "tmax", units = "degC", identified = TRUE),
                     info = precision)
  }
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

test_that("a time origin before 1582-10-15 is refused in the standard calendar, later origins decode", {
  decode_time <- scoRch.simulator:::decode_time
  # CF's standard calendar is Julian before that date and R's is not: these
  # 17067072 hours end on 1948-01-01, where R's arithmetic gives two days later.
  expect_error(decode_time(17067072, "hours since 1-1-1 00:00:0.0"), "1582")
  expect_error(decode_time(711128, "days since 0001-01-01 00:00:00", "gregorian"), "Re-encode the time axis")
  expect_error(decode_time(0, "days since 1582-10-14"), "1582")
  expect_equal(decode_time(17067072, "hours since 1-1-1 00:00:0.0", "proleptic_gregorian"), as.Date("1948-01-03"))
  expect_equal(decode_time(0, "days since 1582-10-15"), as.Date("1582-10-15"))
  expect_equal(decode_time(1297320, "hours since 1800-01-01 00:00:0.0"), as.Date("1948-01-01"))
  expect_equal(decode_time(c(0, 17531), "days since 1900-01-01"), as.Date(c("1900-01-01", "1948-01-01")))
  expect_equal(decode_time(c(0, 24), "hours since 1900-01-01 00:00:0.0", "gregorian"),
               as.Date(c("1900-01-01", "1900-01-02")))
})
