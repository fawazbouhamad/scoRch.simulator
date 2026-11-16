# Coordinates stored in single precision (32-bit floating point) are rounded
# by up to 2^-24 of their size. On a fine grid far from zero that is more than
# the readers allow between two steps (0.1 degrees beyond 128, 0.05 degrees
# beyond 64), so such a grid is read as the even grid between its first and
# last coordinate. Whatever the readers accepted before stays as it was.

# The numbers that a single-precision file holds for these values.
single <- function(values) {
  readBin(writeBin(as.numeric(values), raw(), size = 4), "numeric", size = 4, n = length(values))
}

# Hourly 2-m temperature (K) of two July days on a grid, written as a raw file
# and, reduced to its daily maxima in degrees Celsius, as a prepared file.
# `coordinates` is "float" or "double"; the temperatures are the same.
grid_files <- function(folder, name, lon, lat, coordinates, seed = 1) {
  set.seed(seed)
  hourly <- array(290 + 15 * runif(48 * length(lat) * length(lon)), c(48, length(lat), length(lon)))
  daily <- array(NA_real_, c(2, dim(hourly)[2:3]))
  for (day in 1:2) daily[day, , ] <- apply(hourly[(day - 1) * 24 + 1:24, , , drop = FALSE], c(2, 3), max)
  list(hourly = write_grid(file.path(folder, paste0(name, "-hourly-", coordinates, ".nc")), list(t2m = hourly), 0:47,
                           lon, lat, units = "K", time_units = "hours since 2000-07-01",
                           dims = c("longitude", "latitude", "valid_time"), coordinates = coordinates),
       daily = write_grid(file.path(folder, paste0(name, "-daily-", coordinates, ".nc")), daily - 273.15, 0:1,
                          lon, lat, time_units = "days since 2000-07-01", coordinates = coordinates))
}

# What two readings must share to be the same data on the same cells.
parts <- c("dates", "cells", "tmax", "grid")

# Latitudes (degrees, north to south) of a Gaussian grid with `n` latitudes:
# the roots of the Legendre polynomial of degree n, by Newton's method.
gaussian_latitudes <- function(n) {
  x <- cos(pi * (seq_len(n) - 0.25) / (n + 0.5))
  for (iteration in 1:20) {
    before <- 1
    value <- x
    for (k in 2:n) {
      following <- ((2 * k - 1) * x * value - (k - 1) * before) / k
      before <- value
      value <- following
    }
    x <- x - value * (1 - x^2) / (n * (before - x * value))
  }
  asin(x) * 180 / pi
}

test_that("coordinates are made even only where single precision explains their spacing", {
  # Accepted as they are: nothing changes.
  exact <- seq(100, 103, by = 0.25)
  expect_identical(even_coordinates(exact), list(values = exact, slack = 0))
  near_100 <- single(seq(99.9, 103.9, by = 0.1))             # one step in 13,000 here: accepted before
  expect_false(identical(near_100, seq(99.9, 103.9, by = 0.1)))
  expect_identical(even_coordinates(near_100), list(values = near_100, slack = 0))
  expect_identical(even_coordinates(c(250.1, 250.2)), list(values = c(250.1, 250.2), slack = 0))
  expect_identical(even_coordinates(1:4), list(values = c(1, 2, 3, 4), slack = 0))

  # Evenly spaced only to the rounding: the even grid between the first and the last value.
  true <- 249.95 + 0:39 / 10
  stored <- single(true)
  expect_error(grid_step(stored, "longitude"), "not evenly spaced")
  even <- even_coordinates(stored)
  expect_identical(even$values, seq(stored[1], stored[40], length.out = 40))
  expect_identical(even$slack, 2^-22 * max(abs(stored)))
  expect_equal(grid_step(even$values, "longitude"), 0.1, tolerance = 1e-5)
  expect_lte(max(abs(even$values - true)), 2^-24 * max(abs(true)))
  expect_lt(max(abs(even$values - true)), even$slack)
  expect_identical(even_coordinates(even$values), list(values = even$values, slack = 0))
  descending <- even_coordinates(rev(stored))
  expect_identical(descending$values, seq(stored[40], stored[1], length.out = 40))

  # Really uneven, repeated, missing or infinite: unchanged, for grid_step() to refuse as before.
  longer <- cumsum(c(250, rep(0.1, 5), 0.101, rep(0.1, 5)))   # one step 1 percent longer
  repeated <- c(250, 250.1, 250.1, 250.2)
  gaussian <- gaussian_latitudes(96)                           # its steps grow from 1.850 to 1.865 degrees
  expect_equal(gaussian[1:3], c(88.572169, 86.722531, 84.861970), tolerance = 1e-7)
  for (values in list(longer, single(longer), repeated, single(repeated), gaussian, single(gaussian),
                      c(250, NA, 250.2), c(250, 250.1, Inf))) {
    expect_identical(even_coordinates(values), list(values = values, slack = 0))
    expect_error(grid_step(values, "longitude"))
  }
})

test_that("raw input keeps the coordinates that were read before, and only those", {
  none <- c(longitude = 0, latitude = 0)
  lon <- seq(10, 12, by = 0.25)
  # grid_step() measures from the first step, and raw input is sorted before it
  # is measured. Latitudes of 0.05 degrees stored north to south across 64 N
  # are uneven in the stored order and even enough in the sorted one: such a
  # file was read before on its own grid, so its coordinates stay as stored.
  north <- single(seq(66, 62, by = -0.05))
  expect_error(grid_step(north, "latitude"), "not evenly spaced")
  expect_silent(grid_step(sort(north), "latitude"))
  expect_gt(even_coordinates(north)$slack, 0)
  expect_identical(raw_coordinates(lon, north),
                   list(lon = lon, lat = sort(north), lon_order = 1:9, lat_order = 81:1, slack = none))
  # Averaged to cells it was refused, because the first step (0.0499992) does
  # not divide one degree: there the latitudes are made even.
  expect_error(cell_axis(sort(north), "latitude", 1, "grid.nc"), "does not divide era5_cell_size_deg")
  averaged <- raw_coordinates(lon, north, cell_size = 1)
  expect_identical(averaged$lat, rev(seq(north[1], north[81], length.out = 81)))
  expect_identical(averaged$slack, c(longitude = 0, latitude = 2^-22 * 66))
  expect_identical(raw_coordinates(lon, north, cell_size = 1, even = FALSE)[c("lat", "slack")],
                   list(lat = sort(north), slack = none))
  # South of 64 S it is the other way round: uneven once sorted, so refused
  # before and made even now.
  south <- single(seq(-62.05, -66.05, by = -0.05))
  expect_silent(grid_step(south, "latitude"))
  expect_error(grid_step(sort(south), "latitude"), "not evenly spaced")
  expect_identical(even_coordinates(south), list(values = south, slack = 0))
  expect_identical(raw_coordinates(lon, south)$lat, rev(seq(south[1], south[81], length.out = 81)))
  expect_gt(raw_coordinates(lon, south)$slack[["latitude"]], 0)
  # Single-precision coordinates that were averaged to cells before stay as
  # stored: from 0 E the first step is 0.1 to eight digits.
  from_zero <- single(0:100 / 10)
  expect_false(identical(from_zero, 0:100 / 10))
  expect_identical(raw_coordinates(from_zero, north, cell_size = 1)[c("lon", "lon_order")],
                   list(lon = from_zero, lon_order = 1:101))
  expect_identical(raw_coordinates(from_zero, north, cell_size = 1)$slack[["longitude"]], 0)
  # Longitudes counted from 0 to 360 are made even before they are counted from -180.
  west <- single(249.95 + 0:39 / 10)
  expect_identical(raw_coordinates(west, north)$lon, wrap_longitudes(even_coordinates(west)$values)$lon)
  expect_true(all(raw_coordinates(west, north)$lon < 0))

  folder <- withr::local_tempdir()
  for (lat in list(seq(66, 62, by = -0.05), seq(-62.05, -66.05, by = -0.05))) {
    name <- if (lat[1] > 0) "north" else "south"
    stored <- grid_files(folder, name, lon, lat, "float")$hourly
    exact <- grid_files(folder, name, lon, lat, "double")$hourly
    read_stored <- suppressMessages(read_era5(stored, months = 7, cell_size = NULL))
    read_exact <- suppressMessages(read_era5(exact, months = 7, cell_size = NULL))
    expect_identical(read_stored$tmax, read_exact$tmax)
    expect_identical(read_stored$grid$lat, raw_coordinates(lon, single(lat))$lat)
    expect_identical(suppressMessages(read_era5(stored, months = 7))[parts],
                     suppressMessages(read_era5(exact, months = 7))[parts])
  }
})

test_that("grids with single-precision coordinates are read like the same grids in double precision", {
  folder <- withr::local_tempdir()
  # Longitudes near 100 E, near 250 E and near 355 E (counted from 0 to 360),
  # with points on the edges of the cells; latitudes north to south. `as_stored`
  # marks the one grid that the readers accepted before: 0.1 degrees near 100 E.
  cases <- list(
    list(name = "tenth-100", lon = 99.9 + 0:40 / 10, lat = 41.9 - 0:30 / 10, as_stored = TRUE),
    list(name = "twentieth-100", lon = 99.95 + 0:60 / 20, lat = 66.95 - 0:40 / 20, as_stored = FALSE),
    list(name = "tenth-250", lon = 249.9 + 0:40 / 10, lat = 41.9 - 0:30 / 10, as_stored = FALSE),
    list(name = "twentieth-250", lon = 249.95 + 0:60 / 20, lat = 39.95 - 0:40 / 20, as_stored = FALSE),
    list(name = "tenth-355", lon = 354.9 + 0:40 / 10, lat = 41.9 - 0:30 / 10, as_stored = FALSE),
    list(name = "twentieth-355", lon = 356.95 + 0:60 / 20, lat = 66.95 - 0:40 / 20, as_stored = FALSE))
  for (case in cases) {
    lon <- case$lon
    lat <- case$lat
    stored <- grid_files(folder, case$name, lon, lat, "float")
    exact <- grid_files(folder, case$name, lon, lat, "double")
    kept <- single(lon)
    even <- if (case$as_stored) kept else seq(kept[1], kept[length(kept)], length.out = length(kept))
    expect_identical(is.null(tryCatch(grid_step(kept, "longitude"), error = function(e) NULL)), !case$as_stored,
                     info = case$name)

    # Prepared daily values: the same temperatures, on coordinates that agree to the rounding.
    daily_stored <- read_tmax(stored$daily, months = 7)
    daily_exact <- read_tmax(exact$daily, months = 7)
    expect_identical(daily_stored$tmax, daily_exact$tmax, info = case$name)
    expect_identical(daily_stored$dates, daily_exact$dates, info = case$name)
    expect_identical(daily_stored$grid$lon, even, info = case$name)
    expect_lte(max(abs(daily_stored$cells$lon - daily_exact$cells$lon)), 2^-24 * max(abs(lon)))
    expect_lte(max(abs(daily_stored$cells$lat - daily_exact$cells$lat)), 2^-24 * max(abs(lat)))

    # Raw hourly values averaged to cells: the very same cells and values.
    for (size in c(1, 0.5)) {
      cells_stored <- suppressMessages(read_era5(stored$hourly, months = 7, cell_size = size))
      cells_exact <- suppressMessages(read_era5(exact$hourly, months = 7, cell_size = size))
      expect_identical(cells_stored[parts], cells_exact[parts], info = paste(case$name, size))
      expect_gte(nrow(cells_stored$cells), 4L)
    }
    # Raw hourly values on the grid of the file.
    grid_stored <- suppressMessages(read_era5(stored$hourly, months = 7, cell_size = NULL))
    grid_exact <- suppressMessages(read_era5(exact$hourly, months = 7, cell_size = NULL))
    expect_identical(grid_stored$tmax, grid_exact$tmax, info = case$name)
    expect_identical(grid_stored$dates, grid_exact$dates, info = case$name)
    expect_lte(max(abs(grid_stored$cells$lon - grid_exact$cells$lon)), 2^-24 * max(abs(lon)))
    expect_lte(max(abs(grid_stored$cells$lat - grid_exact$cells$lat)), 2^-24 * max(abs(lat)))
    # Longitudes beyond 180 are counted from -180, as for double precision.
    expect_identical(all(grid_stored$cells$lon < 0), lon[1] > 180, info = case$name)
  }
})

test_that("a point on the edge of a cell stays in its cell when its coordinate is rounded", {
  # 249.9 is stored as 249.899994, so the even grid lies 6e-6 degrees west of
  # the true one and the point at 250 E just below the edge of its cell.
  true <- 249.9 + 0:40 / 10
  even <- even_coordinates(single(true))
  expect_lt(even$values[2], 250)
  exact <- cell_axis(true - 360, "longitude", 1, "grid.nc")
  rounded <- cell_axis(even$values - 360, "longitude", 1, "grid.nc", slack = even$slack)
  expect_identical(rounded, exact)
  expect_identical(exact$ids, c(-110, -109, -108, -107))
  expect_identical(exact$centre, c(-109.5, -108.5, -107.5, -106.5))
  expect_identical(exact$per_cell, 10)
  # Without the slack that point falls into the cell to its west.
  expect_false(identical(cell_axis(even$values - 360, "longitude", 1, "grid.nc")$cell, exact$cell))
  # Without slack the rule is the one of average_to_cells() until now.
  lon <- seq(20, 22, by = 0.25)
  quarter <- cell_axis(lon, "longitude", 1, "grid.nc")
  expect_identical(quarter, list(cell = floor(lon + 1e-6), ids = c(20, 21), centre = c(20.5, 21.5), per_cell = 4))
  expect_error(cell_axis(lon, "longitude", 0.3, "grid.nc"), "does not divide era5_cell_size_deg")
  expect_error(cell_axis(lon[1:3], "longitude", 1, "grid.nc"), "too small for one 1-degree cell")
  # The spacing of an even grid between rounded ends divides the cell size only nearly.
  fine <- even_coordinates(single(-125 + 1 / 48 + 0:71 / 24))
  expect_gt(abs(1 / grid_step(fine$values, "longitude") - 24), 1e-6)
  expect_identical(cell_axis(fine$values, "longitude", 1, "grid.nc", slack = fine$slack)$per_cell, 24)
  expect_error(cell_axis(fine$values, "longitude", 1, "grid.nc"), "does not divide era5_cell_size_deg")
})

test_that("a grid of 1/24 degree with single-precision coordinates is read", {
  folder <- withr::local_tempdir()
  lon <- -125 + 1 / 48 + 0:71 / 24
  lat <- 49 - 1 / 48 - 0:47 / 24
  stored <- grid_files(folder, "fine", lon, lat, "float")
  exact <- grid_files(folder, "fine", lon, lat, "double")
  expect_error(grid_step(single(lon), "longitude"), "not evenly spaced")
  daily <- read_tmax(stored$daily, months = 7)
  expect_identical(daily$tmax, read_tmax(exact$daily, months = 7)$tmax)
  expect_equal(daily$grid$lon_step, 1 / 24, tolerance = 1e-5)
  expect_lte(max(abs(daily$grid$lon - lon)), 2^-24 * max(abs(lon)))
  cells <- suppressMessages(read_era5(stored$hourly, months = 7))
  expect_identical(cells[parts], suppressMessages(read_era5(exact$hourly, months = 7))[parts])
  expect_equal(cells$cells$lon, rep(c(-124.5, -123.5, -122.5), 2))
  expect_equal(cells$cells$lat, rep(c(47.5, 48.5), each = 3))
  expect_identical(suppressMessages(read_era5(stored$hourly, months = 7, cell_size = NULL))$tmax,
                   suppressMessages(read_era5(exact$hourly, months = 7, cell_size = NULL))$tmax)
})

test_that("unevenly spaced coordinates are still refused, in either precision", {
  folder <- withr::local_tempdir()
  lon <- 250 + 0:11 / 10
  uneven <- list(longer = list(lon = cumsum(c(250, rep(0.1, 5), 0.101, rep(0.1, 5))), lat = 41.9 - 0:11 / 10,
                               refusal = "longitude values are not evenly spaced"),
                 repeated = list(lon = c(lon[1:6], lon[6:11]), lat = 41.9 - 0:11 / 10,
                                 refusal = "longitude values are not evenly spaced \\(steps from 0 to"),
                 gaussian = list(lon = lon, lat = gaussian_latitudes(96)[1:12],
                                 refusal = "latitude values are not evenly spaced"))
  values <- array(20, c(2, 12, 12))
  for (name in names(uneven)) {
    grid <- uneven[[name]]
    expect_error(tmax_from_array(values, as.Date("2000-07-01") + 0:1, grid$lon, grid$lat), grid$refusal)
    expect_error(tmax_from_array(values, as.Date("2000-07-01") + 0:1, single(grid$lon), single(grid$lat)), grid$refusal)
    for (coordinates in c("float", "double")) {
      files <- grid_files(folder, name, grid$lon, grid$lat, coordinates)
      expect_error(read_tmax(files$daily, months = 7), grid$refusal)
      expect_error(suppressMessages(read_era5(files$hourly, months = 7, cell_size = NULL)), grid$refusal)
      expect_error(suppressMessages(read_era5(files$hourly, months = 7, cell_size = 0.5)), grid$refusal)
    }
  }
})

test_that("several files are made even only where their spacing is measured", {
  folder <- withr::local_tempdir()
  # A grid of 0.005 degrees near 10 E: single precision moves a coordinate by
  # less than the 1e-6 degrees that files of one grid may differ, and more than
  # the spacing allows. One day in double and one in single precision.
  lon <- 10 + 0:20 / 200
  lat <- 12 + 0:20 / 200
  day <- function(name, n, coordinates) {
    set.seed(n)
    write_grid(file.path(folder, name), list(t2m = array(290 + 15 * runif(24 * 21 * 21), c(24, 21, 21))),
               (n - 1) * 24 + 0:23, lon, lat, units = "K", time_units = "hours since 2000-07-01",
               dims = c("longitude", "latitude", "valid_time"), coordinates = coordinates)
  }
  first <- day("day1-double.nc", 1, "double")
  second <- day("day2-float.nc", 2, "float")
  both <- day("day2-double.nc", 2, "double")
  expect_error(grid_step(single(lon), "longitude"), "not evenly spaced")
  # Without averaging only the first file's spacing is measured, and the second
  # must match it: this was read before, with the coordinates of the first file.
  alone <- function(file, even) {
    read_era5_file(file, "auto", "auto", "auto", "auto", "auto", 1:12, NULL, NULL, NULL, even = even)
  }
  expect_identical(alone(second, FALSE)[c("lon", "lat")], list(lon = single(lon), lat = single(lat)))
  expect_identical(alone(second, FALSE)$slack, c(longitude = 0, latitude = 0))
  expect_identical(alone(second, TRUE)$lon, seq(single(10), single(10.1), length.out = 21))
  expect_true(all(alone(second, TRUE)$slack > 0))
  expect_identical(alone(first, TRUE)[c("lon", "lat", "slack")],
                   list(lon = lon, lat = lat, slack = c(longitude = 0, latitude = 0)))
  mixed <- suppressMessages(read_era5(c(first, second), months = 7, cell_size = NULL))
  exact <- suppressMessages(read_era5(c(first, both), months = 7, cell_size = NULL))
  expect_identical(mixed[parts], exact[parts])
  expect_identical(mixed$grid$lon, lon)
  # The single-precision file first was refused before; now its even grid is the grid of the reading.
  turned <- suppressMessages(read_era5(c(second, first), months = 7, cell_size = NULL))
  expect_identical(turned$tmax, exact$tmax)
  expect_identical(turned$grid$lon, seq(single(10), single(10.1), length.out = 21))
  # Averaged to cells, every file is measured, and files in either precision give the same cells.
  expect_identical(suppressMessages(read_era5(c(first, second), months = 7, cell_size = 0.05))[parts],
                   suppressMessages(read_era5(c(first, both), months = 7, cell_size = 0.05))[parts])
})

test_that("only coordinates that are single-precision numbers are made even", {
  # Longitudes near 350 with steps of 0.001 and 0.00115 are 15 percent uneven,
  # whatever their size; in double precision they are refused as they are.
  uneven <- 350 + cumsum(c(0, rep(c(0.001, 0.00115), 5)))
  expect_identical(even_coordinates(uneven), list(values = uneven, slack = 0))
  expect_error(grid_step(uneven, "longitude"), "not evenly spaced")
  # The same grid of 0.1 degrees as stored in single precision is made even.
  single <- readBin(writeBin(250 + (0:5) / 10, raw(), size = 4L), "double", size = 4L, n = 6L)
  even <- even_coordinates(single)
  expect_gt(even$slack, 0)
  expect_equal(even$values, 250 + (0:5) / 10, tolerance = 1e-6)
})

test_that("year subfolders give one kind of file, and results folders of runs are not data", {
  root <- withr::local_tempdir()
  for (year in c("2001", "2002")) dir.create(file.path(root, year))
  file.create(file.path(root, "2001", "a.nc"), file.path(root, "2002", "b.nc4"))
  dir.create(file.path(root, "SCORCH_run_001"))
  file.create(file.path(root, "SCORCH_run_001", "prepared_tmax.nc"))
  dir.create(file.path(root, "results"))
  file.create(file.path(root, "results", c("Summary.txt", "prepared_tmax.nc")))
  expect_identical(basename(folder_netcdf(root)), "a.nc")
  unlink(file.path(root, "2001", "a.nc"))
  expect_identical(basename(folder_netcdf(root)), "b.nc4")
})
