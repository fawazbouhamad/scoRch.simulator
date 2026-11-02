test_that("write_tmax() writes a file that read_tmax() reads back", {
  dates <- as.Date("2000-06-01") + 0:9
  values <- array(20 + seq_len(10 * 2 * 3) / 7, c(10, 2, 3))
  values[, 1, 1] <- NA                                              # a cell without data on any date
  tmax <- tmax_from_array(values, dates, lon = c(10, 11, 12), lat = c(50, 51), source = "test array")
  expect_true(suppressMessages(nrow(tmax$cells)) == 5)
  packed <- write_tmax(tmax, tempfile(fileext = ".nc"), title = "test", licence = "none")
  back <- read_tmax(packed, months = 6)
  expect_equal(back$cells[c("lon", "lat")], tmax$cells[c("lon", "lat")], ignore_attr = TRUE)
  expect_equal(back$dates, tmax$dates)
  expect_lte(max(abs(back$tmax - tmax$tmax)), 0.0051)
  exact <- write_tmax(tmax, tempfile(fileext = ".nc"), precision = "float32")
  expect_equal(read_tmax(exact, months = 6)$tmax, tmax$tmax, tolerance = 1e-6)
  nc <- ncdf4::nc_open(packed)
  expect_equal(ncdf4::ncatt_get(nc, 0, "title")$value, "test")
  expect_match(ncdf4::ncatt_get(nc, 0, "source")$value, "test array")
  expect_match(ncdf4::ncatt_get(nc, 0, "history")$value, "scoRch.simulator")
  expect_equal(ncdf4::ncatt_get(nc, "tmax", "scale_factor")$value, 0.01)
  ncdf4::nc_close(nc)
  expect_error(write_tmax(tmax, packed), "already exists")
  expect_error(write_tmax(list(), tempfile()), "scorch_tmax|read_era5")
})


test_that("the shipped CDS sample reduces to the cells of the demonstration data", {
  sample <- system.file("extdata", "cds_daily_tmax_2012_great_plains.nc", package = "scoRch.simulator")
  demo <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  skip_if(!nzchar(demo), "the demonstration data are not installed")
  from_cds <- suppressMessages(read_era5(sample, months = 4:9, cell_size = 1))
  expect_equal(nrow(from_cds$cells), 100)
  expect_equal(length(from_cds$dates), 183)
  whole <- read_tmax(demo, start = "2012-01-01", end = "2012-12-31")
  columns <- match(paste(from_cds$cells$lon, from_cds$cells$lat), paste(whole$cells$lon, whole$cells$lat))
  expect_false(anyNA(columns))
  expect_equal(whole$dates, from_cds$dates)
  # the same ERA5 values from two sources (ARCO-ERA5 and the CDS), float32
  # rounding apart, plus the 0.01-degree packing of the demonstration file
  expect_lte(max(abs(whole$tmax[, columns] - from_cds$tmax)), 0.0065)
})

