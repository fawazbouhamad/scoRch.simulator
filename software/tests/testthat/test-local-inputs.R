test_that("the configuration accepts only local input and gives a useful migration error", {
  read <- scoRch.simulator:::read_config
  file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  expect_equal(read(list(input = list(file = file, format = "daily_tmax")))$input$file, file)
  expect_error(read(list(input = list(source = "cds"))), "Unknown settings.*source")
  expect_error(read(list(input = list(cds = list(area = c(50, -130, 25, -58))))), "Unknown settings.*cds")
  expect_error(read(list(input = list(file = file, processed_file = 1))), "processed_file")
})

test_that("locally downloaded ERA5 can save a reusable prepared input", {
  file <- system.file("extdata", "cds_daily_tmax_2012_great_plains.nc", package = "scoRch.simulator")
  saved <- file.path(tempfile("local-era5"), "processed.nc")
  cfg <- scoRch.simulator:::read_config(list(input = list(file = file, format = "era5", processed_file = saved)))
  prepared <- suppressMessages(scoRch.simulator:::read_configured_input(cfg))
  expect_true(file.exists(saved))
  back <- read_tmax(saved)
  expect_identical(back$dates, prepared$dates)
  expect_identical(back$tmax, prepared$tmax)
  expect_equal(back$cells, prepared$cells)
  expect_match(prepared$preprocessing, "saved to")
  cfg$input$processed_file <- file
  expect_error(suppressMessages(scoRch.simulator:::read_configured_input(cfg)), "must not replace")
  cfg$input$file <- saved
  cfg$input$format <- "daily_tmax"
  cfg$input$processed_file <- tempfile(fileext = ".nc")
  expect_error(scoRch.simulator:::read_configured_input(cfg), "leave it null")
})
