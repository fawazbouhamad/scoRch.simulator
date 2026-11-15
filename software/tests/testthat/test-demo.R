test_that("the installed demo profiles preserve the scientific and figure choices", {
  prepared <- scoRch.simulator:::demo_settings()
  hourly <- scoRch.simulator:::demo_settings("hourly", input = "hourly-input")
  a <- scoRch.simulator:::read_config(prepared, input_required = FALSE)
  b <- scoRch.simulator:::read_config(hourly, input_required = FALSE)
  for (section in c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp", "figures")) {
    expect_identical(a[[section]], b[[section]])
  }
  expect_equal(a$power_law$bootstrap_repetitions, 5000)
  expect_equal(a$figures$representative_events$type_3, c("2012-06-28", "2023-08-04"))
  expect_true(file.exists(prepared$input$file))
  expect_equal(hourly$period$start_date, "1996-04-01")
  expect_equal(hourly$period$end_date, "2025-09-30")
  expect_equal(hourly$input$processed_file, "output/hourly/prepared_tmax.nc")
  expect_error(run_demo("hourly"), "Supply your local hourly")
  expect_error(run_demo("missing-config.yaml"), "does not exist")
  expect_error(run_scorch(trace_date = "invalid-date"), "trace_date must be")
})

test_that("legacy demo blocks retain their defaults and explicit overrides", {
  legacy <- yaml::read_yaml(test_path("fixtures", "legacy-config.yaml"))
  folder <- tempfile("scorch-legacy-config-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  write_scorch_config()
  prepared <- scoRch.simulator:::demo_settings()
  hourly <- scoRch.simulator:::demo_settings("hourly", input = "hourly-input")
  yaml::write_yaml(legacy, "config.yaml")
  expect_identical(scoRch.simulator:::demo_settings(), prepared)
  expect_identical(scoRch.simulator:::demo_settings("hourly", input = "hourly-input"), hourly)

  legacy$demo$prepared$figures$representative_events$type_3 <- "2012-06-28"
  legacy$demo$hourly$output$directory <- "legacy-hourly"
  legacy$demo$hourly$period$start_date <- "2012-04-01"
  legacy$demo$hourly$period$end_date <- "2012-09-30"
  yaml::write_yaml(legacy, "config.yaml")
  expect_equal(scoRch.simulator:::demo_settings()$figures$representative_events$type_3, "2012-06-28")
  hourly <- scoRch.simulator:::demo_settings("hourly", input = "hourly-input")
  expect_equal(hourly$output$directory, "legacy-hourly")
  expect_equal(hourly$input$processed_file, "legacy-hourly/prepared_tmax.nc")
  expect_equal(hourly$period$start_date, "2012-04-01")
  expect_equal(hourly$period$end_date, "2012-09-30")

  legacy$figures$representative_events$type_3 <- "2023-08-04"
  yaml::write_yaml(legacy, "config.yaml")
  expect_equal(scoRch.simulator:::demo_settings()$figures$representative_events$type_3, "2023-08-04")
  legacy$input$file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  legacy$figures$representative_events$type_3 <- NULL
  yaml::write_yaml(legacy, "config.yaml")
  own <- scoRch.simulator:::demo_settings("config.yaml")
  expect_null(own$figures$representative_events$type_3)
  expect_null(own$period$start_date)
})

test_that("an intentionally omitted legacy demo profile stays omitted", {
  folder <- tempfile("scorch-legacy-profiles-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  config <- list(demo = list(prepared = list()))
  yaml::write_yaml(config, "config.yaml")
  expect_length(scoRch.simulator:::demo_settings()$figures$representative_events, 0L)
  hourly <- scoRch.simulator:::demo_settings("hourly", input = "hourly-input")
  expect_equal(hourly$output$directory, "output")
  expect_equal(hourly$input$format, "auto")
  expect_null(hourly$period$start_date)
  expect_null(hourly$input$processed_file)
})

test_that("the demo reads local settings and respects a configured input", {
  folder <- tempfile("scorch-config-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  write_scorch_config()
  config <- yaml::read_yaml("config.yaml")
  config$output$directory <- "my-results"
  config$heatwaves$threshold_percentile <- 90
  config$power_law$bootstrap_repetitions <- 200
  config$figures$representative_events$type_3 <- "2012-06-28"
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings()
  expect_equal(settings$output$directory, "my-results")
  expect_equal(settings$heatwaves$threshold_percentile, 90)
  expect_equal(settings$power_law$bootstrap_repetitions, 200)
  expect_equal(settings$figures$representative_events$type_3, "2012-06-28")
  expect_true(file.exists(settings$input$file))
  expect_identical(settings, scoRch.simulator:::demo_settings("config.yaml"))

  config$input$file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  config$figures$representative_events <- list(type_1 = NULL, type_2 = NULL, type_3 = NULL, type_4 = NULL)
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings()
  expect_equal(settings$input$file, config$input$file)
  expect_null(settings$figures$representative_events$type_3)
  expect_equal(settings$output$directory, "my-results")
  config$input$file <- NULL
  config$input$format <- "era5"
  yaml::write_yaml(config, "config.yaml")
  expect_error(scoRch.simulator:::demo_settings(), "Set input: file")
})

test_that("changing the bundled demo period uses automatic figure choices", {
  folder <- tempfile("scorch-period-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  write_scorch_config()
  original <- yaml::read_yaml("config.yaml")
  periods <- list(
    one_year = list(warm_season_months = 4:9, start_date = "2012-01-01", end_date = "2012-12-31"),
    july = list(warm_season_months = 7L, start_date = NULL, end_date = NULL))
  for (name in names(periods)) {
    config <- original
    config$period <- periods[[name]]
    config$output$directory <- name
    config$power_law$bootstrap_repetitions <- 20
    yaml::write_yaml(config, "config.yaml")
    settings <- scoRch.simulator:::demo_settings("config.yaml")
    expect_length(settings$figures$representative_events, 0L)
    result <- suppressMessages(run_scorch("config.yaml"))
    expect_s3_class(result, "scorch_result")
    expect_equal(length(result$input$dates), if (name == "one_year") 183L else 930L)
    expect_true(file.exists(file.path(name, "Summary.txt")))
    expect_true(any(result$plots$status == "generated"))
  }
  # Explicit figure choices still belong to the user, including invalid dates
  # that the workflow must report rather than silently replacing.
  config$figures$representative_events$type_3 <- "1900-01-01"
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings("config.yaml")
  expect_equal(settings$figures$representative_events$type_3, "1900-01-01")
})

test_that("changed event-selection settings use automatic demo figure choices", {
  folder <- tempfile("scorch-event-settings-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  write_scorch_config()
  original <- yaml::read_yaml("config.yaml")
  changes <- list(
    local_threshold = list("heatwaves", "threshold_percentile", 90),
    minimum_duration = list("heatwaves", "min_duration_days", 4),
    minimum_exceeding = list("heatwaves", "min_exceeding_days", 4),
    regional_threshold = list("regional_days", "percentile", 95),
    clustering_distances = list("clustering", "eps_values", c(1, 2, 3)),
    clustering_minimum = list("clustering", "min_samples_values", 5:8))
  for (name in names(changes)) {
    change <- changes[[name]]
    config <- original
    config[[change[[1]]]][[change[[2]]]] <- change[[3]]
    config$output$directory <- name
    config$power_law$bootstrap_repetitions <- 20
    yaml::write_yaml(config, "config.yaml")
    settings <- scoRch.simulator:::demo_settings("config.yaml")
    expect_length(settings$figures$representative_events, 0L)
    hourly <- scoRch.simulator:::demo_settings("hourly", input = "hourly-input")
    expect_length(hourly$figures$representative_events, 0L)
    if (name %in% c("local_threshold", "clustering_distances")) {
      result <- suppressMessages(run_scorch("config.yaml"))
      expect_s3_class(result, "scorch_result")
      expect_true(file.exists(file.path(name, "Summary.txt")))
      expect_true(any(result$plots$status == "generated"))
    }
  }
  config$figures$representative_events$type_3 <- "1900-01-01"
  yaml::write_yaml(config, "config.yaml")
  expect_equal(scoRch.simulator:::demo_settings("config.yaml")$figures$representative_events$type_3,
               "1900-01-01")
})

test_that("output and downstream model settings retain the reference figure dates", {
  folder <- tempfile("scorch-model-settings-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  write_scorch_config()
  config <- yaml::read_yaml("config.yaml")
  config$output$directory <- "my-results"
  config$power_law$bootstrap_repetitions <- 20
  config$lgcp$raster_km <- 50
  config$ellipses$scale_factor <- 1.5
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings("config.yaml")
  expect_equal(settings$figures$representative_events$type_3, c("2012-06-28", "2023-08-04"))
})

test_that("hourly YAML input saves prepared data only at the requested path", {
  folder <- tempfile("scorch-prepared-path-")
  dir.create(folder)
  previous <- setwd(folder)
  on.exit(setwd(previous), add = TRUE)
  dims <- list(ncdf4::ncdim_def("longitude", "degrees_east", seq(20, 22, by = 0.25)),
               ncdf4::ncdim_def("latitude", "degrees_north", seq(10, 12, by = 0.25)),
               ncdf4::ncdim_def("valid_time", "hours since 2000-07-01", 0:71))
  variable <- ncdf4::ncvar_def("t2m", "K", dims, prec = "double")
  nc <- ncdf4::nc_create("hourly.nc", variable)
  ncdf4::ncvar_put(nc, variable, array(300 + seq_len(9 * 9 * 72) / 1e5, c(9, 9, 72)))
  ncdf4::nc_close(nc)
  write_scorch_config()
  config <- yaml::read_yaml("config.yaml")
  config$input$file <- "hourly.nc"
  config$input$format <- "era5"
  config$input$processed_file <- "my prepared data/chosen.nc"
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings("config.yaml")
  prepared <- suppressMessages(scoRch.simulator:::read_configured_input(settings))
  expect_true(file.exists(config$input$processed_file))
  expect_false(file.exists("output/prepared_tmax.nc"))
  expect_identical(read_tmax(config$input$processed_file)$tmax, prepared$tmax)
  # The explicit hourly demonstration keeps its default destination, while
  # respecting a path supplied in the local configuration.
  expect_equal(scoRch.simulator:::demo_settings("hourly")$input$processed_file,
               config$input$processed_file)
  config$input$processed_file <- NULL
  config$output$directory <- "without saved input"
  yaml::write_yaml(config, "config.yaml")
  settings <- scoRch.simulator:::demo_settings("config.yaml")
  expect_null(settings$input$processed_file)
  suppressMessages(scoRch.simulator:::read_configured_input(settings))
  expect_false(dir.exists("without saved input"))
})

test_that("successful publishing retires former names and preserves user files", {
  out <- tempfile("scorch-migration-")
  dir.create(file.path(out, "Heatwave_Catalogs"), recursive = TRUE)
  dir.create(file.path(out, "Modelling"))
  writeLines("old", file.path(out, "Heatwave_Catalogs", "Heatwaves.xlsx"))
  writeLines("old", file.path(out, "Modelling", "Power_Law_Model.xlsx"))
  writeLines("old summary", file.path(out, "Run_Summary.txt"))
  writeLines("keep", file.path(out, "Heatwave_Catalogs", "user.csv"))
  stage <- scoRch.simulator:::prepare_run_output(out)
  writeLines("new", file.path(stage, "catalogs", "01_heatwave_events.xlsx"))
  writeLines("new summary", file.path(stage, "Summary.txt"))
  scoRch.simulator:::publish_run_output(stage, out)
  expect_false(file.exists(file.path(out, "Heatwave_Catalogs", "Heatwaves.xlsx")))
  expect_false(dir.exists(file.path(out, "Modelling")))
  expect_false(file.exists(file.path(out, "Run_Summary.txt")))
  expect_equal(readLines(file.path(out, "Summary.txt")), "new summary")
  expect_equal(readLines(file.path(out, "Heatwave_Catalogs", "user.csv")), "keep")
  expect_equal(readLines(file.path(out, "catalogs", "01_heatwave_events.xlsx")), "new")
})
