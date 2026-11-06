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

test_that("successful publishing retires former names and preserves user files", {
  out <- tempfile("scorch-migration-")
  dir.create(file.path(out, "Heatwave_Catalogs"), recursive = TRUE)
  dir.create(file.path(out, "Modelling"))
  writeLines("old", file.path(out, "Heatwave_Catalogs", "Heatwaves.xlsx"))
  writeLines("old", file.path(out, "Modelling", "Power_Law_Model.xlsx"))
  writeLines("keep", file.path(out, "Heatwave_Catalogs", "user.csv"))
  stage <- scoRch.simulator:::prepare_run_output(out)
  writeLines("new", file.path(stage, "catalogs", "01_heatwave_events.xlsx"))
  scoRch.simulator:::publish_run_output(stage, out)
  expect_false(file.exists(file.path(out, "Heatwave_Catalogs", "Heatwaves.xlsx")))
  expect_false(dir.exists(file.path(out, "Modelling")))
  expect_equal(readLines(file.path(out, "Heatwave_Catalogs", "user.csv")), "keep")
  expect_equal(readLines(file.path(out, "catalogs", "01_heatwave_events.xlsx")), "new")
})
