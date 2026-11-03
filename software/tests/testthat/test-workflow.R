example_file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")

test_that("the complete workflow runs on the example data and writes every output", {
  out <- file.path(tempfile("scorch"), "results")
  dir.create(file.path(out, "figures"), recursive = TRUE)
  writeLines("old", file.path(out, "figures", "figure_2.png"))
  writeLines("keep", file.path(out, "figures", "user-plot.png"))
  config <- list(input = list(file = example_file), output = list(directory = out),
                 power_law = list(bootstrap_repetitions = 20))
  expect_output(result <- suppressMessages(run_scorch(config, trace_date = "2012-06-30")), "29 compound events")
  expect_s3_class(result, "scorch_result")
  expect_equal(result$regional$threshold_cells, 353)
  expect_equal(nrow(result$regional$selected_days), 140)
  expect_equal(nrow(result$clusters$clusters), 376)
  expect_equal(nrow(result$ellipses), 376)
  expect_equal(nrow(result$events$events), 29)
  expect_equal(tabulate(result$events$events$event_type, 4), c(1, 4, 20, 4))
  expect_equal(sum(result$events$table_1$number_of_compound_events), 29)
  expect_equal(result$power_law$summary$reps, c(20, 20))
  expect_true(all(is.finite(result$lgcp$parameters)))
  expect_equal(nrow(result$lgcp$concentration), 6)
  expect_true(all(result$lgcp$cells$concentration_rank > 0 & result$lgcp$cells$concentration_rank <= 1))
  figures <- c("ellipse_spatial_patterns", "single_day_events", "consistent_multiday_events", "mixed_multiday_events",
               "ellipse_geometry_by_type", "event_geometry_distributions", "annual_event_counts_and_duration",
               "power_law_model", "spatial_risk_model")
  expected <- c(file.path("catalogs", c("01_heatwave_events.xlsx", "02_regionally_extensive_heatwaves.xlsx", "03_extreme_heatwave_clusters.xlsx",
                                                 "04_extreme_heatwave_ellipses.xlsx", "05_compound_heatwave_events.xlsx")),
                file.path("modeling", c("Power_Law_Model.xlsx", "Spatial_Risk_Model.xlsx")),
                file.path("Heatwave_Statistics.xlsx"),
                file.path("figures", paste0(figures, ".png")), "Run_Summary.txt")
  expect_true(all(file.exists(file.path(out, expected))))
  expect_true(all(result$plots$status == "generated"))
  expect_setequal(list.files(out, recursive = TRUE), c(expected, "figures/figure_2.png", "figures/user-plot.png"))
  readme <- readLines(file.path(out, "Run_Summary.txt"))
  expect_equal(sum(readme == "TRACED EVENT"), 1L)
  expect_true(any(grepl(paste0(out, "/figures/"), readme, fixed = TRUE)))
  expect_false(any(grepl("scorch-stage-", readme, fixed = TRUE)))
  expect_equal(readme[1], paste("scoRch.simulator", as.character(packageVersion("scoRch.simulator")), "results"))
  expect_true(any(readme == "  Stage 5  Compound heatwave events: 29"))
  expect_true(any(grepl("^  ellipse_spatial_patterns.png: ", readme)))
  expect_false(any(grepl("not drawn", readme)))
  expect_equal(readme[which(readme == "WARNINGS") + 1], "  None.")
  expect_true(any(grepl("Heatwave Events (65790 rows)", readme, fixed = TRUE)))
  expect_equal(readLines(file.path(out, "figures", "figure_2.png")), "old")      # files the package did not write stay
  expect_equal(readLines(file.path(out, "figures", "user-plot.png")), "keep")
  compound <- file.path(out, "catalogs", "05_compound_heatwave_events.xlsx")
  expect_equal(sheet_names(compound), c("Compound Events", "Event Days"))
  catalog_5 <- openxlsx2::read_xlsx(compound, sheet = "Compound Events")
  expect_equal(catalog_5$compound_event_id, seq_len(29))
  expect_equal(catalog_5$start_date, result$events$events$start_date)
  expect_equal(catalog_5$mean_area_km2, result$events$events$mean_area_km2, tolerance = 1e-14)
  summary_book <- file.path(out, "Heatwave_Statistics.xlsx")
  expect_equal(sheet_names(summary_book),
               c("Event Type Summary", "Cell Thresholds", "Daily Heatwave Extent", "Duration Trends"))
  expect_equal(nrow(openxlsx2::read_xlsx(summary_book, sheet = "Event Type Summary")), 4)
  extent <- openxlsx2::read_xlsx(summary_book, sheet = "Daily Heatwave Extent")
  expect_equal(nrow(extent), length(result$input$dates))
  expect_equal(is.na(extent$heatwave_share_of_exceeding_cells), is.na(result$regional$daily$heatwave_share_of_exceeding_cells))
  expect_equal(nrow(openxlsx2::read_xlsx(file.path(out, "catalogs", "01_heatwave_events.xlsx"), sheet = "Heatwave Events")),
               nrow(result$heatwaves$events))
  # A figure can be redrawn from the result, and the overrides are checked.
  png_file <- tempfile(fileext = ".png")
  expect_true(scorch_figure(result, name = "annual_event_counts_and_duration", file = png_file))
  expect_true(file.exists(png_file))
  expect_error(scorch_figure(result, 2), "no visualization")
  type_4 <- result$events$events[result$events$events$event_type == 4, ]
  result$config$figures$representative_events$type_4 <- format(type_4$start_date[1])
  shown <- scoRch.simulator:::representative_events(result, 4)[[1]]
  expect_equal(shown[1], type_4$start_date[1])
  result$config$figures$representative_events$type_4 <- "1900-01-01"
  expect_error(scoRch.simulator:::representative_events(result, 4), "not inside a Type 4 event")
  previous_png <- unname(tools::md5sum(png_file))
  expect_error(scorch_figure(result, "mixed_multiday_events", png_file), "not inside a Type 4 event")
  expect_equal(unname(tools::md5sum(png_file)), previous_png)
  result_without_lgcp <- result
  result_without_lgcp$lgcp <- NULL
  skipped_file <- tempfile(fileext = ".png")
  expect_false(suppressMessages(scorch_figure(result_without_lgcp, "spatial_risk_model", skipped_file)))
  expect_false(file.exists(skipped_file))
  expect_length(scoRch.simulator:::representative_events(result, 3), 2)   # other types unaffected
  type_3 <- result$events$events[result$events$events$event_type == 3, ]
  result$config$figures$representative_events$type_3 <- format(type_3$start_date[1] + 0:1)
  expect_error(scoRch.simulator:::representative_events(result, 3), "different Type 3 events")
  result$config$figures$representative_events$type_3 <- format(type_3$end_date[1])
  expect_error(scoRch.simulator:::representative_events(result, 3), "last day")
  result$config$figures$representative_events$type_4 <- NULL
  automatic <- scoRch.simulator:::representative_events(result, 4)
  expect_true(all(diff(automatic[[1]]) == 1))
  bad_config <- config
  bad_config$figures <- list(representative_events = list(type_4 = "1900-01-01"))
  previous_catalog <- file.info(compound)$mtime
  expect_error(suppressMessages(run_scorch(bad_config)), "not inside a Type 4 event")
  expect_equal(file.info(compound)$mtime, previous_catalog)
  # Data already in R can replace the input file.
  tmax <- read_tmax(example_file, start = "2015-01-01")
  again <- suppressMessages(run_scorch(list(output = list(directory = file.path(out, "again")),
                                            power_law = list(bootstrap_repetitions = 10)), tmax = tmax))
  expect_equal(nrow(again$input$cells), 1800)
  expect_true(file.exists(file.path(out, "again", "Run_Summary.txt")))
  july <- suppressMessages(run_scorch(list(output = list(directory = file.path(out, "july")),
                                           period = list(warm_season_months = 7),
                                           power_law = list(bootstrap_repetitions = 10)), tmax = tmax))
  expect_true(all(format(july$input$dates, "%m") == "07"))
  expect_equal(length(july$input$dates), 31L * 11L)
})

test_that("a no-cluster rerun replaces old results with clearly partial output", {
  out <- tempfile("scorch-partial-")
  dir.create(out)
  for (folder in c("catalogs", "modeling", "figures")) dir.create(file.path(out, folder))
  writeLines("old", file.path(out, "catalogs", "04_extreme_heatwave_ellipses.xlsx"))
  writeLines("old", file.path(out, "modeling", "Spatial_Risk_Model.xlsx"))
  writeLines("old", file.path(out, "figures", "spatial_risk_model.png"))
  writeLines("keep", file.path(out, "user-notes.txt"))
  writeLines("keep", file.path(out, "modeling", "user-table.csv"))
  values <- array(30, c(10, 3, 3))
  dates <- as.Date("2000-06-01") + 0:9
  tmax <- tmax_from_array(values, dates, 1:3, 1:3)
  config <- list(output = list(directory = out),
                 clustering = list(min_samples_values = 1000))
  expect_error(suppressMessages(run_scorch(config, tmax = tmax)), "No selected day formed a cluster")
  expect_true(file.exists(file.path(out, "catalogs", "03_extreme_heatwave_clusters.xlsx")))
  expect_false(file.exists(file.path(out, "catalogs", "04_extreme_heatwave_ellipses.xlsx")))
  expect_false(file.exists(file.path(out, "modeling", "Spatial_Risk_Model.xlsx")))
  expect_false(file.exists(file.path(out, "figures", "spatial_risk_model.png")))
  expect_equal(sheet_names(file.path(out, "Heatwave_Statistics.xlsx")),
               c("Cell Thresholds", "Daily Heatwave Extent"))
  readme <- readLines(file.path(out, "Run_Summary.txt"))
  expect_match(readme[1], "INCOMPLETE RUN")
  expect_true(any(grepl("The run is incomplete: no selected day formed a cluster", readme, fixed = TRUE)))
  expect_equal(sum(grepl("[.]txt$", list.files(out, recursive = TRUE))), 2)   # Run_Summary.txt and the user notes
  expect_equal(readLines(file.path(out, "user-notes.txt")), "keep")
  expect_equal(readLines(file.path(out, "modeling", "user-table.csv")), "keep")
})

test_that("an unavailable LGCP and absent event types are reported explicitly", {
  out <- tempfile("scorch-lgcp-")
  tmax <- tmax_from_array(array(30, c(3, 3, 3)),
                          as.Date("2000-06-01") + 0:2, 1:3, 1:3)
  config <- list(output = list(directory = out),
                 power_law = list(bootstrap_repetitions = 1),
                 clustering = list(eps_values = 1, min_samples_values = 1))
  result <- suppressMessages(suppressWarnings(run_scorch(config, tmax = tmax)))
  expect_null(result$lgcp)
  expect_match(result$lgcp_error, "needs more centroids than the 3 available")
  expect_equal(result$plots$status[result$plots$name == "spatial_risk_model"], "skipped")
  expect_equal(result$plots$status[result$plots$name == "annual_event_counts_and_duration"], "generated")
  readme <- readLines(file.path(out, "Run_Summary.txt"))
  expect_true(any(grepl("Log-Gaussian Cox process: not fitted. Reason: The LGCP needs more centroids", readme, fixed = TRUE)))
  expect_true(any(grepl("^  spatial_risk_model.png: .*[[]not drawn: .+[]]$", readme)))
  expect_true(any(grepl("^  The LGCP was not fitted: ", readme)))
  expect_true(any(grepl("^  Figure spatial_risk_model.png was not drawn: ", readme)))
  expect_false(any(readme == ")"))
  expect_false(file.exists(file.path(out, "modeling", "Spatial_Risk_Model.xlsx")))
  expect_false(file.exists(file.path(out, "figures", "spatial_risk_model.png")))
})

test_that("the stage functions can be chained by hand", {
  tmax <- read_tmax(example_file, start = "2010-01-01")
  clusters <- cluster_heatwaves(select_regional_days(detect_heatwaves(tmax)))
  events <- classify_events(clusters, fit_ellipses(clusters, tmax))
  expect_true(nrow(events$events) > 0)
  expect_equal(sum(events$events$total_clusters), nrow(events$ellipses))
  expect_true(all(events$event_days$date %in% clusters$daily_settings$date))
  expect_equal(sum(events$event_days$n_clusters), nrow(events$ellipses))
  power_law <- fit_power_law(events, n_bootstrap = 10)
  expect_equal(nrow(power_law$summary), 2)
  expect_error(fit_ellipses(clusters, tmax, scale_factor = -1), "scale_factor")
  expect_error(detect_heatwaves(tmax, threshold_percentile = 150), "threshold_percentile")
})
