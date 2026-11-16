test_that("ZIP selections can be inspected, read, and replayed from the original file", {
  folder <- withr::local_tempdir()
  dates <- seq(as.Date("2012-01-01"), as.Date("2012-12-31"), by = "day")
  values <- array(seq_len(length(dates) * 4) / 100, c(length(dates), 2, 2))
  tiny <- tmax_from_array(values, dates, lon = 0:1, lat = 10:11, months = 1:12)
  input <- file.path(folder, "small.nc")
  write_tmax(tiny, input, precision = "float64")
  archive <- file.path(folder, "data.zip")
  zip::zipr(archive, input)
  cfg <- read_config(list(input = list(file = archive), period = list(warm_season_months = 6:8)))
  source <- open_temperature_source(archive)
  withr::defer(close_temperature_source(source))
  expect_equal(inspect_temperature_source(source, cfg$input)$dates, tiny$dates)
  back <- read_configured_input(cfg)
  summer <- as.integer(format(dates, "%m")) %in% 6:8
  expect_equal(back$dates, dates[summer])
  expect_length(back$dates, 92L)
  expect_identical(back$tmax, tiny$tmax[summer, , drop = FALSE])
  expect_match(back$source, "data.zip", fixed = TRUE)
  write_guided_config(cfg, folder)
  saved <- load_guided_config(file.path(folder, "config.yaml"))
  expect_identical(saved$input$file, normalizePath(archive, winslash = "/"))
  expect_false(grepl(source$scratch, saved$input$file, fixed = TRUE))
  close_temperature_source(source)
  expect_false(dir.exists(source$scratch))
  invalid <- file.path(folder, "no-data.zip")
  zip::zipr(invalid, file.path(folder, "config.yaml"))
  expect_error(open_temperature_source(invalid), "no NetCDF")
})

# Every analysis setting of the window at the value of a configuration.
window_inputs <- function(cfg, ...) {
  args <- list(...)
  for (section in c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp")) {
    for (key in names(cfg[[section]])) {
      value <- cfg[[section]][[key]]
      args[[paste(section, key, sep = "_")]] <- if (section == "clustering") paste(value, collapse = ",") else value
    }
  }
  args
}

test_that("the launcher waits for Run and retains a selected dataset when loading settings", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  nc <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  write_scorch_config("settings.yaml")
  other <- yaml::read_yaml("settings.yaml")
  other$input <- list(file = "another-region.nc", format = "era5", variable = "t2m")
  other$regional_days$percentile <- 99
  yaml::write_yaml(other, "other.yaml")
  shiny::testServer(scorch_server, {
    session$flushReact()
    expect_null(state$source)
    expect_false(dir.exists("output"))
    session$setInputs(use_example = 1)
    expect_null(state$path)
    expect_identical(header()$format, "daily_tmax")
    choose("data", nc)
    choose("config", file.path(project, "other.yaml"))
    expect_identical(header()$format, "daily_tmax")
    expect_identical(base()$regional_days$percentile, 99)
    choose("config", file.path(project, "settings.yaml"))
    expect_identical(state$path, nc)
    expect_equal(length(header()$dates), 5490)
    expect_false(dir.exists("output"))
    do.call(session$setInputs, window_inputs(base(), month_from = "6", month_to = "8", limit_dates = TRUE,
      dates = as.Date(c("2012-01-01", "2012-12-31")), native_grid = FALSE, cell_size = 1))
    expect_equal(range(plan()$dates), as.Date(c("2012-06-01","2012-08-31")))
    expect_length(plan()$dates, 92L)
    expect_identical(plan()$config$input$file, nc)
    expect_identical(period_text(plan()$period), "June\u2013August 2012")
    expect_false(dir.exists("output"))
    session$setInputs(dates = as.Date(c("1980-01-01", "1980-12-31")))
    expect_match(conditionMessage(plan()), "no observations")
    # The whole record: the months apply in every selected year.
    session$setInputs(limit_dates = FALSE)
    expect_identical(period_text(plan()$period), "June\u2013August in every selected year (1996\u20132025)")
    expect_length(plan()$dates, 92L * 30L)
    expect_null(plan()$config$period$start_date)
    # A results folder chosen in the dialog receives the next numbered run.
    choose("output", file.path(project, "my results"))
    expect_identical(dest(), file.path(guided_path(file.path(project, "my results")), "SCORCH_run_001"))
    expect_false(dir.exists(file.path(project, "my results")))
  })
})

test_that("a guided run never takes over an older run folder", {
  root <- withr::local_tempdir()
  first <- next_run_folder(root, reserve = TRUE)
  writeLines("previous analysis", file.path(first, "Summary.txt"))
  second <- next_run_folder(root, reserve = TRUE)
  expect_identical(basename(second), "SCORCH_run_002")
  expect_identical(readLines(file.path(first, "Summary.txt")), "previous analysis")
  expect_identical(season_limits(c(1, 2, 12)), c(12, 2))
})

test_that("selected months are described for every selected year", {
  dates <- seq(as.Date("1996-01-01"), as.Date("2025-12-31"), by = "day")
  month <- as.integer(format(dates, "%m"))
  summer <- period_summary(dates[month %in% 6:8], 6:8, dates)
  expect_identical(summer[c("months", "years", "n_years", "n_days")],
                   list(months = "June\u2013August", years = "1996\u20132025", n_years = 30L, n_days = 2760L))
  expect_identical(summer$unused, "September\u2013May")
  expect_identical(summer$absent, "")
  expect_identical(period_text(summer), "June\u2013August in every selected year (1996\u20132025)")
  note <- as.character(period_note(summer))
  expect_match(note, "June\u2013August in every selected year:", fixed = TRUE)
  expect_match(note, "30 years (1996\u20132025), 2,760 days.", fixed = TRUE)
  expect_match(note, "This file also holds September\u2013May. Those months are not analyzed.", fixed = TRUE)

  one <- period_summary(dates[month %in% 6:8 & format(dates, "%Y") == "2012"], 6:8, dates)
  expect_identical(period_text(one), "June\u2013August 2012")
  expect_match(as.character(period_note(one)), "92 days.", fixed = TRUE)

  # A season that crosses New Year belongs to the year in which it starts.
  winter <- period_summary(dates[month %in% c(11, 12, 1, 2, 3)], c(11, 12, 1, 2, 3), dates)
  expect_identical(winter[c("months", "years", "n_years")],
                   list(months = "November\u2013March", years = "1995/96\u20132025/26", n_years = 31L))
  whole <- period_summary(dates, 1:12, dates)
  expect_identical(whole[c("months", "unused", "absent")], list(months = "Every month", unused = "", absent = ""))

  # A file that holds only part of the configured season starts from what it has.
  held <- dates[month %in% 6:8]
  expect_identical(available_season(4:9, held), 6:8)
  expect_identical(available_season(4:9, dates), 4:9)
  expect_identical(available_season(4:9, dates[month %in% c(11, 12, 1)]), c(1L, 11L, 12L))
  partial <- period_summary(held, 4:9, held)
  expect_identical(partial$absent, "April\u2013May, September")
  expect_identical(month_runs(c(12, 1, 2)), "December\u2013February")
  expect_identical(month_runs(7), "July")
})

test_that("the file dialog lists folders first and only what it can open", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "era5 files"))
  dir.create(file.path(root, "Alpha"))
  file.create(file.path(root, c("b.nc", "A.NC", "archive.zip", "notes.txt", "config.yaml")))
  file.create(file.path(root, "era5 files", c("y1.nc", "y2.nc", "readme.txt")))
  data <- chooser_listing(root, "data")
  expect_identical(data$rows$name, c("Alpha", "era5 files", "A.NC", "archive.zip", "b.nc"))
  expect_identical(data$rows$folder, c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(data$rows$type, c("Folder", "Folder", "NetCDF data", "ZIP archive", "NetCDF data"))
  expect_identical(data$netcdf, 2L)
  expect_identical(data$hidden, 0L)
  expect_identical(chooser_listing(root, "config")$rows$name, c("Alpha", "era5 files", "config.yaml"))
  expect_identical(chooser_listing(root, "output")$rows$name, c("Alpha", "era5 files"))
  expect_identical(chooser_listing(file.path(root, "era5 files"), "data")$netcdf, 2L)
  expect_identical(chooser_listing(root, "data", limit = 2L)$hidden, 3L)
  places <- chooser_places()
  expect_true(all(dir.exists(places)))
  expect_true("Working folder" %in% names(places))
  parents <- path_parents(normalizePath(root, winslash = "/"))
  expect_identical(unname(parents[length(parents)]), normalizePath(root, winslash = "/"))
  expect_identical(unname(parents[1]), dirname(unname(parents[1])))
  expect_identical(file_size_label(c(0, 1023, 1024, 33841054)), c("0 bytes", "1023 bytes", "1.0 KB", "32.3 MB"))

  # The dialog offers "Use this folder" for data and results, and "New folder" for results.
  body <- function(kind, folder = root) {
    as.character(chooser_body(c(list(kind = kind, folder = normalizePath(folder, winslash = "/"), problem = NULL),
                                chooser_listing(folder, kind)), places))
  }
  expect_match(body("data"), ">Use this folder<", fixed = TRUE)
  expect_match(body("data"), ">Open<", fixed = TRUE)
  expect_match(body("data", file.path(root, "era5 files")), "This folder has 2 NetCDF files", fixed = TRUE)
  expect_match(body("output"), ">New folder<", fixed = TRUE)
  expect_false(grepl(">Open<", body("output"), fixed = TRUE))
  expect_false(grepl(">Use this folder<", body("config"), fixed = TRUE))
})

test_that("the window asks one question about an unclear file and keeps the answer", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  silent <- write_grid(file.path(folder, "no-units.nc"), array(300 + seq_len(48) / 10, c(4, 3, 4)), 151:154,
                       c(10, 11, 12, 13), c(40, 41, 42), units = "")
  shiny::testServer(scorch_server, {
    choose("data", silent)
    session$flushReact()
    expect_identical(question()$id, "units")
    expect_identical(question()$recommended, "K")
    expect_match(conditionMessage(plan()), "Answer the question")
    session$setInputs(answer = "degF", answer_apply = 1)
    expect_identical(question()$id, "units")
    session$setInputs(answer = "K", answer_apply = 2)
    expect_null(question())
    expect_identical(header()$units, "K")
    expect_equal(header()$sample_c, c(300.1, 304.5) - 273.15)
    do.call(session$setInputs, window_inputs(base(), month_from = "5", month_to = "6", limit_dates = FALSE,
                                             native_grid = FALSE, cell_size = 1))
    expect_identical(plan()$config$input$units, "K")
    expect_identical(plan()$config$input$format, "daily_tmax")
    session$setInputs(answer_reset = 1)
    expect_identical(question()$id, "units")
    # Choosing other data forgets the answer given for this file.
    session$setInputs(answer = "K", answer_apply = 3)
    expect_null(question())
    session$setInputs(use_example = 1)
    expect_length(state$answers, 0L)
    expect_null(question())
    session$setInputs(change_data = 1)
    expect_null(state$source)
  })
})

test_that("a guided run reports all four types and keeps every run in its own folder", {
  local_figure_test_dpi()
  window <- example_window()
  folder <- withr::local_tempdir()
  prepared <- file.path(folder, "prepared.nc")
  write_tmax(tmax_from_array(window$celsius, window$dates, window$lon, window$lat, months = 6:7), prepared,
             precision = "float64")
  base <- read_config(list(power_law = list(bootstrap_repetitions = 10)), input_required = FALSE)
  settings <- guided_settings(base, window_inputs(base, month_from = "6", month_to = "7", native_grid = FALSE, cell_size = 1),
                              prepared, detect_input(prepared), file.path(folder, "results", "SCORCH_run_001"))
  parent <- file.path(folder, "results")
  source <- open_temperature_source(prepared)
  first <- suppressWarnings(suppressMessages(execute_guided_run(settings, parent, source)))
  expect_null(first$problem)
  expect_identical(first$folder, file.path(parent, "SCORCH_run_001"))
  expect_true(all(file.exists(file.path(first$folder, c("Summary.txt", "config.yaml", "Heatwave_Statistics.xlsx",
    file.path("catalogs", "05_compound_heatwave_events.xlsx"), file.path("modeling", "Power_Law_Model.xlsx"))))))
  expect_gt(length(list.files(file.path(first$folder, "figures"), pattern = "[.]png$", recursive = TRUE)), 0L)
  expect_length(first$cell_tmax, nrow(first$result$clusters$cluster_cells))
  summary <- readLines(first$summary)
  expect_true(any(grepl("^  Results folder: ", summary)))
  expect_identical(yaml::read_yaml(file.path(first$folder, "config.yaml"))$period$warm_season_months, 6:7)

  page <- as.character(completion_ui(c(first, list(dataset = "prepared.nc", period = settings$period))))
  expect_match(page, "Analysis complete")
  for (type in 1:4) expect_match(page, sprintf(">Type %d<", type), fixed = TRUE)
  for (name in scoRch.simulator:::TYPE_NAMES) expect_match(page, name, fixed = TRUE)
  expect_match(page, first$folder, fixed = TRUE)
  expect_match(page, "June\u2013July 2012", fixed = TRUE)
  for (label in c("Open summary", "Open results folder", "Set up another run")) expect_match(page, label, fixed = TRUE)
  events <- first$result$events$events
  expect_match(page, sprintf("%d compound events from %d ellipses", nrow(events), nrow(first$result$ellipses)), fixed = TRUE)

  # The saved settings replay through the scripted command with the same result.
  replay <- yaml::read_yaml(file.path(first$folder, "config.yaml"))
  replay$output$directory <- file.path(folder, "scripted")
  scripted <- suppressWarnings(suppressMessages(run_scorch(replay)))
  for (stage in c("heatwaves", "regional", "clusters", "ellipses", "events", "power_law")) {
    expect_identical(scripted[[stage]], first$result[[stage]], info = stage)
  }

  second <- suppressWarnings(suppressMessages(execute_guided_run(settings, parent, source)))
  expect_identical(basename(second$folder), "SCORCH_run_002")
  expect_identical(readLines(first$summary), summary)

  # A run that cannot finish still leaves its own folder, summary and settings.
  settings$config$clustering$min_samples_values <- 1000
  stopped <- suppressWarnings(suppressMessages(execute_guided_run(settings, parent, source)))
  expect_match(stopped$problem, "No selected day formed a cluster")
  expect_identical(basename(stopped$folder), "SCORCH_run_003")
  expect_true(all(file.exists(file.path(stopped$folder, c("Summary.txt", "config.yaml")))))
  page <- as.character(completion_ui(c(stopped, list(dataset = "prepared.nc"))))
  expect_match(page, "Analysis stopped")
  expect_match(page, "Open summary", fixed = TRUE)
})
