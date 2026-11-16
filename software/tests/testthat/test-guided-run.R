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

# What a rendered form shows: the value of every field by its id, TRUE or
# FALSE for a checkbox, and the ticked values of a group of checkboxes.
shown_in <- function(ui) {
  html <- paste(as.character(ui), collapse = "\n")
  attribute <- function(tags, name) {
    found <- regmatches(tags, regexec(sprintf('[ ]%s="([^"]*)"', name), tags))
    vapply(found, function(hit) if (length(hit)) hit[2L] else NA_character_, "")
  }
  inputs <- regmatches(html, gregexpr("<input[^>]*>", html))[[1L]]
  id <- attribute(inputs, "id")
  group <- attribute(inputs, "name")
  box <- attribute(inputs, "type") %in% "checkbox"
  ticked <- grepl("checked", inputs, fixed = TRUE)
  shown <- stats::setNames(as.list(attribute(inputs, "value")[!box]), id[!box])
  shown[id[box & !is.na(id)]] <- as.list(ticked[box & !is.na(id)])
  for (name in unique(group[box & !is.na(group)])) shown[[name]] <- attribute(inputs[box & group %in% name & ticked], "value")
  for (select in regmatches(html, gregexpr("<select[^>]*>", html))[[1L]]) {
    from <- regexpr(select, html, fixed = TRUE)
    options <- sub("</select>.*$", "", substring(html, from))
    shown[[attribute(select, "id")]] <- regmatches(options, regexec('<option value="([^"]*)" selected', options))[[1L]][2L]
  }
  shown
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

  # A run is numbered after the highest one in the folder, so the name of a
  # run that was deleted is not given to a newer analysis.
  dir.create(file.path(root, "SCORCH_run_004"))
  unlink(second, recursive = TRUE)
  expect_identical(basename(next_run_folder(root)), "SCORCH_run_005")
  expect_false(dir.exists(file.path(root, "SCORCH_run_005")))
  # Names that only look like runs are left alone and do not stop the count.
  dir.create(file.path(root, "SCORCH_run_202611151230"))
  dir.create(file.path(root, "SCORCH_run_4 copy"))
  expect_identical(basename(next_run_folder(root, reserve = TRUE)), "SCORCH_run_005")
  expect_identical(basename(next_run_folder(root)), "SCORCH_run_006")
  # A folder given with its closing slash, as a drive root is, does not get two.
  slashed <- normalizePath(root, winslash = "/")
  expect_identical(next_run_folder(paste0(slashed, "/")), paste0(slashed, "/SCORCH_run_006"))
  expect_identical(next_run_folder(file.path(slashed, "no such folder")),
                   file.path(slashed, "no such folder", "SCORCH_run_001"))
})

test_that("a drive letter typed without a slash is the top of that drive", {
  # "D:" in the file dialog names the drive, as "D:/" does, not a folder "D:" inside the one that is open.
  for (drive in c("C:", "d:", "Q:")) {
    expect_identical(guided_path(drive, "elsewhere"), guided_path(paste0(drive, "/"), "elsewhere"), info = drive)
    expect_false(grepl("elsewhere", guided_path(drive, "elsewhere"), fixed = TRUE), info = drive)
  }
  expect_identical(guided_path(c("C:", "notes"), "elsewhere"),
                   c(guided_path("C:/", "elsewhere"), guided_path("notes", "elsewhere")))
  # A name that only begins like a drive is still looked for in that folder.
  expect_match(guided_path("C:data", "elsewhere"), "elsewhere", fixed = TRUE)
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
  # A place on a server is listed without being asked for, so it is not asked for here either.
  expect_true(all(dir.exists(places[!on_server(places)])))
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
    session$setInputs(change_data = 1)
    expect_null(state$source)
    expect_length(state$answers, 0L)
    session$setInputs(use_example = 1)
    expect_null(state$path)
    expect_null(question())
    session$setInputs(change_data = 2)
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
  # Untouched settings are saved as a scripted run saves them: whole numbers stay whole ("- 4", not "- 4.0").
  expect_identical(settings$config$clustering, base$clustering)
  for (section in c("heatwaves", "regional_days", "ellipses", "power_law", "lgcp")) {
    expect_identical(settings$config[[section]], base[[section]], info = section)
  }
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
  # The summary and the saved settings list the neighborhood sizes as the reference output does.
  for (lines in list(summary, readLines(file.path(first$folder, "config.yaml")))) {
    expect_identical(trimws(lines[grep("min_samples_values:", lines, fixed = TRUE) + 1:9]), paste("-", 4:12))
  }

  page <- as.character(completion_ui(c(first, list(dataset = "prepared.nc", period = settings$period))))
  expect_match(page, "Analysis complete")
  for (type in 1:4) expect_match(page, sprintf(">Type %d<", type), fixed = TRUE)
  for (name in scoRch.simulator:::TYPE_NAMES) expect_match(page, name, fixed = TRUE)
  expect_match(page, first$folder, fixed = TRUE)
  expect_match(page, "June\u2013July 2012", fixed = TRUE)
  for (label in c("Open summary", "Open results folder", "Set up another run")) expect_match(page, label, fixed = TRUE)
  events <- first$result$events$events
  expect_match(page, sprintf("%d compound events from %d ellipses", nrow(events), nrow(first$result$ellipses)), fixed = TRUE)
  # A count of one is named in the singular.
  lone <- first
  lone$result$events$events <- events[1L, , drop = FALSE]
  lone$result$ellipses <- first$result$ellipses[1L, , drop = FALSE]
  lone$result$regional$selected_days <- first$result$regional$selected_days[1L, , drop = FALSE]
  expect_match(as.character(completion_ui(c(lone, list(dataset = "prepared.nc")))),
               ">1 compound event from 1 ellipse on 1 regionally extensive day.<", fixed = TRUE)
  expect_identical(c(count_noun(0L, "ellipse"), count_noun(1L, "ellipse"), count_noun(2100L, "ellipse")),
                   c("0 ellipses", "1 ellipse", "2,100 ellipses"))
  # A variable that the file did not identify is named under "What was analyzed"; one that it did identify is not.
  expect_false(grepl("<dt>Temperature</dt>", page, fixed = TRUE))
  chosen <- "Variable tasmin, as you chose. The file does not identify it as daily maximum temperature."
  noted <- as.character(completion_ui(c(first, list(dataset = "prepared.nc", period = settings$period, temperature = chosen))))
  expect_match(gsub("[[:space:]]+", " ", noted),
               sprintf("<dt>Dataset</dt> <dd>prepared.nc</dd> <dt>Temperature</dt> <dd>%s</dd>", chosen), fixed = TRUE)

  # The saved settings replay through the scripted command (compared at the end of this test).
  replay <- yaml::read_yaml(file.path(first$folder, "config.yaml"))
  replay$output$directory <- file.path(folder, "scripted")
  scripted <- suppressWarnings(suppressMessages(run_scorch(replay)))

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
  # The heading of the results can take the keyboard focus when it replaces the form.
  expect_match(page, '<h1 id="results-title" tabindex="-1">Analysis stopped</h1>', fixed = TRUE)

  # The scripted replay of the saved settings gives the same result, bit for bit.
  skip_stage_identity_from_source()
  for (stage in c("heatwaves", "regional", "clusters", "ellipses", "events", "power_law")) {
    expect_identical(scripted[[stage]], first$result[[stage]], info = stage)
  }
})

test_that("a run that stops speaks of an earlier run only where one left its summary", {
  folder <- withr::local_tempdir()
  nc <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  base <- read_config(input_required = FALSE)
  # June 2012 of the included example has no regionally extensive day: the run stops before it publishes.
  form <- window_inputs(base, month_from = "6", month_to = "6", limit_dates = TRUE,
                        dates = as.Date(c("2012-01-01", "2012-12-31")), native_grid = FALSE, cell_size = 1)
  parent <- file.path(folder, "results")
  settings <- guided_settings(base, form, NULL, detect_input(nc), file.path(parent, "SCORCH_run_001"))
  source <- open_temperature_source(nc)
  withr::defer(close_temperature_source(source))
  heard <- function(code) {
    said <- character()
    value <- withCallingHandlers(suppressWarnings(code), message = function(m) {
      said <<- c(said, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    list(value = value, said = said)
  }
  # The folder was made for this very run and holds what the run wrote: nothing in it is from an earlier one.
  run <- heard(execute_guided_run(settings, parent, source))
  expect_match(run$value$problem, "regional threshold is zero")
  expect_setequal(list.files(run$value$folder), c("config.yaml", "Summary.txt"))
  expect_true(any(grepl("Stage 2", run$said, fixed = TRUE)))
  expect_false(any(grepl("earlier run", run$said, fixed = TRUE)))
  # The scripted command sent to the same folder finds the summary of that run there, and says whose the files are.
  again <- heard(tryCatch(run_scorch(file.path(run$value$folder, "config.yaml")), error = conditionMessage))
  expect_match(again$value, "regional threshold is zero")
  expect_true(any(grepl("did not publish output; files in .* belong to an earlier run", again$said)))
  # A folder that exists and holds no summary has no earlier run in it.
  replay <- yaml::read_yaml(file.path(run$value$folder, "config.yaml"))
  replay$output$directory <- file.path(folder, "empty")
  dir.create(replay$output$directory)
  yaml::write_yaml(replay, file.path(folder, "empty.yaml"), precision = 17)
  fresh <- heard(tryCatch(run_scorch(file.path(folder, "empty.yaml")), error = conditionMessage))
  expect_match(fresh$value, "regional threshold is zero")
  expect_false(any(grepl("earlier run", fresh$said, fixed = TRUE)))
})

test_that("Run analysis on the included example completes and its saved settings replay", {
  local_figure_test_dpi()
  # R's temporary folder can be a Windows short name ("RUNNER~1"), which a
  # path keeps only until its folder exists. Start from the long name.
  project <- normalizePath(withr::local_tempdir(), winslash = "/")
  withr::local_dir(project)
  seen <- new.env()
  # The sessions of one window share `kept`, as the launcher arranges it.
  kept <- new.env()
  window <- scorch_server
  formals(window)$kept <- kept
  shiny::testServer(window, {
    session$setInputs(use_example = 1)
    quick <- base()
    quick$power_law$bootstrap_repetitions <- 10
    do.call(session$setInputs, window_inputs(quick, month_from = "6", month_to = "8", limit_dates = TRUE,
      dates = as.Date(c("2012-01-01", "2012-12-31")), native_grid = FALSE, cell_size = 1))
    expect_null(plan()$config$input$file)
    # A results folder that cannot be created: nothing starts, and the Review card says so beside the button.
    writeLines("a file where the results folder would be", "output")
    session$setInputs(start = 1)
    expect_null(state$finished)
    expect_match(output$review$html, "Could not create the results folder", fixed = TRUE)
    # The start that works takes the message away.
    file.remove("output")
    suppressWarnings(suppressMessages(session$setInputs(start = 2)))
    expect_null(state$error)
    expect_null(state$failed)
    seen$done <- state$finished
  })
  done <- seen$done
  # The file identifies its variable as daily maximum temperature, so the results add nothing about it.
  expect_null(done$temperature)
  # A reload of the window opens a new session, which starts from the finished run: its summary and its map.
  reloaded <- shiny::MockShinySession$new()
  sent <- character()
  reloaded$sendCustomMessage <- function(type, message) sent <<- c(sent, type)
  shiny::testServer(window, {
    session$flushReact()
    expect_null(state$source)
    expect_identical(state$finished$folder, done$folder)
    expect_match(output$completion$html, "Analysis complete", fixed = TRUE)
    expect_match(output$completion$html, done$folder, fixed = TRUE)
    expect_true("scorch-events" %in% sent)
    # "Set up another run" forgets it. No data are open in this session, so the opening screen follows.
    session$setInputs(again = 1)
    expect_null(state$finished)
    expect_null(kept$finished)
  }, session = reloaded)
  expect_null(done$problem)
  expect_identical(done$dataset, "North America example")
  expect_identical(done$result$input$mode, "Bundled North America example")
  expect_identical(normalizePath(done$folder, winslash = "/"), file.path(guided_path("output"), "SCORCH_run_001"))
  expect_match(as.character(completion_ui(done)), "Analysis complete")
  # The saved settings name no file, so they replay on any computer.
  saved <- yaml::read_yaml(file.path(done$folder, "config.yaml"))
  expect_null(saved$input$file)
  expect_identical(saved$clustering$min_samples_values, 4:12)
  saved$output$directory <- file.path(project, "scripted")
  yaml::write_yaml(saved, "replay.yaml", precision = 17)
  scripted <- suppressWarnings(suppressMessages(run_scorch("replay.yaml")))
  expect_identical(scripted$input$mode, "Bundled North America example")
  skip_stage_identity_from_source()
  for (stage in c("heatwaves", "regional", "clusters", "ellipses", "events", "power_law")) {
    expect_identical(scripted[[stage]], done$result[[stage]], info = stage)
  }
})

test_that("every setting the window shows starts at the value of its configuration", {
  study <- read_config(input_required = FALSE)
  other <- read_config(list(
    input = list(format = "era5", variable = "t2m_alt", longitude = "x", latitude = "y", time = "t", units = "K",
                 era5_cell_size_deg = 0.5, processed_file = "kept.nc"),
    period = list(warm_season_months = c(11, 12, 1, 2), start_date = "2001-01-01"),
    heatwaves = list(threshold_percentile = 90.5, min_duration_days = 4, min_exceeding_days = 2),
    regional_days = list(percentile = 80), clustering = list(eps_values = c(1, 2.5), min_samples_values = c(3, 6)),
    ellipses = list(scale_factor = 1.5),
    power_law = list(bootstrap_repetitions = 12, seed_daily_maxima = 7, seed_event_maxima = 8),
    lgcp = list(covariates = c("lat", "std_tmax"), raster_km = 40, window_margin_km = 10)), input_required = FALSE)
  native <- read_config(list(input = list(era5_cell_size_deg = NULL)), input_required = FALSE)
  expect_null(native$input$era5_cell_size_deg)
  for (cfg in list(study, other, native)) {
    shown <- shown_in(settings_ui(cfg))
    # Every analysis setting has a field, and the field starts at the configured value.
    for (section in c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp")) {
      for (key in names(cfg[[section]])) {
        id <- paste(section, key, sep = "_")
        expect_true(id %in% names(shown), info = id)
        if (!id %in% names(shown)) next
        field <- shown[[id]]
        if (is.numeric(cfg[[section]][[key]])) field <- as.numeric(strsplit(field, "[,[:space:]]+")[[1L]])
        expect_equal(field, cfg[[section]][[key]], tolerance = 0, info = id)
      }
    }
    expect_identical(c(shown$month_from, shown$month_to), as.character(season_limits(cfg$period$warm_season_months)))
    expect_identical(shown$limit_dates, !is.null(cfg$period$start_date) || !is.null(cfg$period$end_date))
    for (key in c("format", "units", "variable", "time", "longitude", "latitude")) {
      expect_identical(shown[[paste0("input_", key)]], cfg$input[[key]], info = key)
    }
    expect_identical(shown$native_grid, is.null(cfg$input$era5_cell_size_deg))
    # A configuration that keeps the input grid has no cell size: its hidden field holds the study's.
    size <- if (is.null(cfg$input$era5_cell_size_deg)) study$input$era5_cell_size_deg else cfg$input$era5_cell_size_deg
    expect_equal(as.numeric(shown$cell_size), size, tolerance = 0)
    expect_identical(shown$save_tmax, !is.null(cfg$input$processed_file))
  }
})

test_that("a refused setting is named as the window labels it, and marked when it is an advanced one", {
  nc <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
  info <- detect_input(nc)
  base <- read_config(input_required = FALSE)
  html <- paste(as.character(settings_ui(base)), collapse = "\n")
  label_of <- function(id) {
    regmatches(html, regexec(sprintf('<label[^>]*for="%s"[^>]*>([^<]*)</label>', id), html))[[1L]][2L]
  }
  filled <- window_inputs(base, month_from = "6", month_to = "8", native_grid = FALSE, cell_size = 1)
  outcome <- function(changed = list(), given = info) {
    form <- filled
    for (id in names(changed)) form[id] <- list(changed[[id]])
    tryCatch(guided_settings(base, form, nc, given, "run"), error = identity)
  }
  expect_false(inherits(outcome(), "error"))
  # The message carries the label on screen and no name from the YAML file.
  refused <- function(problem, id) {
    expect_s3_class(problem, "advanced_setting")
    said <- conditionMessage(problem)
    expect_match(said, sprintf("'%s'", label_of(id)), fixed = TRUE)
    expect_false(any(vapply(names(SETTING_LABELS), grepl, logical(1), x = said, fixed = TRUE)), info = said)
    said
  }
  numbers <- c("heatwaves_threshold_percentile", "regional_days_percentile", "heatwaves_min_duration_days",
               "heatwaves_min_exceeding_days", "ellipses_scale_factor", "power_law_bootstrap_repetitions",
               "power_law_seed_daily_maxima", "power_law_seed_event_maxima", "lgcp_raster_km", "lgcp_window_margin_km")
  for (id in numbers) {
    # An emptied number field reaches R as a missing value.
    said <- refused(outcome(stats::setNames(list(NA), id)), id)
    expect_match(said, "(it is empty)", fixed = TRUE)
    expect_false(grepl("NA", said, fixed = TRUE), info = said)
  }
  # The two fields of the ERA5 grid are on screen for raw data, and are read for raw data only.
  raw <- list(format = "era5", file = "grid.nc", lon = seq(20, 23, by = 0.25), lat = seq(10, 13, by = 0.25),
              dates = info$dates, per_day = 24L, variable = "t2m", units = "K")
  expect_false(inherits(outcome(given = raw), "error"))
  expect_match(refused(outcome(list(cell_size = NA), given = raw), "cell_size"), "(it is empty)", fixed = TRUE)
  expect_match(refused(outcome(list(cell_size = 0), given = raw), "cell_size"), "(it is '0')", fixed = TRUE)
  expect_null(outcome(list(native_grid = TRUE, cell_size = NA), given = raw)$config$input$era5_cell_size_deg)
  expect_identical(outcome(list(cell_size = 0.5), given = raw)$config$input$era5_cell_size_deg, 0.5)
  # What raw data left in them cannot stop prepared data or the included example, where nothing on
  # screen could repair it. The saved settings keep the grid size of the configuration.
  for (left in list(list(cell_size = NA), list(cell_size = 0), list(cell_size = 0.3), list(native_grid = TRUE))) {
    kept <- outcome(left)
    expect_false(inherits(kept, "error"), info = paste(names(left), left))
    expect_identical(kept$config$input$era5_cell_size_deg, base$input$era5_cell_size_deg)
  }
  expect_match(refused(outcome(list(heatwaves_threshold_percentile = 150)), "heatwaves_threshold_percentile"),
               "between 0 and 100 (it is '150')", fixed = TRUE)
  expect_match(refused(outcome(list(power_law_bootstrap_repetitions = 0.5)), "power_law_bootstrap_repetitions"), "whole number")
  expect_match(refused(outcome(list(ellipses_scale_factor = 0)), "ellipses_scale_factor"), "must be positive")
  for (id in c("clustering_eps_values", "clustering_min_samples_values")) {
    for (text in c("", "1, two, 3", "2.5, -1")) {
      expect_match(refused(outcome(stats::setNames(list(text), id)), id), "must list distinct")
    }
  }
  expect_identical(refused(outcome(list(lgcp_covariates = NULL)), "lgcp_covariates"), "Tick at least one of the 'Predictors'.")
  same <- refused(outcome(list(input_longitude = "lat", input_latitude = "lat")), "input_longitude")
  expect_match(same, sprintf("'%s' and '%s'", label_of("input_latitude"), label_of("input_time")), fixed = TRUE)

  # A problem with the months or dates is worded for the window too, but is not an advanced setting.
  order <- outcome(list(limit_dates = TRUE, dates = as.Date(c("2012-08-01", "2011-08-01"))))
  expect_identical(conditionMessage(order), "The first of the 'Analysis dates' must be on or before the last.")
  expect_false(inherits(order, "advanced_setting"))
  nothing <- outcome(list(limit_dates = TRUE, dates = as.Date(c("1980-01-01", "1980-12-31"))))
  expect_match(conditionMessage(nothing), "no observations")
  expect_false(inherits(nothing, "advanced_setting"))

  # Every labelled field of the Advanced section is known by its label.
  advanced <- substring(html, regexpr("<details", html, fixed = TRUE))
  labels <- regmatches(advanced, gregexpr('<label[^>]*for="[^"]*"[^>]*>[^<]*</label>', advanced))[[1L]]
  labels <- sub("</label>$", "", sub("^<label[^>]*>", "", labels))
  expect_gte(length(labels), 20L)
  expect_identical(setdiff(labels, SETTING_LABELS), character())
  expect_match(advanced, sprintf("<span>%s</span>", SETTING_LABELS[["input: processed_file"]]), fixed = TRUE)
})

test_that("raw input that cannot be averaged to the chosen cells stops before Run, in the reader's words", {
  base <- read_config(input_required = FALSE)
  days <- seq(as.Date("2001-07-01"), by = "day", length.out = 10L)
  raw <- function(lon, lat) {
    list(format = "era5", file = file.path("some folder", "grid.nc"), lon = lon, lat = lat, dates = days,
         per_day = 24L, variable = "t2m", units = "K")
  }
  grids <- list(quarter = raw(seq(20, 23, by = 0.25), seq(10, 13, by = 0.25)),
                whole = raw(20:24, 10:14),
                third = raw(seq(20, 23, by = 0.3), seq(10, 13, by = 0.25)),
                latitude = raw(seq(20, 23, by = 0.25), seq(13, 10, by = -0.3)),
                coarse = raw(seq(20, 30, by = 2.5), seq(10, 20, by = 2.5)),
                small = raw(c(20.25, 20.5, 20.75), seq(10, 13, by = 0.25)),
                wrapped = raw(seq(250, 253, by = 0.25), seq(13, 10, by = -0.25)),
                # Quarter-degree points that fill one row, one column, and two by two one-degree cells.
                row = raw(seq(20, 21.75, by = 0.25), seq(10, 10.75, by = 0.25)),
                column = raw(seq(20, 20.75, by = 0.25), seq(10, 11.75, by = 0.25)),
                square = raw(seq(20, 21.75, by = 0.25), seq(10, 11.75, by = 0.25)),
                # One column of cells, and latitudes that cannot be averaged at all.
                mixed = raw(seq(20, 20.75, by = 0.25), seq(10, 13, by = 0.3)))
  refusals <- 0L
  lone <- 0L
  for (name in names(grids)) {
    for (size in c(1, 0.5, 0.75, 1.5)) {
      info <- grids[[name]]
      lon <- wrap_longitudes(info$lon)$lon
      lat <- sort(info$lat)
      # The reader averages the points to cells and then measures the spacing of those cells.
      reader <- tryCatch({
        cells <- average_to_cells(array(0, c(1L, length(lat), length(lon))), lon, lat, size, "grid.nc")
        tmax_from_array(array(20, c(2L, length(cells$lat), length(cells$lon))), days[1:2], cells$lon, cells$lat)
      }, error = identity)
      window <- tryCatch(check_cell_size(info, size), error = identity)
      # The window stops exactly where the reader would, and says the same with the label of the field.
      expect_identical(inherits(window, "error"), inherits(reader, "error"), info = paste(name, size))
      if (!inherits(reader, "error") || !inherits(window, "error")) next
      refusals <- refusals + 1L
      expect_s3_class(window, "advanced_setting")
      said <- sub("'Output grid size (degrees)'", "era5_cell_size_deg", conditionMessage(window), fixed = TRUE)
      # One cell alone along an axis is made by the reader and refused later, in words about
      # coordinate values. The window names the setting and the two ways out instead.
      axis <- regmatches(conditionMessage(reader), regexec("^At least two (longitude|latitude) values are needed",
                                                           conditionMessage(reader)))[[1L]][2L]
      if (is.na(axis)) {
        expect_identical(said, conditionMessage(reader), info = paste(name, size))
      } else {
        lone <- lone + 1L
        expect_identical(said, sprintf(paste("The ERA5 grid in grid.nc gives fewer than two %g-degree cells along its %s.",
                                             "Choose a smaller era5_cell_size_deg, or tick 'Keep the input grid resolution'."), size, axis),
                         info = paste(name, size))
      }
    }
  }
  expect_gte(refusals, 8L)
  expect_gte(lone, 6L)
  form <- window_inputs(base, month_from = "7", month_to = "7", native_grid = FALSE, cell_size = 1)
  # A region one cell tall is refused before Run, not after it; smaller cells or its own grid are accepted.
  flat <- tryCatch(guided_settings(base, form, "grid.nc", grids$row, "run"), error = identity)
  expect_s3_class(flat, "advanced_setting")
  expect_identical(conditionMessage(flat),
                   paste("The ERA5 grid in grid.nc gives fewer than two 1-degree cells along its latitude.",
                         "Choose a smaller 'Output grid size (degrees)', or tick 'Keep the input grid resolution'."))
  expect_length(guided_settings(base, form, "grid.nc", grids$square, "run")$dates, 10L)
  for (way in list(list(cell_size = 0.5), list(native_grid = TRUE))) {
    other <- form
    other[names(way)] <- way
    expect_length(guided_settings(base, other, "grid.nc", grids$row, "run")$dates, 10L)
  }
  stopped <- tryCatch(guided_settings(base, form, "grid.nc", grids$third, "run"), error = identity)
  expect_s3_class(stopped, "advanced_setting")
  expect_identical(conditionMessage(stopped),
                   "The ERA5 longitude spacing (0.3 degrees) in grid.nc does not divide 'Output grid size (degrees)' (1).")
  expect_length(guided_settings(base, form, "grid.nc", grids$quarter, "run")$dates, 10L)
  # Keeping the input grid averages nothing, so any spacing is accepted.
  form$native_grid <- TRUE
  expect_null(guided_settings(base, form, "grid.nc", grids$third, "run")$config$input$era5_cell_size_deg)
})

test_that("the dataset card describes the grid and the temperature that the run will use", {
  days <- seq(as.Date("2001-07-01"), by = "day", length.out = 10L)
  facts <- function(changed = list(), cell_size = 1) {
    info <- list(format = "era5", variable = "t2m", units = "K", per_day = 24L, dates = days, lon = 20:24, lat = 10:14,
                 sample_c = c(20, 30))
    info[names(changed)] <- changed
    dataset_facts(info, cell_size)
  }
  # Points one cell apart are not averaged: each becomes the cell that holds it, as the reader labels it.
  cells <- average_to_cells(array(0, c(1L, 5L, 5L)), 20:24, 10:14, 1, "grid.nc")
  expect_identical(c(range(cells$lon), range(cells$lat)), c(20.5, 24.5, 10.5, 14.5))
  whole <- facts()[["Preparation"]]
  expect_identical(whole, paste("SCORCH takes the maximum of each UTC day. Each point becomes one 1\u00b0 cell,",
                                "labelled by its centre: 20.5\u00b0E to 24.5\u00b0E and 10.5\u00b0N to 14.5\u00b0N."))
  quarter <- facts(list(lon = seq(20, 23, by = 0.25), lat = seq(10, 13, by = 0.25)))
  expect_identical(quarter[["Preparation"]],
                   "SCORCH takes the maximum of each UTC day and averages complete blocks to 1\u00b0 cells.")
  expect_match(facts(list(lon = seq(20, 23, by = 0.3)))[["Preparation"]], "This grid cannot be averaged to 1\u00b0 cells.",
               fixed = TRUE)
  expect_identical(facts(cell_size = NULL)[["Preparation"]], "SCORCH takes the maximum of each UTC day and keeps this grid.")
  expect_identical(facts(cell_size = NA)[["Preparation"]], "SCORCH takes the maximum of each UTC day.")
  # One value per day is used as that day's maximum, whichever daily statistic the file holds. The
  # card says so, in a sentence, with each way the cells are made.
  daily <- function(changed = list(), ...) facts(c(list(per_day = 1L), changed), ...)[["Preparation"]]
  begins <- "SCORCH uses each daily value as that day's maximum"
  expect_identical(daily(), paste0(begins, sub("SCORCH takes the maximum of each UTC day", "", whole, fixed = TRUE)))
  expect_identical(daily(list(lon = seq(20, 23, by = 0.25), lat = seq(10, 13, by = 0.25))),
                   paste(begins, "and averages complete blocks to 1\u00b0 cells."))
  expect_identical(daily(list(lon = seq(20, 23, by = 0.3))),
                   paste0(begins, ". This grid cannot be averaged to 1\u00b0 cells."))
  expect_identical(daily(cell_size = NULL), paste(begins, "and keeps this grid."))
  expect_identical(daily(cell_size = NA), paste0(begins, "."))
  expect_identical(daily(list(variable = "tasmin", identified = FALSE), cell_size = NULL),
                   paste(begins, "and keeps this grid."))

  # Longitudes from 0 to 360: the reader of raw input moves them to -180..180, a prepared file keeps its own.
  expect_match(facts(list(lon = 250:254))[["Grid"]], "110\u00b0W to 106\u00b0W", fixed = TRUE)
  prepared <- facts(list(format = "daily_tmax", variable = "tmax", units = "degC", per_day = 1L, lon = 250.5:254.5))
  expect_match(prepared[["Grid"]], "5 by 5 cells at 1\u00b0, 250.5\u00b0E to 254.5\u00b0E", fixed = TRUE)
  expect_false("Preparation" %in% names(prepared))

  # Coordinates stored in single precision are named as the grid they describe, in the range and in
  # the spacing, without the noise that begins at their eighth digit.
  single <- function(values) readBin(writeBin(values, raw(), size = 4L), "numeric", n = length(values), size = 4L)
  expect_identical(format(single(22.9), digits = 15), "22.8999996185303")
  expect_identical(facts(list(lon = single(seq(20, 22.9, by = 0.1)), lat = single(seq(10, 12.9, by = 0.1))))[["Grid"]],
                   "30 by 30 points at 0.1\u00b0, 20\u00b0E to 22.9\u00b0E and 10\u00b0N to 12.9\u00b0N")
  grid_of <- function(lon, lat) {
    facts(list(format = "daily_tmax", variable = "tmax", units = "degC", per_day = 1L, lon = lon, lat = lat))[["Grid"]]
  }
  expect_identical(grid_of(single(seq(-70.35, -69.45, by = 0.1)), single(seq(-33.25, -32.35, by = 0.1))),
                   "10 by 10 cells at 0.1\u00b0, 70.35\u00b0W to 69.45\u00b0W and 33.25\u00b0S to 32.35\u00b0S")
  # Nor does moving raw longitudes from 0-360 leave the trace of a subtraction (9.89999999999998).
  expect_identical(facts(list(lon = seq(350.1, 350.9, by = 0.1), lat = seq(10, 10.4, by = 0.1)))[["Grid"]],
                   "9 by 5 points at 0.1\u00b0, 9.9\u00b0W to 9.1\u00b0W and 10\u00b0N to 10.4\u00b0N")
  # Both spacings are named when longitudes and latitudes are not equally far apart.
  expect_identical(grid_of(seq(20.25, 23.75, by = 0.5), 10:13 + 0.5),
                   "8 by 4 cells at 0.5\u00b0 x 1\u00b0, 20.25\u00b0E to 23.75\u00b0E and 10.5\u00b0N to 13.5\u00b0N")
  expect_match(facts(list(lon = seq(20, 24, by = 0.25), lat = seq(10, 14, by = 0.5)))[["Grid"]],
               "17 by 9 points at 0.25\u00b0 x 0.5\u00b0, 20\u00b0E to 24\u00b0E", fixed = TRUE)
  # A fine grid whose coordinates need more than seven digits keeps its one spacing: rounding them
  # for the card makes it neither uneven nor two-sized.
  expect_identical(grid_of(-125 + (1:12) / 24 - 1 / 48, 40 + (1:12) / 24 - 1 / 48),
                   paste("12 by 12 cells at 0.04166667\u00b0, 124.9792\u00b0W to 124.5208\u00b0W",
                         "and 40.02083\u00b0N to 40.47917\u00b0N"))
  expect_match(grid_of(250 + (0:9) / 32, 30 + (0:9) / 32), "10 by 10 cells at 0.03125\u00b0, 250\u00b0E to 250.28",
               fixed = TRUE)

  # A variable that was accepted without being identified is not called the daily maximum.
  expect_identical(prepared[["Contains"]], "Prepared daily maximum temperature (variable tmax)")
  expect_identical(facts()[["Contains"]], "Raw 2 m temperature, 24 values per day (variable t2m)")
  expect_identical(facts(list(identified = TRUE))[["Contains"]], facts()[["Contains"]])
  daily <- facts(list(format = "daily_tmax", variable = "tasmin", per_day = 1L, identified = FALSE))[["Contains"]]
  expect_identical(daily, "Daily temperature (variable tasmin, not identified as daily maximum temperature)")
  hourly <- facts(list(variable = "d2m", identified = FALSE))[["Contains"]]
  expect_identical(hourly, paste("Sub-daily temperature, 24 values per day",
                                 "(variable d2m, not identified as daily maximum temperature)"))
  several <- facts(list(per_day = 1L, identified = FALSE, files = 3L))[["Contains"]]
  expect_identical(several, "Daily temperature (variable t2m, not identified as daily maximum temperature), in 3 NetCDF files")
  for (line in c(daily, hourly, several)) {
    expect_false(grepl("2 m|maximum", sub("not identified as daily maximum temperature", "", line, fixed = TRUE)), info = line)
  }

  # The results of a run name such a variable, and add nothing about one that the file identifies.
  noted <- function(...) temperature_note(list(...))
  expect_null(noted(format = "daily_tmax", variable = "tmax", per_day = 1L, identified = TRUE))
  expect_null(noted(format = "era5", variable = "t2m", per_day = 24L))
  expect_identical(noted(format = "daily_tmax", variable = "tasmin", per_day = 1L, identified = FALSE),
                   "Variable tasmin, as you chose. The file does not identify it as daily maximum temperature.")
  expect_identical(noted(format = "era5", variable = "tasmin", per_day = 1L, identified = FALSE),
                   "Variable tasmin, as you chose. The file does not identify it as daily maximum temperature.")
  expect_identical(noted(format = "era5", variable = "d2m", per_day = 24L, identified = FALSE),
                   paste("Variable d2m, as you chose. The file does not identify it as 2 m air temperature;",
                         "its highest value of each day was used."))
})

test_that("the address of a window holds a secret that leaves the session's random numbers alone", {
  withr::local_preserve_seed()
  first <- launch_token()
  expect_match(first, "^[A-Za-z0-9]{32}$")
  expect_false(identical(launch_token(), first))
  # A seed set by the user neither fixes the token nor is moved on by it.
  symbols <- c(letters, LETTERS, 0:9)
  set.seed(42)
  from_seed <- paste(symbols[sample.int(length(symbols), 32L, replace = TRUE)], collapse = "")
  set.seed(42)
  expected <- stats::runif(3)
  set.seed(42)
  token <- launch_token()
  expect_identical(stats::runif(3), expected)
  expect_false(identical(token, from_seed))
  # Two tokens drawn after the same seed share no more characters than chance gives.
  set.seed(42)
  again <- launch_token()
  expect_lt(sum(strsplit(token, "")[[1L]] == strsplit(again, "")[[1L]]), 8L)
  # A session without a seed is left without one.
  rm(".Random.seed", envir = globalenv())
  expect_match(launch_token(), "^[A-Za-z0-9]{32}$")
  expect_false(exists(".Random.seed", envir = globalenv()))
})

test_that("text cut in the middle of a character in a name field is a wrong name, not the end of the session", {
  # "tmax" followed by half of a four-byte character, as text cut inside an emoji is.
  broken <- rawToChar(as.raw(c(0x74, 0x6d, 0x61, 0x78, 0xf0, 0x9f)))
  Encoding(broken) <- "UTF-8"
  base <- read_config(input_required = FALSE)$input
  controls <- input_controls(list(input_variable = broken), base)
  expect_true(validEnc(controls$variable))
  expect_match(controls$variable, "^tmax")
  expect_false(identical(controls$variable, "tmax"))
  expect_identical(input_controls(list(input_variable = "  tasmax "), base)$variable, "tasmax")
})

test_that("only a client that holds the token of the window gets its page or a session", {
  page <- scorch_page("the window", "s3cret")
  expect_identical(page(list(QUERY_STRING = "?token=s3cret")), "the window")
  expect_identical(page(list(QUERY_STRING = "?from=bookmark&token=s3cret")), "the window")
  for (query in list(NULL, "", "?", "?token=", "?token=S3CRET", "?token=s3cret1", "?tokenx=s3cret", "?t=s3cret")) {
    refused <- page(list(QUERY_STRING = query))
    expect_s3_class(refused, "httpResponse")
    expect_identical(refused$status, 403L)
    expect_match(refused$content_type, "text/plain", fixed = TRUE)
    expect_match(refused$content, "belongs to an R session", fixed = TRUE)
    expect_match(refused$content, "scoRch.simulator::run_scorch()", fixed = TRUE)
  }

  project <- withr::local_tempdir()
  withr::local_dir(project)
  quits <- 0L
  app <- scorch_app("s3cret", new.env(), quit = function() quits <<- quits + 1L, grace = 0.1)
  holder <- function() {
    session <- shiny::MockShinySession$new()
    session$clientData <- list(url_search = "?token=s3cret")
    session
  }
  settle <- function() {
    Sys.sleep(0.3)
    later::run_now()
  }
  # A stranger (the test session's own query has no token) gets no session: nothing
  # is set up for it, and nothing stops the app when it has gone.
  shiny::testServer(app, {
    expect_true(session$isClosed())
    expect_error(output$destination)
  }, session = shiny::MockShinySession$new())
  settle()
  expect_identical(quits, 0L)
  # The window itself gets the real server.
  first_run <- file.path(guided_path("output"), "SCORCH_run_001")
  shiny::testServer(app, {
    expect_false(session$isClosed())
    expect_identical(output$destination, first_run)
  }, session = holder())
  # That session has ended. A reload opens the next one within the grace period, so the app stays.
  shiny::testServer(app, {
    settle()
    expect_identical(quits, 0L)
    expect_identical(output$destination, first_run)
  }, session = holder())
  # Now the last window has gone: the app is ended, once.
  settle()
  expect_identical(quits, 1L)
  settle()
  expect_identical(quits, 1L)
})

# A test session that keeps what the server sends to the page.
recording_session <- function() {
  session <- shiny::MockShinySession$new()
  seen <- new.env()
  seen$inputs <- list()
  seen$custom <- list()
  seen$modals <- 0L
  session$sendInputMessage <- function(inputId, message) {
    seen$inputs[[length(seen$inputs) + 1L]] <- c(list(id = inputId), message)
  }
  session$sendCustomMessage <- function(type, message) {
    seen$custom[[length(seen$custom) + 1L]] <- list(type = type, message = message)
  }
  session$sendModal <- function(type, message) seen$modals <- seen$modals + 1L
  list(session = session, seen = seen)
}

test_that("the months of the first dataset wait until the form can take them", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  winter <- seq(as.Date("2001-12-01"), as.Date("2002-02-28"), by = "day")
  file <- write_grid(file.path(folder, "winter.nc"), array(seq_len(length(winter) * 12) %% 9, c(length(winter), 3, 4)),
                     as.numeric(winter - as.Date("2000-01-01")), c(10, 11, 12, 13), c(40, 41, 42))
  wired <- recording_session()
  month_updates <- function() {
    sent <- Filter(function(m) m$id %in% c("month_from", "month_to"), wired$seen$inputs)
    vapply(sent, function(m) paste(m$id, m$value), "")
  }
  shiny::testServer(scorch_server, {
    choose("data", file)
    session$flushReact()
    expect_false(inherits(header(), "error"))
    # The month fields have not reported yet: nothing is sent to a page that could not take it.
    expect_true(state$fresh)
    expect_length(month_updates(), 0L)
    # The form reports the months it was built with. Now the season of the file is sent.
    session$setInputs(month_from = "4", month_to = "9")
    expect_false(state$fresh)
    expect_identical(month_updates(), c("month_from 12", "month_to 2"))
    # Later changes of the months by hand are left alone.
    session$setInputs(month_from = "1")
    expect_identical(month_updates(), c("month_from 12", "month_to 2"))
  }, session = wired$session)
})

test_that("a loaded configuration keeps its dates, and its note stays when the data change", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  # One file per year of daily maxima is first refused as several prepared files.
  dir.create("yearly")
  for (year in 2001:2003) {
    days <- seq(as.Date(sprintf("%d-01-01", year)), as.Date(sprintf("%d-12-31", year)), by = "day")
    write_grid(file.path("yearly", sprintf("tasmax_day_%d.nc", year)),
               list(tasmax = array(290 + seq_len(length(days) * 12) %% 17, c(length(days), 3, 4))),
               as.numeric(days - as.Date("2000-01-01")), c(10, 11, 12, 13), c(40, 41, 42), units = "K",
               attributes = list(tasmax = list(standard_name = "air_temperature", cell_methods = "time: maximum")))
  }
  dir.create("saved")
  yaml::write_yaml(list(input = list(file = file.path(getwd(), "yearly"), format = "era5", variable = "tasmax",
                                     era5_cell_size_deg = NULL),
                        period = list(warm_season_months = 6:8, start_date = "2002-01-01", end_date = "2002-12-31")),
                   file.path("saved", "config.yaml"))
  wired <- recording_session()
  resets <- function() Filter(function(m) m$id %in% c("month_from", "month_to", "limit_dates"), wired$seen$inputs)
  shiny::testServer(scorch_server, {
    choose("data", file.path(folder, "yearly"))
    session$flushReact()
    expect_match(conditionMessage(header()), "select one NetCDF file")
    expect_true(state$fresh)
    # The configuration makes the folder readable. Its months and dates are not replaced by the folder's.
    choose("config", file.path(folder, "saved", "config.yaml"))
    expect_false(state$fresh)
    do.call(session$setInputs, window_inputs(base(), month_from = "6", month_to = "8", limit_dates = TRUE,
      dates = as.Date(c("2002-01-01", "2002-12-31")), native_grid = TRUE, cell_size = 1))
    expect_identical(header()$format, "era5")
    expect_length(resets(), 0L)
    expect_identical(plan()$config$period[c("start_date", "end_date")], list(start_date = "2002-01-01", end_date = "2002-12-31"))
    expect_length(plan()$dates, 92L)
    expect_match(output$date_fields$html, "2002-01-01", fixed = TRUE)
    # Other data: the loaded settings stay in use, and so does the note that says so.
    expect_identical(state$loaded, "config.yaml")
    session$setInputs(change_data = 1)
    expect_identical(state$loaded, "config.yaml")
    session$setInputs(use_example = 1)
    expect_identical(state$loaded, "config.yaml")
    expect_identical(base()$period$warm_season_months, 6:8)
    expect_match(gsub("[[:space:]]+", " ", output$loaded$html),
                 paste("Settings loaded from <strong>config.yaml</strong>: the advanced settings and the results folder",
                       "stay in use when you change the data. The months, the date limit and"), fixed = TRUE)
    # The page sets the fields that describe a file back to detection, as it is told to for new data.
    session$setInputs(input_format = "auto", input_units = "auto", input_variable = "auto", input_time = "auto",
                      input_longitude = "auto", input_latitude = "auto")
    expect_identical(header()$format, "daily_tmax")
    # Newly chosen data start from their own months and from the whole record, as before.
    expect_setequal(vapply(resets(), "[[", "", "id"), c("month_from", "month_to", "limit_dates"))
  }, session = wired$session)
})

test_that("a ZIP keeps the page busy while it is opened, and a click that waited is not obeyed afterwards", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  dates <- seq(as.Date("2012-06-01"), as.Date("2012-08-31"), by = "day")
  input <- write_grid(file.path(folder, "small.nc"), array(20 + seq_len(length(dates) * 12) %% 15, c(length(dates), 3, 4)),
                      as.numeric(dates - as.Date("2000-01-01")), c(10, 11, 12, 13), c(40, 41, 42))
  archive <- file.path(folder, "data.zip")
  zip::zipr(archive, input)
  writeLines("no temperature here", file.path(folder, "notes.txt"))
  empty <- file.path(folder, "no-data.zip")
  zip::zipr(empty, file.path(folder, "notes.txt"))
  wired <- recording_session()
  busy <- function() unlist(lapply(Filter(function(m) m$type == "scorch-busy", wired$seen$custom), "[[", "message"))
  shiny::testServer(scorch_server, {
    choose("data", archive)
    expect_identical(busy(), TRUE)
    # The busy state ends once the screen that follows has been sent, not before.
    session$flushReact()
    expect_identical(busy(), c(TRUE, FALSE))
    expect_identical(state$path, normalizePath(archive, winslash = "/"))
    expect_identical(header()$format, "daily_tmax")
    # A click or key press on the opening screen that waited behind the extraction arrives now. It changes nothing.
    session$setInputs(use_example = 1, browse_file = 1)
    expect_identical(state$path, normalizePath(archive, winslash = "/"))
    expect_identical(wired$seen$modals, 0L)
    # A ZIP that cannot be opened ends the busy state too, and the opening screen works again.
    session$setInputs(change_data = 1)
    choose("data", empty)
    session$flushReact()
    expect_match(state$error, "no NetCDF")
    expect_null(state$source)
    expect_identical(busy(), c(TRUE, FALSE, TRUE, FALSE))
    session$setInputs(use_example = 2)
    expect_null(state$error)
    expect_null(state$path)
    expect_identical(header()$format, "daily_tmax")
  }, session = wired$session)
})

test_that("an emptied advanced field is reported in the Review card and above the Advanced section", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  shiny::testServer(scorch_server, {
    session$setInputs(use_example = 1)
    do.call(session$setInputs, window_inputs(base(), month_from = "6", month_to = "8", limit_dates = FALSE,
                                             native_grid = FALSE, cell_size = 1))
    expect_false(inherits(plan(), "error"))
    expect_match(output$run_button$html, 'id="start"', fixed = TRUE)
    # The grid size of raw ERA5 data has no field on screen for the example. Emptied while raw data
    # were open, it stops nothing here.
    session$setInputs(cell_size = NA)
    expect_false(inherits(plan(), "error"))
    expect_match(output$run_button$html, 'id="start"', fixed = TRUE)
    session$setInputs(cell_size = 1)
    # "Candidate distances" emptied: a message beside a disabled button, not an empty card.
    session$setInputs(clustering_eps_values = "")
    expect_s3_class(plan(), "advanced_setting")
    for (shown in list(output$review$html, output$advanced_problem$html)) {
      expect_match(shown, "Candidate distances (grid cells)", fixed = TRUE)
      expect_match(shown, "must list distinct, finite positive numbers", fixed = TRUE)
    }
    expect_match(output$review$html, 'role="alert"', fixed = TRUE)
    expect_false(grepl('role="alert"', output$advanced_problem$html, fixed = TRUE))
    expect_match(output$run_button$html, "disabled", fixed = TRUE)
    # A problem outside the Advanced section is shown in the Review card only.
    session$setInputs(clustering_eps_values = "1, 2", limit_dates = TRUE, dates = as.Date(c("1980-01-01", "1980-12-31")))
    expect_match(output$review$html, "no observations", fixed = TRUE)
    expect_false(grepl("no observations", paste(unlist(output$advanced_problem), collapse = " "), fixed = TRUE))
    session$setInputs(limit_dates = FALSE)
    expect_length(plan()$dates, 92L * 30L)
  })
})

test_that("every load of a configuration rebuilds the form, and dates picked by hand outlast a change of how the file is read", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  yaml::write_yaml(list(period = list(warm_season_months = 6:8, start_date = "2012-01-01", end_date = "2012-12-31"),
                        power_law = list(bootstrap_repetitions = 10)), "saved.yaml")
  yaml::write_yaml(list(period = list(warm_season_months = 7:8)), "other.yaml")
  # How often the form and its date fields are built: the page shows new values only when they are.
  built <- c(form = 0L, dates = 0L)
  form <- settings_ui
  testthat::local_mocked_bindings(settings_ui = function(cfg) {
    built[["form"]] <<- built[["form"]] + 1L
    form(cfg)
  }, .package = "scoRch.simulator")
  date_range <- shiny::dateRangeInput
  testthat::local_mocked_bindings(dateRangeInput = function(...) {
    built[["dates"]] <<- built[["dates"]] + 1L
    date_range(...)
  }, .package = "shiny")
  shiny::testServer(scorch_server, {
    # What the page reports once it has built the form from the configuration in use.
    page_reports <- function(...) do.call(session$setInputs, window_inputs(base(), ..., native_grid = FALSE, cell_size = 1))
    session$setInputs(use_example = 1)
    page_reports(month_from = "4", month_to = "9", limit_dates = FALSE)
    choose("config", file.path(project, "saved.yaml"))
    page_reports(month_from = "6", month_to = "8", limit_dates = TRUE, dates = as.Date(c("2012-01-01", "2012-12-31")))
    expect_length(plan()$dates, 92L)
    # Months, dates and repetitions changed by hand ...
    session$setInputs(month_from = "4", month_to = "9", limit_dates = FALSE, power_law_bootstrap_repetitions = 5000)
    expect_length(plan()$dates, 5490L)
    # ... and the same file reused to get its values back. Its settings are the ones in use already, so
    # only a form that is built again shows them.
    before <- built
    choose("config", file.path(project, "saved.yaml"))
    session$flushReact()
    expect_gt(built[["form"]], before[["form"]])
    expect_gt(built[["dates"]], before[["dates"]])
    shown <- shown_in(output$settings$html)
    expect_identical(shown[c("month_from", "month_to", "limit_dates", "power_law_bootstrap_repetitions")],
                     list(month_from = "6", month_to = "8", limit_dates = TRUE, power_law_bootstrap_repetitions = "10"))
    expect_match(output$date_fields$html, "2012-01-01", fixed = TRUE)
    page_reports(month_from = "6", month_to = "8", limit_dates = TRUE, dates = as.Date(c("2012-01-01", "2012-12-31")))
    expect_length(plan()$dates, 92L)
    expect_identical(plan()$config$power_law$bootstrap_repetitions, 10)

    # Dates picked by hand stay when a field under "How the file is read" is set to what the file has:
    # the same record is read again, and the date fields are not built again.
    session$setInputs(dates = as.Date(c("2012-07-01", "2012-07-31")))
    before <- built
    session$setInputs(input_units = "degC")
    expect_identical(header()$units, "degC")
    session$setInputs(input_variable = "tmax")
    expect_identical(built[["dates"]], before[["dates"]])
    expect_length(plan()$dates, 31L)
    # They start again when an error is corrected, ...
    session$setInputs(input_variable = "no such variable")
    expect_s3_class(header(), "error")
    expect_null(record())
    session$setInputs(input_variable = "auto")
    expect_gt(built[["dates"]], before[["dates"]])
    # ... for other data, also for the same data opened once more, ...
    before <- built
    session$setInputs(change_data = 1)
    session$setInputs(use_example = 2)
    expect_gt(built[["dates"]], before[["dates"]])
    expect_identical(built[["form"]], before[["form"]])
    # ... and for another configuration.
    before <- built
    choose("config", file.path(project, "other.yaml"))
    session$flushReact()
    expect_gt(built[["form"]], before[["form"]])
    expect_gt(built[["dates"]], before[["dates"]])
  })
})

test_that("a configuration that waits for its data reads the data chosen next as it stands", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  days <- seq(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
  file <- write_grid(file.path(folder, "moved.nc"),
                     list(maxair = array(seq_len(length(days) * 12) %% 17, c(length(days), 3, 4))),
                     as.numeric(days - as.Date("2000-01-01")), c(10, 11, 12, 13), c(40, 41, 42))
  # The settings of a run that was saved on another computer: the data are not where they say.
  yaml::write_yaml(list(input = list(file = file.path(folder, "elsewhere", "moved.nc"), variable = "maxair", units = "degC"),
                        period = list(warm_season_months = 6:8, start_date = "2002-01-01", end_date = "2002-12-31")),
                   "config.yaml")
  wired <- recording_session()
  sent <- function() unique(vapply(wired$seen$inputs, "[[", "", "id"))
  reading <- paste0("input_", c("format", "units", "variable", "longitude", "latitude", "time"))
  shiny::testServer(scorch_server, {
    choose("config", file.path(folder, "config.yaml"))
    session$flushReact()
    expect_null(state$source)
    expect_match(state$error, "could not be found")
    expect_true(state$waiting)
    expect_identical(state$loaded, "config.yaml")
    # The page has built the form from the configuration.
    do.call(session$setInputs, c(window_inputs(base(), month_from = "6", month_to = "8", limit_dates = TRUE,
      dates = as.Date(c("2002-01-01", "2002-12-31")), native_grid = FALSE, cell_size = 1),
      list(input_format = "auto", input_units = "degC", input_variable = "maxair", input_longitude = "auto",
           input_latitude = "auto", input_time = "auto")))
    choose("data", file)
    session$flushReact()
    # Nothing on the page is set back: the months, the dates and "How the file is read" are the configuration's.
    expect_false(state$waiting)
    expect_false(state$fresh)
    expect_length(sent(), 0L)
    expect_null(state$error)
    expect_null(question())
    expect_identical(header()$variable, "maxair")
    expect_identical(plan()$config$period[c("start_date", "end_date")], list(start_date = "2002-01-01", end_date = "2002-12-31"))
    expect_length(plan()$dates, 92L)
    expect_match(output$date_fields$html, "2002-01-01", fixed = TRUE)
    # The note is true here as well: it speaks of what happens when the data change.
    expect_match(gsub("[[:space:]]+", " ", output$loaded$html),
                 "start again with other data; reuse the file to apply its months and dates again.", fixed = TRUE)
    # Other data after that start again, as they do without a configuration.
    session$setInputs(change_data = 1)
    choose("data", file)
    session$flushReact()
    expect_setequal(sent(), c(reading, "month_from", "month_to", "limit_dates"))
    expect_identical(state$loaded, "config.yaml")
  }, session = wired$session)

  # Data that the configuration finds are opened with it, and nothing waits.
  yaml::write_yaml(list(input = list(file = file, variable = "maxair", units = "degC")), "here.yaml")
  shiny::testServer(scorch_server, {
    choose("config", file.path(folder, "here.yaml"))
    expect_false(is.null(state$source))
    expect_false(state$waiting)
    expect_null(state$error)
  })
})

test_that("a file that is not a configuration is refused by name and in plain words", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  writeLines("just a sentence", "sentence.yaml")
  writeLines(c("- June", "- July"), "words.yaml")
  writeLines(c("- warm_season_months: 6", "- start_date: 2012-01-01"), "unnamed.yaml")
  writeLines(c("heatwaves:", "  threshold_percentile: 150"), "range.yaml")
  writeLines("period: [6, 7", "broken.yaml")
  writeLines("", "empty.yaml")
  for (name in c("sentence.yaml", "words.yaml", "unnamed.yaml")) {
    expect_error(load_guided_config(name), "This file does not hold SCORCH settings. A configuration has named sections",
                 fixed = TRUE, info = name)
  }
  expect_error(load_guided_config("broken.yaml"), "could not be read as YAML")
  expect_error(load_guided_config("range.yaml"), "must be one number between 0 and 100")
  # An empty file stays a configuration that changes no setting.
  expect_identical(load_guided_config("empty.yaml")$heatwaves, read_config(input_required = FALSE)$heatwaves)
  shiny::testServer(scorch_server, {
    session$setInputs(use_example = 1)
    study <- base()
    for (name in c("sentence.yaml", "words.yaml", "unnamed.yaml", "range.yaml", "broken.yaml")) {
      choose("config", file.path(folder, name))
      # The message names the file that was refused, and nothing of it is in use.
      expect_true(startsWith(state$error, paste0(name, " was not loaded: ")), info = state$error)
      expect_null(state$loaded)
      expect_identical(base(), study)
    }
    choose("config", file.path(folder, "sentence.yaml"))
    expect_identical(state$error, paste("sentence.yaml was not loaded: This file does not hold SCORCH settings.",
                                        "A configuration has named sections such as 'period' and 'heatwaves'."))
    choose("config", file.path(folder, "range.yaml"))
    expect_identical(state$error, paste("range.yaml was not loaded: The setting 'threshold_percentile' must be one number",
                                        "between 0 and 100 (it is '150')."))
    session$flushReact()
    expect_match(output$selection$html, "range.yaml was not loaded", fixed = TRUE)
  })
})

test_that("an unidentified variable is confirmed with an answer, listed while that answer decides, and named in the results", {
  folder <- withr::local_tempdir()
  withr::local_dir(folder)
  days <- seq(as.Date("2001-06-01"), as.Date("2001-08-31"), by = "day")
  file <- write_grid(file.path(folder, "tasmin_day.nc"),
                     list(tasmin = array(10 + seq_len(length(days) * 12) %% 9, c(length(days), 3, 4))),
                     as.numeric(days - as.Date("2000-01-01")), c(10, 11, 12, 13), c(40, 41, 42),
                     attributes = list(tasmin = list(standard_name = "air_temperature", cell_methods = "time: minimum")))
  said <- function(shown) gsub("[[:space:]]+", " ", paste(unlist(shown), collapse = " "))
  shiny::testServer(scorch_server, {
    choose("data", file)
    session$flushReact()
    expect_identical(question()$id, "variable")
    expect_match(said(output$question), 'id="answer_problem"', fixed = TRUE)
    expect_false(grepl("Choose an answer first", said(output$answer_problem), fixed = TRUE))
    # "Use this answer" before an answer is chosen says what is missing.
    session$setInputs(answer_apply = 1)
    expect_identical(question()$id, "variable")
    expect_match(said(output$answer_problem), '<p class="problem" role="alert">Choose an answer first.</p>', fixed = TRUE)
    # Choosing an answer takes the reminder away, and using it settles the question.
    session$setInputs(answer = "tasmin")
    expect_false(grepl("Choose an answer first", said(output$answer_problem), fixed = TRUE))
    session$setInputs(answer_apply = 2)
    expect_null(question())
    expect_false(header()$identified)
    expect_match(said(output$selection), "Your answer: variable = tasmin", fixed = TRUE)
    # A reminder does not outlive the question it was about.
    session$setInputs(answer_reset = 1)
    session$setInputs(answer = NULL)
    session$setInputs(answer_apply = 3)
    expect_true(state$unanswered)
    session$setInputs(change_data = 1)
    expect_false(state$unanswered)
    choose("data", file)
    session$setInputs(answer = "tasmin", answer_apply = 4)
    expect_null(question())

    # The same thing named in the field under Advanced settings replaces the answer, which is then not listed.
    session$setInputs(input_variable = "tasmin")
    expect_false(grepl("Your answer", said(output$selection), fixed = TRUE))
    expect_length(answers_in_effect(input, base()$input, state$answers), 0L)
    session$setInputs(input_variable = "auto")
    expect_match(said(output$selection), "Your answer: variable = tasmin", fixed = TRUE)
    expect_identical(answers_in_effect(input, base()$input, state$answers), list(variable = "tasmin"))

    # The run records which variable was analyzed. This one stops early, at clusters no day can form.
    quick <- window_inputs(base(), month_from = "6", month_to = "8", limit_dates = FALSE, native_grid = FALSE, cell_size = 1)
    quick$clustering_min_samples_values <- "1000"
    do.call(session$setInputs, quick)
    expect_length(plan()$dates, 92L)
    suppressWarnings(suppressMessages(session$setInputs(start = 1)))
    expect_null(state$failed)
    expect_identical(basename(state$finished$folder), "SCORCH_run_001")
    expect_identical(state$finished$temperature,
                     "Variable tasmin, as you chose. The file does not identify it as daily maximum temperature.")
  })
})

test_that("a start that fails is reported beside the Run button until another results folder is chosen", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  writeLines("a file, not a folder", "blocker")
  shiny::testServer(scorch_server, {
    session$setInputs(use_example = 1)
    do.call(session$setInputs, window_inputs(base(), month_from = "6", month_to = "8", limit_dates = FALSE,
                                             native_grid = FALSE, cell_size = 1))
    choose("output", file.path(project, "blocker", "results"))
    session$flushReact()
    expect_match(output$run_button$html, 'id="start"', fixed = TRUE)
    expect_false(grepl('role="alert"', output$review$html, fixed = TRUE))
    session$setInputs(start = 1)
    expect_null(state$finished)
    # The Review card holds the button that was clicked: the message is there, with the plan and the button as they were.
    for (wanted in c("Could not create the results folder", 'role="alert"', "selected dates")) {
      expect_match(output$review$html, wanted, fixed = TRUE)
    }
    expect_match(output$run_button$html, 'id="start"', fixed = TRUE)
    # The dataset card at the top of the page does not repeat it.
    expect_null(state$error)
    expect_false(grepl("Could not create", output$selection$html, fixed = TRUE))
    # Another results folder: the message said nothing about that one.
    choose("output", file.path(project, "results"))
    session$flushReact()
    expect_match(output$review$html, "selected dates", fixed = TRUE)
    expect_false(grepl("Could not create", output$review$html, fixed = TRUE))
    expect_false(dir.exists(file.path(project, "results")))
  })
})
