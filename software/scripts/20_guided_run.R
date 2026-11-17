# Settings and output for an interactive run. The saved document is also
# a valid input to the standard scripted command.
guided_path <- function(path, relative_to = getwd()) {
  path <- path.expand(path)
  # A drive letter alone ("D:") is the top of that drive.
  alone <- grepl("^[A-Za-z]:$", path)
  path[alone] <- paste0(path[alone], "/")
  absolute <- grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)
  path[!absolute] <- file.path(relative_to, path[!absolute])
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

load_guided_config <- function(file) {
  # The parser's own message begins with the whole path of the file.
  doc <- tryCatch(yaml::read_yaml(file, eval.expr = FALSE), error = function(e) {
    fail("This file could not be read as YAML (",
         sub(paste0("(", file, ") "), "", conditionMessage(e), fixed = TRUE), ").")
  })
  if (is.null(doc)) doc <- list()
  # Any text is YAML of a kind: one sentence, a list of words. A configuration
  # is a document of named sections.
  if (!is.list(doc) || (length(doc) && is.null(names(doc)))) {
    fail("This file does not hold SCORCH settings. A configuration has named sections such as 'period' and 'heatwaves'.")
  }
  if (is.list(doc) && is.list(doc$input) && is.character(doc$input$file))
    doc$input$file <- guided_path(doc$input$file, dirname(file))
  if (is.list(doc) && (is.null(doc$output) || is.list(doc$output))) {
    dest <- doc$output$directory
    if (is.null(dest)) dest <- scorch_defaults()$output$directory
    if (is.character(dest)) doc$output$directory <- guided_path(dest, dirname(file))
  }
  read_config(doc, input_required = FALSE)
}

scorch_bundled_file <- function() {
  system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator", mustWork = TRUE)
}

next_run_folder <- function(parent, reserve = FALSE) {
  if (reserve && !dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(parent)) fail("Could not create the results folder: ", parent)
  # Number after the highest run in the folder, so the name of a deleted run
  # is not given to a newer one. Nine digits stay within R's whole numbers.
  used <- list.files(parent, pattern = "^SCORCH_run_[0-9]{1,9}$")
  i <- max(0L, as.integer(sub("^SCORCH_run_", "", used))) + 1L
  # A drive or file-system root already ends in a slash.
  root <- sub("/+$", "", parent)
  repeat {
    folder <- file.path(root, sprintf("SCORCH_run_%03d", i))
    if (!file.exists(folder)) {
      if (!reserve || dir.create(folder, showWarnings = FALSE)) return(folder)
      if (!file.exists(folder)) fail("Could not create ", folder, ". Choose another results folder.")
    }
    i <- i + 1L
  }
}

season_limits <- function(months) {
  if (length(months) == 12L) return(c(1L, 12L))
  start <- months[!(1L + (months + 10L) %% 12L) %in% months]
  c(start, 1L + (start + length(months) - 2L) %% 12L)
}

# Calendar months in words, as runs: "June-August", "January-May, September".
month_runs <- function(months) {
  months <- sort(unique(as.integer(months)))
  if (!length(months)) return("")
  runs <- unname(split(months, cumsum(c(TRUE, diff(months) > 1L))))
  # December and January join when both ends of the year are present.
  if (length(runs) > 1L && months[1] == 1L && months[length(months)] == 12L) {
    runs <- c(runs[-c(1L, length(runs))], list(c(runs[[length(runs)]], runs[[1L]])))
  }
  paste(vapply(runs, function(run) {
    if (length(run) == 1L) month.name[run] else paste(month.name[run[1L]], month.name[run[length(run)]], sep = "\u2013")
  }, ""), collapse = ", ")
}

# The selected dates in words that stay true for a year-round file: which
# months, in how many years, and what the file holds outside the selection.
period_summary <- function(dates, months, available) {
  start <- season_start_month(months)
  years <- season_year(dates, start)
  crosses <- length(months) < 12L && any(months < start)
  name <- function(year) if (crosses) sprintf("%d/%02d", year, (year + 1L) %% 100L) else as.character(year)
  in_file <- sort(unique(as.integer(format(available, "%m"))))
  list(months = if (length(months) == 12L) "Every month" else month_runs(months),
       years = if (min(years) == max(years)) name(min(years)) else paste(name(min(years)), name(max(years)), sep = "\u2013"),
       n_years = length(unique(years)), n_days = length(dates), first = min(dates), last = max(dates),
       selected = sort(unique(as.integer(months))), in_file = in_file,
       unused = month_runs(setdiff(in_file, months)), absent = month_runs(setdiff(months, in_file)))
}

# The explicit start and end dates of a configuration's period, or NULL when it has neither.
period_bounds <- function(period) {
  if (is.null(period$start_date) && is.null(period$end_date)) return(NULL)
  list(start_date = period$start_date, end_date = period$end_date)
}

# How the file is read: the fields of the window, then the answers given to
# its questions for anything still left to detection.
input_controls <- function(input, base, answers = list()) {
  for (key in c("format", "variable", "longitude", "latitude", "time", "units")) {
    value <- input[[paste0("input_", key)]]
    # Text cut in the middle of a character cannot be trimmed or looked up;
    # with its broken part replaced it fails like any other wrong name.
    if (is.character(value) && !all(validEnc(value))) value <- iconv(value, "UTF-8", "UTF-8", sub = "?")
    if (!is.null(value)) base[[key]] <- if (nzchar(trimws(value))) trimws(value) else "auto"
  }
  for (key in names(answers)) if (identical(base[[key]], "auto")) base[[key]] <- answers[[key]]
  base
}

# The answers that still decide something. A field of the window that was
# filled in since replaces the answer given for the same thing.
answers_in_effect <- function(input, base, answers) {
  fields <- input_controls(input, base)
  answers[vapply(names(answers), function(key) identical(fields[[key]], "auto"), logical(1))]
}

guided_settings <- function(base, form, path, info, dest, answers = list()) {
  cfg <- base
  cfg$input <- input_controls(form, base$input, answers)
  cfg$input["file"] <- list(path)
  cfg$input$format <- info$format
  # A name that `auto` would not find is written out for scripted replays.
  expected <- reader_names(info$format)
  for (key in names(expected)) {
    if (identical(cfg$input[[key]], "auto") && length(info[[key]]) == 1L && !info[[key]] %in% expected[[key]])
      cfg$input[[key]] <- info[[key]]
  }
  # The two fields of the ERA5 grid are on screen for raw data only. What raw
  # data left in them is not read for other data, where nothing could repair it.
  if (info$format == "era5") {
    if (isTRUE(form$native_grid)) cfg$input["era5_cell_size_deg"] <- list(NULL)
    else cfg$input$era5_cell_size_deg <- form$cell_size
  }
  cfg$input["processed_file"] <- list(if (info$format == "era5" && isTRUE(form$save_tmax))
    file.path(dest, "prepared_tmax.nc"))
  a <- as.integer(form$month_from)
  b <- as.integer(form$month_to)
  if (length(a) != 1L || length(b) != 1L || !all(c(a, b) %in% 1:12)) fail("Choose the first and last month.")
  months <- 1L + (a - 1L + seq.int(0L, (b - a) %% 12L)) %% 12L
  cfg$period <- list(warm_season_months = months, start_date = NULL, end_date = NULL)
  dates <- info$dates
  if (isTRUE(form$limit_dates)) {
    if (length(form$dates) != 2L || anyNA(form$dates)) fail("Choose both analysis dates.")
    cfg$period$start_date <- as.character(form$dates[1])
    cfg$period$end_date <- as.character(form$dates[2])
  } else if (is.null(form$limit_dates) && length(form$bounds)) {
    # The window has no date fields; the explicit dates of a reused configuration apply as they are.
    # A configuration that was read holds them as dates; a run is given them as YYYY-MM-DD, like the fields gave them.
    for (key in c("start_date", "end_date")) {
      cfg$period[key] <- list(if (is.null(form$bounds[[key]])) NULL else as.character(as.Date(form$bounds[[key]])))
    }
  }
  for (section in c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp")) {
    for (key in names(base[[section]])) {
      value <- form[[paste(section, key, sep = "_")]]
      if (section == "clustering") {
        value <- suppressWarnings(as.numeric(strsplit(trimws(value), "[,[:space:]]+")[[1]]))
        # Whole numbers are saved as a scripted run saves them: "- 4", not "- 4.0".
        if (key == "min_samples_values" && all(is.finite(value)) && all(value == round(value)) &&
            all(abs(value) <= .Machine$integer.max)) value <- as.integer(value)
      }
      cfg[[section]][key] <- list(value)
    }
  }
  cfg$output$directory <- dest
  # A results folder whose path is too long for Windows is named before Run,
  # in the words with which a run would stop at its start.
  unwritable <- output_path_problem(dest)
  if (!is.null(unwritable)) fail(unwritable)
  # A refused setting is named as the window labels it, not by its YAML key.
  checked <- tryCatch(read_config(cfg, input_required = FALSE),
                      error = function(e) form_problem(conditionMessage(e)))
  if (info$format == "era5" && !is.null(checked$input$era5_cell_size_deg))
    check_cell_size(info, checked$input$era5_cell_size_deg)
  keep <- as.integer(format(dates, "%m")) %in% months
  if (!is.null(checked$period$start_date)) keep <- keep & dates >= checked$period$start_date
  if (!is.null(checked$period$end_date)) keep <- keep & dates <= checked$period$end_date
  if (!any(keep)) fail("Your chosen months and dates contain no observations in this dataset.")
  list(config = cfg, dates = dates[keep], period = period_summary(dates[keep], months, dates))
}

# The reader averages raw grid points to cells only when the cell size is a
# whole multiple of the grid spacing (average_to_cells()). The same rule and
# words, applied to the coordinates of the header, stop such a selection
# before Run instead of after it.
check_cell_size <- function(info, size) {
  file <- if (length(info$file)) basename(info$file[[1L]]) else "the selected data"
  # The coordinates as the reader takes them (raw_coordinates()), and its own
  # rule for the cells of each axis (cell_axis()), in the words of the window.
  coordinates <- raw_coordinates(info$lon, info$lat, size)
  axes <- list(longitude = coordinates$lon, latitude = coordinates$lat)
  complete <- c(longitude = 0L, latitude = 0L)
  for (name in names(axes)) {
    cells <- tryCatch(cell_axis(axes[[name]], name, size, file, coordinates$slack[[name]]), error = function(e) {
      fail_advanced(sub("era5_cell_size_deg", sprintf("'%s'", SETTING_LABELS[["era5_cell_size_deg"]]),
                        conditionMessage(e), fixed = TRUE))
    })
    complete[[name]] <- length(cells$ids)
  }
  # The reader then needs two cells along each axis, to measure their spacing
  # (tmax_from_array()). What it says there does not name this setting.
  for (name in names(axes)) {
    if (complete[[name]] < 2L) {
      fail_advanced(sprintf(paste("The ERA5 grid in %s gives fewer than two %g-degree cells along its %s.",
                                  "Choose a smaller '%s', or tick 'Keep the input grid resolution'."),
                            file, size, name, SETTING_LABELS[["era5_cell_size_deg"]]))
    }
  }
  invisible(NULL)
}

write_guided_config <- function(cfg, where) {
  # An untouched example has null input so it can be replayed on another
  # computer. A ZIP stays a ZIP in this document, never a temporary path.
  if (!length(Filter(Negate(is.null), cfg$figures$representative_events))) cfg$figures <- NULL
  yaml::write_yaml(cfg, file.path(where, "config.yaml"), precision = 17)
}

execute_guided_run <- function(settings, parent, source) {
  dest <- next_run_folder(parent, reserve = TRUE)
  cfg <- settings$config
  cfg$output$directory <- dest
  if (!is.null(cfg$input$processed_file)) cfg$input$processed_file <- file.path(dest, "prepared_tmax.nc")
  config <- file.path(dest, "config.yaml")
  summary <- file.path(dest, "Summary.txt")
  result <- NULL
  cell_tmax <- NULL
  problem <- tryCatch({
    write_guided_config(cfg, dest)
    # The event map colours cells by temperature; keep those values only.
    result <- run_scorch_analysis(config, source = source, observe = function(result, tmax) {
      cell_tmax <<- tryCatch(cluster_cell_tmax(result, tmax), error = function(e) NULL)
    })
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(problem) && !file.exists(summary)) {
    write_text_file(c("SCORCH analysis stopped", "", problem, "", "Selected settings",
                      if (file.exists(config)) readLines(config, warn = FALSE)), summary)
  }
  cat("\nRUN FILES\n  Results folder: ", dest, "\n  Saved settings: ", config, "\n",
      file = summary, append = TRUE, sep = "")
  list(result = result, folder = dest, summary = summary, problem = problem, cell_tmax = cell_tmax)
}

open_run_file <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  # shell.exec() exists on Windows only; looked up by name, other platforms do not miss it.
  if (.Platform$OS.type == "windows") get("shell.exec", envir = baseenv())(path)
  else if (Sys.info()[["sysname"]] == "Darwin") system2("open", shQuote(path), wait = FALSE)
  else if (nzchar(Sys.which("xdg-open"))) system2("xdg-open", shQuote(path), wait = FALSE)
  else utils::browseURL(paste0("file://", utils::URLencode(path)))
  invisible(NULL)
}
