# Settings and output for an interactive run. The saved document is also
# a valid input to the standard scripted command.
guided_path <- function(path, relative_to = getwd()) {
  path <- path.expand(path)
  absolute <- grepl("^([A-Za-z]:[/\\\\]|[/\\\\])", path)
  path[!absolute] <- file.path(relative_to, path[!absolute])
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

load_guided_config <- function(file) {
  cfg <- read_config(file, input_required = FALSE)
  if (!is.null(cfg$input$file))
    cfg$input$file <- guided_path(cfg$input$file, dirname(file))
  cfg$output$directory <- guided_path(cfg$output$directory, dirname(file))
  cfg
}

next_run_folder <- function(parent, reserve = FALSE) {
  if (reserve && !dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE) &&
      !dir.exists(parent)) fail("Could not create the results folder: ", parent)
  i <- 1L
  repeat {
    folder <- file.path(parent, sprintf("SCORCH_run_%03d", i))
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

input_controls <- function(input, base) {
  for (key in c("format", "variable", "longitude", "latitude", "time", "units")) {
    value <- input[[paste0("input_", key)]]
    if (!is.null(value)) base[[key]] <- if (nzchar(trimws(value))) trimws(value) else "auto"
  }
  base
}

guided_settings <- function(base, form, path, info, dest) {
  cfg <- base
  cfg$input <- input_controls(form, base$input)
  cfg$input["file"] <- list(path)
  cfg$input$format <- info$format
  if (isTRUE(form$native_grid)) cfg$input["era5_cell_size_deg"] <- list(NULL)
  else cfg$input$era5_cell_size_deg <- form$cell_size
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
  }
  for (section in c("heatwaves", "regional_days", "clustering", "ellipses", "power_law", "lgcp")) {
    for (key in names(base[[section]])) {
      value <- form[[paste(section, key, sep = "_")]]
      if (section == "clustering")
        value <- suppressWarnings(as.numeric(strsplit(trimws(value), "[,[:space:]]+")[[1]]))
      cfg[[section]][key] <- list(value)
    }
  }
  cfg$output$directory <- dest
  checked <- read_config(cfg, input_required = FALSE)
  keep <- as.integer(format(dates, "%m")) %in% months
  if (!is.null(checked$period$start_date)) keep <- keep & dates >= checked$period$start_date
  if (!is.null(checked$period$end_date)) keep <- keep & dates <= checked$period$end_date
  if (!any(keep)) fail("Your chosen months and dates contain no observations in this dataset.")
  list(config = cfg, dates = dates[keep])
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
  problem <- tryCatch({
    write_guided_config(cfg, dest)
    result <- run_scorch_analysis(config, source = source)
    NULL
  }, error = function(e) conditionMessage(e))
  if (!is.null(problem) && !file.exists(summary)) {
    write_text_file(c("SCORCH analysis stopped", "", problem, "", "Selected settings",
                      if (file.exists(config)) readLines(config, warn = FALSE)), summary)
  }
  cat("\nRUN FILES\n  Results folder: ", dest, "\n  Saved settings: ", config, "\n",
      file = summary, append = TRUE, sep = "")
  list(result = result, folder = dest, summary = summary, problem = problem)
}

open_run_file <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  if (.Platform$OS.type == "windows") shell.exec(path)
  else if (Sys.info()[["sysname"]] == "Darwin") system2("open", shQuote(path), wait = FALSE)
  else if (nzchar(Sys.which("xdg-open"))) system2("xdg-open", shQuote(path), wait = FALSE)
  else utils::browseURL(paste0("file://", utils::URLencode(path)))
  invisible(NULL)
}
