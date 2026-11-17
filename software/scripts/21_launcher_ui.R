# The setup window: an opening screen with two choices, the settings that
# need a decision, and the summary of a finished run.

# Heatwave cells with a fitted ellipse, its two axes and its centroid.
BRAND_MARK <- paste0(
  '<svg class="brand-mark" viewBox="0 0 48 48" aria-hidden="true" focusable="false">',
  '<g stroke="#fff" stroke-width="1">',
  '<rect x="20" y="8" width="8" height="8" fill="#ffb347"/><rect x="28" y="8" width="8" height="8" fill="#ffd166"/>',
  '<rect x="12" y="16" width="8" height="8" fill="#ff8c42"/><rect x="20" y="16" width="8" height="8" fill="#d93a2b"/>',
  '<rect x="28" y="16" width="8" height="8" fill="#f4633a"/><rect x="36" y="16" width="8" height="8" fill="#ffb347"/>',
  '<rect x="4" y="24" width="8" height="8" fill="#ffd166"/><rect x="12" y="24" width="8" height="8" fill="#f4633a"/>',
  '<rect x="20" y="24" width="8" height="8" fill="#a8201a"/><rect x="28" y="24" width="8" height="8" fill="#ff8c42"/>',
  '<rect x="12" y="32" width="8" height="8" fill="#ffb347"/><rect x="20" y="32" width="8" height="8" fill="#ffd166"/></g>',
  '<g transform="rotate(-24 24 24)" fill="none" stroke="#1c2330" stroke-linecap="round">',
  '<ellipse cx="24" cy="24" rx="21" ry="11.5" stroke-width="2.2"/>',
  '<path d="M3 24H45" stroke-width="1.6"/><path d="M24 12.5V35.5" stroke-width="1.1"/></g>',
  '<circle cx="24" cy="24" r="2.6" fill="#1c2330" stroke="#fff" stroke-width="1"/></svg>')

# The four stages of a run as the window names them.
RUN_STAGES <- c("Stage 1: Heatwave Detection", "Stage 2: Heatwave Clustering",
                "Stage 3: Heatwave Classification", "Stage 4: Heatwave Modeling")

# The title of the SCORCH window and of its page.
WINDOW_TITLE <- "SCORCH | Spatiotemporal Classification Of Regional Compound Heatwaves"

scorch_ui <- function() {
  tags <- shiny::tags
  assets <- system.file("launcher", package = "scoRch.simulator")
  shiny::addResourcePath("scorch", assets)
  # A newer installation must not reuse a cached script or stylesheet.
  stamp <- paste0("?v=", utils::packageVersion("scoRch.simulator"), "-",
                  as.integer(max(file.mtime(list.files(assets, full.names = TRUE)))))
  asset <- function(name) paste0("scorch/", name, stamp)
  # The name spelled out as in Figure 1 of the study: each initial that makes the acronym in bold.
  initials <- c("Spatiotemporal", "Classification", "Of", "Regional", "Compound", "Heatwaves")
  # One string: tags would put a space between each initial and the rest of its word.
  spelled <- tags$span(class = "brand-name", shiny::HTML(paste0(
    "<b>", substr(initials, 1L, 1L), "</b>", substring(initials, 2L), collapse = " ")))
  shiny::fluidPage(
    tags$head(tags$title(WINDOW_TITLE),
              # One light display, whatever the system's appearance.
              tags$meta(name = "color-scheme", content = "light only"),
              tags$link(rel = "icon", type = "image/svg+xml",
                        href = paste0("data:image/svg+xml,", utils::URLencode(
                          sub("<svg ", "<svg xmlns=\"http://www.w3.org/2000/svg\" ", BRAND_MARK, fixed = TRUE), reserved = TRUE))),
              tags$link(rel = "stylesheet", href = asset("style.css")),
              tags$script(src = asset("scorch.js"), charset = "utf-8", defer = NA)),
    tags$main(class = "scorch", `data-showcase` = asset("showcase.json"),
      tags$header(class = "masthead",
        tags$div(class = "brand-block", tags$div(class = "brand", "SCORCH"), spelled),
        tags$div(class = "masthead-tools",
          # RStudio's dialog has a fixed size; the same SCORCH, with its finished run, opens in a
          # browser window instead, which can be resized and maximized.
          shiny::conditionalPanel("output.in_dialog",
            shiny::actionButton("open_browser", "Open in browser", class = "theme-toggle open-browser",
                                title = "Continue in a web browser window that you can resize and maximize")))),
      # The running view: shown by scorch.js while a run is under way, from the run's own progress.
      tags$section(id = "running", class = "running", hidden = NA, `aria-live` = "polite",
        tags$div(class = "running-art", `aria-hidden` = "true"),
        tags$h1(class = "running-title", "Running scoRch.simulator"),
        tags$ol(class = "running-stages",
          lapply(seq_along(RUN_STAGES), function(k) tags$li(`data-stage` = k,
            tags$span(class = "stage-mark", `aria-hidden` = "true"), tags$span(class = "stage-label", RUN_STAGES[k])))),
        tags$p(class = "running-note"),
        tags$p(class = "running-time", "Elapsed ", tags$span(class = "running-elapsed", "0:00")),
        tags$div(class = "running-error", hidden = NA, role = "alert",
          tags$p(class = "problem running-error-text"),
          tags$button(type = "button", class = "btn btn-primary running-back", "Back to settings"))),
      shiny::conditionalPanel("output.screen == 'welcome'",
        tags$section(class = "hero",
          tags$div(class = "hero-text",
            tags$h1("scoRch.simulator"),
            tags$p(class = "lead", paste(
              "Explore where compound heatwaves occur, how they evolve over space and time, and discover",
              "their unique typologies through interactive maps and visualizations.")),
            NULL),
          tags$div(class = "choices",
            tags$div(shiny::actionButton("browse_file", "Select Your Data", class = "btn-primary btn-lg"),
                     tags$p("Choose a processed or raw NetCDF file, a ZIP archive, or a folder of ERA5 files.")),
            tags$div(shiny::actionButton("use_example", "Use included example", class = "btn-lg"),
                     tags$p(tags$strong("30 Summers Across the USA (1996\u20132025)")))),
          shiny::uiOutput("welcome_problem")),
        tags$div(id = "explorer-demo", class = "explorer", `data-explorer` = "demo")),
      shiny::conditionalPanel("output.screen == 'settings'",
        shiny::uiOutput("selection"),
        shiny::uiOutput("question"),
        shiny::conditionalPanel("!output.asking",
          tags$section(class = "card",
            tags$div(class = "section-title", tags$h2("Months to analyze"),
                     shiny::actionButton("load_config", "Reuse a saved configuration", class = "link-button")),
            shiny::uiOutput("loaded"),
            shiny::uiOutput("settings")),
          tags$section(class = "card",
            tags$div(class = "section-title", tags$h2("Results folder"),
                     shiny::actionButton("browse_output", "Choose or create folder")),
            tags$code(class = "path", shiny::textOutput("destination", inline = TRUE)),
            tags$p(class = "fine", "SCORCH creates this new folder when the run starts. Earlier runs are never overwritten.")),
          tags$section(class = "card review", shiny::uiOutput("review"), shiny::uiOutput("run_button")))),
      shiny::uiOutput("completion"),
      tags$footer(
        tags$span("Reopen this window at any time with ", tags$code("scoRch.simulator::run_scorch()"),
                  ". The package stays installed."),
        shiny::conditionalPanel("output.screen != 'welcome'",
                                shiny::actionButton("done", "Close", class = "link-button")))),
    lang = "en")
}

# The fields of the Advanced section: the name read_config() gives each
# setting in a message, and the label the window shows beside its field. A
# problem with a setting is reported under that label.
SETTING_LABELS <- c(
  "threshold_percentile" = "Local temperature percentile",
  "regional_days: percentile" = "Regional extent percentile",
  "min_duration_days" = "Minimum heatwave duration (days)",
  "min_exceeding_days" = "Minimum days at or above threshold",
  "eps_values" = "Candidate distances (grid cells)",
  "min_samples_values" = "Candidate neighborhood sizes",
  "scale_factor" = "Ellipse scale factor",
  "bootstrap_repetitions" = "Bootstrap repetitions",
  "seed_daily_maxima" = "Random seed for daily maxima",
  "seed_event_maxima" = "Random seed for event maxima",
  "lgcp: covariates" = "Predictors",
  "raster_km" = "Raster cell size (km)",
  "window_margin_km" = "Window margin (km)",
  "era5_cell_size_deg" = "Output grid size (degrees)",
  "input: processed_file" = "Save prepared daily Tmax in this run folder",
  "input: format" = "Data format",
  "input: units" = "Temperature units",
  "input: variable" = "Temperature variable",
  "input: time" = "Time coordinate",
  "input: longitude" = "Longitude coordinate",
  "input: latitude" = "Latitude coordinate")

# Stop on a field of the Advanced section. The window repeats such a message
# where that section begins, because the Review card can be far below it.
fail_advanced <- function(...) {
  stop(structure(class = c("advanced_setting", "error", "condition"), list(message = paste0(...), call = NULL)))
}

# What read_config() says about a refused setting, in the words of the
# window: the label beside the field for the name in the YAML file, and
# "empty" for a field without a value.
form_problem <- function(message) {
  if (startsWith(message, "The setting 'start_date' must be on or before")) {
    fail("The first of the 'Analysis dates' must be on or before the last.")
  }
  if (startsWith(message, "The setting 'lgcp: covariates' must list")) {
    fail_advanced(sprintf("Tick at least one of the '%s'.", SETTING_LABELS[["lgcp: covariates"]]))
  }
  if (startsWith(message, "The input longitude, latitude and time dimension names must be distinct")) {
    fail_advanced(sprintf("'%s', '%s' and '%s' must name three different dimensions.", SETTING_LABELS[["input: longitude"]],
                          SETTING_LABELS[["input: latitude"]], SETTING_LABELS[["input: time"]]))
  }
  inside <- FALSE
  for (key in names(SETTING_LABELS)) {
    quoted <- paste0("'", key, "'")
    if (!grepl(quoted, message, fixed = TRUE)) next
    message <- gsub(quoted, paste0("'", SETTING_LABELS[[key]], "'"), message, fixed = TRUE)
    inside <- TRUE
  }
  # An emptied field reaches R as a missing value.
  for (blank in c("(it is 'NA')", "(it is '')")) message <- sub(blank, "(it is empty)", message, fixed = TRUE)
  if (inside) fail_advanced(message) else fail(message)
}

settings_ui <- function(cfg) {
  tags <- shiny::tags
  label <- function(name) SETTING_LABELS[[name]]
  num <- function(section, key, min = NA, step = NA, name = key)
    shiny::numericInput(paste(section, key, sep = "_"), label(name), cfg[[section]][[key]], min = min, step = step)
  choices <- stats::setNames(1:12, month.name)
  months <- season_limits(cfg$period$warm_season_months)
  cell_size <- cfg$input$era5_cell_size_deg
  comma <- function(key) shiny::textInput(paste0("clustering_", key), label(key),
                                           paste(cfg$clustering[[key]], collapse = ", "))
  shiny::tagList(
    tags$div(class = "fields",
      shiny::selectInput("month_from", "First month each year", choices, months[1], selectize = FALSE),
      shiny::selectInput("month_to", "Last month each year", choices, months[2], selectize = FALSE)),
    # A new run covers every date of the data in the chosen months. The explicit dates of a reused
    # configuration still apply (the server keeps them), and the summary above names the period.
    shiny::uiOutput("period_note"),
    # A refused advanced setting, repeated here for the eye. The Review card
    # announces it, so this copy stays out of the spoken updates.
    shiny::uiOutput("advanced_problem", `aria-live` = "off"),
    tags$details(class = "advanced", tags$summary("Advanced settings"),
      tags$h3("Heatwave definition"),
      tags$div(class = "fields",
        num("heatwaves", "threshold_percentile", 0, 0.5),
        num("regional_days", "percentile", 0, 0.5, name = "regional_days: percentile"),
        num("heatwaves", "min_duration_days", 1, 1),
        num("heatwaves", "min_exceeding_days", 1, 1)),
      tags$h3("Clustering and ellipses"),
      tags$div(class = "fields",
        comma("eps_values"),
        comma("min_samples_values"),
        num("ellipses", "scale_factor", 0, 0.05)),
      tags$h3("Power-law model"),
      tags$div(class = "fields",
        num("power_law", "bootstrap_repetitions", 1, 1),
        num("power_law", "seed_daily_maxima", step = 1),
        num("power_law", "seed_event_maxima", step = 1)),
      tags$h3("Spatial model"),
      shiny::checkboxGroupInput("lgcp_covariates", label("lgcp: covariates"),
        c("Longitude" = "lon", "Latitude" = "lat", "Mean Tmax" = "mean_tmax", "Tmax standard deviation" = "std_tmax"),
        selected = cfg$lgcp$covariates, inline = TRUE),
      tags$div(class = "fields",
        num("lgcp", "raster_km", 0.000001, 1),
        num("lgcp", "window_margin_km", 0, 1)),
      shiny::conditionalPanel("output.rawInput",
        tags$h3("ERA5 preprocessing"),
        shiny::checkboxInput("native_grid", "Keep the input grid resolution", is.null(cell_size)),
        shiny::conditionalPanel("!input.native_grid",
          # A configuration that keeps the input grid has no size; the field then starts at the study's.
          shiny::numericInput("cell_size", label("era5_cell_size_deg"),
                              if (is.null(cell_size)) scorch_defaults()$input$era5_cell_size_deg else cell_size,
                              min = 0.000001, step = 0.25)),
        shiny::checkboxInput("save_tmax", label("input: processed_file"), !is.null(cfg$input$processed_file)))))
  # How the file is read is not on the page: SCORCH detects the format, units and names, asks a
  # question where the file leaves one open, and keeps the explicit entries of a reused configuration.
}

# Thousands separators for counts shown to people.
count_label <- function(n) format(n, big.mark = ",", trim = TRUE, scientific = FALSE)

# A count with its noun: "1 ellipse", "2,100 ellipses".
count_noun <- function(n, noun) paste(count_label(n), if (n == 1L) noun else paste0(noun, "s"))

# "June-August in every selected year (1996-2025)" or "June-August 2012".
period_text <- function(period) {
  if (period$n_years == 1L) paste(period$months, period$years)
  else sprintf("%s in every selected year (%s)", period$months, period$years)
}

# How the points of raw input become the cells of a run, for the dataset
# card. The rule is the reader's, in average_to_cells().
cell_note <- function(lon, lat, size) {
  if (is.null(size)) return(" and keeps this grid.")
  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) || size <= 0) return(".")
  # The stored coordinates as the reader takes them, and its rule for cells.
  cells <- tryCatch({
    axes <- raw_coordinates(lon, lat, size)
    list(lon = cell_axis(axes$lon, "longitude", size, "", axes$slack[["longitude"]]),
         lat = cell_axis(axes$lat, "latitude", size, "", axes$slack[["latitude"]]))
  }, error = function(e) NULL)
  if (is.null(cells)) return(sprintf(". This grid cannot be averaged to %g\u00b0 cells.", size))
  if (cells$lon$per_cell > 1 || cells$lat$per_cell > 1) {
    return(sprintf(" and averages complete blocks to %g\u00b0 cells.", size))
  }
  # Nothing is averaged: each point is labelled with the centre of the cell that holds it.
  sprintf(". Each point becomes one %g\u00b0 cell, labelled by its centre: %s to %s and %s to %s.", size,
          format_degrees(min(cells$lon$centre), c("E", "W")), format_degrees(max(cells$lon$centre), c("E", "W")),
          format_degrees(min(cells$lat$centre), c("N", "S")), format_degrees(max(cells$lat$centre), c("N", "S")))
}

# What SCORCH found in the selected data, as label and value pairs.
dataset_facts <- function(info, cell_size) {
  # Coordinates stored in single precision miss the grid they describe from the
  # eighth digit on (22.8999996185303 for 22.9), so the card names seven.
  tidy <- function(values) signif(as.numeric(values), 7)
  # The spacing of the rounded coordinates, or of the stored ones where a fine
  # grid needs more digits and rounding would make it uneven.
  degrees <- function(values) {
    steps <- diff(sort(tidy(values)))
    even <- all(steps > 0) && all(abs(steps - steps[1L]) <= 1e-6 * steps[1L])
    if (!even) steps <- diff(sort(as.numeric(values)))
    steps[1L]
  }
  raw <- info$format == "era5"
  hourly <- raw && any(info$per_day > 1L)
  # A variable accepted without evidence that it is the daily maximum is not called one.
  unsure <- any(info$identified %in% FALSE)
  months <- sort(unique(as.integer(format(info$dates, "%m"))))
  # The reader moves 0-360 longitudes of raw input to -180..180; a prepared file keeps its own.
  lon <- if (raw) raw_coordinates(info$lon, info$lat)$lon else even_coordinates(info$lon)$values
  files <- if (is.null(info$files)) 1L else info$files
  # One spacing is named, or that of longitude and of latitude ("0.5 x 1") where
  # the two differ by more than rounding in a file explains.
  spacing <- c(degrees(lon), degrees(info$lat))
  if (anyNA(spacing) || abs(spacing[1L] - spacing[2L]) <= 1e-3 * max(spacing)) spacing <- spacing[1L]
  # The card says what the data are in a few words: an hourly file is still named hourly, and a
  # variable that the file does not identify as the daily maximum is still named, with that caveat.
  c("Contains" = paste0(
      if (hourly) sprintf("%s, %s values per day", if (unsure) "Sub-daily temperature" else "Raw 2 m temperature",
                          paste(info$per_day, collapse = " or "))
      else if (unsure) "Daily temperature"
      else if (raw) "Daily ERA5 2 m temperature" else "daily maximum temperature",
      if (unsure) sprintf(" (variable %s, not identified as daily maximum temperature)", paste(info$variable, collapse = ", ")),
      if (files > 1L) sprintf(", in %d NetCDF files", files)),
    "Date coverage" = sprintf("%s to %s: %s days, %s", min(info$dates), max(info$dates), count_label(length(info$dates)),
                              if (length(months) == 12L) "every month of the year" else month_runs(months)),
    "Grid" = sprintf("%d by %d %s at %s, %s to %s and %s to %s", length(info$lon), length(info$lat),
                     if (raw) "points" else "cells", paste0(sprintf("%.7g", spacing), "\u00b0", collapse = " x "),
                     format_degrees(tidy(min(lon)), c("E", "W")), format_degrees(tidy(max(lon)), c("E", "W")),
                     format_degrees(tidy(min(info$lat)), c("N", "S")), format_degrees(tidy(max(info$lat)), c("N", "S"))),
    # The unit SCORCH analyzes, and the conversion where the file holds Kelvin.
    "Temperature" = if (all(info$units == "K")) "K \u2192 \u00b0C" else "\u00b0C",
    # One value per day is taken as that day's maximum, whichever daily statistic the file holds.
    if (raw) c("Preparation" = paste0(
      if (hourly) "SCORCH takes the maximum of each UTC day" else "SCORCH uses each daily value as that day's maximum",
      cell_note(info$lon, info$lat, cell_size))))
}

# What the results say about a variable that the file itself does not
# identify as the temperature SCORCH analyzes: the user chose it, in the
# question about the file, in a field or with a configuration.
temperature_note <- function(info) {
  if (!any(info$identified %in% FALSE)) return(NULL)
  sprintf("Variable %s, as you chose. The file does not identify it as %s", paste(info$variable, collapse = ", "),
          if (info$format == "era5" && any(info$per_day > 1L)) "2 m air temperature; its highest value of each day was used."
          else "daily maximum temperature.")
}

dataset_card <- function(title, path, info, bundled, note = NULL, answers = NULL, cell_size = 1) {
  tags <- shiny::tags
  failed <- inherits(info, "error")
  asking <- !failed && length(info$questions) > 0L
  facts <- if (!failed && !asking) dataset_facts(info, cell_size)
  tags$section(class = "card dataset", role = "status",
    tags$div(class = "section-title",
      tags$div(tags$span(class = paste("eyebrow", if (bundled) "bundled"),
                         if (bundled) "Included example" else "Your data"),
               tags$h2(title)),
      shiny::actionButton("change_data", "Change data", class = "link-button")),
    if (bundled) tags$p(paste(
      "ERA5-derived daily maximum temperature on 1,800 one-degree cells between 25\u201350\u00b0N and 130\u201358\u00b0W.",
      "It is bundled with scoRch.simulator and archived at doi.org/10.5281/zenodo.24232552."))
    else tags$code(class = "path", paste(path, collapse = ", ")),
    note,
    if (failed) tags$p(class = "problem", role = "alert", conditionMessage(info)),
    if (length(facts)) tags$dl(class = "facts", lapply(names(facts), function(name) {
      shiny::tagList(tags$dt(name), tags$dd(facts[[name]]))
    })),
    if (length(answers)) tags$p(class = "fine answered",
      "Your answer: ", paste(sprintf("%s = %s", names(answers), unlist(answers)), collapse = ", "), ". ",
      shiny::actionButton("answer_reset", "Ask again", class = "link-button")))
}

question_card <- function(question) {
  tags <- shiny::tags
  tags$section(class = "card question",
    tags$span(class = "eyebrow", "One question about this file"),
    tags$h2(question$title), tags$p(question$detail),
    shiny::radioButtons("answer", NULL, choices = question$choices,
                        selected = if (is.null(question$recommended) || is.na(question$recommended)) character(0)
                                   else question$recommended),
    if (!is.null(question$recommended) && !is.na(question$recommended))
      tags$p(class = "fine", "The suggested answer comes from the numbers in the file."),
    # Filled when the button is clicked before an answer is chosen.
    shiny::uiOutput("answer_problem"),
    shiny::actionButton("answer_apply", "Use this answer", class = "btn-primary"))
}

# Twelve months: analyzed, in the file but not analyzed, or not in the file.
period_note <- function(period) {
  tags <- shiny::tags
  one_year <- period$n_years == 1L
  tags$div(class = "period",
    tags$ol(class = "months", `aria-hidden` = "true", lapply(1:12, function(month) {
      tags$li(class = if (month %in% period$selected && month %in% period$in_file) "on"
                      else if (month %in% period$in_file) "off" else "none", substr(month.name[month], 1L, 1L))
    })),
    tags$p(class = "period-line",
      tags$strong(if (one_year) paste0(period$months, " ", period$years, ":")
                  else paste(period$months, "in every selected year:")),
      if (one_year) paste0(count_noun(period$n_days, "day"), ".")
      else sprintf("%d years (%s), %s days.", period$n_years, period$years, count_label(period$n_days))),
    if (nzchar(period$unused)) tags$p(class = "fine", sprintf("This file also holds %s. Those months are not analyzed.", period$unused)),
    if (nzchar(period$absent)) tags$p(class = "fine", sprintf("This file has no data for %s.", period$absent)))
}

# The finished run: outcome, the four types, where the files are, and what
# was analyzed with which settings.
completion_ui <- function(finished) {
  tags <- shiny::tags
  result <- finished$result
  stopped <- !is.null(finished$problem)
  status <- if (stopped) "stopped" else if (run_status(result) == "COMPLETE") "complete" else "warnings"
  pairs <- function(lines) {
    lines <- trimws(lines)
    tags$dl(class = "facts", lapply(lines, function(line) {
      shiny::tagList(tags$dt(sub(":.*$", "", line)), tags$dd(sub("^[^:]*: *", "", line)))
    }))
  }
  shiny::tagList(
    tags$section(class = paste("card outcome", status),
      tags$span(class = "eyebrow", finished$dataset),
      # The heading can take the keyboard focus when the results replace the form.
      tags$h1(id = "results-title", tabindex = "-1",
              c(stopped = "Analysis stopped", complete = "Analysis complete",
                warnings = "Analysis complete with warnings")[[status]]),
      if (stopped) tags$p(class = "problem", role = "alert", finished$problem)
      else tags$p(class = "lead", sprintf("%s from %s on %s.",
        count_noun(nrow(result$events$events), "compound event"), count_noun(nrow(result$ellipses), "ellipse"),
        count_noun(nrow(result$regional$selected_days), "regionally extensive day"))),
      if (!stopped) tags$ol(class = "type-counts", lapply(1:4, function(type) {
        n <- sum(result$events$events$event_type == type)
        tags$li(style = paste0("--type:", TYPE_COLOURS[type]),
                tags$span(class = "type-n", paste("Type", type)), tags$strong(count_label(n)),
                tags$span(class = "type-word", if (n == 1L) "event" else "events"),
                tags$span(class = "type-name", TYPE_NAMES[type]))
      })),
      tags$h2("Results folder"),
      tags$code(class = "path", finished$folder),
      tags$div(class = "actions",
        shiny::actionButton("open_summary", "Open summary", class = "btn-primary"),
        shiny::actionButton("open_folder", "Open results folder"),
        shiny::actionButton("again", "Set up another run")),
      tags$p(class = "fine", paste(
        "Summary.txt records this run in full and config.yaml holds its settings for reuse.",
        if (!stopped) "The catalogs, model workbooks and figures are in the same folder."))),
    # What was analyzed, stage by stage and with which settings, is in Summary.txt and config.yaml.
    if (!stopped) tags$div(id = "explorer-run", class = "explorer", `data-explorer` = "run"))
}
