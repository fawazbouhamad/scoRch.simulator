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

scorch_ui <- function() {
  tags <- shiny::tags
  assets <- system.file("launcher", package = "scoRch.simulator")
  shiny::addResourcePath("scorch", assets)
  # A newer installation must not reuse a cached script or stylesheet.
  stamp <- paste0("?v=", utils::packageVersion("scoRch.simulator"), "-",
                  as.integer(max(file.mtime(list.files(assets, full.names = TRUE)))))
  asset <- function(name) paste0("scorch/", name, stamp)
  shiny::fluidPage(
    tags$head(tags$title("SCORCH | Regional compound heatwaves"),
              tags$link(rel = "icon", type = "image/svg+xml",
                        href = paste0("data:image/svg+xml,", utils::URLencode(
                          sub("<svg ", "<svg xmlns=\"http://www.w3.org/2000/svg\" ", BRAND_MARK, fixed = TRUE), reserved = TRUE))),
              tags$link(rel = "stylesheet", href = asset("style.css")),
              tags$script(src = asset("scorch.js"), charset = "utf-8", defer = NA)),
    tags$main(class = "scorch", `data-showcase` = asset("showcase.json"),
      tags$header(class = "masthead", shiny::HTML(BRAND_MARK),
        tags$div(tags$div(class = "brand", "SCORCH"),
                 tags$div(class = "brand-name", "Spatiotemporal Classification of Regional Compound Heatwaves"))),
      shiny::conditionalPanel("output.screen == 'welcome'",
        tags$section(class = "hero",
          tags$div(class = "hero-text",
            tags$h1("Find, map and classify regional compound heatwaves"),
            tags$p(class = "lead", paste(
              "SCORCH reads gridded daily maximum temperature, finds the days when heatwaves cover an unusually",
              "large share of a region, fits an ellipse to every cluster of heatwave cells and assigns each event",
              "to one of four types.")),
            tags$p(class = "fine", "Files are read on this computer. Nothing is analyzed until you click Run analysis.")),
          tags$div(class = "choices",
            tags$div(shiny::actionButton("browse_file", "Choose your own data", class = "btn-primary btn-lg"),
                     tags$p("A prepared NetCDF, a raw ERA5 NetCDF, a ZIP, or a folder of ERA5 files.")),
            tags$div(shiny::actionButton("use_example", "Use included example", class = "btn-lg"),
                     tags$p("North America, April\u2013September 1996\u20132025, bundled with the package."))),
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
                                shiny::actionButton("done", "Close", class = "link-button")))))
}

settings_ui <- function(cfg) {
  tags <- shiny::tags
  num <- function(section, key, label, min = NA, step = NA)
    shiny::numericInput(paste(section, key, sep = "_"), label, cfg[[section]][[key]], min = min, step = step)
  choices <- stats::setNames(1:12, month.name)
  months <- season_limits(cfg$period$warm_season_months)
  dates <- cfg$period
  cell_size <- cfg$input$era5_cell_size_deg
  override <- function(key, label) shiny::textInput(paste0("input_", key), label, cfg$input[[key]])
  comma <- function(key, label) shiny::textInput(paste0("clustering_", key), label,
                                                  paste(cfg$clustering[[key]], collapse = ", "))
  shiny::tagList(
    tags$div(class = "fields",
      shiny::selectInput("month_from", "First month each year", choices, months[1], selectize = FALSE),
      shiny::selectInput("month_to", "Last month each year", choices, months[2], selectize = FALSE)),
    shiny::uiOutput("period_note"),
    shiny::checkboxInput("limit_dates", "Limit the analysis dates",
                         !is.null(dates$start_date) || !is.null(dates$end_date)),
    shiny::conditionalPanel("input.limit_dates", shiny::uiOutput("date_fields")),
    tags$details(class = "advanced", tags$summary("Advanced settings"),
      tags$p(class = "fine", paste("The defaults reproduce the SCORCH study method.",
                                   "Review them when you change the region, season or grid resolution.")),
      tags$h3("Heatwave definition"),
      tags$div(class = "fields",
        num("heatwaves", "threshold_percentile", "Local temperature percentile", 0, 0.5),
        num("regional_days", "percentile", "Regional extent percentile", 0, 0.5),
        num("heatwaves", "min_duration_days", "Minimum heatwave duration (days)", 1, 1),
        num("heatwaves", "min_exceeding_days", "Minimum days at or above threshold", 1, 1)),
      tags$h3("Clustering and ellipses"),
      tags$div(class = "fields",
        comma("eps_values", "Candidate distances (grid cells)"),
        comma("min_samples_values", "Candidate neighborhood sizes"),
        num("ellipses", "scale_factor", "Ellipse scale factor", 0, 0.05)),
      tags$h3("Power-law model"),
      tags$div(class = "fields",
        num("power_law", "bootstrap_repetitions", "Bootstrap repetitions", 1, 1),
        num("power_law", "seed_daily_maxima", "Random seed for daily maxima", step = 1),
        num("power_law", "seed_event_maxima", "Random seed for event maxima", step = 1)),
      tags$h3("Spatial model"),
      shiny::checkboxGroupInput("lgcp_covariates", "Predictors",
        c("Longitude" = "lon", "Latitude" = "lat", "Mean Tmax" = "mean_tmax", "Tmax standard deviation" = "std_tmax"),
        selected = cfg$lgcp$covariates, inline = TRUE),
      tags$div(class = "fields",
        num("lgcp", "raster_km", "Raster cell size (km)", 0.000001, 1),
        num("lgcp", "window_margin_km", "Window margin (km)", 0, 1)),
      shiny::conditionalPanel("output.rawInput",
        tags$h3("ERA5 preprocessing"),
        shiny::checkboxInput("native_grid", "Keep the input grid resolution", is.null(cell_size)),
        shiny::conditionalPanel("!input.native_grid",
          shiny::numericInput("cell_size", "Output grid size (degrees)", if (is.null(cell_size)) 1 else cell_size,
                              min = 0.000001, step = 0.25)),
        shiny::checkboxInput("save_tmax", "Save prepared daily Tmax in this run folder",
                             !is.null(cfg$input$processed_file))),
      tags$h3("How the file is read"),
      tags$p(class = "fine", "SCORCH detects these from the file. Change one only if the summary above is wrong."),
      tags$div(class = "fields",
        shiny::selectInput("input_format", "Data format",
          c("Detect automatically" = "auto", "Prepared daily Tmax" = "daily_tmax", "ERA5 temperature" = "era5"),
          cfg$input$format, selectize = FALSE),
        shiny::selectInput("input_units", "Temperature units",
          c("Read from file" = "auto", "Degrees Celsius" = "degC", "Kelvin" = "K",
            if (!cfg$input$units %in% c("auto", "degC", "K")) cfg$input$units),
          cfg$input$units, selectize = FALSE),
        override("variable", "Temperature variable"), override("time", "Time coordinate"),
        override("longitude", "Longitude coordinate"), override("latitude", "Latitude coordinate"))))
}

# Thousands separators for counts shown to people.
count_label <- function(n) format(n, big.mark = ",", trim = TRUE, scientific = FALSE)

# "June-August in every selected year (1996-2025)" or "June-August 2012".
period_text <- function(period) {
  if (period$n_years == 1L) paste(period$months, period$years)
  else sprintf("%s in every selected year (%s)", period$months, period$years)
}

# What SCORCH found in the selected data, as label and value pairs.
dataset_facts <- function(info, cell_size) {
  degrees <- function(values) abs(diff(sort(values))[1L])
  raw <- info$format == "era5"
  hourly <- raw && any(info$per_day > 1L)
  months <- sort(unique(as.integer(format(info$dates, "%m"))))
  lon <- wrap_longitudes(as.numeric(info$lon))$lon
  files <- if (is.null(info$files)) 1L else info$files
  c("Contains" = paste0(
      if (hourly) sprintf("Raw 2 m temperature, %s values per day", paste(info$per_day, collapse = " or "))
      else if (raw) "Daily ERA5 2 m temperature" else "Prepared daily maximum temperature",
      sprintf(" (variable %s)", paste(info$variable, collapse = ", ")),
      if (files > 1L) sprintf(", in %d NetCDF files", files)),
    "Date coverage" = sprintf("%s to %s: %s days, %s", min(info$dates), max(info$dates), count_label(length(info$dates)),
                              if (length(months) == 12L) "every month of the year" else month_runs(months)),
    "Grid" = sprintf("%d by %d %s at %g\u00b0, %s to %s and %s to %s", length(info$lon), length(info$lat),
                     if (raw) "points" else "cells",
                     degrees(info$lon), format_degrees(min(lon), c("E", "W")), format_degrees(max(lon), c("E", "W")),
                     format_degrees(min(info$lat), c("N", "S")), format_degrees(max(info$lat), c("N", "S"))),
    "Temperature" = paste0(
      if (all(info$units == "K")) "Kelvin in the file. SCORCH converts it to \u00b0C once, when it reads the data."
      else "\u00b0C in the file, used as it is.",
      if (!anyNA(info$sample_c)) sprintf(" The first time step ranges from %.1f to %.1f \u00b0C.",
                                          info$sample_c[1L], info$sample_c[2L])),
    if (raw) c("Preparation" = paste0(
      if (hourly) "SCORCH takes the maximum of each UTC day" else "Daily values are used as they are",
      if (is.null(cell_size)) " and keeps this grid." else sprintf(" and averages complete blocks to %g\u00b0 cells.", cell_size))))
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
      if (one_year) sprintf("%s days.", count_label(period$n_days))
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
      tags$h1(c(stopped = "Analysis stopped", complete = "Analysis complete",
                warnings = "Analysis complete with warnings")[[status]]),
      if (stopped) tags$p(class = "problem", role = "alert", finished$problem)
      else tags$p(class = "lead", sprintf("%s compound events from %s ellipses on %s regionally extensive days.",
        count_label(nrow(result$events$events)), count_label(nrow(result$ellipses)),
        count_label(nrow(result$regional$selected_days)))),
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
    if (!stopped) tags$section(class = "card summary",
      tags$h2("What was analyzed"),
      pairs(c(paste("Dataset:", finished$dataset), readme_input(result, result$config)[-1L],
              if (!is.null(finished$period)) sprintf("Months analyzed: %s", period_text(finished$period)),
              sprintf("Finished in: %s", if (result$elapsed_minutes < 2) sprintf("%.0f seconds", result$elapsed_minutes * 60)
                                          else sprintf("%.1f minutes", result$elapsed_minutes)))),
      tags$h3("Results by stage"),
      pairs(c(sprintf("Local heatwaves: %s", count_label(nrow(result$heatwaves$events))),
              sprintf("Regionally extensive days: %s of %s days (at least %s heatwave cells)",
                      count_label(nrow(result$regional$selected_days)), count_label(nrow(result$regional$daily)),
                      count_label(result$regional$threshold_cells)),
              sprintf("Clusters and ellipses: %s", count_label(nrow(result$ellipses))),
              sprintf("Compound events: %s", count_label(nrow(result$events$events))))),
      tags$h3("Settings"),
      pairs(readme_settings(result, result$config)),
      if (!identical(readme_warnings(result), "  None.")) shiny::tagList(tags$h3("Warnings"),
        tags$ul(class = "problem", lapply(trimws(readme_warnings(result)), tags$li)))),
    if (!stopped) tags$div(id = "explorer-run", class = "explorer", `data-explorer` = "run"))
}
