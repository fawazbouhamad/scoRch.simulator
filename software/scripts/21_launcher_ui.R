scorch_ui <- function() {
  tags <- shiny::tags
  style <- system.file("launcher", "style.css", package = "scoRch.simulator")
  shiny::fluidPage(
    tags$head(tags$title("SCORCH | Heatwave analysis"),
              if (nzchar(style)) shiny::includeCSS(style)),
    tags$main(class = "scorch",
      tags$header(tags$div(class = "brand", "SCORCH"), tags$span("Heatwave analysis")),
      shiny::conditionalPanel("!output.finished",
        tags$section(tags$div(class = "section-title", tags$h1("Choose your data"),
          shiny::actionButton("load_config", "Load saved settings", class = "link-button")),
          tags$p("Open a temperature dataset on this computer or try the North America example."),
          tags$div(class = "actions",
            shiny::actionButton("browse_file", "Choose NetCDF or ZIP", class = "btn-primary"),
            shiny::actionButton("browse_folder", "Choose ERA5 folder"),
            shiny::actionButton("use_example", "Use included example")),
          shiny::uiOutput("selection")),
        shiny::conditionalPanel("output.selected",
          tags$section(tags$h2("Analysis settings"), shiny::uiOutput("settings")),
          tags$section(tags$div(class = "section-title", tags$h2("Results folder"),
                       shiny::actionButton("browse_output", "Choose folder")),
                       tags$p("SCORCH creates a new folder for this run."),
                       tags$code(class = "path", shiny::textOutput("destination", inline = TRUE))),
          tags$section(class = "review", tags$h2("Review and run"), shiny::uiOutput("review"),
                       shiny::uiOutput("run_button")))),
      shiny::uiOutput("completion"),
      tags$footer(shiny::actionButton("done", "Close", class = "link-button"))))
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
    shiny::checkboxInput("limit_dates", "Limit the analysis dates",
                         !is.null(dates$start_date) || !is.null(dates$end_date)),
    shiny::conditionalPanel("input.limit_dates", shiny::uiOutput("date_fields")),
    tags$div(class = "fields",
      num("heatwaves", "threshold_percentile", "Local temperature percentile", 0, 0.5),
      num("regional_days", "percentile", "Regional extent percentile", 0, 0.5),
      num("heatwaves", "min_duration_days", "Minimum heatwave duration (days)", 1, 1),
      num("heatwaves", "min_exceeding_days", "Minimum days at or above threshold", 1, 1)),
    tags$details(tags$summary("Input settings"),
      tags$div(class = "fields",
        shiny::selectInput("input_format", "Data format",
          c("Detect automatically" = "auto", "Prepared daily Tmax" = "daily_tmax", "ERA5 temperature" = "era5"),
          cfg$input$format, selectize = FALSE),
        shiny::selectInput("input_units", "Temperature units",
          c("Read from file" = "auto", "Degrees Celsius" = "degC", "Kelvin" = "K",
            if (!cfg$input$units %in% c("auto", "degC", "K")) cfg$input$units),
          cfg$input$units, selectize = FALSE),
        override("variable", "Temperature variable"), override("time", "Time coordinate"),
        override("longitude", "Longitude coordinate"), override("latitude", "Latitude coordinate"))),
    shiny::conditionalPanel("output.rawInput",
      tags$details(tags$summary("ERA5 preprocessing"),
        shiny::checkboxInput("native_grid", "Keep the input grid resolution", is.null(cell_size)),
        shiny::conditionalPanel("!input.native_grid",
          shiny::numericInput("cell_size", "Output grid size (degrees)", if (is.null(cell_size)) 1 else cell_size,
                              min = 0.000001, step = 0.25)),
        shiny::checkboxInput("save_tmax", "Save prepared daily Tmax in this run folder",
                             !is.null(cfg$input$processed_file)))),
    tags$details(tags$summary("Advanced analysis settings"),
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
        num("lgcp", "window_margin_km", "Window margin (km)", 0, 1))))
}

scorch_picker <- function(input, session, selected) {
  # Browse local paths without uploading data through the viewer. Starting
  # locations and a direct path field also work outside RStudio Desktop.
  state <- shiny::reactiveVal(NULL)
  listing <- shiny::reactiveVal(character())
  user_home <- if (.Platform$OS.type == "windows" && nzchar(Sys.getenv("USERPROFILE")))
    Sys.getenv("USERPROFILE") else path.expand("~")
  places <- c(Downloads = file.path(user_home, "Downloads"), Project = getwd(), Home = user_home)
  places <- places[dir.exists(places)]
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS, ":/")
    drives <- drives[dir.exists(drives)]
    places <- c(places, stats::setNames(drives, drives))
  } else places <- c(places, "File system" = "/")
  browse <- function(kind, folder = NULL) {
    if (is.null(folder) || !dir.exists(folder)) folder <- unname(places[1])
    folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
    state(list(kind = kind, folder = folder))
    paths <- list.files(folder, full.names = TRUE, no.. = TRUE)
    dirs <- dir.exists(paths)
    ext <- switch(kind, data = "[.](nc|zip)$", config = "[.]ya?ml$", "(?!)")
    paths <- c(paths[dirs], paths[!dirs & grepl(ext, paths, perl = TRUE, ignore.case = TRUE)])
    listing(paths)
    choices <- stats::setNames(paths,
      ifelse(dir.exists(paths), paste0(basename(paths), "/"), basename(paths)))
    is_folder <- kind %in% c("era5", "output")
    title <- switch(kind, data = "Choose a NetCDF or ZIP file", config = "Choose a configuration",
                    era5 = "Choose an ERA5 folder", output = "Choose the results location")
    shiny::showModal(shiny::modalDialog(title = title,
      shiny::tags$div(class = "picker-tools", shiny::selectInput("pick_place", "Go to",
        c("Choose a location" = "", places), selected = "", selectize = FALSE),
        shiny::actionButton("pick_up", "Up one folder")),
      shiny::tags$div(class = "picker-tools", shiny::textInput("pick_path", "Current folder", folder),
                      shiny::actionButton("pick_go", "Go")),
      shiny::tags$div(ondblclick = "document.getElementById('pick_open').click()",
        shiny::selectInput("pick_entry", "Folders and files", choices = choices, selected = character(),
                           size = 9, selectize = FALSE)),
      if (!length(paths)) shiny::tags$p("No matching files or folders here."),
      footer = shiny::tagList(shiny::modalButton("Cancel"),
        shiny::actionButton("pick_open", if (is_folder) "Open selected folder" else "Open selected item",
                            class = if (!is_folder) "btn-primary"),
        if (is_folder) shiny::actionButton("pick_here", "Select current folder", class = "btn-primary"))),
      session = session)
  }
  choose <- function(path) {
    kind <- state()$kind
    shiny::removeModal(session)
    selected(kind, path)
  }
  shiny::observeEvent(input$pick_place, {
    if (nzchar(input$pick_place)) browse(state()$kind, input$pick_place)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pick_up, browse(state()$kind, dirname(state()$folder)), ignoreInit = TRUE)
  shiny::observeEvent(input$pick_go, {
    if (!dir.exists(input$pick_path)) {
      shiny::showNotification("That folder could not be opened.", type = "error", session = session)
    } else if (!identical(normalizePath(input$pick_path, winslash = "/"), state()$folder)) {
      browse(state()$kind, input$pick_path)
    }
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pick_open, {
    path <- input$pick_entry
    if (length(path) != 1L || !path %in% listing()) return()
    if (dir.exists(path)) browse(state()$kind, path) else choose(path)
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$pick_here, choose(state()$folder), ignoreInit = TRUE)
  browse
}
