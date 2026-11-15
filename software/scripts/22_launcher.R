scorch_launcher <- function() {
  app <- shiny::shinyApp(ui = scorch_ui(), server = function(input, output, session) {
    result <- scorch_server(input, output, session)
    session$onSessionEnded(function() shiny::stopApp(shiny::isolate(result())))
  })
  invisible(shiny::runApp(app, host = "127.0.0.1", display.mode = "normal",
    launch.browser = shiny::dialogViewer("SCORCH | Heatwave analysis", width = 940, height = 780)))
}

scorch_server <- function(input, output, session) {
  tags <- shiny::tags
  base <- shiny::reactiveVal(read_config(input_required = FALSE))
  state <- shiny::reactiveValues(source = NULL, path = NULL, parent = guided_path("output"),
                                 loaded = NULL, error = NULL, finished = NULL, busy = FALSE)
  report <- function(e) state$error <- conditionMessage(e)
  open_source <- function(path = NULL) {
    close_temperature_source(state$source)
    state$source <- NULL
    state$path <- path
    state$error <- NULL
    actual <- if (is.null(path)) system.file("extdata", "north_america_tmax.nc",
                 package = "scoRch.simulator", mustWork = TRUE) else path
    if (!all(file.exists(actual))) fail("The configured data could not be found. Choose the data file or folder.")
    if (any(grepl("[.]zip$", actual, ignore.case = TRUE))) {
      state$source <- shiny::withProgress(message = "Opening ZIP", value = 0,
        open_temperature_source(actual, function(i, total, name) {
          shiny::setProgress(value = (i - 1) / total,
                             detail = sprintf("Extracting %d of %d: %s", i, total, name), session = session)
        }), session = session)
    } else state$source <- open_temperature_source(actual)
  }
  choose <- function(kind, path) {
    tryCatch({
      if (kind == "output") {
        state$parent <- guided_path(path)
      } else if (kind == "config") {
        cfg <- load_guided_config(path)
        base(cfg)
        state$loaded <- basename(path)
        state$error <- NULL
        parent <- cfg$output$directory
        state$parent <- if (grepl("^SCORCH_run_[0-9]+$", basename(parent))) dirname(parent) else parent
        # When the user picked data first, importing settings keeps that choice.
        if (is.null(state$source)) open_source(cfg$input$file)
      } else open_source(guided_path(path))
    }, error = report)
  }
  picker <- scorch_picker(input, session, choose)
  shiny::observeEvent(input$browse_file, picker("data"))
  shiny::observeEvent(input$browse_folder, picker("era5"))
  shiny::observeEvent(input$load_config, picker("config"))
  shiny::observeEvent(input$browse_output, picker("output", if (dir.exists(state$parent)) state$parent else getwd()))
  shiny::observeEvent(input$use_example, {
    tryCatch({
      open_source()
      # A previously chosen custom variable or coordinate belongs to its file.
      for (name in c("format", "units")) shiny::updateSelectInput(session, paste0("input_", name), selected = "auto")
      for (name in c("variable", "longitude", "latitude", "time"))
        shiny::updateTextInput(session, paste0("input_", name), value = "auto")
    }, error = report)
  })
  header <- shiny::reactive({
    shiny::req(state$source)
    settings <- input_controls(input, base()$input)
    settings["file"] <- list(state$path)
    tryCatch(inspect_temperature_source(state$source, settings), error = identity)
  })
  output$selected <- shiny::reactive(!is.null(state$source))
  output$finished <- shiny::reactive(!is.null(state$finished))
  output$rawInput <- shiny::reactive(!is.null(state$source) && !inherits(header(), "error") && header()$format == "era5")
  for (id in c("selected", "finished", "rawInput")) shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  output$settings <- shiny::renderUI(settings_ui(base()))
  output$date_fields <- shiny::renderUI({
    info <- header()
    if (inherits(info, "error")) return()
    period <- base()$period
    shiny::dateRangeInput("dates", "Analysis dates",
      start = if (is.null(period$start_date)) min(info$dates) else period$start_date,
      end = if (is.null(period$end_date)) max(info$dates) else period$end_date)
  })
  output$selection <- shiny::renderUI({
    note <- if (!is.null(state$loaded)) tags$p("Loaded settings from ", tags$strong(state$loaded), ".")
    problem <- if (!is.null(state$error)) tags$p(class = "problem", role = "alert", state$error)
    if (is.null(state$source)) return(shiny::tagList(note, problem))
    title <- if (is.null(state$path)) "Included North America example" else paste(state$path, collapse = ", ")
    info <- header()
    tags$div(class = "data-info", role = "status",
      note, problem, tags$strong(title),
      if (inherits(info, "error")) tags$p(class = "problem", conditionMessage(info)) else
        tags$p(sprintf("%s | %d NetCDF file%s | %s to %s",
          if (info$format == "era5") "ERA5 temperature" else "Prepared daily Tmax",
          length(state$source$files), if (length(state$source$files) == 1L) "" else "s",
          min(info$dates), max(info$dates))),
      if (!inherits(info, "error")) tags$p(sprintf("Variable %s | Units %s | Input grid %d longitudes by %d latitudes",
        paste(info$variable, collapse = ", "), paste(info$units, collapse = ", "), length(info$lon), length(info$lat))))
  })
  dest <- shiny::reactive({
    state$finished # Recalculate the preview after reserving a run folder.
    next_run_folder(state$parent)
  })
  output$destination <- shiny::renderText(dest())
  plan <- shiny::reactive({
    info <- header()
    if (inherits(info, "error")) return(info)
    shiny::req(input$month_from, input$clustering_eps_values)
    tryCatch(guided_settings(base(), input, state$path, info, dest()), error = identity)
  })
  output$review <- shiny::renderUI({
    p <- plan()
    if (inherits(p, "error")) return(tags$p(class = "problem", role = "alert", conditionMessage(p)))
    cfg <- p$config
    tags$p(sprintf("%s | %s | %s to %s | %s selected dates",
      if (is.null(state$path)) "North America example" else paste(basename(state$path), collapse = ", "),
      paste(month.abb[cfg$period$warm_season_months], collapse = ", "), min(p$dates), max(p$dates),
      format(length(p$dates), big.mark = ",")))
  })
  output$run_button <- shiny::renderUI({
    if (inherits(plan(), "error") || state$busy)
      return(tags$button(class = "btn btn-primary", disabled = "disabled", "Run analysis"))
    shiny::actionButton("start", "Run analysis", class = "btn-primary")
  })
  shiny::observeEvent(input$start, {
    if (state$busy || !is.null(state$finished) || is.null(state$source)) return()
    p <- plan()
    if (inherits(p, "error")) return()
    state$busy <- TRUE
    on.exit({ state$busy <- FALSE }, add = TRUE)
    state$error <- NULL
    tryCatch({
      progress <- shiny::Progress$new(session)
      on.exit(progress$close(), add = TRUE)
      progress$set(message = "Running SCORCH", value = 0, detail = "Reading the selected data")
      steps <- c("Reading ", "Stage 1:", "Stage 2:", "Stage 3:", "Stage 4:", "Stage 5:",
                 "Power-law fits", "Log-Gaussian", "Writing the result", "Plot ")
      state$finished <- withCallingHandlers(execute_guided_run(p, state$parent, state$source),
        message = function(m) {
          line <- sub("^[0-9:]+ ", "", trimws(conditionMessage(m)))
          stage <- which(startsWith(line, steps))
          if (length(stage)) progress$set(value = stage[1] / (length(steps) + 1), detail = line)
        })
    }, error = report)
  })
  output$completion <- shiny::renderUI({
    finished <- state$finished
    shiny::req(finished)
    result <- finished$result
    tags$section(tags$h1(if (!is.null(finished$problem)) "Analysis stopped"
      else if (run_status(result) == "COMPLETE") "Analysis complete" else "Analysis complete with warnings"),
      if (!is.null(finished$problem)) tags$p(class = "problem", finished$problem)
      else tags$p(sprintf("%d compound events and %d ellipses.", nrow(result$events$events), nrow(result$ellipses))),
      tags$code(class = "path", finished$folder),
      tags$p("Summary.txt records this run. Load config.yaml to reuse its settings."),
      tags$div(class = "actions",
        shiny::actionButton("open_summary", "Open summary", class = "btn-primary"),
        shiny::actionButton("open_folder", "Open results folder"),
        shiny::actionButton("again", "Set up another run")))
  })
  show_file <- function(path) tryCatch(open_run_file(path), error = function(e)
    shiny::showNotification(conditionMessage(e), type = "error", session = session))
  shiny::observeEvent(input$open_summary, show_file(state$finished$summary))
  shiny::observeEvent(input$open_folder, show_file(state$finished$folder))
  shiny::observeEvent(input$again, { state$finished <- NULL })
  shiny::observeEvent(input$done, shiny::stopApp(state$finished$result))
  session$onSessionEnded(function() shiny::isolate(close_temperature_source(state$source)))
  shiny::reactive(state$finished$result)
}
