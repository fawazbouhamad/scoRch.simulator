# An installation made after this session loaded the package leaves old code
# in memory beside new files on disk. Say so instead of opening a broken window.
session_built <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  session_built$stamp <- suppressWarnings(utils::packageDescription(pkgname, lib.loc = libname, fields = "Built"))
}

stale_session <- function() {
  folder <- dirname(getNamespaceInfo("scoRch.simulator", "path"))
  now <- suppressWarnings(utils::packageDescription("scoRch.simulator", lib.loc = folder, fields = "Built"))
  loaded <- session_built$stamp
  if (is.character(loaded) && !is.na(loaded) && is.character(now) && !is.na(now) && !identical(now, loaded)) {
    fail("scoRch.simulator was updated after this R session loaded it.\n",
         "Restart R (in RStudio: Session > Restart R), then run scoRch.simulator::run_scorch()\n",
         "The new version is already installed; do not run the installation block again.")
  }
  invisible(NULL)
}

# A secret for the address of one window. It is drawn without touching the
# session's random numbers: their state is put aside first, so a set.seed()
# of the user neither fixes the token nor is moved on by it.
launch_token <- function() {
  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv()) else NULL
  on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
          else assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  if (!is.null(old_seed)) rm(".Random.seed", envir = globalenv())
  symbols <- c(letters, LETTERS, 0:9)
  draw <- sample.int(length(symbols), 32L, replace = TRUE)
  # R starts an unset generator from the clock and the process number, which
  # another program could work out. The draw is therefore shifted by what such
  # a program does not have: bytes of the system's random device where there
  # is one (then the token cannot be guessed), and the random name of this
  # session's temporary folder, which on Windows lies in the user's profile.
  device <- suppressWarnings(tryCatch(as.integer(readBin("/dev/urandom", "raw", 32L)), error = function(e) integer()))
  spread <- function(values) if (length(values) && !anyNA(values)) rep_len(values, 32L) else 0L
  draw <- draw + spread(utf8ToInt(basename(tempdir()))) + spread(device)
  paste(symbols[draw %% length(symbols) + 1L], collapse = "")
}

# Does the query of an address ("?token=...") carry the token of the window?
# A query that cannot be read does not.
holds_token <- function(query, token) {
  identical(tryCatch(shiny::parseQueryString(query)[["token"]], error = function(e) NULL), token)
}

# What a request for the page receives: the window, or, without its token,
# one plain sentence about the address (status 403).
scorch_page <- function(page, token, seen = NULL) {
  refused <- shiny::httpResponse(403L, "text/plain; charset=UTF-8", paste(
    "This address belongs to an R session on this computer and opens only with the key that R adds to it.",
    "To open the SCORCH window, run scoRch.simulator::run_scorch() in R."))
  function(request) {
    if (holds_token(request$QUERY_STRING, token)) return(page)
    if (is.environment(seen)) seen$refused <- TRUE
    refused
  }
}

# The app behind one window. Its address holds a secret, `token`: without it
# there is no page and no session, so another program on this computer can
# neither use the window nor close it. `quit` ends the app `grace` seconds
# after the last window has gone; a reload or a second tab keeps it open.
# `seen` notes whether the page was refused and whether a window has opened.
scorch_app <- function(token, kept, quit = shiny::stopApp, grace = 2, seen = new.env(parent = emptyenv())) {
  windows <- 0L
  shiny::shinyApp(
    ui = scorch_page(scorch_ui(), token, seen),
    server = function(input, output, session) {
      # A client without the token gets no session. Nothing is registered for
      # it, so its arrival or departure cannot stop the app either.
      if (!holds_token(shiny::isolate(session$clientData$url_search), token)) return(session$close())
      seen$opened <- TRUE
      windows <<- windows + 1L
      session$onSessionEnded(function() {
        windows <<- windows - 1L
        # A reload ends this session just before the next one begins.
        later::later(function() if (windows == 0L) quit(), delay = grace)
      })
      scorch_server(input, output, session, kept)
    })
}

scorch_launcher <- function() {
  stale_session()
  kept <- new.env(parent = emptyenv())
  token <- launch_token()
  # A window that goes after this call has returned must not stop a later one.
  running <- TRUE
  on.exit(running <- FALSE, add = TRUE)
  seen <- new.env(parent = emptyenv())
  app <- scorch_app(token, kept, quit = function() if (running) shiny::stopApp(), seen = seen)
  viewer <- shiny::dialogViewer("SCORCH | Regional compound heatwaves", width = 1060, height = 860)
  message("Opening the SCORCH window. Nothing is analyzed until you click Run analysis.")
  on.exit(message("The SCORCH window is closed. To open it again, run scoRch.simulator::run_scorch()\n",
                  "The package stays installed; do not run the installation block again."), add = TRUE)
  # RStudio closes its dialog by interrupting R. The last finished analysis is returned all the same.
  # quiet: Shiny's own "Listening on" line gives the address without its key.
  tryCatch(shiny::runApp(app, host = "127.0.0.1", display.mode = "normal", quiet = TRUE,
    launch.browser = function(url) {
      address <- paste0(url, "/?token=", token)
      # Outside RStudio the window is a browser tab, and only this address opens it.
      if (is.null(getOption("shinygadgets.showdialog")))
        message("If no browser opens, paste this address into one on this computer: ", address)
      viewer(address)
      # A viewer that passed the address on without its key shows the notice, not
      # the window. Say how to get the window instead of leaving a dead end.
      later::later(function() {
        if (running && isTRUE(seen$refused) && !isTRUE(seen$opened))
          message("The window was asked for without the key in its address, so it shows a notice instead of SCORCH.\n",
                  "To use SCORCH now, paste this address into a web browser on this computer:\n  ", address)
      }, delay = 8)
    }),
    interrupt = function(e) NULL)
  invisible(kept$result)
}

# The months of the configured season that the data hold. A file with other
# months only starts from its own season instead of an empty selection.
available_season <- function(months, dates) {
  held <- sort(unique(as.integer(format(dates, "%m"))))
  one_season <- function(x) length(x) && !inherits(try(validate_season_months(x), silent = TRUE), "try-error")
  if (all(months %in% held)) months
  else if (one_season(intersect(months, held))) intersect(months, held)
  else if (one_season(held)) held else months
}

scorch_server <- function(input, output, session, kept = new.env(parent = emptyenv())) {
  tags <- shiny::tags
  base <- shiny::reactiveVal(read_config(input_required = FALSE))
  state <- shiny::reactiveValues(source = NULL, path = NULL, parent = guided_path("output"), loaded = NULL,
                                 error = NULL, finished = NULL, busy = FALSE, answers = list(), fresh = FALSE)
  report <- function(e) state$error <- conditionMessage(e)
  reset_input_names <- function() {
    state$answers <- list()
    for (name in c("format", "units")) shiny::updateSelectInput(session, paste0("input_", name), selected = "auto")
    for (name in c("variable", "longitude", "latitude", "time"))
      shiny::updateTextInput(session, paste0("input_", name), value = "auto")
  }
  open_source <- function(path = NULL) {
    close_temperature_source(state$source)
    state$source <- NULL
    state$path <- path
    state$error <- NULL
    actual <- if (is.null(path)) scorch_bundled_file() else path
    if (!all(file.exists(actual))) fail("The configured data could not be found. Choose the data file or folder.")
    if (any(grepl("[.]zip$", actual, ignore.case = TRUE))) {
      # Extracting can take minutes. The page stays busy until the screen
      # that follows has been sent, so that nothing on the opening screen can
      # be clicked while the archive is still being opened.
      session$sendCustomMessage("scorch-busy", TRUE)
      session$onFlushed(function() session$sendCustomMessage("scorch-busy", FALSE))
      state$source <- shiny::withProgress(message = "Opening ZIP", value = 0,
        open_temperature_source(actual, function(i, total, name) {
          shiny::setProgress(value = (i - 1) / total,
                             detail = sprintf("Extracting %d of %d: %s", i, total, name), session = session)
        }), session = session)
    } else {
      opened <- open_temperature_source(actual)
      # A folder holding one NetCDF file is that file, prepared or raw.
      if (length(actual) == 1L && dir.exists(actual) && length(opened$files) == 1L)
        state$path <- normalizePath(opened$files, winslash = "/")
      state$source <- opened
    }
  }
  choose <- function(kind, path) {
    tryCatch({
      if (kind == "output") {
        state$parent <- guided_path(path)
      } else if (kind == "config") {
        cfg <- load_guided_config(path)
        # Format, units and variable names describe a particular file. Reusing
        # a processed-data config with an already recognized hourly source
        # should reuse its scientific settings without misreading temperatures.
        if (!is.null(state$source) && !identical(cfg$input$file, state$path) &&
            !inherits(header(), "error")) {
          keys <- c("format", "variable", "longitude", "latitude", "time", "units")
          cfg$input[keys] <- input_controls(input, base()$input)[keys]
        }
        base(cfg)
        state$loaded <- basename(path)
        # The months and dates of this configuration stay, also when it is
        # what makes a selection readable that showed an error before.
        state$fresh <- FALSE
        state$error <- NULL
        parent <- cfg$output$directory
        state$parent <- if (grepl("^SCORCH_run_[0-9]+$", basename(parent))) dirname(parent) else parent
        # When the user picked data first, importing settings keeps that choice.
        if (is.null(state$source)) open_source(cfg$input$file)
      } else {
        open_source(guided_path(path))
        reset_input_names()
        state$fresh <- TRUE
      }
    }, error = report)
  }
  chooser <- scorch_chooser(input, output, session, choose)
  existing <- function(folder) {
    while (!dir.exists(folder) && !identical(dirname(folder), folder)) folder <- dirname(folder)
    folder
  }
  # The two buttons of the opening screen act only while no data are open. A
  # click or key press that waited behind a long extraction is not obeyed
  # afterwards, when it would replace the data that were just opened.
  shiny::observeEvent(input$browse_file, if (is.null(state$source)) chooser("data"))
  shiny::observeEvent(input$load_config, chooser("config", existing(state$parent)))
  shiny::observeEvent(input$browse_output, chooser("output", existing(state$parent)))
  shiny::observeEvent(input$use_example, {
    if (!is.null(state$source)) return()
    tryCatch({
      open_source()
      reset_input_names()
      state$fresh <- TRUE
    }, error = report)
  })
  # Other data keep the settings of a loaded configuration, so its note stays too.
  shiny::observeEvent(input$change_data, {
    close_temperature_source(state$source)
    state$source <- state$path <- state$error <- NULL
    state$answers <- list()
  })
  header <- shiny::reactive({
    shiny::req(state$source)
    settings <- input_controls(input, base()$input, state$answers)
    settings["file"] <- list(state$path)
    tryCatch(inspect_temperature_source(state$source, settings), error = identity)
  })
  question <- shiny::reactive({
    if (is.null(state$source)) return(NULL)
    info <- header()
    if (inherits(info, "error") || !length(info$questions)) NULL else info$questions[[1L]]
  })
  shiny::observeEvent(input$answer_apply, {
    asked <- question()
    if (is.null(asked) || !length(input$answer) || !input$answer %in% asked$choices) return()
    state$answers[[asked$id]] <- input$answer
  })
  shiny::observeEvent(input$answer_reset, state$answers <- list())
  # Newly chosen data start from the months they hold and from the whole record.
  shiny::observe({
    info <- header()
    if (!isTRUE(state$fresh) || inherits(info, "error") || length(info$questions)) return()
    # An update sent before the month fields are in the page would be lost.
    shiny::req(input$month_from)
    state$fresh <- FALSE
    limits <- season_limits(available_season(shiny::isolate(base())$period$warm_season_months, info$dates))
    shiny::updateSelectInput(session, "month_from", selected = limits[1L])
    shiny::updateSelectInput(session, "month_to", selected = limits[2L])
    shiny::updateCheckboxInput(session, "limit_dates", value = FALSE)
  })
  output$screen <- shiny::reactive({
    if (!is.null(state$finished)) "results" else if (!is.null(state$source)) "settings" else "welcome"
  })
  output$asking <- shiny::reactive(!is.null(question()))
  output$rawInput <- shiny::reactive({
    !is.null(state$source) && !inherits(header(), "error") && identical(header()$format, "era5")
  })
  for (id in c("screen", "asking", "rawInput")) shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  output$settings <- shiny::renderUI(settings_ui(base()))
  # The form is built while the opening screen still shows, so the months of
  # the first dataset chosen in a session arrive at fields that exist.
  shiny::outputOptions(output, "settings", suspendWhenHidden = FALSE)
  output$date_fields <- shiny::renderUI({
    info <- header()
    if (inherits(info, "error") || length(info$questions)) return()
    period <- base()$period
    shiny::dateRangeInput("dates", "Analysis dates",
      start = if (is.null(period$start_date)) min(info$dates) else period$start_date,
      end = if (is.null(period$end_date)) max(info$dates) else period$end_date)
  })
  dataset <- shiny::reactive({
    if (is.null(state$path)) "North America example" else paste(basename(state$path), collapse = ", ")
  })
  output$welcome_problem <- shiny::renderUI({
    if (!is.null(state$error) && is.null(state$source)) tags$p(class = "problem", role = "alert", state$error)
  })
  output$selection <- shiny::renderUI({
    shiny::req(state$source)
    cell_size <- if (isTRUE(input$native_grid)) NULL else if (is.null(input$cell_size)) base()$input$era5_cell_size_deg
                 else input$cell_size
    dataset_card(dataset(), state$path, header(), bundled = is.null(state$path),
      note = if (!is.null(state$error)) tags$p(class = "problem", role = "alert", state$error),
      answers = state$answers, cell_size = cell_size)
  })
  output$question <- shiny::renderUI({
    asked <- question()
    if (!is.null(asked)) question_card(asked)
  })
  output$loaded <- shiny::renderUI({
    if (!is.null(state$loaded)) tags$p(class = "fine", "Settings loaded from ", tags$strong(state$loaded),
      " stay in use when you change the data, including the advanced settings and the results folder.")
  })
  dest <- shiny::reactive({
    state$finished # Recalculate the preview after reserving a run folder.
    next_run_folder(state$parent)
  })
  output$destination <- shiny::renderText(dest())
  plan <- shiny::reactive({
    info <- header()
    if (inherits(info, "error")) return(info)
    if (length(info$questions)) return(simpleError("Answer the question about this file to continue."))
    # Wait for the form, not for its values: an emptied field is reported, not passed over.
    shiny::req(!is.null(input$month_from), !is.null(input$clustering_eps_values))
    tryCatch(guided_settings(base(), input, state$path, info, dest(), state$answers), error = identity)
  })
  # A refused advanced setting is also shown where that section begins: the
  # Review card can be far below the field being edited.
  output$advanced_problem <- shiny::renderUI({
    p <- plan()
    if (inherits(p, "advanced_setting")) tags$p(class = "problem", conditionMessage(p))
  })
  output$period_note <- shiny::renderUI({
    p <- plan()
    if (!inherits(p, "error")) period_note(p$period)
  })
  output$review <- shiny::renderUI({
    p <- plan()
    if (inherits(p, "error")) return(tags$p(class = "problem", role = "alert", conditionMessage(p)))
    shiny::tagList(tags$h2("Review and run"),
      tags$p(sprintf("%s | %s | %s to %s | %s selected dates", dataset(), period_text(p$period),
                     min(p$dates), max(p$dates), count_label(length(p$dates)))))
  })
  output$run_button <- shiny::renderUI({
    if (inherits(plan(), "error") || state$busy)
      return(tags$button(class = "btn btn-primary btn-lg", disabled = "disabled", "Run analysis"))
    shiny::actionButton("start", "Run analysis", class = "btn-primary btn-lg")
  })
  shiny::observeEvent(input$start, {
    if (state$busy || !is.null(state$finished) || is.null(state$source)) return()
    p <- plan()
    if (inherits(p, "error")) return()
    state$busy <- TRUE
    session$sendCustomMessage("scorch-busy", TRUE)
    on.exit({
      state$busy <- FALSE
      session$sendCustomMessage("scorch-busy", FALSE)
    }, add = TRUE)
    state$error <- NULL
    tryCatch({
      progress <- shiny::Progress$new(session)
      on.exit(progress$close(), add = TRUE)
      progress$set(message = "Running SCORCH", value = 0, detail = "Reading the selected data")
      steps <- c("Reading ", "Stage 1:", "Stage 2:", "Stage 3:", "Stage 4:", "Stage 5:",
                 "Power-law fits", "Log-Gaussian", "Writing the result", "Plot ")
      finished <- withCallingHandlers(execute_guided_run(p, state$parent, state$source),
        message = function(m) {
          line <- sub("^[0-9:]+ ", "", trimws(conditionMessage(m)))
          stage <- which(startsWith(line, steps))
          if (length(stage)) progress$set(value = stage[1] / (length(steps) + 1), detail = line)
        })
      finished$dataset <- dataset()
      finished$period <- p$period
      if (!is.null(finished$result)) kept$result <- finished$result
      state$finished <- finished
    }, error = report)
  })
  output$completion <- shiny::renderUI({
    finished <- state$finished
    shiny::req(finished)
    completion_ui(finished)
  })
  # The map of a finished run shows that run: its grid, events and settings.
  shiny::observeEvent(state$finished, {
    finished <- state$finished
    if (is.null(finished$result) || is.null(finished$cell_tmax)) return()
    overview <- tryCatch(event_overview(finished$result, finished$cell_tmax, "Events from this run",
                                        finished$dataset), error = function(e) NULL)
    if (!is.null(overview)) session$onFlushed(function() session$sendCustomMessage("scorch-events", overview))
  })
  shiny::observeEvent(input$explorer_event, {
    finished <- state$finished
    id <- suppressWarnings(as.integer(input$explorer_event$id))
    if (is.null(finished$result) || is.null(finished$cell_tmax) || length(id) != 1L ||
        !id %in% finished$result$events$events$compound_event_id) return()
    session$sendCustomMessage("scorch-frames",
      list(id = id, frames = event_frames(finished$result, finished$cell_tmax, id)))
  })
  show_file <- function(path) tryCatch(open_run_file(path), error = function(e)
    shiny::showNotification(conditionMessage(e), type = "error", session = session))
  shiny::observeEvent(input$open_summary, show_file(state$finished$summary))
  shiny::observeEvent(input$open_folder, show_file(state$finished$folder))
  shiny::observeEvent(input$again, { state$finished <- NULL })
  shiny::observeEvent(input$done, shiny::stopApp())
  session$onSessionEnded(function() shiny::isolate(close_temperature_source(state$source)))
  shiny::reactive({
    state$finished
    kept$result
  })
}
