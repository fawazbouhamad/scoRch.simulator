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
  # another program could work out. The draw is therefore shifted by bytes of
  # the system's random device where there is one (then the token cannot be
  # guessed). Windows has no such device for R to read; there, and everywhere
  # else as well, the draw is shifted by what another account on this computer
  # cannot see: the name of this session's temporary folder, which lies in the
  # user's profile, the microsecond of this call, and where in memory a new
  # object was placed.
  device <- suppressWarnings(tryCatch(as.integer(readBin("/dev/urandom", "raw", 32L)), error = function(e) integer()))
  spread <- function(values) if (length(values) && !anyNA(values)) rep_len(values, 32L) else 0L
  moment <- sprintf("%06.0f", (as.numeric(Sys.time()) %% 1) * 1e6)
  place <- tryCatch(gsub("[^0-9a-f]", "", sub("^.*0x", "", paste(utils::capture.output(print(new.env())), collapse = ""))),
                    error = function(e) "")
  draw <- draw + spread(utf8ToInt(basename(tempdir()))) + spread(device) + spread(utf8ToInt(paste0(moment, place)))
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
    if (holds_token(request$QUERY_STRING, token)) {
      # When the page was handed out: a reload asks for it before its old
      # window goes. Every hand-out waits in `pending` for its window.
      if (is.environment(seen)) {
        seen$page <- Sys.time()
        seen$pending <- c(seen$pending, as.numeric(seen$page))
      }
      return(page)
    }
    if (is.environment(seen)) seen$refused <- TRUE
    refused
  }
}

# The app behind one window. Its address holds a secret, `token`: without it
# there is no page and no window, so another program on this computer can
# neither use the window nor close it. `quit` ends the app `grace` seconds
# after the last window has gone; a reload or a second tab keeps it open.
# `seen` notes whether the page was refused, when it was handed out without a
# window having opened for it yet (`pending`), and when a window last opened.
# A page that was handed out is waited for up to `patience` seconds from that
# moment, however slowly the browser connects: a reload, or one that overtakes
# another, does not close the window.
scorch_app <- function(token, kept, quit = shiny::stopApp, grace = 2, seen = new.env(parent = emptyenv()),
                       patience = 20) {
  windows <- 0L
  # Every arrival and departure is counted, so a departure that has waited can
  # tell whether anything happened in the meantime.
  moves <- 0L
  shiny::shinyApp(
    ui = scorch_page(scorch_ui(), token, seen),
    server = function(input, output, session) {
      # A client without the token gets no session. Nothing is registered for
      # it, so its arrival or departure cannot stop the app either.
      if (!holds_token(shiny::isolate(session$clientData$url_search), token)) return(session$close())
      seen$opened <- TRUE
      seen$joined <- Sys.time()
      # This window accounts for the page that has waited longest; pages too
      # old to open a window any more are forgotten.
      waiting <- seen$pending[as.numeric(Sys.time()) - seen$pending < patience]
      seen$pending <- waiting[-1L]
      windows <<- windows + 1L
      moves <<- moves + 1L
      session$onSessionEnded(function() {
        windows <<- windows - 1L
        moves <<- moves + 1L
        mine <- moves
        # A reload ends this session just before the next one begins. Only the
        # departure after which nothing else happened may end the app, and not
        # while a page that was handed out a moment ago has yet to open its window.
        leave <- function() {
          if (windows > 0L || moves != mine) return()
          loading <- any(as.numeric(Sys.time()) - seen$pending < patience)
          if (loading) later::later(leave, delay = 0.25) else quit()
        }
        later::later(leave, delay = grace)
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
  # The window has no file upload; Shiny would otherwise take one from any
  # local program that reaches its socket, key or no key.
  uploads <- options(shiny.maxRequestSize = 1)
  on.exit(options(uploads), add = TRUE)
  # shiny::runApp() attaches shiny. Attached here first, the two lines R would
  # print for that ("Loading required package", "was built under R version")
  # stay out of the first minute.
  if (!"package:shiny" %in% search()) try(suppressPackageStartupMessages(attachNamespace("shiny")), silent = TRUE)
  # A window of an earlier version that was closed in this session may have
  # left its own "stop the app" waiting. It runs now, before there is an app.
  try(later::run_now(), silent = TRUE)
  # RStudio closes its dialog by interrupting R. The last finished analysis is returned all the same.
  # quiet: Shiny's own "Listening on" line gives the address without its key.
  tryCatch(shiny::runApp(app, host = "127.0.0.1", display.mode = "normal", quiet = TRUE,
    launch.browser = function(url) {
      address <- paste0(url, "/?token=", token)
      # Outside RStudio the window is a browser tab, and only this address opens it.
      printed <- is.null(getOption("shinygadgets.showdialog"))
      if (printed) message("If no browser opens, paste this address into one on this computer: ", address)
      viewer(address)
      # A viewer that passed the address on without its key shows the notice,
      # not the window, and one that shows nothing leaves no window at all.
      # Say how to reach the window instead of leaving a dead end.
      asked <- Sys.time()
      watch <- function() {
        if (!running || isTRUE(seen$opened)) return()
        if (isTRUE(seen$refused)) {
          return(message("The window was asked for without the key in its address, so it shows a notice instead of SCORCH.\n",
                         "To use SCORCH now, paste this address into a web browser on this computer:\n  ", address))
        }
        if (!printed && as.numeric(difftime(Sys.time(), asked, units = "secs")) >= 30) {
          return(message("No SCORCH window has opened yet. If none appears, paste this address into a web browser ",
                         "on this computer:\n  ", address))
        }
        later::later(watch, delay = 2)
      }
      later::later(watch, delay = 8)
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
  # `form` counts the configurations that were loaded and `opened` the data
  # that were opened: the form is rebuilt for each of the first, the date
  # fields for each of both. `waiting` is a configuration without its data,
  # `failed` why the last click on Run analysis started nothing, `unanswered`
  # a click on "Use this answer" before an answer was chosen. A run that
  # finished in this window is shown again after a reload (`kept`).
  state <- shiny::reactiveValues(source = NULL, path = NULL, parent = guided_path("output"), loaded = NULL,
                                 error = NULL, finished = kept$finished, busy = FALSE, answers = list(), fresh = FALSE,
                                 form = 0L, opened = 0L, waiting = FALSE, failed = NULL, unanswered = FALSE)
  report <- function(e) state$error <- conditionMessage(e)
  reset_input_names <- function() {
    state$answers <- list()
    for (name in c("format", "units")) shiny::updateSelectInput(session, paste0("input_", name), selected = "auto")
    for (name in c("variable", "longitude", "latitude", "time"))
      shiny::updateTextInput(session, paste0("input_", name), value = "auto")
  }
  # Data that were just opened are read by detection and start from their own
  # months and whole record. A configuration that waited for its data reads
  # them as it stands instead: its months, dates and file-reading entries stay.
  arrived <- function() {
    if (isTRUE(state$waiting)) {
      state$waiting <- FALSE
      return(invisible(NULL))
    }
    reset_input_names()
    state$fresh <- TRUE
  }
  open_source <- function(path = NULL) {
    close_temperature_source(state$source)
    state$source <- NULL
    state$path <- path
    state$error <- NULL
    state$opened <- shiny::isolate(state$opened) + 1L
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
        # A start that failed says nothing about the folder chosen now.
        state$failed <- NULL
      } else if (kind == "config") {
        # A file that is refused is named in what the window says about it.
        cfg <- tryCatch(load_guided_config(path), error = function(e) {
          fail(basename(path), " was not loaded: ", conditionMessage(e))
        })
        # Format, units and variable names describe a particular file. Reusing
        # a processed-data config with an already recognized hourly source
        # should reuse its scientific settings without misreading temperatures.
        if (!is.null(state$source) && !identical(cfg$input$file, state$path) &&
            !inherits(header(), "error")) {
          keys <- c("format", "variable", "longitude", "latitude", "time", "units")
          cfg$input[keys] <- input_controls(input, base()$input)[keys]
        }
        base(cfg)
        # The same file holds the same settings, which by itself would change
        # nothing on the page. Every load rebuilds the form, so that the
        # values of the file return and make its data readable again.
        state$form <- shiny::isolate(state$form) + 1L
        state$loaded <- basename(path)
        # The months and dates of this configuration stay, also when it is
        # what makes a selection readable that showed an error before.
        state$fresh <- FALSE
        state$error <- NULL
        state$failed <- NULL
        parent <- cfg$output$directory
        state$parent <- if (grepl("^SCORCH_run_[0-9]+$", basename(parent))) dirname(parent) else parent
        # When the user picked data first, importing settings keeps that choice.
        # Without data the configuration opens its own. Where those cannot be
        # found on this computer it waits for them: the data chosen next are
        # read with it as it stands, which replays a run saved elsewhere.
        if (is.null(state$source)) {
          state$waiting <- TRUE
          open_source(cfg$input$file)
          state$waiting <- FALSE
        }
      } else {
        open_source(guided_path(path))
        arrived()
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
      arrived()
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
    if (is.null(asked)) return()
    # Without an answer the card says what is missing instead of doing nothing.
    chosen <- length(input$answer) == 1L && input$answer %in% asked$choices
    state$unanswered <- !chosen
    if (chosen) state$answers[[asked$id]] <- input$answer
  })
  # That reminder goes when an answer is chosen, and with the question it was about.
  shiny::observeEvent(input$answer, state$unanswered <- FALSE)
  shiny::observeEvent(question(), state$unanswered <- FALSE, ignoreNULL = FALSE)
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
  output$settings <- shiny::renderUI({
    # Built again for every configuration that is loaded, the same one included.
    state$form
    settings_ui(base())
  })
  # The form is built while the opening screen still shows, so the months of
  # the first dataset chosen in a session arrive at fields that exist.
  shiny::outputOptions(output, "settings", suspendWhenHidden = FALSE)
  # The first and last date of the data that are open. It changes with other
  # data, and when a selection becomes readable or stops being so, not each
  # time the same record is read again.
  record <- shiny::reactiveVal(NULL)
  shiny::observe({
    info <- if (!is.null(state$source)) header()
    record(if (is.null(info) || inherits(info, "error") || length(info$questions)) NULL
           else list(opened = shiny::isolate(state$opened), first = min(info$dates), last = max(info$dates)))
  })
  # The date fields start again for other data and for another configuration.
  # Dates picked by hand stay when a field under "How the file is read" changes.
  output$date_fields <- shiny::renderUI({
    span <- record()
    state$form
    if (is.null(span)) return()
    period <- base()$period
    shiny::dateRangeInput("dates", "Analysis dates",
      start = if (is.null(period$start_date)) span$first else period$start_date,
      end = if (is.null(period$end_date)) span$last else period$end_date)
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
      # An answer that a field under Advanced settings has since replaced is not listed.
      answers = answers_in_effect(input, base()$input, state$answers), cell_size = cell_size)
  })
  output$question <- shiny::renderUI({
    asked <- question()
    if (!is.null(asked)) question_card(asked)
  })
  output$answer_problem <- shiny::renderUI({
    if (isTRUE(state$unanswered)) tags$p(class = "problem", role = "alert", "Choose an answer first.")
  })
  # True whichever came first, the data or the configuration. It promises the
  # months and dates of a reused file only: its entries for reading a file are
  # applied to its own data and to data that cannot be read without them, not
  # to other data that detection reads (see choose()).
  output$loaded <- shiny::renderUI({
    if (!is.null(state$loaded)) tags$p(class = "fine", "Settings loaded from ", tags$strong(state$loaded, .noWS = "after"),
      ": the advanced settings and the results folder stay in use when you change the data. ",
      "The months, the date limit and 'How the file is read' start again with other data; ",
      "reuse the file to apply its months and dates again.")
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
      tags$p(sprintf("%s | %s | %s to %s | %s", dataset(), period_text(p$period),
                     min(p$dates), max(p$dates), count_noun(length(p$dates), "selected date"))),
      # A start that failed is reported here, beside the button that was clicked.
      if (!is.null(state$failed)) tags$p(class = "problem", role = "alert", state$failed))
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
    state$failed <- NULL
    # A start that fails (the results folder cannot be created) is reported in
    # the Review card, where the click was made, not on the dataset card.
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
      # Summary.txt speaks of daily maximum temperature. Where the file did not
      # identify its variable as that, the results name what was analyzed.
      finished$temperature <- temperature_note(header())
      if (!is.null(finished$result)) kept$result <- finished$result
      # A reload opens a new session of this window, which starts from this run.
      kept$finished <- finished
      state$finished <- finished
    }, error = function(e) state$failed <- conditionMessage(e))
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
  shiny::observeEvent(input$again, {
    state$finished <- NULL
    # A later reload starts from the opening screen, not from this run.
    kept$finished <- NULL
  })
  shiny::observeEvent(input$done, shiny::stopApp())
  session$onSessionEnded(function() shiny::isolate(close_temperature_source(state$source)))
  shiny::reactive({
    state$finished
    kept$result
  })
}
