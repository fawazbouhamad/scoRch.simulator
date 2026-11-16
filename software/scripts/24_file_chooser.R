# One file dialog for data, saved settings and the results folder. R lists
# this computer's folders, so a large archive is opened where it is instead
# of being uploaded through the browser. It also works in RStudio Server,
# where the files are on the server.
CHOOSER_KINDS <- list(
  data = list(title = "Choose your temperature data", files = "[.](nc|nc4|zip)$",
              hint = paste("Select a NetCDF or ZIP file and click Open.",
                           "For a folder of ERA5 files, open the folder and click Use this folder.")),
  config = list(title = "Reuse a saved configuration", files = "[.]ya?ml$",
                hint = "Every results folder holds the config.yaml of its run. Select one and click Open."),
  output = list(title = "Choose where results are saved", files = NULL,
                hint = "SCORCH creates a new numbered folder for every run inside the folder you choose."))

# Starting points in the order of a desktop file dialog. Windows keeps
# Desktop and Documents under OneDrive on many computers.
chooser_places <- function() {
  windows <- .Platform$OS.type == "windows"
  home <- if (windows && nzchar(Sys.getenv("USERPROFILE"))) Sys.getenv("USERPROFILE") else path.expand("~")
  roots <- c(home, if (windows) Sys.getenv(c("OneDrive", "OneDriveConsumer", "OneDriveCommercial")))
  roots <- unique(roots[nzchar(roots)])
  first <- function(name) {
    found <- file.path(roots, name)
    found <- found[dir.exists(found)]
    if (length(found)) found[1L] else NA_character_
  }
  places <- c(Downloads = first("Downloads"), Desktop = first("Desktop"), Documents = first("Documents"),
              Home = home, "Working folder" = getwd())
  drives <- if (windows) {
    letters <- paste0(LETTERS, ":/")
    letters <- letters[dir.exists(letters)]
    stats::setNames(letters, sub("/$", "", letters))
  } else c(Computer = "/")
  places <- c(places[!is.na(places) & dir.exists(places)], drives)
  stats::setNames(normalizePath(places, winslash = "/", mustWork = FALSE), names(places))
}

# Folders first, then the files this dialog can open, each in name order.
chooser_listing <- function(folder, kind, limit = 2000L) {
  paths <- list.files(folder, full.names = TRUE, no.. = TRUE)
  paths <- paths[!grepl("^([$]|System Volume Information$)", basename(paths))]
  info <- file.info(paths, extra_cols = FALSE)
  is_folder <- info$isdir %in% TRUE
  pattern <- CHOOSER_KINDS[[kind]]$files
  shown <- which(is_folder | (!is.null(pattern) & grepl(paste0(pattern, ""), paths, ignore.case = TRUE)))
  shown <- shown[order(!is_folder[shown], tolower(basename(paths[shown])))]
  rows <- data.frame(path = normalizePath(paths[shown], winslash = "/", mustWork = FALSE),
                     name = basename(paths[shown]), folder = is_folder[shown],
                     size = info$size[shown], modified = info$mtime[shown], stringsAsFactors = FALSE)
  rows$type <- ifelse(rows$folder, "Folder",
    ifelse(grepl("[.]zip$", rows$name, ignore.case = TRUE), "ZIP archive",
      ifelse(grepl("[.]ya?ml$", rows$name, ignore.case = TRUE), "Configuration", "NetCDF data")))
  list(rows = utils::head(rows, limit), hidden = max(0L, nrow(rows) - limit),
       netcdf = sum(!is_folder & grepl("[.]nc$", paths, ignore.case = TRUE)))
}

file_size_label <- function(bytes) {
  units <- c("bytes", "KB", "MB", "GB", "TB")
  power <- pmin(floor(log(pmax(bytes, 1), 1024)), length(units) - 1L)
  ifelse(is.na(bytes), "", ifelse(power == 0, paste(bytes, "bytes"),
                                  sprintf("%.1f %s", bytes / 1024^power, units[power + 1L])))
}

# The folder and each of its parents, for the clickable path above the list.
path_parents <- function(folder) {
  parts <- folder
  while (!identical(dirname(parts[1L]), parts[1L]) && length(parts) < 40L) parts <- c(dirname(parts[1L]), parts)
  labels <- ifelse(dirname(parts) == parts, sub("(.)/$", "\\1", parts), basename(parts))
  stats::setNames(parts, labels)
}

chooser_body <- function(now, places) {
  tags <- shiny::tags
  kind <- CHOOSER_KINDS[[now$kind]]
  rows <- now$rows
  parents <- path_parents(now$folder)
  button <- function(label, do, class = "btn-default", disabled = FALSE, ...) {
    tags$button(type = "button", class = paste("btn", class), `data-do` = do,
                disabled = if (disabled) NA, label, ...)
  }
  status <- if (now$kind == "data" && now$netcdf > 0L) {
    sprintf("This folder has %d NetCDF file%s. Use this folder analyzes %s as one ERA5 dataset.",
            now$netcdf, plural(now$netcdf), if (now$netcdf == 1L) "it" else "them together")
  } else kind$hint
  tags$div(class = "chooser", `data-folder` = now$folder, `data-kind` = now$kind,
    tags$nav(class = "chooser-places", `aria-label` = "Places", lapply(names(places), function(name) {
      tags$button(type = "button", class = paste("chooser-place", if (identical(unname(places[name]), now$folder)) "is-current"),
                  `data-go` = unname(places[name]), name)
    })),
    tags$div(class = "chooser-main",
      tags$div(class = "chooser-bar",
        button("\u2191", "up", title = "Up one folder", `aria-label` = "Up one folder",
               disabled = identical(dirname(now$folder), now$folder)),
        tags$div(class = "chooser-crumbs", role = "group", `aria-label` = "Current folder",
          lapply(seq_along(parents), function(i) {
            tags$button(type = "button", class = "chooser-crumb", `data-go` = unname(parents[i]),
                        `aria-current` = if (i == length(parents)) "location", names(parents)[i])
          })),
        if (now$kind == "output") button("New folder", "name")),
      if (now$kind == "output") tags$div(class = "chooser-new", hidden = NA,
        tags$label(`for` = "chooser_name", "Name of the new folder"),
        tags$input(id = "chooser_name", type = "text", class = "form-control", autocomplete = "off", spellcheck = "false"),
        button("Create", "mkdir", "btn-primary"), button("Cancel", "unname")),
      if (!is.null(now$problem)) tags$p(class = "problem", role = "alert", now$problem),
      tags$div(class = "chooser-head", `aria-hidden` = "true",
               tags$span("Name"), tags$span("Type"), tags$span("Size"), tags$span("Modified")),
      tags$div(id = "chooser_list", class = "chooser-list", role = "listbox", tabindex = "0",
        `aria-label` = "Folders and files",
        if (!nrow(rows)) tags$p(class = "chooser-empty", if (now$kind == "output") "This folder has no subfolders."
                                else "Nothing to open here. Folders and matching files appear in this list."),
        lapply(seq_len(nrow(rows)), function(i) {
          tags$div(class = paste("chooser-row", if (rows$folder[i]) "is-folder" else "is-file"), role = "option",
            id = paste0("chooser_row_", i), `aria-selected` = "false", `data-path` = rows$path[i],
            tags$span(class = "chooser-name", rows$name[i]), tags$span(rows$type[i]),
            tags$span(if (rows$folder[i]) "" else file_size_label(rows$size[i])),
            tags$span(format(rows$modified[i], "%Y-%m-%d %H:%M")))
        }),
        if (now$hidden > 0L) tags$p(class = "chooser-empty", sprintf("%d more items are not listed. Type a path below to open one.", now$hidden))),
      tags$div(class = "chooser-path",
        tags$label(`for` = "chooser_path", if (now$kind == "output") "Folder" else "File or folder"),
        tags$input(id = "chooser_path", type = "text", class = "form-control", value = now$folder,
                   autocomplete = "off", spellcheck = "false")),
      tags$p(class = "chooser-hint", role = "status", status),
      tags$div(class = "chooser-actions",
        tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", "Cancel"),
        if (now$kind != "config") button("Use this folder", "use", if (now$kind == "output") "btn-primary" else "btn-default"),
        if (now$kind != "output") button("Open", "go", "btn-primary"))))
}

# The dialog. `selected(kind, path)` receives the chosen file or folder.
scorch_chooser <- function(input, output, session, selected) {
  now <- shiny::reactiveVal(NULL)
  places <- chooser_places()
  last <- new.env(parent = emptyenv())
  show <- function(kind, folder, problem = NULL) {
    folder <- normalizePath(folder, winslash = "/", mustWork = TRUE)
    now(c(list(kind = kind, folder = folder, problem = problem), chooser_listing(folder, kind)))
  }
  finish <- function(path) {
    kind <- now()$kind
    assign(kind, now()$folder, envir = last)
    shiny::removeModal(session)
    now(NULL)
    selected(kind, path)
  }
  # "Use this folder" means the highlighted folder, or else the open one.
  use_folder <- function(here, path) {
    if (here$kind == "config") return()
    target <- if (path %in% here$rows$path[here$rows$folder]) path else here$folder
    if (here$kind == "data" && !chooser_listing(target, "data")$netcdf) {
      show(here$kind, target, paste("This folder has no NetCDF (.nc) files. Open the folder that holds your",
                                    "ERA5 files, or select one file and click Open."))
    } else finish(target)
  }
  output$chooser <- shiny::renderUI({
    shiny::req(now())
    chooser_body(now(), places)
  })
  shiny::observeEvent(input$chooser_action, {
    here <- now()
    action <- input$chooser_action
    if (is.null(here) || !is.list(action) || !is.character(action$do)) return()
    path <- if (is.character(action$path) && length(action$path) == 1L) trimws(action$path) else ""
    tryCatch(switch(action$do,
      up = show(here$kind, dirname(here$folder)),
      use = use_folder(here, path),
      mkdir = {
        if (here$kind != "output") return()
        if (!nzchar(path) || grepl("[<>:\"/\\\\|?*[:cntrl:]]", path) || path %in% c(".", ".."))
          fail("Give the new folder a name without slashes or the characters < > : \" | ? *")
        folder <- file.path(here$folder, path)
        if (!dir.exists(folder) && !dir.create(folder, showWarnings = FALSE)) fail("Could not create ", folder, ".")
        show(here$kind, folder)
      },
      go = {
        if (!nzchar(path)) return()
        target <- guided_path(path, here$folder)
        if (dir.exists(target)) show(here$kind, target)
        else if (here$kind != "output" && file.exists(target) &&
                 grepl(CHOOSER_KINDS[[here$kind]]$files, target, ignore.case = TRUE)) finish(target)
        else fail(if (file.exists(target)) "This dialog cannot open that kind of file: " else "That path could not be found: ", path)
      }), error = function(e) show(here$kind, here$folder, conditionMessage(e)))
  })
  function(kind, folder = NULL) {
    if (is.null(folder) || !dir.exists(folder)) folder <- if (exists(kind, envir = last)) get(kind, envir = last) else NULL
    if (is.null(folder) || !dir.exists(folder)) folder <- unname(places[1L])
    show(kind, folder)
    shiny::showModal(shiny::modalDialog(title = CHOOSER_KINDS[[kind]]$title, shiny::uiOutput("chooser"),
                                        footer = NULL, size = "l", easyClose = TRUE), session = session)
  }
}
