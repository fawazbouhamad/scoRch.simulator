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

# Windows writes some folders with a name between percent signs, such as
# %USERPROFILE%. A name that is not set stays, and the folder is not found.
expand_percent <- function(path) {
  for (name in unique(regmatches(path, gregexpr("%[^%]+%", path))[[1L]])) {
    value <- Sys.getenv(substr(name, 2L, nchar(name) - 1L), NA_character_)
    if (!is.na(value) && nzchar(value)) path <- gsub(name, value, path, fixed = TRUE)
  }
  path
}

# Where Windows itself says Downloads, Desktop and Documents are. With
# OneDrive backup, or after a folder was moved, they are not the folders of
# that name in the profile, which other programs keep recreating there.
shell_folders <- function() {
  found <- if (.Platform$OS.type == "windows") {
    # utils has readRegistry() on Windows only, so it is fetched by name.
    tryCatch(get("readRegistry", envir = asNamespace("utils"))(
      "Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders", "HCU"), error = function(e) NULL)
  }
  if (!is.list(found)) found <- list()
  keys <- c(Downloads = "{374DE290-123F-4565-9164-39C4925E467B}", Desktop = "Desktop", Documents = "Personal")
  values <- lapply(keys, function(key) found[[key]])
  values <- Filter(function(value) is.character(value) && length(value) == 1L && !is.na(value), values)
  vapply(values, expand_percent, "")
}

# Starting points in the order of a desktop file dialog. Windows keeps
# Desktop and Documents under OneDrive on many computers. Where it does not
# say where a folder is, the folder of that name in the profile is taken.
chooser_places <- function(known = shell_folders()) {
  windows <- .Platform$OS.type == "windows"
  home <- if (windows && nzchar(Sys.getenv("USERPROFILE"))) Sys.getenv("USERPROFILE") else path.expand("~")
  roots <- c(home, if (windows) Sys.getenv(c("OneDrive", "OneDriveConsumer", "OneDriveCommercial")))
  roots <- unique(roots[nzchar(roots)])
  # A folder on a server is not asked for while the window starts: a server
  # that does not answer would keep the window blank for most of a minute. It
  # is listed, and the dialog says so if it cannot be opened when it is clicked.
  present <- function(paths) {
    found <- !is.na(paths)
    near <- found & !on_server(paths)
    found[near] <- dir.exists(paths[near])
    found
  }
  first <- function(name) {
    found <- c(known[name], file.path(roots, name))
    found <- found[present(found)]
    if (length(found)) found[[1L]] else NA_character_
  }
  places <- c(Downloads = first("Downloads"), Desktop = first("Desktop"), Documents = first("Documents"),
              Home = home, "Working folder" = getwd())
  drives <- if (windows) {
    letters <- paste0(LETTERS, ":/")
    letters <- letters[dir.exists(letters)]
    stats::setNames(letters, sub("/$", "", letters))
  } else c(Computer = "/")
  places <- c(places[present(places)], drives)
  far <- on_server(places)
  places[!far] <- normalizePath(places[!far], winslash = "/", mustWork = FALSE)
  # Written as R writes an open folder on a Windows server, two backslashes
  # and then slashes, so that the place is marked while it is open.
  if (windows) places[far] <- paste0("\\\\", substring(gsub("\\", "/", places[far], fixed = TRUE), 3L))
  places
}

# Whether a path is on a server: it begins with two slashes or backslashes.
on_server <- function(path) grepl("^[/\\\\]{2}", path)

# Names that Windows keeps in the profile and on the drive for old programs.
# They are junctions that cannot be opened, and Explorer never shows them.
WINDOWS_JUNCTIONS <- c("Application Data", "Cookies", "Local Settings", "My Documents", "NetHood", "PrintHood",
                       "Recent", "SendTo", "Start Menu", "Templates", "Documents and Settings",
                       "My Music", "My Pictures", "My Videos", "Default User")

# Folders first, then the files this dialog can open, each in name order.
chooser_listing <- function(folder, kind, limit = 2000L) {
  paths <- list.files(folder, full.names = TRUE, no.. = TRUE)
  # A name that is not valid text in this session can be neither shown nor sent back.
  paths <- paths[validEnc(paths)]
  name <- basename(paths)
  keep <- !grepl("^([$]|System Volume Information$)", name)
  if (.Platform$OS.type == "windows") {
    # A folder of the user's own that only shares such a name can be read or
    # shows what it holds, and stays in the list.
    junction <- which(keep & name %in% WINDOWS_JUNCTIONS)
    keep[junction] <- file.access(paths[junction], 4L) == 0L |
      vapply(paths[junction], function(path) length(suppressWarnings(list.files(path))) > 0L, TRUE)
  }
  paths <- paths[keep]
  name <- name[keep]
  # A folder can hold tens of thousands of files. Entries are told apart
  # cheaply; size, date and full path are read for the listed rows only.
  is_folder <- dir.exists(paths)
  pattern <- CHOOSER_KINDS[[kind]]$files
  shown <- which(is_folder | (!is.null(pattern) & grepl(paste0(pattern, ""), name, ignore.case = TRUE)))
  shown <- shown[order(!is_folder[shown], tolower(name[shown]))]
  listed <- utils::head(shown, limit)
  # Windows refuses details of protected entries; they are listed without them.
  info <- suppressWarnings(file.info(paths[listed], extra_cols = FALSE))
  rows <- data.frame(path = normalizePath(paths[listed], winslash = "/", mustWork = FALSE),
                     name = name[listed], folder = is_folder[listed],
                     size = info$size, modified = info$mtime, stringsAsFactors = FALSE)
  rows$type <- ifelse(rows$folder, "Folder",
    ifelse(grepl("[.]zip$", rows$name, ignore.case = TRUE), "ZIP archive",
      ifelse(grepl("[.]ya?ml$", rows$name, ignore.case = TRUE), "Configuration", "NetCDF data")))
  list(rows = rows, hidden = max(0L, length(shown) - limit),
       netcdf = sum(!is_folder & grepl("[.]nc$", name, ignore.case = TRUE)))
}

file_size_label <- function(bytes) {
  units <- c("bytes", "KB", "MB", "GB", "TB")
  power <- pmin(floor(log(pmax(bytes, 1), 1024)), length(units) - 1L)
  ifelse(is.na(bytes), "", ifelse(power == 0, paste(bytes, "bytes"),
                                  sprintf("%.1f %s", bytes / 1024^power, units[power + 1L])))
}

# The share of a network folder, //server/share, or NA elsewhere. It is the
# top of what can be opened there: the server alone is not a folder.
share_root <- function(folder) {
  found <- regmatches(folder, regexpr("^[/\\\\]{2}[^/\\\\]+[/\\\\]+[^/\\\\]+", folder))
  if (length(found)) found else NA_character_
}

# The folder and each of its parents, for the clickable path above the list.
path_parents <- function(folder) {
  share <- share_root(folder)
  if (is.na(share)) {
    parts <- folder
    while (!identical(dirname(parts[1L]), parts[1L]) && length(parts) < 40L) parts <- c(dirname(parts[1L]), parts)
    labels <- ifelse(dirname(parts) == parts, sub("(.)/$", "\\1", parts), basename(parts))
  } else {
    below <- strsplit(substring(folder, nchar(share) + 1L), "[/\\\\]+")[[1L]]
    below <- below[nzchar(below)]
    parts <- Reduce(function(above, name) paste(above, name, sep = "/"), below, share, accumulate = TRUE)
    parts[length(parts)] <- folder
    # The share as Windows writes it, with backslashes.
    labels <- c(gsub("/", "\\", share, fixed = TRUE), below)
  }
  stats::setNames(parts, labels)
}

# The folder above, or NA where there is none to open: at a drive, at a
# network share and at the top of the file system.
folder_above <- function(folder) {
  above <- dirname(folder)
  if (identical(above, folder) || !dir.exists(above)) NA_character_ else above
}

# The folder itself or, when it is gone (renamed, an unplugged drive, a lost
# network share), the nearest folder above it that still exists. NA if none.
nearest_folder <- function(folder) {
  share <- share_root(folder)
  if (!is.na(share) && !dir.exists(share)) return(NA_character_)
  while (!dir.exists(folder) && !identical(dirname(folder), folder)) folder <- dirname(folder)
  # The bare slashes that are left of a network path are not a folder to list.
  if (dir.exists(folder) && !grepl("^[/\\\\]{2,}$", folder)) folder else NA_character_
}

# "Copy as path" in Windows puts double quotes around the path it copies.
unquoted <- function(path) sub("^\"(.*)\"$", "\\1", path)

# Whether a folder directly holds a file with this ending. A folder can carry
# such a name too, so entries are looked at until the first file is found.
holds_files <- function(folder, ending) {
  for (path in list.files(folder, pattern = ending, full.names = TRUE, ignore.case = TRUE, no.. = TRUE)) {
    if (!dir.exists(path)) return(TRUE)
  }
  FALSE
}

# Which of these folders hold NetCDF files: the positions of the first four,
# to name three and to know of more. There can be thousands of folders, or a
# slow drive: the first 200 are looked into, each up to its first file, and
# for no longer than a second.
netcdf_subfolders <- function(folders, most = 200L, seconds = 1) {
  found <- integer()
  started <- proc.time()[["elapsed"]]
  for (i in seq_len(min(length(folders), most))) {
    if (holds_files(folders[i], "[.]nc$")) found <- c(found, i)
    if (length(found) > 3L || proc.time()[["elapsed"]] - started > seconds) break
  }
  found
}

# The answer to a folder whose NetCDF files are in the folders inside it.
# SCORCH reads the files of one folder, and a ZIP file with all its folders.
in_subfolders <- function(names) {
  one <- length(names) == 1L
  listed <- paste0(paste(utils::head(names, 3L), collapse = ", "), if (length(names) > 3L) " and others")
  paste0("This folder has no NetCDF (.nc) files of its own. They are in its subfolder", if (!one) "s", ": ", listed, ". ",
         if (one) "Open it and click Use this folder." else
           paste("Open one of them and click Use this folder. To analyze them together, put the subfolders",
                 "into one ZIP file, or the files into one folder, each under its own name."))
}

# Whether two texts are the same but for the blanks at their ends, which the
# page leaves out of the text it sends. Text that is not valid cannot be
# trimmed and equals nothing.
same_trimmed <- function(text, other) {
  if (!all(validEnc(c(text, other)))) return(FALSE)
  identical(trimws(text, whitespace = "[\\h\\v]"), trimws(other, whitespace = "[\\h\\v]"))
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
  # An entry whose details could not be read has no size and no date.
  sizes <- ifelse(rows$folder, "", file_size_label(rows$size))
  dates <- ifelse(is.na(rows$modified), "", format(rows$modified, "%Y-%m-%d %H:%M"))
  tags$div(class = "chooser", `data-folder` = now$folder, `data-kind` = now$kind,
    tags$nav(class = "chooser-places", `aria-label` = "Places", lapply(names(places), function(name) {
      tags$button(type = "button", class = paste("chooser-place", if (identical(unname(places[name]), now$folder)) "is-current"),
                  `data-go` = unname(places[name]), name)
    })),
    tags$div(class = "chooser-main",
      tags$div(class = "chooser-bar",
        button("\u2191", "up", title = "Up one folder", `aria-label` = "Up one folder",
               disabled = is.na(folder_above(now$folder))),
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
      # One alert; a second message starts on its own line.
      if (length(now$problem)) tags$p(class = "problem", role = "alert", now$problem[1L],
                                      lapply(now$problem[-1L], function(more) list(tags$br(), more))),
      tags$div(class = "chooser-head", `aria-hidden` = "true",
               tags$span("Name"), tags$span("Type"), tags$span("Size"), tags$span("Modified")),
      # The list is named after the open folder, so a screen reader says where it is.
      tags$div(id = "chooser_list", class = "chooser-list", role = "listbox", tabindex = "0",
        `aria-label` = paste(if (now$kind == "output") "Folders in" else "Folders and files in",
                             names(parents)[length(parents)]),
        if (!nrow(rows)) tags$p(class = "chooser-empty", if (now$kind == "output") "This folder has no subfolders."
                                else "Nothing to open here. Folders and matching files appear in this list."),
        lapply(seq_len(nrow(rows)), function(i) {
          tags$div(class = paste("chooser-row", if (rows$folder[i]) "is-folder" else "is-file"), role = "option",
            id = paste0("chooser_row_", i), `aria-selected` = "false", `data-path` = rows$path[i],
            # Long names are cut short in the list; the title shows the whole name.
            tags$span(class = "chooser-name", title = rows$name[i], rows$name[i]), tags$span(rows$type[i]),
            tags$span(sizes[i]), tags$span(dates[i]))
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

# The window around the list. Shiny's own container has no role and no name,
# and it would have every newly opened folder read out from top to bottom.
chooser_dialog <- function(kind) {
  title <- CHOOSER_KINDS[[kind]]$title
  shiny::tagAppendAttributes(
    shiny::modalDialog(title = title, shiny::uiOutput("chooser", `aria-live` = "off"),
                       footer = NULL, size = "l", easyClose = TRUE),
    role = "dialog", `aria-modal` = "true", `aria-label` = title)
}

# The dialog. `selected(kind, path)` receives the chosen file or folder.
scorch_chooser <- function(input, output, session, selected) {
  now <- shiny::reactiveVal(NULL)
  places <- chooser_places()
  last <- new.env(parent = emptyenv())
  # A folder can disappear while the dialog shows it, and no listing may stop
  # the window: the nearest folder that still exists is shown, with a note.
  show <- function(kind, folder, problem = NULL) {
    asked <- normalizePath(folder, winslash = "/", mustWork = FALSE)
    folder <- nearest_folder(asked)
    if (is.na(folder)) {
      # A place on this computer: one on a server may not answer for most of a minute.
      near <- places[!on_server(places)]
      folder <- c(unname(near[dir.exists(near)]), normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE))[1L]
    }
    if (!identical(folder, asked)) {
      # The folder that is gone is named once: what was asked for may have named it already.
      note <- "The dialog now shows a folder that still exists."
      named <- any(endsWith(as.character(problem), asked))
      problem <- c(problem, if (named) note else paste(asked, "is no longer available.", note))
    }
    listing <- tryCatch(chooser_listing(folder, kind), error = function(e) NULL)
    if (is.null(listing)) {
      listing <- chooser_listing(character(), kind)
      problem <- c(problem, "The contents of this folder could not be read. A path can still be typed below.")
    }
    now(c(list(kind = kind, folder = folder, problem = problem), listing))
  }
  finish <- function(path) {
    kind <- now()$kind
    assign(kind, now()$folder, envir = last)
    shiny::removeModal(session)
    now(NULL)
    selected(kind, path)
  }
  # "Use this folder" takes what the path field says: the open folder, a
  # folder highlighted in the list, or a folder typed or pasted there.
  use_folder <- function(here, path) {
    if (here$kind == "config") return()
    path <- unquoted(path)
    # The open folder itself, also when its name ends in a blank that the text
    # no longer has: Linux and macOS allow such names.
    if (same_trimmed(path, here$folder)) path <- ""
    target <- if (nzchar(path)) guided_path(path, here$folder) else here$folder
    named <- if (nzchar(path)) path else target
    # A file of the open folder, as highlighted in the list, stands for that folder.
    if (!dir.exists(target) && (target %in% here$rows$path[!here$rows$folder] ||
                                (file.exists(target) && identical(dirname(target), here$folder)))) {
      target <- named <- here$folder
    }
    if (!dir.exists(target)) {
      fail(if (file.exists(target)) "That is a file, not a folder: " else "That folder could not be found: ", named)
    }
    if (here$kind == "data") {
      # The open folder was counted when it was listed and is not read again.
      # Files can have arrived since then: a count of none is looked into once more.
      found <- (identical(target, here$folder) && here$netcdf > 0L) || holds_files(target, "[.]nc$")
      if (!found) {
        nc4 <- holds_files(target, "[.]nc4$")
        show(here$kind, target, if (nc4) {
          paste("This folder has no NetCDF (.nc) files, only .nc4 files. SCORCH reads .nc files from folders.",
                "A single .nc4 file can be opened directly: select it and click Open.")
        } else {
          paste("This folder has no NetCDF (.nc) files. Open the folder that holds your",
                "ERA5 files, or select one file and click Open.")
        })
        # The files may be one level down, as with downloads unpacked into a
        # folder each. The folders are in the list that was just made, so the
        # folder is not read a second time to find them.
        shown <- now()
        inside <- shown$rows[shown$rows$folder, ]
        within <- if (!nc4 && identical(shown$folder, target)) inside$name[netcdf_subfolders(inside$path)]
        if (length(within)) {
          shown$problem[1L] <- in_subfolders(within)
          now(shown)
        }
        return()
      }
    }
    finish(target)
  }
  output$chooser <- shiny::renderUI({
    shiny::req(now())
    chooser_body(now(), places)
  })
  shiny::observeEvent(input$chooser_action, {
    here <- now()
    action <- input$chooser_action
    if (is.null(here) || !is.list(action) || !is.character(action$do) || length(action$do) != 1L) return()
    text <- is.character(action$path) && length(action$path) == 1L && !is.na(action$path)
    # Text cut in the middle of a character, such as half an emoji, can be
    # neither trimmed nor looked up, and an error here would close the window.
    if (text && !validEnc(action$path)) {
      return(show(here$kind, here$folder, "That text holds characters that cannot be read. Type the path again."))
    }
    path <- if (text) trimws(action$path) else ""
    tryCatch(switch(action$do,
      up = {
        # Nothing is above a drive or a network share. A folder that has
        # disappeared gives way to the nearest one that still exists.
        above <- folder_above(here$folder)
        if (!is.na(above)) show(here$kind, above) else if (!dir.exists(here$folder)) show(here$kind, here$folder)
      },
      use = use_folder(here, path),
      mkdir = {
        if (here$kind != "output") return()
        if (!nzchar(path) || grepl("[<>:\"/\\\\|?*[:cntrl:]]", path) || path %in% c(".", ".."))
          fail("Give the new folder a name without slashes or the characters < > : \" | ? *")
        if (!dir.exists(here$folder)) fail("No folder was created.")
        folder <- file.path(here$folder, path)
        if (!dir.exists(folder)) dir.create(folder, showWarnings = FALSE)
        # Windows reports some names, such as NUL, as created although no folder appears.
        if (!dir.exists(folder)) fail("A folder named ", path, " could not be created here. ",
                                      "Give it another name, or choose a folder you can write to.")
        show(here$kind, folder)
      },
      go = {
        path <- unquoted(path)
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
    # Without a folder to return to, the first place on this computer. A place
    # on a server is opened when it is clicked, not before.
    if (is.null(folder) || !dir.exists(folder)) folder <- unname(places[!on_server(places)][1L])
    show(kind, folder)
    shiny::showModal(chooser_dialog(kind), session = session)
  }
}
