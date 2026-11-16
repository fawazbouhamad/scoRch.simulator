# The file dialog in a test session. `open(kind, folder)` shows it, `act()`
# sends what a click or a key in the browser sends, `dialog()` is what the
# dialog shows (NULL once it has closed) and `taken()` what was selected
# since it was last asked.
chooser_server <- function(chooser = scorch_chooser) {
  function(input, output, session) {
    chosen <- new.env()
    open <- chooser(input, output, session, function(kind, path) chosen$last <- list(kind = kind, path = path))
    act <- function(do, path = NULL) {
      chosen$sent <- sum(chosen$sent, 1)
      session$setInputs(chooser_action = list(do = do, path = path, at = chosen$sent))
    }
    dialog <- function() environment(open)$now()
    taken <- function() {
      value <- chosen$last
      chosen$last <- NULL
      value
    }
  }
}

# The dialog with another function in the place of chooser_listing().
chooser_listing_as <- function(listing) {
  replaced <- new.env(parent = environment(scorch_chooser))
  replaced$chooser_listing <- listing
  chooser <- scorch_chooser
  environment(chooser) <- replaced
  chooser
}

# chooser_listing() with stand-ins for functions of base R that it calls.
listing_with <- function(...) {
  replaced <- list2env(list(...), parent = environment(chooser_listing))
  listing <- chooser_listing
  environment(listing) <- replaced
  listing
}

# Any function of the package with stand-ins for functions that it calls.
with_stand_ins <- function(fun, ...) {
  environment(fun) <- list2env(list(...), parent = environment(fun))
  fun
}

# A new folder for one test, written as the dialog writes folders.
test_folder <- function(env = parent.frame()) {
  normalizePath(withr::local_tempdir(.local_envir = env), winslash = "/")
}

# The dialog as HTML: the contents of `folder`, shown under the path `shown`.
dialog_html <- function(kind, folder, shown = folder, problem = NULL) {
  now <- c(list(kind = kind, folder = shown, problem = problem), chooser_listing(folder, kind))
  as.character(chooser_body(now, chooser_places(known = character())))
}
opening_tag <- function(html, mark) regmatches(html, regexpr(paste0("<[a-z]+[^>]*", mark, "[^>]*>"), html))

test_that("Use this folder takes the folder written in the dialog", {
  root <- test_folder()
  shown <- file.path(root, "shown")
  typed <- file.path(root, "typed", "era5 hourly")
  empty <- file.path(root, "empty")
  for (folder in c(shown, typed, empty)) dir.create(folder, recursive = TRUE)
  file.create(file.path(shown, c("first.nc", "notes.txt")), file.path(typed, "second.nc"))
  shiny::testServer(chooser_server(), {
    # A folder typed or pasted into the field wins over the folder that is open.
    open("data", shown)
    act("use", typed)
    expect_identical(taken(), list(kind = "data", path = typed))
    expect_null(dialog())
    # No text, or the open folder itself, is the open folder.
    for (path in list(NULL, "", shown)) {
      open("data", shown)
      act("use", path)
      expect_identical(taken(), list(kind = "data", path = shown))
    }
    # A highlighted file stands for the folder it is in.
    open("data", shown)
    act("use", file.path(shown, "first.nc"))
    expect_identical(taken(), list(kind = "data", path = shown))
    # Relative to the open folder, and in the quotes of "Copy as path".
    open("data", shown)
    act("use", "../typed/era5 hourly")
    expect_identical(taken(), list(kind = "data", path = typed))
    open("data", shown)
    act("use", paste0("\"", typed, "\""))
    expect_identical(taken(), list(kind = "data", path = typed))

    # What cannot be used is said, and the dialog stays open where it was.
    open("data", shown)
    act("use", file.path(root, "nowhere"))
    expect_null(taken())
    expect_identical(dialog()$folder, shown)
    expect_identical(dialog()$problem, paste0("That folder could not be found: ", file.path(root, "nowhere")))
    act("use", file.path(typed, "second.nc"))
    expect_null(taken())
    expect_identical(dialog()$problem, paste0("That is a file, not a folder: ", file.path(typed, "second.nc")))
    # Data still need NetCDF files: the typed folder is shown with the reason.
    act("use", empty)
    expect_null(taken())
    expect_identical(dialog()$folder, empty)
    expect_match(dialog()$problem, "This folder has no NetCDF (.nc) files. Open the folder", fixed = TRUE)

    # The results dialog has no other confirming button.
    open("output", shown)
    act("use", typed)
    expect_identical(taken(), list(kind = "output", path = typed))
    open("output", shown)
    act("use", file.path(root, "not made yet"))
    expect_null(taken())
    expect_identical(dialog()$problem, paste0("That folder could not be found: ", file.path(root, "not made yet")))
    act("use", empty)
    expect_identical(taken(), list(kind = "output", path = empty))
    # Saved settings are a file: that dialog has no such button.
    open("config", shown)
    act("use", typed)
    expect_null(taken())
    expect_identical(dialog()$folder, shown)
  })
})

test_that("a results folder typed in the window's dialog becomes the destination", {
  project <- test_folder()
  withr::local_dir(project)
  # Not a folder of the list the dialog opens with: only the typed text names it.
  typed <- file.path(project, "elsewhere", "results parent")
  dir.create(typed, recursive = TRUE)
  shiny::testServer(scorch_server, {
    session$setInputs(browse_output = 1)
    session$setInputs(chooser_action = list(do = "use", path = typed, at = 1))
    expect_identical(dest(), file.path(typed, "SCORCH_run_001"))
    expect_false(dir.exists(dest()))
  })
})

test_that("a folder of .nc4 files is explained, and one .nc4 file opens", {
  root <- test_folder()
  merra <- file.path(root, "merra2")
  dir.create(merra)
  file.create(file.path(merra, c("MERRA2_400.20010601.nc4", "MERRA2_400.20010602.NC4")))
  listing <- chooser_listing(merra, "data")
  expect_identical(listing$rows$type, c("NetCDF data", "NetCDF data"))
  # Folders and ZIPs are read for .nc files only.
  expect_identical(listing$netcdf, 0L)
  expect_true(holds_files(merra, "[.]nc4$"))
  expect_false(holds_files(merra, "[.]nc$"))
  shiny::testServer(chooser_server(), {
    open("data", root)
    act("use", merra)
    expect_null(taken())
    expect_identical(dialog()$folder, merra)
    expect_match(dialog()$problem, "no NetCDF (.nc) files, only .nc4 files", fixed = TRUE)
    expect_match(dialog()$problem, "SCORCH reads .nc files from folders", fixed = TRUE)
    expect_match(dialog()$problem, "A single .nc4 file can be opened directly", fixed = TRUE)
    act("go", file.path(merra, "MERRA2_400.20010601.nc4"))
    expect_identical(taken(), list(kind = "data", path = file.path(merra, "MERRA2_400.20010601.nc4")))
  })
})

test_that("a folder whose NetCDF files are in its subfolders is answered with what works", {
  root <- test_folder()
  # Downloads unpacked into a folder each, all under the one file name.
  yearly <- file.path(root, "yearly downloads")
  years <- file.path(yearly, 2001:2005)
  for (year in years) {
    dir.create(year, recursive = TRUE)
    file.create(file.path(year, "data_stream-oper_stepType-instant.nc"))
  }
  # A folder without NetCDF files of its own is not named, whatever lies deeper in it.
  notes <- file.path(yearly, "1999 notes")
  dir.create(file.path(notes, "deeper"), recursive = TRUE)
  file.create(file.path(notes, "readme.txt"), file.path(notes, "deeper", "old.nc"))

  # The first four are found, to name three and to know of more.
  expect_identical(netcdf_subfolders(c(notes, years), seconds = Inf), 2:5)
  # No more folders than asked are looked into, and none once the time is up.
  expect_identical(netcdf_subfolders(c(notes, years), most = 3L, seconds = Inf), 2:3)
  expect_identical(netcdf_subfolders(c(notes, years), seconds = -1), integer())
  expect_identical(netcdf_subfolders(years, seconds = -1), 1L)
  expect_identical(netcdf_subfolders(character()), integer())
  expect_identical(in_subfolders(c("a b", "c")), paste(
    "This folder has no NetCDF (.nc) files of its own. They are in its subfolders: a b, c. Open one of them and click",
    "Use this folder. To analyze them together, put the subfolders into one ZIP file, or the files into one folder,",
    "each under its own name."))

  open_one <- "Open one of them and click Use this folder."
  together <- paste("To analyze them together, put the subfolders into one ZIP file,",
                    "or the files into one folder, each under its own name.")
  # The dialog of this test notes the folders it lists and looks into subfolders for as long
  # as that takes on a busy computer: the limit of a second is tried above.
  listed <- character()
  counting <- with_stand_ins(scorch_chooser, chooser_listing = function(folder, kind, limit = 2000L) {
    listed <<- c(listed, folder)
    chooser_listing(folder, kind, limit)
  }, netcdf_subfolders = function(folders) netcdf_subfolders(folders, seconds = Inf))
  shiny::testServer(chooser_server(counting), {
    # Highlighted or typed while the folder above is open, and as the open folder itself.
    for (from in c(root, yearly)) {
      open("data", from)
      before <- length(listed)
      act("use", yearly)
      expect_null(taken())
      expect_identical(dialog()$folder, yearly)
      expect_identical(dialog()$problem, paste(
        "This folder has no NetCDF (.nc) files of its own. They are in its subfolders: 2001, 2002, 2003 and others.",
        open_one, together))
      # The folder is read once, for the list that is shown with the answer.
      expect_identical(listed[seq_along(listed) > before], yearly)
    }
    expect_match(output$chooser$html, "They are in its subfolders: 2001, 2002, 2003 and others.", fixed = TRUE)
    # What the answer says works: one of the named folders is taken.
    act("use", years[1L])
    expect_identical(taken(), list(kind = "data", path = years[1L]))

    # Three or fewer are all named.
    unlink(years[4:5], recursive = TRUE)
    open("data", root)
    act("use", yearly)
    expect_identical(dialog()$problem, paste(
      "This folder has no NetCDF (.nc) files of its own. They are in its subfolders: 2001, 2002, 2003.", open_one, together))
    # With one such folder there is nothing to put together.
    unlink(years[2:3], recursive = TRUE)
    act("use", yearly)
    expect_identical(dialog()$problem, paste("This folder has no NetCDF (.nc) files of its own.",
                                             "They are in its subfolder: 2001. Open it and click Use this folder."))
    # Subfolders are looked into, not what they hold in folders of their own.
    unlink(years[1L], recursive = TRUE)
    act("use", yearly)
    expect_null(taken())
    expect_identical(dialog()$folder, yearly)
    expect_match(dialog()$problem, "This folder has no NetCDF (.nc) files. Open the folder", fixed = TRUE)
  })
})

test_that("a path pasted in double quotes is opened", {
  expect_identical(unquoted(c("\"C:/my data/era5.zip\"", "plain", "\"", "\"\"", "\"\"twice\"\"")),
                   c("C:/my data/era5.zip", "plain", "\"", "", "\"twice\""))
  root <- test_folder()
  folder <- file.path(root, "my data")
  dir.create(folder)
  file <- file.path(folder, "era5.nc")
  file.create(file)
  shiny::testServer(chooser_server(), {
    open("data", root)
    act("go", paste0("\"", folder, "\""))
    expect_identical(dialog()$folder, folder)
    expect_null(dialog()$problem)
    # Only the quotes: nothing to open.
    act("go", "\"\"")
    expect_identical(dialog()$folder, folder)
    act("go", paste0("  \"", file, "\"  "))
    expect_identical(taken(), list(kind = "data", path = file))
    if (.Platform$OS.type == "windows") {
      # As Explorer copies it, with backslashes.
      open("data", root)
      act("go", paste0("\"", gsub("/", "\\", file, fixed = TRUE), "\""))
      expect_identical(taken(), list(kind = "data", path = file))
    }
  })
})

test_that("a folder that disappears while the dialog shows it does not close the window", {
  root <- test_folder()
  stick <- file.path(root, "usb-stick")
  moved <- "The dialog now shows a folder that still exists."
  steps <- list(
    list(kind = "data", do = "go", path = file.path(root, "no", "such", "place"), says = "That path could not be found: "),
    list(kind = "data", do = "go", path = stick, says = "That path could not be found: "),
    list(kind = "data", do = "go", path = file.path(stick, "sub"), says = "That path could not be found: "),
    list(kind = "data", do = "use", path = stick, says = "That folder could not be found: "),
    list(kind = "data", do = "use", path = file.path(stick, "a.nc"), says = "That folder could not be found: ", named = stick),
    list(kind = "data", do = "use", path = NULL, says = "That folder could not be found: ", named = stick),
    list(kind = "output", do = "use", path = stick, says = "That folder could not be found: "),
    list(kind = "output", do = "mkdir", path = "results here", says = "No folder was created.", named = ""),
    # The folder above is still there, and so is a place: nothing to report.
    list(kind = "data", do = "up", path = NULL, says = NULL),
    list(kind = "data", do = "go", path = root, says = NULL))
  # A folder on a drive letter that Windows does not have at the moment.
  unplugged <- if (.Platform$OS.type == "windows") paste0(LETTERS[-(1:2)], ":/era5/2019") else character()
  unplugged <- utils::head(unplugged[!dir.exists(dirname(dirname(unplugged)))], 1L)
  shiny::testServer(chooser_server(), {
    for (i in seq_along(steps)) {
      step <- steps[[i]]
      dir.create(file.path(stick, "sub"), recursive = TRUE)
      file.create(file.path(stick, "a.nc"))
      open(step$kind, stick)
      expect_identical(dialog()$folder, stick)
      # Renamed, as in Explorer.
      if (!suppressWarnings(file.rename(stick, paste0(stick, "-renamed-", i)))) unlink(stick, recursive = TRUE)
      expect_false(dir.exists(stick))
      act(step$do, step$path)
      label <- paste(step$kind, step$do, step$path)
      expect_false(session$isClosed(), label = label)
      expect_null(taken(), label = label)
      expect_identical(dialog()$folder, root, label = label)
      named <- if (is.null(step$named)) step$path else step$named
      # The folder that is gone is named once, by the first message or by the second.
      gone <- if (identical(named, stick)) moved else paste(stick, "is no longer available.", moved)
      expect_identical(dialog()$problem, if (!is.null(step$says)) c(paste0(step$says, named), gone), label = label)
      expect_false(dir.exists(stick), label = label)
    }
    # Both messages are in the one alert of the dialog, each on its own line.
    dir.create(stick)
    open("data", stick)
    unlink(stick, recursive = TRUE)
    act("go", "missing.nc")
    html <- output$chooser$html
    expect_match(html, "That path could not be found: missing.nc\\s*<br/>\\s*[^<]*is no longer available")
    expect_length(gregexpr("role=\"alert\"", html, fixed = TRUE)[[1L]], 1L)

    # Everything above is gone as well: the nearest folder that is left.
    deep <- file.path(root, "drive", "era5", "2019")
    dir.create(deep, recursive = TRUE)
    open("data", deep)
    unlink(file.path(root, "drive"), recursive = TRUE)
    act("up")
    expect_false(session$isClosed())
    expect_identical(dialog()$folder, root)
    expect_identical(dialog()$problem, paste(deep, "is no longer available.", moved))

    # An unplugged drive leaves nothing above the folder: a place that is still there.
    if (length(unplugged)) {
      expect_identical(nearest_folder(unplugged), NA_character_)
      open("data", root)
      environment(open)$show("data", unplugged)
      expect_false(identical(dialog()$folder, root))
      expect_true(dir.exists(dialog()$folder))
      expect_identical(dialog()$problem, paste(unplugged, "is no longer available.", moved))
    }
  })
  expect_identical(nearest_folder(root), root)
  expect_identical(nearest_folder(file.path(root, "a", "b", "c")), root)
})

test_that("a listing that fails, or a name that is not valid text, does not stop the dialog", {
  root <- test_folder()
  file.create(file.path(root, c("ordinary.nc", "second.nc")))
  # Windows and macOS cannot store such a name, so one is added to what the folder holds.
  odd <- file.path(root, "caf\xe9.nc")
  listing <- listing_with(list.files = function(...) c(base::list.files(...), odd))
  found <- NULL
  expect_error(found <- listing(root, "data"), NA)
  expect_true(all(c("ordinary.nc", "second.nc") %in% found$rows$name))
  if (!validEnc(odd)) {
    expect_identical(found$rows$name, c("ordinary.nc", "second.nc"))
    expect_identical(found$netcdf, 2L)
  }

  # Whatever else goes wrong while a folder is read: an empty list and a message.
  failing <- chooser_listing_as(function(folder, kind, limit = 2000L) {
    if (length(folder)) stop("the folder cannot be read") else chooser_listing(folder, kind, limit)
  })
  shiny::testServer(chooser_server(failing), {
    open("data", root)
    expect_false(session$isClosed())
    expect_identical(dialog()$folder, root)
    expect_identical(nrow(dialog()$rows), 0L)
    expect_identical(dialog()$problem, "The contents of this folder could not be read. A path can still be typed below.")
    expect_match(output$chooser$html, "could not be read", fixed = TRUE)
  })
})

test_that("text that is not valid UTF-8 is answered in the dialog and does not close the window", {
  root <- test_folder()
  dir.create(file.path(root, "sub"))
  # Half an emoji as a browser sends it, and a letter of Latin-1 in text called UTF-8.
  half <- rawToChar(as.raw(c(0x78, 0xed, 0xb0, 0x80, 0x79)))
  latin <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9)))
  Encoding(half) <- "UTF-8"
  Encoding(latin) <- "UTF-8"
  expect_false(any(validEnc(c(half, latin))))
  shiny::testServer(chooser_server(), {
    for (text in c(half, latin)) for (do in c("up", "go", "use", "mkdir")) {
      kind <- if (do == "mkdir") "output" else "data"
      open(kind, root)
      act(do, text)
      label <- paste(do, "in the dialog for", kind)
      expect_false(session$isClosed(), label = label)
      expect_null(taken(), label = label)
      # Up takes no text: the page sends none with it, and what it does with one is left open.
      if (do == "up") next
      expect_identical(dialog()$folder, root, label = label)
      expect_identical(dialog()$problem, "That text holds characters that cannot be read. Type the path again.", label = label)
    }
    expect_match(output$chooser$html, "That text holds characters that cannot be read.", fixed = TRUE)
    # Nothing was made under such a name, and the dialog goes on as before.
    expect_identical(list.files(root), "sub")
    act("go", "sub")
    expect_identical(dialog()$folder, file.path(root, "sub"))
    expect_null(dialog()$problem)
  })
})

test_that("a large folder is counted whole, and only the listed rows are looked up", {
  root <- test_folder()
  for (name in c("b folder", "A folder", "looks like a file.nc")) dir.create(file.path(root, name))
  file.create(file.path(root, c(sprintf("era5_%03d.nc", 1:40), "archive.zip", "grid.nc4", "notes.txt")))
  whole <- chooser_listing(root, "data")
  expect_identical(nrow(whole$rows), 45L)
  expect_identical(whole$hidden, 0L)
  short <- chooser_listing(root, "data", limit = 5L)
  expect_identical(short$rows$name, c("A folder", "b folder", "looks like a file.nc", "archive.zip", "era5_001.nc"))
  expect_identical(short$rows$folder, c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_identical(as.list(short$rows), lapply(whole$rows, utils::head, 5L))
  expect_true(all(file.exists(short$rows$path)))
  # Counts are for the whole folder; a folder is not a NetCDF file whatever its name.
  expect_identical(short$hidden, 40L)
  expect_identical(short$netcdf, 40L)
  expect_identical(whole$netcdf, 40L)
  expect_identical(chooser_listing(root, "output", limit = 2L)$hidden, 1L)

  # Size, date and full path are asked of the file system for the listed rows only.
  looked_up <- integer()
  listing <- listing_with(
    file.info = function(...) {
      looked_up <<- c(looked_up, length(..1))
      base::file.info(...)
    },
    normalizePath = function(path, ...) {
      looked_up <<- c(looked_up, length(path))
      base::normalizePath(path, ...)
    })
  expect_identical(listing(root, "data", limit = 5L), short)
  expect_identical(looked_up, c(5L, 5L))
})

test_that("Use this folder does not read a listed folder a second time", {
  root <- test_folder()
  era5 <- file.path(root, "era5")
  other <- file.path(root, "other")
  for (folder in c(era5, other)) {
    dir.create(folder)
    file.create(file.path(folder, c("2001.nc", "2002.nc")))
  }
  listed <- character()
  counting <- chooser_listing_as(function(folder, kind, limit = 2000L) {
    listed <<- c(listed, folder)
    chooser_listing(folder, kind, limit)
  })
  shiny::testServer(chooser_server(counting), {
    open("data", era5)
    expect_identical(dialog()$netcdf, 2L)
    act("use", era5)
    expect_identical(taken(), list(kind = "data", path = era5))
    # A typed folder is only looked into for its first NetCDF file.
    open("data", era5)
    act("use", other)
    expect_identical(taken(), list(kind = "data", path = other))
  })
  expect_identical(listed, c(era5, era5))
})

test_that("Use this folder takes a folder whose NetCDF files arrived after it was listed", {
  root <- test_folder()
  filling <- file.path(root, "download in progress")
  empty <- file.path(root, "stays empty")
  for (folder in c(filling, empty)) dir.create(folder)
  arrived <- file.path(filling, "era5_t2m_2001.nc")
  shiny::testServer(chooser_server(), {
    # With the folder written in the field, and with no text at all.
    for (path in list(filling, NULL)) {
      unlink(arrived)
      open("data", filling)
      expect_identical(dialog()$netcdf, 0L)
      # The download ends while the dialog still shows the folder as it was.
      file.create(arrived)
      act("use", path)
      expect_identical(taken(), list(kind = "data", path = filling))
      expect_null(dialog())
    }
    # A folder that is still empty is refused as before.
    open("data", empty)
    act("use", empty)
    expect_null(taken())
    expect_identical(dialog()$folder, empty)
    expect_match(dialog()$problem, "This folder has no NetCDF (.nc) files. Open the folder", fixed = TRUE)
  })
})

test_that("text that is the open folder but for the blanks at its ends means that folder", {
  expect_true(same_trimmed("/data/era5", "/data/era5 "))
  expect_true(same_trimmed("/data/era5", "/data/era5"))
  expect_true(same_trimmed(" /data/era5\t", "/data/era5  "))
  # The page also leaves out blanks that are not the space of the keyboard.
  expect_true(same_trimmed("/data/era5", paste0("/data/era5", intToUtf8(160L))))
  expect_true(same_trimmed("/data/era5", paste0("/data/era5", intToUtf8(12288L))))
  expect_false(same_trimmed("/data/era5", "/data/era5 2019"))
  expect_false(same_trimmed("/data/era", "/data/era5 "))
  expect_false(same_trimmed("", "/data/era5 "))
  # Text that is not valid cannot be trimmed: it equals nothing, and raises no error.
  odd <- rawToChar(as.raw(c(0x2f, 0x63, 0x61, 0x66, 0xe9)))
  Encoding(odd) <- "UTF-8"
  expect_false(same_trimmed(odd, paste0(odd, " ")))
  expect_false(same_trimmed("/data/era5", paste0(odd, " ")))

  # Every system can hold a name that ends in a no-break space, which the page trims as well.
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "names beyond ASCII need a UTF-8 session")
  root <- test_folder()
  ending <- file.path(root, paste0("era5", intToUtf8(160L)))
  twin <- file.path(root, "era5")
  other <- file.path(root, "other")
  for (folder in c(ending, other)) {
    dir.create(folder)
    file.create(file.path(folder, "2001.nc"))
  }
  shiny::testServer(chooser_server(), {
    for (kind in c("data", "output")) {
      open(kind, ending)
      expect_identical(dialog()$folder, ending)
      # What the page sends for the open folder: its path without the blank.
      act("use", twin)
      expect_identical(taken(), list(kind = kind, path = ending))
    }
    # Another folder written in the field is still that folder.
    open("data", ending)
    act("use", other)
    expect_identical(taken(), list(kind = "data", path = other))
  })
})

test_that("an open folder whose name ends in a space is taken with Use this folder", {
  # Windows cannot hold such a name; Linux and macOS can.
  skip_on_os("windows")
  root <- test_folder()
  ending <- file.path(root, "era5 ")
  dir.create(ending)
  file.create(file.path(ending, "2001.nc"))
  shiny::testServer(chooser_server(), {
    for (kind in c("data", "output")) {
      open(kind, ending)
      expect_identical(dialog()$folder, ending)
      # The page and R trim the text of the path field: the space does not arrive.
      act("use", trimws(ending))
      expect_identical(taken(), list(kind = kind, path = ending))
      expect_null(dialog())
    }
  })
})

test_that("Documents, Desktop and Downloads are where Windows says they are", {
  root <- test_folder()
  withr::local_envvar(SCORCH_TEST_PROFILE = root)
  expect_identical(expand_percent("%SCORCH_TEST_PROFILE%/OneDrive/Documents"), paste0(root, "/OneDrive/Documents"))
  expect_identical(expand_percent("%SCORCH_TEST_PROFILE%/%SCORCH_TEST_PROFILE%"), paste0(root, "/", root))
  expect_identical(expand_percent("%SCORCH_NAME_THAT_IS_NOT_SET%/Documents"), "%SCORCH_NAME_THAT_IS_NOT_SET%/Documents")
  expect_identical(expand_percent("C:/Users/me/Documents"), "C:/Users/me/Documents")

  # With OneDrive backup the folder is not the one of that name in the profile.
  moved <- file.path(root, "OneDrive", "Documents")
  dir.create(moved, recursive = TRUE)
  guessed <- chooser_places(known = character())
  places <- chooser_places(known = c(Documents = moved))
  expect_identical(places[["Documents"]], moved)
  expect_identical(places[names(places) != "Documents"], guessed[names(guessed) != "Documents"])
  # A folder Windows names but that is not there: the name is looked for as before.
  expect_identical(chooser_places(known = c(Documents = file.path(root, "not there"))), guessed)

  known <- shell_folders()
  if (.Platform$OS.type == "windows") {
    expect_true(all(names(known) %in% c("Downloads", "Desktop", "Documents")))
    expect_false(any(grepl("%USERPROFILE%", known, ignore.case = TRUE)))
    places <- chooser_places()
    for (name in names(known)[dir.exists(known)]) {
      expect_identical(places[[name]], normalizePath(known[[name]], winslash = "/"), label = name)
    }
  } else {
    expect_length(known, 0L)
  }
})

test_that("a place on a server is listed without asking the server, and opened only when it is clicked", {
  root <- test_folder()
  desktop <- file.path(root, "Desktop")
  dir.create(desktop)
  windows <- .Platform$OS.type == "windows"
  # Stand-ins that note every path the file system is asked for.
  asked <- character()
  noting <- function(ask) function(paths, ...) {
    asked <<- c(asked, paths)
    ask(paths, ...)
  }
  places_of <- with_stand_ins(chooser_places, dir.exists = noting(base::dir.exists),
                              normalizePath = noting(base::normalizePath))
  guessed <- chooser_places(known = character())
  # A folder that Windows keeps on a file server, as its registry writes it and with slashes.
  for (remote in c("\\\\fileserver.invalid\\home\\me\\Documents", "//fileserver.invalid/home/me/Documents")) {
    expect_true(on_server(remote))
    asked <- character()
    places <- places_of(known = c(Desktop = desktop, Documents = remote))
    # The folders of this computer are asked for; the server, which may not answer, is not.
    expect_true(desktop %in% asked)
    expect_false(any(on_server(asked)))
    expect_identical(places[["Desktop"]], desktop)
    # Listed as R writes an open folder on a server, so that the place is marked while it is open.
    expect_identical(places[["Documents"]], if (windows) "\\\\fileserver.invalid/home/me/Documents" else remote)
    expect_identical(places[!names(places) %in% c("Desktop", "Documents")],
                     guessed[!names(guessed) %in% c("Desktop", "Documents")])
  }
  expect_identical(on_server(c("C:/Users/me", "/home/me", "relative/path", "/", NA)), rep(FALSE, 5L))

  # The dialog with its first place on a server.
  places <- c(Downloads = "\\\\fileserver.invalid/home/me/Downloads", Home = root, Desktop = desktop)
  dialog_of <- with_stand_ins(scorch_chooser, chooser_places = function(...) places,
                              dir.exists = noting(base::dir.exists), normalizePath = noting(base::normalizePath))
  # A folder that is gone with everything above it: an unplugged drive, or a share that is lost.
  gone <- if (windows) paste0(LETTERS[-(1:2)], ":/era5/2019") else "//fileserver.invalid/data/era5/2019"
  gone <- utils::head(gone[!dir.exists(dirname(dirname(gone)))], 1L)
  asked <- character()
  shiny::testServer(chooser_server(dialog_of), {
    # Without a folder to return to, it opens in the first place on this computer.
    open("data")
    expect_identical(dialog()$folder, root)
    expect_null(dialog()$problem)
    expect_match(output$chooser$html, "data-go=\"\\\\fileserver.invalid/home/me/Downloads\"", fixed = TRUE)
    # Such a place also stands in for a folder of which nothing is left.
    if (length(gone)) {
      environment(open)$show("data", gone)
      expect_identical(dialog()$folder, root)
      expect_match(dialog()$problem, "is no longer available. The dialog now shows a folder that still exists.", fixed = TRUE)
    }
    # The place on the server was asked for by neither.
    expect_true(root %in% asked)
    expect_false(places[["Downloads"]] %in% asked)
  })
})

test_that("details that Windows refuses raise no warnings, and the user's own folders stay listed", {
  root <- test_folder()
  # Folders of the user's own that only share a junction's name are listed.
  mine <- c("My Documents", "Recent", "Templates")
  for (name in c(mine, "$RECYCLE.BIN", "System Volume Information")) dir.create(file.path(root, name))
  expect_identical(chooser_listing(root, "output")$rows$name, mine)

  listing <- listing_with(file.info = function(...) {
    warning("cannot open file 'C:/Config.Msi': Access is denied")
    base::file.info(...)
  })
  expect_warning(expect_identical(listing(root, "output")$rows$name, mine), NA)
})

test_that("the junctions Windows keeps for old programs are not listed", {
  skip_on_os(c("mac", "linux", "solaris"))
  home <- Sys.getenv("USERPROFILE")
  old <- file.path(home, WINDOWS_JUNCTIONS)
  # They exist, but can be neither read nor looked into.
  old <- old[dir.exists(old) & file.access(old, 4L) != 0L]
  old <- basename(old[lengths(lapply(old, function(path) suppressWarnings(list.files(path)))) == 0L])
  skip_if_not(length(old) > 0L, "this profile has no junctions for old programs")
  found <- NULL
  expect_warning(found <- chooser_listing(home, "output"), NA)
  expect_false(any(old %in% found$rows$name))
})

test_that("a network share is the top of its path, and Up stops there", {
  expect_identical(share_root("//server/share/project/data"), "//server/share")
  expect_identical(share_root("\\\\server\\share"), "\\\\server\\share")
  expect_identical(share_root("\\\\server/share/data"), "\\\\server/share")
  for (folder in c("//server", "C:/Users/me", "/home/me", "relative/path")) expect_identical(share_root(folder), NA_character_)

  parents <- path_parents("//server/share/project/data")
  expect_identical(unname(parents), c("//server/share", "//server/share/project", "//server/share/project/data"))
  expect_identical(names(parents), c("\\\\server\\share", "project", "data"))
  # As R writes a share on Windows: backslashes first, then slashes.
  parents <- path_parents("\\\\server/share/data")
  expect_identical(unname(parents), c("\\\\server/share", "\\\\server/share/data"))
  expect_identical(names(parents), c("\\\\server\\share", "data"))
  expect_identical(path_parents("//server/share"), c("\\\\server\\share" = "//server/share"))

  root <- test_folder()
  below <- file.path(root, "below")
  dir.create(below)
  expect_identical(folder_above(below), root)
  # A drive or the top of the file system, and a share: its server is not a folder.
  expect_identical(folder_above(unname(path_parents(root)[1L])), NA_character_)
  expect_identical(folder_above("//localhost/share"), NA_character_)

  up <- function(html) opening_tag(html, "data-do=\"up\"")
  expect_false(grepl("disabled", up(dialog_html("output", below)), fixed = TRUE))
  on_share <- dialog_html("output", below, shown = "//localhost/share")
  expect_match(up(on_share), "disabled", fixed = TRUE)
  expect_length(gregexpr("class=\"chooser-crumb\"", on_share, fixed = TRUE)[[1L]], 1L)
  expect_match(on_share, ">\\\\localhost\\share</button>", fixed = TRUE)
})

test_that("a new folder that cannot be made is reported in plain words", {
  root <- test_folder()
  shiny::testServer(chooser_server(), {
    open("output", root)
    act("mkdir", "new results")
    expect_identical(dialog()$folder, file.path(root, "new results"))
    expect_null(dialog()$problem)
    act("mkdir", "a/b")
    expect_match(dialog()$problem, "Give the new folder a name without slashes", fixed = TRUE)
    expect_false(dir.exists(file.path(root, "new results", "a")))
    if (.Platform$OS.type == "windows") {
      # Windows keeps this name for itself and reports the folder as created.
      open("output", root)
      act("mkdir", "NUL")
      expect_false(session$isClosed())
      expect_identical(dialog()$folder, root)
      expect_identical(dialog()$problem, paste("A folder named NUL could not be created here.",
                                               "Give it another name, or choose a folder you can write to."))
    }
  })
})

test_that("the dialog names itself, its list and every row for assistive technology", {
  root <- test_folder()
  folder <- file.path(root, "yearly downloads")
  dir.create(folder)
  long <- sprintf("ERA5_hourly_2m_temperature_North_America_single_levels_download_%d.nc", 1996:1998)
  file.create(file.path(folder, c(long, "R&D <2012>.nc"[.Platform$OS.type != "windows"], "R&D 2012.nc")))
  html <- dialog_html("data", folder)
  # Names cut short on screen can be told apart by their title.
  for (name in long) expect_match(html, sprintf("title=\"%s\"", name), fixed = TRUE)
  expect_match(html, "title=\"R&amp;D 2012.nc\"", fixed = TRUE)
  if (.Platform$OS.type != "windows") expect_match(html, "title=\"R&amp;D &lt;2012&gt;.nc\"", fixed = TRUE)
  expect_length(gregexpr("class=\"chooser-name\" title=|title=\"[^\"]*\" class=\"chooser-name\"", html)[[1L]],
                nrow(chooser_listing(folder, "data")$rows))

  # The list says which folder it shows, instead of one name for every folder.
  expect_match(opening_tag(html, "id=\"chooser_list\""), "aria-label=\"Folders and files in yearly downloads\"", fixed = TRUE)
  expect_match(opening_tag(dialog_html("output", folder), "id=\"chooser_list\""),
               "aria-label=\"Folders in yearly downloads\"", fixed = TRUE)

  # The window is a named dialog, and a newly opened folder is not read out as a whole.
  for (kind in names(CHOOSER_KINDS)) {
    window <- as.character(chooser_dialog(kind))
    frame <- opening_tag(window, "id=\"shiny-modal\"")
    expect_match(frame, "role=\"dialog\"", fixed = TRUE)
    expect_match(frame, "aria-modal=\"true\"", fixed = TRUE)
    expect_match(frame, sprintf("aria-label=\"%s\"", CHOOSER_KINDS[[kind]]$title), fixed = TRUE)
    expect_match(opening_tag(window, "id=\"chooser\""), "aria-live=\"off\"", fixed = TRUE)
  }

  # Two messages share the one alert; an unread size or date is empty, not "NA".
  expect_match(dialog_html("data", folder, problem = c("First.", "Second.")), "First.\\s*<br/>\\s*Second.")
  now <- c(list(kind = "data", folder = folder, problem = NULL), chooser_listing(folder, "data"))
  now$rows$size <- NA_real_
  now$rows$modified <- as.POSIXct(NA)
  unread <- as.character(chooser_body(now, chooser_places(known = character())))
  expect_false(grepl(">NA<", unread, fixed = TRUE))
  expect_false(grepl("NA bytes", unread, fixed = TRUE))
})
