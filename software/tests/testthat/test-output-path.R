# Results folders with a long path: the names a run makes beside and inside
# its results, and the check that stops a run Windows could not finish.

example_file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
scratch_name <- scoRch.simulator:::scratch_name
output_path_problem <- scoRch.simulator:::output_path_problem

# A path of exactly `n` characters below `from`. The default names nothing on
# any system and is absolute for Windows.
path_of <- function(n, from = "C:/made-up") {
  stopifnot(n >= nchar(from) + 2L)
  path <- from
  while (nchar(path) < n) {
    left <- n - nchar(path) - 1L
    # Never leave a single character, which the next separator would use up.
    path <- paste0(path, "/", strrep("a", if (left > 41L) 40L else if (left == 41L) 39L else left))
  }
  path
}

# The smallest complete run: one cluster on each of three days, one event.
small_tmax <- function() tmax_from_array(array(30, c(3, 3, 3)), as.Date("2000-06-01") + 0:2, 1:3, 1:3)
small_config <- function(out) list(output = list(directory = out), power_law = list(bootstrap_repetitions = 1),
                                   clustering = list(eps_values = 1, min_samples_values = 1))

test_that("a scratch name has a fixed length, is free, differs each time and leaves R's random numbers alone", {
  where <- normalizePath(tempfile("scorch-scratch-"), winslash = "/", mustWork = FALSE)
  dir.create(where)
  stage <- scratch_name(".scorch-stage-", where)
  expect_identical(dirname(stage), where)
  expect_match(basename(stage), "^[.]scorch-stage-[0-9a-f]{8}$")
  expect_false(file.exists(stage))
  expect_false(identical(scratch_name(".scorch-stage-", where), stage))
  plots <- vapply(1:50, function(i) basename(scratch_name(".scorch-plot-", where, ".png")), "")
  expect_true(all(grepl("^[.]scorch-plot-[0-9a-f]{8}[.]png$", plots)))
  expect_gt(length(unique(plots)), 1L)
  # The names of a run, with the lengths output_path_problem() counts.
  prefixes <- scoRch.simulator:::SCRATCH_PREFIX
  expect_identical(nchar(basename(scratch_name(prefixes[["stage"]], where))), 22L)
  expect_identical(nchar(basename(scratch_name(prefixes[["saved"]], where))), 22L)
  expect_identical(nchar(basename(scratch_name(prefixes[["plot"]], where, ".png"))), 25L)
  expect_length(list.files(where, all.files = TRUE, no.. = TRUE), 0L)
  # Windows counts every character of a path, so none is added that names
  # nothing: no "./" for the working folder, no second separator after the
  # top folder of a drive.
  expect_match(scratch_name(".scorch-stage-", "."), "^[.]scorch-stage-[0-9a-f]{8}$")
  expect_match(scratch_name(".scorch-stage-", "C:/"), "^C:/[.]scorch-stage-[0-9a-f]{8}$")
  expect_match(scratch_name(".scorch-plot-", "figures/events/Type_1", ".png"),
               "^figures/events/Type_1/[.]scorch-plot-[0-9a-f]{8}[.]png$")
  withr::local_preserve_seed()
  set.seed(1)
  expected <- runif(1)
  set.seed(1)
  scratch_name(".scorch-stage-", where)
  expect_identical(runif(1), expected)
})

test_that("a scratch name pads a short draw, keeps the end of a long one and draws again when it is taken", {
  where <- normalizePath(tempfile("scorch-scratch-"), winslash = "/", mustWork = FALSE)
  dir.create(where)
  file.create(file.path(where, ".scorch-stage-00000abc"))
  draws <- c("abc", "abc", "1234567890abcdef")
  temporary <- base::tempfile
  testthat::local_mocked_bindings(tempfile = function(pattern = "file", ...) {
    if (!identical(pattern, "") || !length(draws)) return(temporary(pattern, ...))
    drawn <- draws[1L]
    draws <<- draws[-1L]
    file.path(tempdir(), drawn)
  }, .package = "base")
  expect_identical(basename(scratch_name(".scorch-stage-", where)), ".scorch-stage-90abcdef")
  expect_length(draws, 0L)
})

test_that("the longest path of a run is the one the check counts", {
  files <- scoRch.simulator:::managed_output_files()
  folders <- scoRch.simulator:::output_folders()
  expect_identical(files[which.max(nchar(files))], "catalogs/02_regionally_extensive_heatwaves.xlsx")
  expect_identical(max(nchar(files)), 47L)
  # A plot while it is drawn, in the folder of an event type, is as long.
  expect_identical(max(nchar(folders)) + 1L + 25L, 47L)
  # Windows makes a folder only up to 247 characters and a file up to 259, so
  # every folder has to stay 12 characters behind the longest file.
  expect_lte(max(nchar(folders)) + 12L, 47L)
  # A run in an ordinary folder: every name it makes and every workbook it
  # writes stays within 22 characters beside the results and 47 below that.
  local_figure_test_dpi()
  parent <- normalizePath(tempfile("scorch-names-"), winslash = "/", mustWork = FALSE)
  out <- file.path(parent, "results")
  seen <- character()
  write <- scoRch.simulator:::write_workbook
  testthat::local_mocked_bindings(
    scratch_name = function(...) {
      path <- scratch_name(...)
      seen <<- c(seen, path)
      path
    },
    write_workbook = function(path, sheets) {
      seen <<- c(seen, path)
      write(path, sheets)
    }, .package = "scoRch.simulator")
  suppressMessages(suppressWarnings(run_scorch(small_config(out), tmax = small_tmax())))
  expect_true(all(startsWith(seen, paste0(parent, "/.scorch-"))))
  below <- substring(seen, nchar(parent) + 2L)
  expect_identical(max(nchar(below)), 22L + 1L + 47L)
  expect_true(any(grepl("^[.]scorch-stage-[0-9a-f]{8}/catalogs/02_regionally_extensive_heatwaves[.]xlsx$", below)))
  expect_true(any(grepl("^[.]scorch-stage-[0-9a-f]{8}/figures/events/Type_[1-4]/[.]scorch-plot-[0-9a-f]{8}[.]png$", below)))
  expect_true(any(grepl("^[.]scorch-saved-[0-9a-f]{8}$", below)))
  expect_identical(max(nchar(list.files(out, recursive = TRUE, all.files = TRUE))), 47L)
  # Nothing of it is left beside or inside the results.
  expect_identical(list.files(parent, all.files = TRUE, no.. = TRUE), "results")
  expect_false(any(grepl("[.]scorch-", list.files(out, recursive = TRUE, all.files = TRUE))))
})

test_that("a results folder is refused exactly when a path of the run would reach 260 characters", {
  testthat::local_mocked_bindings(long_paths_enabled = function() FALSE, .package = "scoRch.simulator")
  problem <- function(out) output_path_problem(out, windows = TRUE)
  # A name of up to 22 characters: the staging folder beside it is the longer
  # one, so the folder around the results may have 259 - 47 - 1 - 22 - 1 = 188.
  for (name in c("r", "results", "SCORCH_run_001", strrep("n", 22L))) {
    expect_null(problem(file.path(path_of(188L), name)))
    refused <- problem(file.path(path_of(189L), name))
    expect_length(refused, 1L)
    expect_match(refused, "The results folder lies in a folder whose path has 189 characters, ", fixed = TRUE)
    expect_match(refused, " only when that path has at most 188 characters; ", fixed = TRUE)
    expect_true(endsWith(refused, paste0("; choose a results folder with a shorter path than ", path_of(189L), "/", name)))
  }
  expect_match(problem(file.path(path_of(230L), "results")), "whose path has 230 characters, .* at most 188 characters; ")
  # A longer name: the results folder itself may have 259 - 47 - 1 = 211.
  for (length in c(23L, 40L, 120L)) {
    name <- strrep("n", length)
    expect_null(problem(file.path(path_of(211L - length - 1L), name)))
    refused <- problem(file.path(path_of(212L - length - 1L), name))
    expect_match(refused, "The results folder has a path of 212 characters, ", fixed = TRUE)
    expect_match(refused, " only when that path has at most 211 characters; ", fixed = TRUE)
    expect_true(endsWith(refused, paste0(" than ", path_of(212L - length - 1L), "/", name)))
  }
  # Directly in the top folder of a drive, whose separator is counted once.
  expect_null(problem("C:/results"))
  expect_null(problem(paste0("C:/", strrep("n", 208L))))
  expect_match(problem(paste0("C:/", strrep("n", 209L))), "has a path of 212 characters, ", fixed = TRUE)
  # Windows counts a path as it is written: a "." in it, a ".." that leads
  # back, a doubled separator and a separator at the end all take their room.
  expect_null(problem(file.path(path_of(186L), ".", "results")))
  expect_match(problem(file.path(path_of(187L), ".", "results")), "whose path has 189 characters, ", fixed = TRUE)
  expect_null(problem(file.path(path_of(182L), "up", "..", "results")))
  expect_match(problem(file.path(path_of(183L), "up", "..", "results")), "whose path has 189 characters, ", fixed = TRUE)
  expect_null(problem(paste0(path_of(179L), "/", strrep("n", 30L), "/")))
  expect_match(problem(paste0(path_of(180L), "/", strrep("n", 30L), "/")), "has a path of 212 characters, ", fixed = TRUE)
  expect_match(problem(paste0("C:\\made-up\\", strrep("n", 201L))), paste0(" than C:/made-up/", strrep("n", 201L), "$"))
  # Other systems have no such limit, and neither has Windows with long paths.
  expect_null(output_path_problem(file.path(path_of(250L), "results"), windows = FALSE))
  testthat::local_mocked_bindings(long_paths_enabled = function() TRUE, .package = "scoRch.simulator")
  expect_null(problem(file.path(path_of(250L), "results")))
})

test_that("a relative results folder is measured from the working folder", {
  testthat::local_mocked_bindings(long_paths_enabled = function() FALSE, .package = "scoRch.simulator")
  base <- tempfile("scorch-relative-")
  dir.create(base)
  withr::local_dir(base)
  here <- nchar(getwd())
  skip_if(here > 180L, "The temporary folder leaves no room for this test.")
  # One folder below the working folder brings the place of the results to `n` characters.
  below <- function(n) strrep("b", n - here - 1L)
  expect_null(output_path_problem("results", windows = TRUE))
  expect_null(output_path_problem(file.path(below(188L), "results"), windows = TRUE))
  refused <- output_path_problem(file.path(below(189L), "results"), windows = TRUE)
  expect_match(refused, "folder whose path has 189 characters, ", fixed = TRUE)
  expect_true(endsWith(refused, paste0(" than ", getwd(), "/", below(189L), "/results")))
  expect_false(dir.exists(below(189L)))
  # A name with a folder of its own, long enough to be the longer one.
  expect_null(output_path_problem(strrep("n", 211L - here - 1L), windows = TRUE))
  refused <- output_path_problem(strrep("n", 212L - here - 1L), windows = TRUE)
  expect_match(refused, "The results folder has a path of 212 characters, ", fixed = TRUE)
  expect_true(endsWith(refused, paste0(" than ", getwd(), "/", strrep("n", 212L - here - 1L))))
  # From the working folder itself, with and without "./" before the name.
  deep <- file.path(base, strrep("d", 188L - here - 1L))
  dir.create(deep)
  withr::local_dir(deep)
  expect_identical(nchar(getwd()), 188L)
  expect_null(output_path_problem("results", windows = TRUE))
  expect_null(output_path_problem("./results", windows = TRUE))
  expect_match(output_path_problem("one/results", windows = TRUE), "folder whose path has 192 characters, ", fixed = TRUE)
  expect_match(output_path_problem("../results", windows = TRUE),
               sprintf("folder whose path has %d characters, ", 188L + 3L), fixed = TRUE)
})

test_that("a character beyond the 16-bit plane counts twice, as Windows counts it", {
  path_length <- scoRch.simulator:::path_length
  expect_identical(path_length("C:/results"), 10L)
  expect_identical(path_length(rawToChar(as.raw(c(97L, 255L)))), 2L)
  skip_if_not(isTRUE(l10n_info()[["UTF-8"]]), "Needs a session that keeps such characters in a path.")
  face <- intToUtf8(128512L)
  expect_identical(path_length(intToUtf8(c(233L, 26085L))), 2L)
  expect_identical(path_length(paste0("ab", face)), 4L)
  testthat::local_mocked_bindings(long_paths_enabled = function() FALSE, .package = "scoRch.simulator")
  expect_null(output_path_problem(file.path(path_of(181L), strrep(face, 3L), "results"), windows = TRUE))
  refused <- output_path_problem(file.path(path_of(182L), strrep(face, 3L), "results"), windows = TRUE)
  expect_match(refused, "folder whose path has 189 characters, ", fixed = TRUE)
})

test_that("long paths count only with the switch of Windows on and a long path that can really be made", {
  enabled <- scoRch.simulator:::long_paths_enabled
  new_enough <- getRversion() >= "4.3.0"
  # The registry is read without the warnings it gives about other values.
  expect_silent(switch_value <- scoRch.simulator:::long_paths_switch())
  if (.Platform$OS.type != "windows") {
    expect_null(switch_value)
    expect_false(enabled())
  }
  # Anything but 1 means off, and nothing is tried.
  for (value in list(NULL, 0L, 2L, NA_integer_, "1", TRUE, c(1L, 1L))) {
    testthat::with_mocked_bindings(expect_false(enabled()), long_paths_switch = function() value,
                                   long_path_works = function() stop("a long path was tried"),
                                   .package = "scoRch.simulator")
  }
  for (value in list(1L, 1)) {
    testthat::with_mocked_bindings(expect_identical(enabled(), new_enough), long_paths_switch = function() value,
                                   long_path_works = function() TRUE, .package = "scoRch.simulator")
    testthat::with_mocked_bindings(expect_false(enabled()), long_paths_switch = function() value,
                                   long_path_works = function() FALSE, .package = "scoRch.simulator")
  }
  # With the switch read as on, the answer is what a long path really does in
  # this session, and the attempt leaves nothing behind.
  top <- tempfile("scorch-deep-")
  deep <- file.path(top, strrep("d", 130L), strrep("e", 130L))
  can <- isTRUE(tryCatch(dir.create(deep, recursive = TRUE, showWarnings = FALSE), error = function(e) FALSE)) &&
    dir.exists(deep)
  unlink(top, recursive = TRUE)
  expect_identical(scoRch.simulator:::long_path_works(), can)
  testthat::with_mocked_bindings(expect_identical(enabled(), can && new_enough), long_paths_switch = function() 1L,
                                 .package = "scoRch.simulator")
  expect_length(list.files(tempdir(), pattern = "^[.]scorch-long-", all.files = TRUE), 0L)
})

test_that("a run stops with the sentence before it reads data or makes a folder", {
  parent <- file.path(tempfile("scorch-stop-"), "not-made")
  out <- file.path(parent, "results")
  asked <- character()
  read <- 0L
  testthat::local_mocked_bindings(
    output_path_problem = function(out, ...) {
      asked <<- c(asked, out)
      "The results folder has a path that Windows cannot use"
    },
    read_configured_input = function(...) {
      read <<- read + 1L
      stop("the input was read")
    }, .package = "scoRch.simulator")
  messages <- character()
  expect_error(withCallingHandlers(run_scorch(list(input = list(file = example_file), output = list(directory = out))),
    message = function(condition) {
      messages <<- c(messages, conditionMessage(condition))
      invokeRestart("muffleMessage")
    }), "^The results folder has a path that Windows cannot use$")
  expect_identical(asked, out)
  expect_identical(read, 0L)
  expect_length(messages, 0L)
  expect_false(dir.exists(dirname(parent)))
  # The same for temperature that is already in R.
  expect_error(run_scorch(small_config(out), tmax = small_tmax()), "^The results folder has a path that Windows cannot use$")
  expect_false(dir.exists(dirname(parent)))
})

test_that("on Windows a run fills the longest accepted results folder, again on a rerun, and stops one character beyond", {
  skip_if_not(.Platform$OS.type == "windows", "Only Windows limits the length of a path in this way.")
  local_figure_test_dpi()
  # Where long paths are switched on, the check is tested as if they were off.
  testthat::local_mocked_bindings(long_paths_enabled = function() FALSE, .package = "scoRch.simulator")
  base <- file.path(normalizePath(tempdir(), winslash = "/"), basename(tempfile("scorch-long-")))
  skip_if(nchar(base) > 150L, "The temporary folder leaves no room to build the longest accepted path.")
  place <- path_of(188L, base)
  dir.create(place, recursive = TRUE)
  out <- file.path(place, "SCORCH_run_001")
  expect_null(output_path_problem(out))
  # Every name the run makes, every workbook it writes and every file it moves.
  seen <- moved <- character()
  write <- scoRch.simulator:::write_workbook
  testthat::local_mocked_bindings(
    scratch_name = function(...) {
      path <- scratch_name(...)
      seen <<- c(seen, path)
      path
    },
    write_workbook = function(path, sheets) {
      seen <<- c(seen, path)
      write(path, sheets)
    }, .package = "scoRch.simulator")
  rename <- base::file.rename
  testthat::local_mocked_bindings(file.rename = function(from, to) {
    moved <<- c(moved, from, to)
    rename(from, to)
  }, .package = "base")
  result <- suppressMessages(suppressWarnings(run_scorch(small_config(out), tmax = small_tmax())))
  expect_s3_class(result, "scorch_result")
  expect_identical(max(nchar(c(seen, moved))), 259L)
  expect_true(any(nchar(seen) == 259L & grepl("/catalogs/02_regionally_extensive_heatwaves[.]xlsx$", seen)))
  expect_true(any(nchar(seen) == 259L & grepl("/figures/events/Type_[1-4]/[.]scorch-plot-[0-9a-f]{8}[.]png$", seen)))
  expect_match(readLines(file.path(out, "Summary.txt"))[1], "results: COMPLETE")
  expect_equal(sheet_names(file.path(out, "catalogs", "02_regionally_extensive_heatwaves.xlsx")),
               c("Regional Days", "Regional Day Cells"))
  expect_length(list.files(file.path(out, "figures/events"), pattern = "^Event_[0-9]+[.]png$", recursive = TRUE), 1L)
  expect_identical(list.files(place, all.files = TRUE, no.. = TRUE), "SCORCH_run_001")
  expect_false(any(grepl("[.]scorch-", list.files(out, recursive = TRUE, all.files = TRUE))))

  # A rerun keeps the files it replaces in a folder beside the results until
  # the new ones are in place: the same longest path once more.
  writeLines("old", file.path(out, "catalogs", "02_regionally_extensive_heatwaves.xlsx"))
  writeLines("mine", file.path(out, "catalogs", "notes.txt"))
  seen <- moved <- character()
  again <- suppressMessages(suppressWarnings(run_scorch(small_config(out), tmax = small_tmax())))
  expect_s3_class(again, "scorch_result")
  expect_true(any(nchar(moved) == 259L &
                    grepl("/[.]scorch-saved-[0-9a-f]{8}/catalogs/02_regionally_extensive_heatwaves[.]xlsx$", moved)))
  expect_identical(max(nchar(c(seen, moved))), 259L)
  expect_equal(sheet_names(file.path(out, "catalogs", "02_regionally_extensive_heatwaves.xlsx")),
               c("Regional Days", "Regional Day Cells"))
  expect_equal(readLines(file.path(out, "catalogs", "notes.txt")), "mine")
  expect_identical(list.files(place, all.files = TRUE, no.. = TRUE), "SCORCH_run_001")
  expect_false(any(grepl("[.]scorch-", list.files(out, recursive = TRUE, all.files = TRUE))))

  # One character more: the run stops at its start with the sentence. It
  # reads nothing and makes nothing, not even the folder around the results.
  far <- path_of(189L, base)
  dir.create(far, recursive = TRUE)
  read <- 0L
  testthat::local_mocked_bindings(read_configured_input = function(...) {
    read <<- read + 1L
    stop("the input was read")
  }, .package = "scoRch.simulator")
  for (refused in c(file.path(far, "SCORCH_run_001"), file.path(place, "n", "results"))) {
    seen <- moved <- messages <- character()
    stopped <- tryCatch(withCallingHandlers(
      run_scorch(list(input = list(file = example_file), output = list(directory = refused))),
      message = function(condition) {
        messages <<- c(messages, conditionMessage(condition))
        invokeRestart("muffleMessage")
      }), error = function(e) e)
    expect_s3_class(stopped, "error")
    expect_identical(conditionMessage(stopped), output_path_problem(refused))
    expect_match(conditionMessage(stopped), sprintf("folder whose path has %d characters, ", nchar(dirname(refused))), fixed = TRUE)
    expect_match(conditionMessage(stopped), " at most 188 characters; ", fixed = TRUE)
    expect_true(endsWith(conditionMessage(stopped), paste0(" than ", refused)))
    expect_identical(read, 0L)
    expect_length(messages, 0L)
    expect_length(moved, 0L)
    expect_false(file.exists(refused))
  }
  expect_length(list.files(far, all.files = TRUE, no.. = TRUE), 0L)
  expect_false(dir.exists(file.path(place, "n")))

  # A relative results folder is handed to Windows after the working folder,
  # with nothing written in between: the same limit.
  seen <- character()
  withr::with_dir(place, suppressMessages(suppressWarnings(run_scorch(small_config("SCORCH_run_002"), tmax = small_tmax()))))
  expect_identical(max(nchar(seen)), 22L + 1L + 47L)
  expect_equal(sheet_names(file.path(place, "SCORCH_run_002", "catalogs", "02_regionally_extensive_heatwaves.xlsx")),
               c("Regional Days", "Regional Day Cells"))
  expect_setequal(list.files(place, all.files = TRUE, no.. = TRUE), c("SCORCH_run_001", "SCORCH_run_002"))
  expect_error(withr::with_dir(far, run_scorch(small_config("SCORCH_run_002"), tmax = small_tmax())),
               "folder whose path has 189 characters, ", fixed = TRUE)
  expect_length(list.files(far, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("on Windows a results folder with a long name of its own is filled up to the longest accepted path", {
  skip_if_not(.Platform$OS.type == "windows", "Only Windows limits the length of a path in this way.")
  local_figure_test_dpi()
  testthat::local_mocked_bindings(long_paths_enabled = function() FALSE, .package = "scoRch.simulator")
  base <- file.path(normalizePath(tempdir(), winslash = "/"), basename(tempfile("scorch-long-")))
  skip_if(nchar(base) > 150L, "The temporary folder leaves no room to build the longest accepted path.")
  # The results folder is not made beforehand, and neither is the folder around it.
  out <- file.path(path_of(211L - 31L, base), strrep("n", 30L))
  expect_identical(nchar(out), 211L)
  expect_null(output_path_problem(out))
  suppressMessages(suppressWarnings(run_scorch(small_config(out), tmax = small_tmax())))
  longest <- file.path(out, "catalogs", "02_regionally_extensive_heatwaves.xlsx")
  expect_identical(nchar(longest), 259L)
  expect_equal(sheet_names(longest), c("Regional Days", "Regional Day Cells"))
  expect_match(readLines(file.path(out, "Summary.txt"))[1], "results: COMPLETE")
  expect_identical(list.files(dirname(out), all.files = TRUE, no.. = TRUE), basename(out))
  beyond <- file.path(path_of(212L - 31L, base), strrep("n", 30L))
  expect_error(suppressMessages(run_scorch(small_config(beyond), tmax = small_tmax())),
               "The results folder has a path of 212 characters, .* at most 211 characters; ")
  expect_false(dir.exists(dirname(beyond)))
})
