# Compare the demo's output/ with demo/output/: every sheet of every
# workbook (read with openxlsx2), Run_Summary.txt apart from its run-time line
# (both read as UTF-8), the presence of the
# figures, and that Run_Summary.txt is the only text file. Run from the top folder
# of the repository after demo/run.R; the demo workflow runs it on every
# change. Values are compared with a relative tolerance of 1e-6 (1e-3 for the
# LGCP workbook, whose fit can differ slightly between platforms); dates and
# text must be identical.

suppressPackageStartupMessages(library(openxlsx2))

expected_dir <- "demo/output"
args <- commandArgs(trailingOnly = TRUE)
results_dir <- if (length(args)) args[1] else "output"
tolerance <- c(default = 1e-6, "modeling/Spatial_Risk_Model.xlsx" = 1e-3)
problems <- character()
note <- function(...) problems <<- c(problems, sprintf(...))

for (book in list.files(expected_dir, pattern = "[.]xlsx$", recursive = TRUE)) {
  expected <- file.path(expected_dir, book)
  actual <- file.path(results_dir, book)
  if (!file.exists(actual)) {
    note("%s is missing", actual)
    next
  }
  expected_sheets <- wb_get_sheet_names(wb_load(expected))
  actual_sheets <- wb_get_sheet_names(wb_load(actual))
  if (!identical(expected_sheets, actual_sheets)) {
    note("%s has the sheets %s instead of %s", book, paste(actual_sheets, collapse = ", "),
         paste(expected_sheets, collapse = ", "))
  }
  for (sheet in intersect(expected_sheets, actual_sheets)) {
    x <- read_xlsx(expected, sheet = sheet)
    y <- read_xlsx(actual, sheet = sheet)
    tol <- if (book %in% names(tolerance)) tolerance[[book]] else tolerance[["default"]]
    same <- all.equal(x, y, tolerance = tol, check.attributes = FALSE)
    if (isTRUE(same)) {
      cat(sprintf("%-45s %-24s %6d rows  same\n", book, sheet, nrow(y)))
    } else {
      note("%s [%s]: %s", book, sheet, paste(same, collapse = "; "))
    }
  }
}

timed <- function(lines) {
  lines <- lines[!grepl("^Run started ", lines)]
  # The installed example's absolute library path varies across platforms.
  sub("^(  Source: |    file: ).*north_america_tmax[.]nc.*$", "\\1<installed north_america_tmax.nc>", lines)
}
expected_readme <- timed(readLines(file.path(expected_dir, "Run_Summary.txt"), encoding = "UTF-8"))
readme <- timed(readLines(file.path(results_dir, "Run_Summary.txt"), encoding = "UTF-8"))
if (identical(expected_readme, readme)) {
  cat("Run_Summary.txt: same apart from the run time\n")
} else {
  n <- min(length(expected_readme), length(readme))
  first <- which(expected_readme[seq_len(n)] != readme[seq_len(n)])[1]
  note("Run_Summary.txt differs from the expected file (first difference at line %s; %d lines expected, %d found)",
       if (is.na(first)) "none" else first, length(expected_readme), length(readme))
}

for (figure in list.files(file.path(expected_dir, "figures"))) {
  if (!file.exists(file.path(results_dir, "figures", figure))) note("figures/%s is missing", figure)
}
text_files <- list.files(results_dir, pattern = "[.]txt$", recursive = TRUE)
if (!identical(text_files, "Run_Summary.txt")) note("text files in output/: %s", paste(text_files, collapse = ", "))

if (length(problems)) {
  stop("The demo output differs from demo/output:\n", paste(problems, collapse = "\n"), call. = FALSE)
}
cat("The demo output matches demo/output.\n")
