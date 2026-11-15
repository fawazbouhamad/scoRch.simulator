# The result workbooks: every table of a run as a sheet of an XLSX file.
#
# Each sheet holds all rows and columns of its table, written with openxlsx2:
# the header row bold, frozen and filtered; dates as date cells; numbers as
# numeric cells whose stored values keep the 15 significant digits of the
# XLSX format (the precision of write.csv() as well) and whose display
# format follows the magnitude of the column; missing values as empty cells,
# distinct from zero. A table too long for a sheet or a file that cannot be
# written stops the run with an explicit error instead of losing rows, and
# every written archive is checked the way other readers open it (see
# check_workbook_archive()).

XLSX_MAX_ROWS <- 1048576L   # rows an XLSX sheet can hold, header included

# The workbooks of a complete run: relative path -> named list of sheets.
result_workbooks <- function(result) {
  books <- list(
    "catalogs/01_heatwave_events.xlsx" = list(
      "Heatwave Events" = result$heatwaves$events),
    "catalogs/02_regionally_extensive_heatwaves.xlsx" = list(
      "Regional Days" = result$regional$selected_days,
      "Regional Day Cells" = result$regional$selected_cells),
    "catalogs/03_extreme_heatwave_clusters.xlsx" = list(
      "Clusters" = result$clusters$clusters,
      "Cluster Cells" = result$clusters$cluster_cells),
    "catalogs/04_extreme_heatwave_ellipses.xlsx" = list(
      "Ellipses" = result$ellipses),
    "catalogs/05_compound_heatwave_events.xlsx" = list(
      "Compound Events" = result$events$events,
      "Event Days" = result$events$event_days),
    "modeling/Power_Law_Model.xlsx" = power_law_export(result$power_law$summary))
  if (!is.null(result$lgcp)) {
    books[["modeling/Spatial_Risk_Model.xlsx"]] <- list(
      "Parameters" = lgcp_parameter_export(result$lgcp$parameter_table),
      "Intensity" = lgcp_intensity_export(result$lgcp$cells),
      "Centroid Concentration" = result$lgcp$concentration)
  }
  books[["Heatwave_Statistics.xlsx"]] <- list(
    "Event Type Summary" = result$events$table_1,
    "Cell Thresholds" = result$heatwaves$thresholds,
    "Daily Heatwave Extent" = result$regional$daily,
    "Duration Trends" = duration_trend_export(result$events$trends),
    "Annual Mean Duration" = result$events$annual_duration)
  books
}

# Reporting transformations only: fitted objects keep their original schema.
power_law_export <- function(summary) {
  fits <- summary[c("dataset", "n", "n_tail", "xmin_km2", "alpha", "ks_distance", "p_value")]
  names(fits) <- c("dataset", "n_observations", "n_tail_observations", "tail_cutoff_km2",
                  "exponent_alpha", "ks_distance", "goodness_of_fit_p_value")
  bootstrap <- summary[c("dataset", "reps", "n_undefined", "n_exceed", "p_lower", "p_upper",
                         "mc_se", "seed", "alpha_boot_median", "alpha_boot_q2.5",
                         "alpha_boot_q25", "alpha_boot_q75", "alpha_boot_q97.5")]
  names(bootstrap)[2:8] <- c("bootstrap_repetitions", "undefined_bootstrap_fits",
                            "n_bootstrap_ks_ge_observed", "p_value_lower_bound", "p_value_upper_bound",
                            "p_monte_carlo_se", "bootstrap_seed")
  list("Power Law Fits" = fits, "Bootstrap Results" = bootstrap)
}

lgcp_parameter_export <- function(parameters) {
  temperature <- parameters$parameter %in% c("mean_tmax", "std_tmax")
  geographic <- parameters$parameter %in% c("lon", "lat")
  parameters$intensity_change_pct_per_1C <- rep(NA_real_, nrow(parameters))
  parameters$intensity_change_pct_per_1_degree <- rep(NA_real_, nrow(parameters))
  parameters$intensity_change_pct_per_1C[temperature] <- 100 * expm1(parameters$value[temperature])
  parameters$intensity_change_pct_per_1_degree[geographic] <- 100 * expm1(parameters$value[geographic])
  parameters
}

lgcp_intensity_export <- function(cells) {
  names(cells)[names(cells) == "concentration_rank"] <- "relative_intensity_rank_0_1"
  cells
}

duration_trend_export <- function(trends) {
  trends$mann_kendall_result <- NULL
  names(trends)[names(trends) == "n_years"] <- "years_with_events"
  trends
}

# The workbooks of a run that stopped because no selected day formed a
# cluster: Catalogs 1-3 and the per-cell and per-day tables.
partial_workbooks <- function(heatwaves, regional, clusters) {
  list(
    "catalogs/01_heatwave_events.xlsx" = list(
      "Heatwave Events" = heatwaves$events),
    "catalogs/02_regionally_extensive_heatwaves.xlsx" = list(
      "Regional Days" = regional$selected_days,
      "Regional Day Cells" = regional$selected_cells),
    "catalogs/03_extreme_heatwave_clusters.xlsx" = list(
      "Clusters" = clusters$clusters,
      "Cluster Cells" = clusters$cluster_cells),
    "Heatwave_Statistics.xlsx" = list(
      "Cell Thresholds" = heatwaves$thresholds,
      "Daily Heatwave Extent" = regional$daily))
}

# One line per sheet for Summary.txt.
sheet_descriptions <- function() c(
  "Heatwave Events" = "every local heatwave of every cell (Catalog 1): dates, duration, exceeding and bridged days, peak",
  "Regional Days" = "the selected regionally extensive days (Catalog 2) with their heatwave-cell counts",
  "Regional Day Cells" = "the heatwave cells of every selected day and their local heatwave",
  "Clusters" = "the DBSCAN clusters of every selected day (Catalog 3): members, core cells, eps and min_samples applied",
  "Cluster Cells" = "every heatwave cell of every selected day with its cluster label, core and noise flags",
  "Ellipses" = "the ellipse of every cluster (Catalog 4): centroids, axes, area, orientation, temperature weights",
  "Compound Events" = "every compound heatwave event (Catalog 5): type, days, cluster counts, mean geometry, largest ellipse",
  "Event Days" = "every day of every compound event with its clusters and largest ellipse",
  "Power Law Fits" = "sample sizes, upper-tail cutoff, exponent, KS distance and goodness-of-fit p-value for daily and event maxima",
  "Bootstrap Results" = "bootstrap counts, p-value bounds, Monte Carlo error, seeds and exponent quantiles",
  "Parameters" = "original LGCP parameters and derived intensity percentage changes for a +1 C or +1 degree increase",
  "Intensity" = "mean centroid intensity per km2 and relative intensity rank (0-1) of every cell",
  "Centroid Concentration" = "share of the centroids inside the top-20% and top-50% intensity zones",
  "Event Type Summary" = "Table 1 of the study: statistics by compound heatwave type, days, events, durations, ellipses per event",
  "Cell Thresholds" = "the local threshold and heatwave counts of every cell",
  "Daily Heatwave Extent" = "heatwave and exceeding cells on every day, the regional threshold and the selection flag",
  "Duration Trends" = "OLS and Sen slopes (days per decade) and tests on annual mean duration; years_with_events counts years containing that type",
  "Annual Mean Duration" = "the annual mean durations used for Type 3 and Type 4 trends, by event start-season year; years without events are omitted")

# Stop before anything is written when a sheet would not fit.
check_workbook_rows <- function(books, limit = XLSX_MAX_ROWS) {
  for (path in names(books)) {
    for (sheet in names(books[[path]])) {
      rows <- nrow(books[[path]][[sheet]]) + 1L
      if (rows > limit) {
        fail("The sheet '", sheet, "' of ", path, " would need ", rows, " rows, more than the ", limit,
             " an XLSX sheet can hold. No results were written, so that none are lost silently;",
             " shorten the period or split the region.")
      }
    }
  }
  invisible(TRUE)
}

write_workbooks <- function(out, books) {
  check_workbook_rows(books)
  for (path in names(books)) write_workbook(file.path(out, path), books[[path]])
  invisible(names(books))
}

# One workbook: `sheets` is a named list of data frames. The file is checked
# after it is written.
write_workbook <- function(path, sheets) {
  wb <- openxlsx2::wb_workbook()
  for (name in names(sheets)) add_sheet(wb, name, sheets[[name]])
  saved <- tryCatch({
    save_workbook(wb, path)
    TRUE
  }, error = function(e) conditionMessage(e))
  if (!isTRUE(saved) || !file.exists(path) || file.size(path) == 0) {
    fail("Could not write ", path, if (is.character(saved)) paste0(": ", saved), ".")
  }
  check_workbook_archive(path, sheets)
  invisible(path)
}

# Use the R zip package for consistent workbook archives across platforms.
save_workbook <- function(wb, path) {
  old_options <- options(openxlsx2.no_utils_zip = TRUE, openxlsx2.no_bsdtar = TRUE, openxlsx2.no_maybe_zip = TRUE)
  on.exit(options(old_options), add = TRUE)
  old_command <- Sys.getenv("R_ZIPCMD", unset = NA)
  Sys.unsetenv("R_ZIPCMD")
  on.exit(if (!is.na(old_command)) Sys.setenv(R_ZIPCMD = old_command), add = TRUE)
  wb$save(path)
  invisible(path)
}

# openxlsx2 workbooks are R6 objects: the methods modify `wb` in place.
add_sheet <- function(wb, name, table) {
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  rownames(table) <- NULL
  rows <- nrow(table) + 1L
  columns <- ncol(table)
  wb$add_worksheet(name)
  wb$add_data(x = table, with_filter = nrow(table) > 0, na.strings = NULL)
  identifiers <- names(table) %in% c("date", "start_date", "end_date", "cell_id",
                                    "local_event_id", "cluster_id", "ellipse_id", "compound_event_id")
  frozen <- if (columns >= 5L) sum(cumprod(identifiers)) else 0L
  wb$freeze_pane(first_active_row = 2L, first_active_col = min(frozen, columns - 1L) + 1L)
  header <- sprintf("A1:%s1", column_letter(columns))
  wb$add_font(dims = header, bold = TRUE)
  wb$add_fill(dims = header, color = openxlsx2::wb_color(hex = "FFE7E6E6"))
  wb$add_border(dims = header, bottom_border = "thin", top_border = "", left_border = "", right_border = "",
                inner_hgrid = "", inner_vgrid = "")
  if (nrow(table)) {
    for (j in seq_len(columns)) {
      format <- cell_format(table[[j]])
      if (!is.null(format)) {
        wb$add_numfmt(dims = sprintf("%s2:%s%d", column_letter(j), column_letter(j), rows), numfmt = format)
      }
    }
  }
  widths <- column_widths(table)
  wb$set_col_widths(cols = seq_len(columns), widths = widths)
  # Wrap only overflowing text cells; ordinary catalog rows retain their height.
  lines <- rep(1, nrow(table))
  for (j in which(vapply(table, is.character, logical(1)))) {
    needed <- ceiling(nchar(table[[j]]) / max(1, widths[j] - 2))
    long <- which(!is.na(needed) & needed > 1)
    for (i in long) {
      wb$add_cell_style(dims = sprintf("%s%d", column_letter(j), i + 1L),
                         wrap_text = TRUE, vertical = "top")
    }
    lines[long] <- pmax(lines[long], needed[long])
  }
  wrapped <- which(lines > 1)
  if (length(wrapped)) {
    # Excel limits a row to 409.5 points. Stored text is never shortened.
    wb$set_row_heights(rows = wrapped + 1L, heights = pmin(409.5, 15 * lines[wrapped] + 3))
  }
  invisible(name)
}

# Column letters of the XLSX cell references: 1 -> A, 27 -> AA.
column_letter <- function(j) {
  letters_of <- function(n) {
    out <- ""
    while (n > 0) {
      out <- paste0(LETTERS[(n - 1) %% 26 + 1], out)
      n <- (n - 1) %/% 26
    }
    out
  }
  vapply(j, letters_of, "")
}

# Display format of a column (NULL: Excel's General format). Whole numbers
# and columns spanning many orders of magnitude are left to General; other
# numbers get a fixed number of decimals chosen from their typical size.
cell_format <- function(values) {
  if (inherits(values, "Date")) return("yyyy-mm-dd")
  if (inherits(values, "POSIXt")) return("yyyy-mm-dd hh:mm:ss")
  if (!is.numeric(values)) return(NULL)
  finite <- values[is.finite(values)]
  if (!length(finite) || all(finite == round(finite))) return(NULL)
  nonzero <- abs(finite[finite != 0])
  if (!length(nonzero)) return(NULL)
  if (diff(range(log10(nonzero))) > 4) return(NULL)
  size <- stats::median(nonzero)
  if (size < 0.01) "0.000E+00" else if (size < 10) "0.000" else if (size < 1000) "0.00" else "0.0"
}

# Column widths in characters: the longer of the header and the values.
column_widths <- function(table) {
  width <- function(values) {
    if (inherits(values, "Date")) return(10)
    if (is.numeric(values)) {
      finite <- values[is.finite(values)]
      if (!length(finite)) return(4)
      return(min(max(nchar(formatC(range(finite), format = "fg", digits = 7))), 16))
    }
    text <- as.character(values)
    text <- text[!is.na(text)]
    if (!length(text)) return(4)
    min(stats::quantile(nchar(text), 0.95, names = FALSE), 48)
  }
  pmin(pmax(vapply(table, width, numeric(1)), nchar(names(table))) + 2, 50)
}

# Validation of a written workbook ------------------------------------------
#
# Check archive relationships, table dimensions and sheet names after saving.

check_workbook_archive <- function(path, sheets) {
  entries <- zip::zip_list(path)$filename
  problems <- archive_problems(entries, function(name, n = 1e7) read_archive_part(path, name, n), sheets)
  if (length(problems)) {
    fail("The workbook ", path, " is not a sound XLSX archive: ", paste(problems, collapse = "; "),
         ". The run was stopped so that no unreadable file is published.")
  }
  invisible(TRUE)
}

# The first `n` bytes of one part of a zip archive, as text.
read_archive_part <- function(path, name, n = 1e7) {
  con <- unz(path, name, open = "rb")
  on.exit(close(con))
  readChar(con, n, useBytes = TRUE)
}

# The defects of an archive, as sentences; empty when it is sound. `part`
# returns the text of a part (its first `n` bytes).
archive_problems <- function(entries, part, sheets) {
  problems <- character()
  for (rels in grep("[.]rels$", entries, value = TRUE)) {
    xml <- part(rels)
    targets <- regmatches(xml, gregexpr('Target="[^"]*"', xml))[[1]]
    targets <- sub('"$', "", sub('^Target="', "", targets))
    modes <- regmatches(xml, gregexpr('<Relationship [^>]*>', xml))[[1]]
    external <- grepl('TargetMode="External"', modes)
    base <- sub("_rels/[^/]*$", "", rels)
    for (k in seq_along(targets)) {
      if (isTRUE(external[k]) || grepl("^[a-z]+:", targets[k])) next
      resolved <- resolve_part_path(base, targets[k])
      if (!resolved %in% entries) {
        problems <- c(problems, sprintf("%s refers to the missing part %s", rels, resolved))
      }
    }
  }
  if (!"xl/workbook.xml" %in% entries) return(c(problems, "the archive has no xl/workbook.xml"))
  workbook <- part("xl/workbook.xml")
  found <- regmatches(workbook, gregexpr('<sheet [^>]*name="[^"]*"', workbook))[[1]]
  found <- sub('"$', "", sub('.*name="', "", found))
  found <- gsub("&amp;", "&", gsub("&quot;", '"', gsub("&apos;", "'", gsub("&lt;", "<", gsub("&gt;", ">", found)))))
  if (!identical(found, names(sheets))) {
    problems <- c(problems, sprintf("the sheet names are %s instead of %s", paste(found, collapse = ", "),
                                    paste(names(sheets), collapse = ", ")))
  }
  for (i in seq_along(sheets)) {
    name <- sprintf("xl/worksheets/sheet%d.xml", i)
    if (!name %in% entries) {
      problems <- c(problems, sprintf("the archive has no %s", name))
      next
    }
    head <- part(name, 4000)
    declared <- regmatches(head, regexpr('<dimension ref="[^"]*"', head))
    declared <- if (length(declared)) sub('"$', "", sub('.*ref="', "", declared)) else "(none)"
    expected <- sprintf("A1:%s%d", column_letter(ncol(sheets[[i]])), nrow(sheets[[i]]) + 1L)
    if (declared != expected) {
      problems <- c(problems, sprintf("sheet %d (%s) declares the dimension %s instead of %s",
                                      i, names(sheets)[i], declared, expected))
    }
  }
  problems
}

# A relationship target resolved against the folder of its source part.
resolve_part_path <- function(base, target) {
  if (startsWith(target, "/")) return(substring(target, 2))
  pieces <- strsplit(paste0(base, target), "/", fixed = TRUE)[[1]]
  out <- character()
  for (piece in pieces) {
    if (piece == "..") out <- out[-length(out)] else if (piece != "." && nzchar(piece)) out <- c(out, piece)
  }
  paste(out, collapse = "/")
}
