# Developer commands; use --help for usage. These are not needed to run the package.

compare_output <- function(results_dir = "output") {
  suppressPackageStartupMessages(library(openxlsx2))

  expected_dir <- "demo/output"
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

  normalize_summary <- function(lines) {
    lines <- lines[!grepl("^Run started ", lines)]
    lines <- sub("^(  Source: |    file: ).*north_america_tmax[.]nc.*$", "\\1<installed north_america_tmax.nc>", lines)
    sub("^(  Figure for this event type: ).*/figures/", "\\1<output>/figures/", lines)
  }
  expected_readme <- normalize_summary(readLines(file.path(expected_dir, "Run_Summary.txt"), encoding = "UTF-8"))
  readme <- normalize_summary(readLines(file.path(results_dir, "Run_Summary.txt"), encoding = "UTF-8"))
  if (identical(expected_readme, readme)) {
    cat("Run_Summary.txt: same apart from run time and local paths\n")
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
}

check_input <- function(sample = Sys.getenv("SCORCH_VALIDATION_FILE", unset =
  file.path("local_validation", "cds_daily_tmax_2012_great_plains.nc"))) {
  library(scoRch.simulator)

  if (!file.exists(sample)) {
    stop("Supply the local ERA5 sample path: Rscript software/setup/maintenance.R check-input path/to/sample.nc", call. = FALSE)
  }
  whole <- read_tmax(system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator"),
                     start = "2012-01-01", end = "2012-12-31")
  processed <- read_era5(sample, months = 4:9, cell_size = 1)
  columns <- match(paste(processed$cells$lon, processed$cells$lat), paste(whole$cells$lon, whole$cells$lat))
  stopifnot(!anyNA(columns), identical(processed$dates, whole$dates))
  maximum_difference <- max(abs(processed$tmax - whole$tmax[, columns]))
  stopifnot(maximum_difference <= 0.0065)

  copy <- write_tmax(processed, tempfile(fileext = ".nc"), precision = "float64")
  back <- read_tmax(copy)
  stopifnot(identical(back$dates, processed$dates),
            identical(back$tmax, processed$tmax))
  cat(sprintf("Local ERA5 preprocessing and prepared-file round trip passed; reference difference %.6f C.\n",
              maximum_difference))
}

make_demo_data <- function(dir = NA_character_) {
  suppressPackageStartupMessages({library(ncdf4); library(scoRch.simulator)})
  if (is.na(dir) || !dir.exists(dir)) stop("give the folder of the yearly <year>_daily_Tmax_NA.nc files")
  years <- 1996:2025
  files <- file.path(dir, sprintf("%d_daily_Tmax_NA.nc", years))
  target <- file.path("demo", "input", "north_america_processed_tmax.nc")

  # 1. Every yearly file: structure, metadata, dates, units, missing values.
  records <- "local_validation"
  if (!dir.exists(records)) dir.create(records)
  manifest <- character()
  for (k in seq_along(files)) {
    f <- files[k]; year <- years[k]
    if (!file.exists(f)) stop("missing: ", f)
    nc <- nc_open(f)
    dims <- vapply(nc$dim, function(d) d$len, 1)
    stopifnot(identical(unname(dims[c("longitude", "latitude", "time")]), c(289, 101, 183)))
    stopifnot(identical(names(nc$var), "Tmax_C"), ncatt_get(nc, "Tmax_C", "units")$value == "degC")
    stopifnot(as.integer(ncatt_get(nc, 0, "year")$value) == year,
              ncatt_get(nc, 0, "source")$value == "ARCO ERA5 (Google Cloud, Zarr)",
              ncatt_get(nc, 0, "latitude_bounds")$value == "25 to 50",
              ncatt_get(nc, 0, "longitude_bounds")$value == "-130 to -58")
    lon <- nc$dim$longitude$vals; lat <- nc$dim$latitude$vals
    stopifnot(all(abs(lon - seq(230, 302, by = 0.25)) < 1e-9), all(abs(lat - seq(50, 25, by = -0.25)) < 1e-9))
    stopifnot(nc$dim$time$units == sprintf("days since %d-04-01 00:00:00", year), all(nc$dim$time$vals == 0:182))
    x <- ncvar_get(nc, "Tmax_C")
    stopifnot(!anyNA(x), min(x) > -40, max(x) < 60)
    nc_close(nc)
    manifest <- c(manifest, sprintf("%s %d %s", unname(tools::md5sum(f)), file.size(f), basename(f)))
    cat(sprintf("%d: ok (%.1f to %.1f degC)\n", year, min(x), max(x)))
  }
  writeLines(c("# Source files for demo/input/north_america_processed_tmax.nc (ARCO-ERA5 daily Tmax, 0.25 degrees).",
               "# Columns: md5 size_bytes filename",
               manifest), file.path(records, "source_checksums.txt"))

  # 2. The package's reduction to 1-degree cells, checked.
  tmax <- read_era5(files, variable = "Tmax_C", months = 4:9, cell_size = 1)
  expected <- seq(as.Date("1996-04-01"), as.Date("2025-09-30"), by = "day")
  expected <- expected[as.integer(format(expected, "%m")) %in% 4:9]
  stopifnot(nrow(tmax$cells) == 1800, length(tmax$dates) == 5490, all(tmax$dates == expected), !anyNA(tmax$tmax),
            all(abs(tmax$grid$lon - seq(-129.5, -58.5)) < 1e-9), all(abs(tmax$grid$lat - seq(25.5, 49.5)) < 1e-9))
  set.seed(20261012)
  for (i in 1:8) {                       # the 4 x 4 mean by hand, from the raw file
    cell <- sample(nrow(tmax$cells), 1); date <- sample(tmax$dates, 1)
    nc <- nc_open(files[match(as.integer(format(date, "%Y")), years)])
    lon <- nc$dim$longitude$vals - 360; lat <- nc$dim$latitude$vals
    rows <- which(lon >= tmax$cells$lon[cell] - 0.5 - 1e-9 & lon < tmax$cells$lon[cell] + 0.5 - 1e-9)
    cols <- which(lat >= tmax$cells$lat[cell] - 0.5 - 1e-9 & lat < tmax$cells$lat[cell] + 0.5 - 1e-9)
    t <- which(as.Date(sub(".*since ", "", nc$dim$time$units)) + nc$dim$time$vals == date)
    block <- ncvar_get(nc, "Tmax_C", start = c(min(rows), min(cols), t), count = c(4, 4, 1))
    nc_close(nc)
    stopifnot(length(rows) == 4, length(cols) == 4, abs(mean(block) - tmax$tmax[match(date, tmax$dates), cell]) < 1e-5)
    cat(sprintf("hand check %d: cell %g N %g W on %s: mean of 16 points %.4f = package %.4f\n", i, tmax$cells$lat[cell],
                -tmax$cells$lon[cell], format(date), mean(block), tmax$tmax[match(date, tmax$dates), cell]))
  }

  # 3. The file.
  write_tmax(tmax, target, precision = "int16", overwrite = TRUE,
             title = "scoRch.simulator demonstration data: daily maximum 2-m temperature, 25-50 N and 130-58 W, April-September 1996-2025",
             source = paste("ERA5 (Hersbach et al., 2020) hourly 2-m temperature from the ARCO-ERA5 archive on Google Cloud",
                            "(Carver and Merose, 2023), reduced by the authors to the daily maximum of each UTC calendar day",
                            "at 0.25 degrees (files <year>_daily_Tmax_NA.nc), averaged to 1-degree cells by",
                            "scoRch.simulator::read_era5() and rounded to 0.01 degC"),
             licence = "CC BY 4.0 for the authors' processing; contains modified Copernicus Climate Change Service information 2026 (ERA5)",
             history = sprintf("written on %s by software/setup/maintenance.R make-demo of scoRch.simulator %s from 30 yearly files (local_validation/source_checksums.txt)",
                               format(Sys.Date()), as.character(packageVersion("scoRch.simulator"))))
  back <- read_tmax(target)
  stopifnot(nrow(back$cells) == 1800, length(back$dates) == 5490, !anyNA(back$tmax), max(abs(back$tmax - tmax$tmax)) <= 0.005 + 1e-9)
  cat(sprintf("wrote %s: %d cells, %d dates, %.1f MB, md5 %s; Tmax %.2f to %.2f degC, round trip within %.4f degC\n",
              target, nrow(back$cells), length(back$dates), file.size(target) / 1e6, unname(tools::md5sum(target)),
              min(tmax$tmax), max(tmax$tmax), max(abs(back$tmax - tmax$tmax))))
}

prepare_test_data <- function(ref = NA_character_) {
  library(ncdf4)
  if (is.na(ref) || !dir.exists(ref)) stop("give the path of the reproducibility repository")
  out <- file.path("software", "tests", "testthat", "fixtures")
  # Accept the current results layout and earlier study archives.
  first_existing <- function(...) {
    paths <- file.path(ref, c(...))
    found <- paths[file.exists(paths)]
    if (!length(found)) stop("the reference folder has no ", paths[1])
    found[1]
  }
  catalogs <- file.path(ref, "output", "catalogs")
  modeling <- first_existing("supporting_files/additional_results/modeling", "output/modeling")
  thresholds_file <- first_existing("supporting_files/additional_results/main_workflow/heatwave_detection/cell_thresholds.csv",
                                    "output/catalogs/01_heatwave_events/cell_thresholds.csv")
  daily_settings_file <- first_existing("supporting_files/additional_results/main_workflow/clustering/daily_settings.csv",
                                        "output/catalogs/03_extreme_heatwave_clusters/daily_settings.csv")

  nc <- nc_open(file.path(ref, "input", "tmax_processed.nc"))
  lon <- nc$dim$lon$vals; lat <- nc$dim$lat$vals
  dates <- as.Date("1940-01-01") + nc$dim$time$vals
  cell_of <- function(id) list(lon_index = (id - 1) %% length(lon) + 1, lat_index = (id - 1) %/% length(lon) + 1)

  # One cell's complete daily Tmax series with its catalog events and threshold.
  cell_id <- 900L
  c <- cell_of(cell_id)
  series <- as.numeric(ncvar_get(nc, "tmax", start = c(c$lon_index, c$lat_index, 1), count = c(1, 1, -1)))
  events <- read.csv(file.path(catalogs, "01_heatwave_events", "heatwave_events.csv"))
  thresholds <- read.csv(thresholds_file)
  saveRDS(list(cell_id = cell_id, lon = lon[c$lon_index], lat = lat[c$lat_index], dates = dates, tmax = series,
               threshold = thresholds$threshold_tmax_c[thresholds$cell_id == cell_id],
               events = events[events$cell_id == cell_id, ]),
          file.path(out, "study_cell_series.rds"), compress = "xz")

  # The heatwave cells of one run of consecutive selected days (compound event 2)
  # with their catalog clusters and settings.
  days <- format(seq(as.Date("1987-07-23"), as.Date("1987-07-27"), by = "day"))
  cells <- read.csv(file.path(catalogs, "03_extreme_heatwave_clusters", "cluster_cells.csv"))
  settings <- read.csv(daily_settings_file)
  # Normalize historical pooled-setting column names.
  if (!"sequence_start_date" %in% names(settings)) {
    settings$sequence_start_date <- ave(settings$date, settings$compound_event_id, FUN = min)
    names(settings)[match(c("event_eps", "event_min_samples"), names(settings))] <- c("sequence_eps", "sequence_min_samples")
  }
  settings <- settings[, c("date", "sequence_start_date", "n_heatwave_cells", "modal_cluster_counts", "modal_frequency", "n_pairs_retained",
                           "daily_eps", "daily_min_samples_mean", "daily_min_samples", "sequence_eps", "sequence_min_samples", "n_clusters")]
  write.csv(cells[cells$date %in% days, c("date", "cell_id", "lat", "lon", "local_event_id", "dbscan_label", "is_core_cell", "n_cells_within_eps")],
            file.path(out, "study_day_cells.csv"), row.names = FALSE)
  write.csv(settings[settings$date %in% days, ], file.path(out, "study_day_settings.csv"), row.names = FALSE)

  # Member cells and Tmax of three clusters with their catalog ellipses.
  ellipses <- read.csv(file.path(catalogs, "04_extreme_heatwave_ellipses", "ellipses.csv"))
  chosen <- c(1L, 2L, 400L)
  members <- cells[cells$cluster_id %in% chosen & !is.na(cells$cluster_id), ]
  members$tmax_c <- vapply(seq_len(nrow(members)), function(i) {
    c <- cell_of(members$cell_id[i])
    as.numeric(ncvar_get(nc, "tmax", start = c(c$lon_index, c$lat_index, match(as.Date(members$date[i]), dates)), count = c(1, 1, 1)))
  }, 0)
  write.csv(members[, c("cluster_id", "date", "cell_id", "lat", "lon", "tmax_c")], file.path(out, "study_cluster_cells.csv"), row.names = FALSE)
  write.csv(ellipses[ellipses$ellipse_id %in% chosen, ], file.path(out, "study_ellipses.csv"), row.names = FALSE)
  nc_close(nc)

  # The power-law samples with the catalog fits.
  pl <- file.path(modeling, "power_law")
  areas <- rbind(data.frame(dataset = "daily_maxima", area_km2 = read.csv(file.path(pl, "daily_max_area.csv"))$area_km2),
                 data.frame(dataset = "event_maxima", area_km2 = read.csv(file.path(pl, "event_max_area.csv"))$area_km2))
  write.csv(areas, file.path(out, "study_power_law_areas.csv"), row.names = FALSE)
  write.csv(read.csv(file.path(pl, "power_law_fits.csv"))[, c("dataset", "n", "n_tail", "xmin_km2", "alpha", "ks_distance", "p_value", "n_exceed", "reps", "seed")],
            file.path(out, "study_power_law_fits.csv"), row.names = FALSE)

  # Projected centroid coordinates (pyproj, Lambert azimuthal equal-area, WGS84).
  centroids <- read.csv(file.path(modeling, "lgcp", "centroids.csv"))
  write.csv(centroids[seq(1, nrow(centroids), by = 40), c("ellipse_id", "centroid_lon", "centroid_lat", "x_km", "y_km")],
            file.path(out, "study_centroids_projected.csv"), row.names = FALSE)
  cat("fixtures written:\n"); print(file.info(list.files(out, full.names = TRUE))["size"])
}

maintenance_help <- function() {
  cat(paste0(
    "Run from the repository root:\n",
    "  Rscript software/setup/maintenance.R compare [output_folder]\n",
    "  Rscript software/setup/maintenance.R check-input [local_sample.nc]\n",
    "  Rscript software/setup/maintenance.R make-demo <raw_daily_folder>\n",
    "  Rscript software/setup/maintenance.R prepare-fixtures <study_repository>\n"
  ))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (!length(args) || args[1] %in% c("-h", "--help")) {
    maintenance_help()
  } else {
    commands <- list(compare = compare_output, "check-input" = check_input,
                     "make-demo" = make_demo_data, "prepare-fixtures" = prepare_test_data)
    if (!args[1] %in% names(commands)) {
      stop("Unknown maintenance command. Use --help to list commands.", call. = FALSE)
    }
    if (length(args) > 2L) stop("Each command accepts one file or folder path.", call. = FALSE)
    do.call(commands[[args[1]]], as.list(args[-1]))
  }
}
