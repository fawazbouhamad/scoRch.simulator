# The source on disk stays in the configuration. Extracted ZIP members are
# private working copies, shared by the preview and analysis for one selection.
open_temperature_source <- function(path, progress = NULL) {
  if (!any(grepl("[.]zip$", path, ignore.case = TRUE) & !dir.exists(path))) {
    return(list(files = input_files(path), archive = NULL, scratch = NULL))
  }
  if (length(path) != 1L) fail("Select one ZIP, or a set of extracted ERA5 files.")
  archive <- normalizePath(path, winslash = "/", mustWork = TRUE)
  entries <- zip::zip_list(archive)
  entries <- entries[grepl("[.]nc$", entries$filename, ignore.case = TRUE) &
                       !grepl("(^|/)(__MACOSX/|[.]_)", entries$filename), , drop = FALSE]
  if (!nrow(entries)) fail("This ZIP contains no NetCDF (.nc) files.")
  members <- entries$filename
  invalid <- grepl("(^/|(^|/)[.][.]?(/|$)|\\\\|:|[[:cntrl:]])", members) |
    grepl("^(CON|AUX|NUL|PRN|COM[1-9]|LPT[1-9])([.]|$)", basename(members), ignore.case = TRUE)
  if (any(invalid) || anyDuplicated(members) || any(entries$type != "file"))
    fail("The ZIP contains a repeated, linked or unsupported NetCDF entry. Choose ordinary NetCDF files.")
  scratch <- tempfile("scorch-input-")
  if (!dir.create(scratch)) fail("Could not create the temporary input folder.")
  ready <- FALSE
  on.exit(if (!ready) unlink(scratch, recursive = TRUE), add = TRUE)
  files <- character(length(members))
  for (i in seq_along(members)) {
    if (!is.null(progress)) progress(i, length(members), basename(members[i]))
    # Each member gets its own directory. Archive paths and duplicate basenames
    # in different folders cannot overwrite another file.
    folder <- file.path(scratch, as.character(i))
    if (!dir.create(folder)) fail("Could not create a folder for the extracted file.")
    zip::unzip(archive, files = members[i], exdir = folder, junkpaths = TRUE, overwrite = FALSE)
    files[i] <- file.path(folder, basename(members[i]))
    if (!isTRUE(file.size(files[i]) == entries$uncompressed_size[i]))
      fail("Could not extract the complete file ", members[i], ". Check available disk space.")
  }
  ready <- TRUE
  list(files = files, archive = archive, scratch = scratch)
}

close_temperature_source <- function(source) {
  if (!is.null(source$scratch)) unlink(source$scratch, recursive = TRUE)
  invisible(NULL)
}

# Read coordinate arrays and attributes only. No temperature cubes are loaded
# until the user presses Run analysis.
temperature_header <- function(file, input) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  format <- input$format
  raw <- c("t2m", "2m_temperature")
  if (format == "auto") {
    if ("tmax" %in% names(nc$var) && any(raw %in% names(nc$var)))
      fail("This file includes both daily Tmax and ERA5 temperature. Choose the data format in Input settings.")
    format <- if (any(raw %in% names(nc$var))) "era5" else "daily_tmax"
  }
  variable <- pick_name(input$variable, if (format == "era5") raw else "tmax",
                        names(nc$var), "temperature variable", basename(file))
  dims <- vapply(nc$var[[variable]]$dim, function(d) d$name, "")
  lon <- pick_name(input$longitude, c("longitude", "lon"), dims, "longitude dimension", basename(file))
  lat <- pick_name(input$latitude, c("latitude", "lat"), dims, "latitude dimension", basename(file))
  time <- pick_name(input$time, c("valid_time", "time"), dims, "time dimension", basename(file))
  cal <- ncdf4::ncatt_get(nc, time, "calendar")
  dates <- unique(decode_time(nc$dim[[time]]$vals, nc$dim[[time]]$units,
                              if (cal$hasatt) cal$value else "standard"))
  units <- input$units
  if (units == "auto") {
    attr <- ncdf4::ncatt_get(nc, variable, "units")
    if (!attr$hasatt) fail("Choose the temperature units in Input settings.")
    units <- attr$value
  }
  list(format = format, variable = variable, units = temperature_unit(units), dates = sort(dates),
       lon = nc$dim[[lon]]$vals, lat = nc$dim[[lat]]$vals)
}

inspect_temperature_source <- function(source, input) {
  headers <- lapply(source$files, temperature_header, input = input)
  first <- headers[[1L]]
  if (any(vapply(headers, function(h) h$format != first$format, logical(1))))
    fail("This selection mixes ERA5 and prepared daily Tmax. Choose one dataset.")
  folder_selected <- length(input$file) > 0L && any(dir.exists(input$file))
  if (first$format == "daily_tmax" && (length(headers) != 1L || folder_selected))
    fail("For prepared daily Tmax, select one NetCDF file or a ZIP containing one prepared file.")
  for (h in headers[-1L]) {
    if (!isTRUE(all.equal(sort(h$lon %% 360), sort(first$lon %% 360), tolerance = 1e-6)) ||
        !isTRUE(all.equal(sort(h$lat), sort(first$lat), tolerance = 1e-6)))
      fail("The selected NetCDF files cover different grids.")
  }
  # The reader checks overlaps within the chosen season and date range.
  dates <- sort(unique(do.call(c, lapply(headers, "[[", "dates"))))
  first$dates <- dates
  first$variable <- unique(vapply(headers, "[[", "", "variable"))
  first$units <- unique(vapply(headers, "[[", "", "units"))
  first
}
