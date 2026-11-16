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
  invalid <- grepl("(^/|(^|/)[.][.](/|$)|\\\\|:|[[:cntrl:]])", members) |
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

# The names that `auto` finds in the readers. A guided run records any other
# detected name in its configuration, so a scripted replay reads the same data.
reader_names <- function(format) {
  if (format == "era5") {
    list(variable = c("t2m", "2m_temperature"), longitude = c("longitude", "lon"),
         latitude = c("latitude", "lat"), time = c("valid_time", "time"))
  } else {
    list(variable = "tmax", longitude = c("lon", "longitude"),
         latitude = c("lat", "latitude"), time = c("time", "valid_time"))
  }
}

# One attribute as text; "" when the variable has none.
nc_text <- function(nc, name, attribute) {
  value <- tryCatch(ncdf4::ncatt_get(nc, name, attribute), error = function(e) list(hasatt = FALSE))
  if (isTRUE(value$hasatt)) trimws(as.character(value$value)[1]) else ""
}

# Which dimension is time, longitude or latitude: CF units and standard names
# first, then axis letters and familiar names for coordinates in degrees.
dimension_roles <- function(nc, input) {
  role <- function(name) {
    dim <- nc$dim[[name]]
    known <- isTRUE(dim$create_dimvar)
    units <- if (known && is.character(dim$units)) tolower(trimws(dim$units)) else ""
    standard <- if (known) tolower(nc_text(nc, name, "standard_name")) else ""
    axis <- if (known) tolower(nc_text(nc, name, "axis")) else ""
    short <- tolower(name)
    if (grepl("^[a-z]+ +since +[0-9]", units) || standard == "time") return("time")
    if (grepl("^degrees?[ _]?(e|east)$", units) || standard == "longitude") return("longitude")
    if (grepl("^degrees?[ _]?(n|north)$", units) || standard == "latitude") return("latitude")
    if (axis == "t" || short %in% c("time", "valid_time", "t", "date")) return("time")
    degrees <- known && is.numeric(dim$vals) && all(is.finite(dim$vals)) &&
      !grepl("^(m|km|met(er|re)s?)$", units) && !grepl("projection|grid_", standard)
    if (degrees && all(abs(dim$vals) <= 360) && (axis == "x" || short %in% c("lon", "longitude", "long", "x")))
      return("longitude")
    if (degrees && all(abs(dim$vals) <= 90) && (axis == "y" || short %in% c("lat", "latitude", "y")))
      return("latitude")
    NA_character_
  }
  roles <- vapply(names(nc$dim), role, "")
  # A name given in the settings decides its role, whatever the file says.
  for (key in c("longitude", "latitude", "time")) {
    chosen <- input[[key]]
    if (identical(chosen, "auto")) next
    if (!chosen %in% names(roles)) {
      fail(sprintf("The file %s has no dimension called '%s'. It has: %s.", basename(nc$filename), chosen,
                   paste(names(roles), collapse = ", ")))
    }
    roles[roles %in% key] <- NA_character_
    roles[[chosen]] <- key
  }
  roles
}

# Gridded variables: one time, one longitude and one latitude dimension. Other
# dimensions must have length one, except ERA5's expver. A variable with one
# unnamed horizontal dimension left is completed from the one that is known.
grid_variables <- function(nc, roles) {
  found <- list()
  for (name in names(nc$var)) {
    var <- nc$var[[name]]
    if (identical(var$prec, "char")) next
    dims <- vapply(var$dim, function(d) d$name, "")
    sizes <- vapply(var$dim, function(d) d$len, 1)
    role <- unname(roles[dims])
    open <- is.na(role) & sizes > 1 & dims != "expver"
    if (sum(role %in% "time") != 1L) next
    horizontal <- sum(role %in% "longitude") + sum(role %in% "latitude")
    if (horizontal == 1L && sum(open) == 1L) {
      role[open] <- setdiff(c("longitude", "latitude"), role)
      open[] <- FALSE
      horizontal <- 2L
    }
    if (horizontal == 0L && sum(open) == 2L) {
      # Two unlabelled dimensions can be asked about only when both could be
      # degrees; metres, rotated poles and projections are another kind of grid.
      degrees <- vapply(dims[open], function(d) {
        dim <- nc$dim[[d]]
        isTRUE(dim$create_dimvar) && is.numeric(dim$vals) && all(is.finite(dim$vals)) && all(abs(dim$vals) <= 360) &&
          !grepl("^(m|km|met(er|re)s?)$", tolower(trimws(paste(dim$units, collapse = "")))) &&
          !grepl("projection|grid_", tolower(nc_text(nc, d, "standard_name")))
      }, logical(1))
      found[[name]] <- if (all(degrees)) list(name = name, unknown = dims[open], time = dims[role %in% "time"])
                       else list(name = name, projected = dims[open])
      next
    }
    if (any(open) || sum(role %in% "longitude") != 1L || sum(role %in% "latitude") != 1L) next
    found[[name]] <- list(name = name, dims = dims, longitude = dims[role %in% "longitude"],
                          latitude = dims[role %in% "latitude"], time = dims[role %in% "time"])
  }
  found
}

# What the metadata of a variable says about temperature.
temperature_evidence <- function(nc, name) {
  units <- nc_text(nc, name, "units")
  unit <- if (nzchar(units)) tryCatch(temperature_unit(units), error = function(e) NA_character_) else NA_character_
  described <- grepl("temperature", paste(nc_text(nc, name, "standard_name"), nc_text(nc, name, "long_name")),
                     ignore.case = TRUE)
  named <- grepl("^(t2m|2m_temperature|tx|tas|temp|mx2t|mn2t)$|tmax|tmin|tasm|tmm[xn]|temperature", name,
                 ignore.case = TRUE)
  list(units = units, unit = unit,
       fahrenheit = grepl("fahrenheit|^deg(ree)?s?[ _]?f$|^f$", units, ignore.case = TRUE),
       temperature = !is.na(unit) || described || named,
       maximum = grepl("max|^tx$|tmmx|mx2t", name, ignore.case = TRUE) ||
         grepl("time: *maximum", nc_text(nc, name, "cell_methods"), ignore.case = TRUE))
}

# One decision the file leaves open, for the setup window to ask.
input_question <- function(id, title, detail, choices, recommended = NULL) {
  list(id = id, title = title, detail = detail, choices = choices, recommended = recommended)
}

# Read coordinate arrays, attributes and one time step only. No temperature
# cubes are loaded until the user presses Run analysis. Decisions that the
# metadata leaves open come back as `questions` instead of a guess.
temperature_header <- function(file, input) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  label <- basename(file)
  roles <- dimension_roles(nc, input)
  grids <- grid_variables(nc, roles)
  raw <- reader_names("era5")$variable
  questions <- list()
  variable <- input$variable
  if (identical(variable, "auto")) {
    names_found <- names(grids)
    evidence <- lapply(names_found, temperature_evidence, nc = nc)
    warm <- names_found[vapply(evidence, function(e) e$temperature, logical(1))]
    if ("tmax" %in% names_found && any(raw %in% names_found)) {
      if (!identical(input$format, "auto")) {
        variable <- if (input$format == "era5") intersect(raw, names_found)[1] else "tmax"
      } else {
        questions$variable <- input_question("variable", "Which temperature should SCORCH analyze?",
          sprintf("%s holds prepared daily maximum temperature and raw 2 m temperature.", label),
          stats::setNames(c("tmax", intersect(raw, names_found)[1]),
                          c("Prepared daily maximum temperature (tmax)",
                            sprintf("Raw 2 m temperature (%s)", intersect(raw, names_found)[1]))))
      }
    } else if ("tmax" %in% names_found) {
      variable <- "tmax"
    } else if (any(raw %in% names_found)) {
      variable <- intersect(raw, names_found)[1]
    } else if (length(warm) == 1L) {
      variable <- warm
    } else if (length(warm) > 1L) {
      highest <- warm[vapply(evidence[match(warm, names_found)], function(e) e$maximum, logical(1))]
      if (length(highest) == 1L) variable <- highest else {
        questions$variable <- input_question("variable", "Which variable holds the temperature?",
          sprintf("%s has %d gridded temperature variables.", label, length(warm)),
          stats::setNames(warm, vapply(warm, function(v) {
            long <- nc_text(nc, v, "long_name")
            if (nzchar(long)) sprintf("%s (%s)", v, long) else v
          }, "")))
      }
    } else if (length(names_found)) {
      # Nothing calls itself temperature. A variable recorded in other units
      # (mm, m s-1) is not one; a variable without units may be.
      other <- vapply(evidence, function(e) nzchar(e$units), logical(1))
      if (all(other)) {
        fail(sprintf("%s does not appear to hold temperature. Its gridded variables are: %s.", label,
                     paste(sprintf("%s (%s)", names_found, vapply(evidence, function(e) e$units, "")), collapse = ", ")))
      }
      if (sum(!other) == 1L) variable <- names_found[!other] else {
        questions$variable <- input_question("variable", "Which variable holds the temperature?",
          sprintf("%s does not say which of its gridded variables is temperature.", label),
          stats::setNames(names_found[!other], names_found[!other]))
      }
    } else {
      fail(sprintf(paste("%s has no temperature variable on a time, latitude and longitude grid.",
                         "Its variables are: %s. SCORCH needs a regular longitude-latitude grid."),
                   label, paste(names(nc$var), collapse = ", ")))
    }
  } else if (!variable %in% names(nc$var)) {
    fail(sprintf("The file %s has no temperature variable called '%s'. It has: %s.", label, variable,
                 paste(names(nc$var), collapse = ", ")))
  } else if (is.null(grids[[variable]])) {
    fail(sprintf("The variable '%s' in %s is not on a time, latitude and longitude grid.", variable, label))
  }
  if (length(questions)) return(list(file = file, questions = questions))

  grid <- grids[[variable]]
  if (!is.null(grid$projected)) {
    fail(sprintf(paste("%s is not on a regular longitude-latitude grid: '%s' uses the dimensions %s, which are",
                       "projected or rotated coordinates. SCORCH needs longitudes and latitudes in degrees."),
                 label, variable, paste(grid$projected, collapse = " and ")))
  }
  if (!is.null(grid$unknown)) {
    span <- vapply(grid$unknown, function(d) {
      values <- nc$dim[[d]]$vals
      sprintf("%s (%d values, %s to %s)", d, length(values), format(min(values)), format(max(values)))
    }, "")
    return(list(file = file, questions = list(longitude = input_question("longitude",
      "Which dimension is the longitude?",
      sprintf("%s does not label the two horizontal dimensions of '%s'. The other one is used as latitude.",
              label, variable),
      stats::setNames(grid$unknown, span)))))
  }
  for (name in c(grid$time, grid$latitude, grid$longitude)) {
    if (!isTRUE(nc$dim[[name]]$create_dimvar)) {
      fail(sprintf("The dimension '%s' in %s has no coordinate values.", name, label))
    }
  }

  # Dates and time step, decoded as the readers decode them.
  time <- nc$dim[[grid$time]]
  calendar <- nc_text(nc, grid$time, "calendar")
  calendar <- if (nzchar(calendar)) calendar else "standard"
  cf <- cf_time_seconds(time$vals, time$units, calendar)
  seconds <- sort(unique(as.numeric(cf$origin) * 86400 + cf$seconds))
  step <- if (length(seconds) > 1L) min(diff(seconds)) else 86400
  if (step > 86400 * (1 + 1e-9)) {
    fail(sprintf("%s has one value every %s days. SCORCH needs daily or hourly temperature.", label,
                 format(round(step / 86400, 1))))
  }
  per_day <- max(1L, as.integer(round(86400 / step)))
  dates <- sort(unique(decode_time(time$vals, time$units, calendar)))

  format <- input$format
  if (identical(format, "auto")) {
    format <- if (per_day > 1L || variable %in% raw) "era5" else "daily_tmax"
  }
  if (format == "daily_tmax" && per_day > 1L) {
    fail(sprintf("%s has %d values per day, so it is not prepared daily Tmax. Set the data format to ERA5 temperature.",
                 label, per_day))
  }

  # One time step shows whether the stored numbers agree with the stated units.
  dims <- vapply(nc$var[[variable]]$dim, function(d) d$name, "")
  sample <- tryCatch(suppressWarnings(range(ncdf4::ncvar_get(nc, variable, start = rep(1, length(dims)),
    count = ifelse(dims == grid$time, 1, -1)), na.rm = TRUE)), error = function(e) c(NA_real_, NA_real_))
  if (!all(is.finite(sample))) sample <- c(NA_real_, NA_real_)
  looks <- if (is.na(sample[1])) NA_character_ else if (sample[1] > 150) "K" else if (sample[2] < 100) "degC" else NA_character_
  evidence <- temperature_evidence(nc, variable)
  if (evidence$fahrenheit) {
    fail(sprintf("%s stores temperature in Fahrenheit. Provide degrees Celsius or Kelvin.", label))
  }
  units <- input$units
  stated <- evidence$unit
  if (identical(units, "auto")) {
    seen <- if (is.na(sample[1])) "" else sprintf(" The first time step ranges from %s to %s.",
                                                  format(round(sample[1], 1)), format(round(sample[2], 1)))
    choices <- c("Kelvin" = "K", "Degrees Celsius" = "degC")
    if (is.na(stated)) {
      questions$units <- input_question("units", "Which units are these temperatures in?",
        paste0(if (nzchar(evidence$units)) sprintf("%s records the units of '%s' as '%s', which SCORCH does not recognize.",
                                                    label, variable, evidence$units)
               else sprintf("%s does not record the units of '%s'.", label, variable), seen),
        choices, looks)
    } else if (!is.na(looks) && looks != stated) {
      questions$units <- input_question("units", "Which units are these temperatures in?",
        sprintf("%s records '%s' for '%s', but the numbers look like %s.%s", label, evidence$units, variable,
                if (looks == "K") "Kelvin" else "degrees Celsius", seen),
        choices, looks)
    } else units <- stated
  } else units <- temperature_unit(units)
  if (length(questions)) return(list(file = file, questions = questions))

  list(file = file, format = format, variable = variable, longitude = grid$longitude,
       latitude = grid$latitude, time = grid$time, units = units, stated_units = evidence$units,
       dates = dates, per_day = per_day, lon = nc$dim[[grid$longitude]]$vals, lat = nc$dim[[grid$latitude]]$vals,
       sample_c = if (units == "K") sample - 273.15 else sample, questions = list())
}

inspect_temperature_source <- function(source, input) {
  first <- temperature_header(source$files[[1L]], input)
  if (length(first$questions)) return(first)
  headers <- c(list(first), lapply(source$files[-1L], function(file) {
    header <- tryCatch(temperature_header(file, input), error = identity)
    if (inherits(header, "error") || length(header$questions)) {
      fail(sprintf(paste("%s does not hold temperature in the same way as %s%s.",
                         "Select the files of one temperature dataset only."), basename(file), basename(first$file),
                   if (inherits(header, "error")) paste0(" (", conditionMessage(header), ")") else ""))
    }
    header
  }))
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
  for (key in c("variable", "longitude", "latitude", "time", "units", "per_day"))
    first[[key]] <- unique(unlist(lapply(headers, "[[", key)))
  first$files <- length(headers)
  first
}
