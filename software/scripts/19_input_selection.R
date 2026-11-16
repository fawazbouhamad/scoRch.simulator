# The source on disk stays in the configuration. Extracted ZIP members are
# private working copies, shared by the preview and analysis for one selection.
open_temperature_source <- function(path, progress = NULL) {
  if (!any(grepl("[.]zip$", path, ignore.case = TRUE) & !dir.exists(path))) {
    return(list(files = input_files(path), archive = NULL, scratch = NULL))
  }
  if (length(path) != 1L) fail("Select one ZIP, or a set of extracted ERA5 files.")
  archive <- normalizePath(path, winslash = "/", mustWork = TRUE)
  entries <- zip::zip_list(archive)
  entries <- entries[!grepl("(^|/)(__MACOSX/|[.]_)", entries$filename), , drop = FALSE]
  # Its .nc members, as in a folder; an archive without any gives its .nc4 members.
  netcdf <- grepl("[.]nc$", entries$filename, ignore.case = TRUE)
  if (!any(netcdf)) netcdf <- grepl("[.]nc4$", entries$filename, ignore.case = TRUE)
  entries <- entries[netcdf, , drop = FALSE]
  if (!nrow(entries)) fail("This ZIP contains no NetCDF (.nc or .nc4) files.")
  members <- entries$filename
  invalid <- grepl("(^/|(^|/)[.][.](/|$)|\\\\|:|[[:cntrl:]])", members) |
    grepl("^(CON|AUX|NUL|PRN|COM[1-9]|LPT[1-9])([.]|$)", basename(members), ignore.case = TRUE)
  # zip_list() reports the entry type from zip 2.3.3 on; older versions cannot report links.
  if (any(invalid) || anyDuplicated(members) || (!is.null(entries$type) && any(entries$type != "file")))
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
  list(files = files, archive = archive, scratch = scratch, labels = members)
}

# How a message names one file of a selection: its name, or for a ZIP its
# path in the archive, and the folder that holds it where two files of the
# selection have the same name (one folder per year, for example).
file_label <- function(source, file) {
  i <- match(file, source$files)
  if (!is.na(i) && !is.null(source$labels)) return(source$labels[[i]])
  if (sum(basename(source$files) == basename(file)) > 1L) return(file.path(basename(dirname(file)), basename(file)))
  basename(file)
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

# One attribute as text; "" when the variable has none. Older files store
# accents and the degree sign in Latin-1, which is read as that.
nc_text <- function(nc, name, attribute) {
  value <- tryCatch(ncdf4::ncatt_get(nc, name, attribute), error = function(e) list(hasatt = FALSE))
  if (!isTRUE(value$hasatt)) return("")
  text <- as.character(value$value)[1]
  if (!validUTF8(text)) text <- iconv(text, "latin1", "UTF-8")
  trimws(text)
}

# Could the coordinate values of a dimension be degrees no larger than `limit`?
# Units, where the file gives any, must then say degrees or a compass
# direction: a pressure, a level number, a distance or degrees of temperature
# is another kind of axis. So are projected or rotated axes, vertical axes, and
# the axes of a variable on a mapped or curvilinear grid (`mapped`). A
# dimension that is called longitude or latitude (`named`) is one whatever its
# units say ("none", "1", "dd"), unless they say a length, radians or a
# temperature.
degree_axis <- function(nc, name, limit, mapped, named = FALSE) {
  dim <- nc$dim[[name]]
  unit <- tolower(trimws(paste(dim$units, collapse = "")))
  angle <- !nzchar(unit) || unit %in% c("e", "east", "w", "west", "n", "north", "s", "south") ||
    (grepl("deg|\u00b0", unit) && is.na(tryCatch(temperature_unit(unit), error = function(e) NA_character_)))
  if (named && !grepl("^(m|km|(kilo)?met(er|re)s?|rad(ians?)?)([ _].*)?$", unit) &&
      is.na(tryCatch(temperature_unit(unit), error = function(e) NA_character_))) angle <- TRUE
  isTRUE(dim$create_dimvar) && is.numeric(dim$vals) && all(is.finite(dim$vals)) && all(abs(dim$vals) <= limit) &&
    angle && !grepl("projection|grid_", tolower(nc_text(nc, name, "standard_name"))) && !name %in% mapped &&
    tolower(nc_text(nc, name, "axis")) != "z" && !nzchar(nc_text(nc, name, "positive"))
}

# Dimensions of variables that say their grid is not plain longitude-latitude:
# a grid mapping of another kind, or latitudes and longitudes given as
# two-dimensional variables in the `coordinates` attribute.
mapped_dimensions <- function(nc) {
  listed <- function(name, attribute) strsplit(nc_text(nc, name, attribute), "[[:space:]]+")[[1L]]
  unique(unlist(lapply(names(nc$var), function(name) {
    # "crs: x y" names a mapping before each colon; "crs" alone is the mapping.
    mapping <- listed(name, "grid_mapping")
    if (any(grepl(":$", mapping))) mapping <- sub(":$", "", mapping[grepl(":$", mapping)])
    plain <- vapply(mapping, function(v) v %in% names(nc$var) &&
      identical(tolower(nc_text(nc, v, "grid_mapping_name")), "latitude_longitude"), logical(1))
    curved <- vapply(intersect(listed(name, "coordinates"), names(nc$var)), function(v) nc$var[[v]]$ndims > 1L &&
      (grepl("^degrees?[ _]?(e|east|n|north)$", tolower(nc_text(nc, v, "units"))) ||
         tolower(nc_text(nc, v, "standard_name")) %in% c("longitude", "latitude")), logical(1))
    if (!all(plain) || any(curved)) vapply(nc$var[[name]]$dim, function(d) d$name, "")
  }), use.names = FALSE))
}

# Which dimension is time, longitude or latitude: CF units and standard names
# first, then axis letters and familiar names for coordinates in degrees.
dimension_roles <- function(nc, input, mapped = mapped_dimensions(nc)) {
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
    if ((axis == "x" || short %in% c("lon", "longitude", "long", "x")) &&
        degree_axis(nc, name, 360, mapped, short %in% c("lon", "longitude", "long")))
      return("longitude")
    if ((axis == "y" || short %in% c("lat", "latitude", "y")) &&
        degree_axis(nc, name, 90, mapped, short %in% c("lat", "latitude")))
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
grid_variables <- function(nc, roles, mapped = mapped_dimensions(nc)) {
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
    # The remaining dimension takes the missing role only when its values could
    # be that coordinate; a pressure level next to a latitude is not a longitude.
    if (horizontal == 1L && sum(open) == 1L &&
        (!isTRUE(nc$dim[[dims[open]]]$create_dimvar) ||
           degree_axis(nc, dims[open], if ("latitude" %in% role) 360 else 90, mapped))) {
      role[open] <- setdiff(c("longitude", "latitude"), role)
      open[] <- FALSE
      horizontal <- 2L
    }
    if (horizontal == 0L && sum(open) == 2L) {
      # Two unlabelled dimensions must both be degrees; metres, rotated poles
      # and projections are another kind of grid. A latitude stays within 90:
      # when only one of the two does, it is the latitude and nothing is asked;
      # when neither does (`beyond`), no answer could be right.
      wide <- vapply(dims[open], function(d) degree_axis(nc, d, 360, mapped), logical(1))
      narrow <- vapply(dims[open], function(d) degree_axis(nc, d, 90, mapped), logical(1))
      # Axes that only count their cells (1..n, 0..n-1), or a "longitude" that
      # spans more than 360 degrees, are not told apart silently: the question
      # is asked, with the one dimension that can be the longitude (`offered`).
      counted <- all(vapply(dims[open], function(d) {
        values <- as.numeric(nc$dim[[d]]$vals)
        isTRUE(all.equal(values, as.numeric(seq_along(values)))) || isTRUE(all.equal(values, seq_along(values) - 1))
      }, logical(1)))
      span <- vapply(dims[open], function(d) diff(range(nc$dim[[d]]$vals)), 1)
      if (all(wide) && sum(narrow) == 1L && (counted || any(span[!narrow] > 360))) {
        found[[name]] <- list(name = name, unknown = dims[open], offered = dims[open][!narrow],
                              time = dims[role %in% "time"])
        next
      }
      if (all(wide) && sum(narrow) == 1L) {
        role[open] <- ifelse(narrow, "latitude", "longitude")
        open[] <- FALSE
      } else {
        found[[name]] <- if (all(narrow)) list(name = name, unknown = dims[open], time = dims[role %in% "time"])
                         else list(name = name, projected = dims[open], beyond = all(wide))
        next
      }
    }
    if (any(open) || sum(role %in% "longitude") != 1L || sum(role %in% "latitude") != 1L) next
    found[[name]] <- list(name = name, dims = dims, longitude = dims[role %in% "longitude"],
                          latitude = dims[role %in% "latitude"], time = dims[role %in% "time"])
  }
  found
}

# What the metadata of a variable says about temperature: its units, and
# whether its name, standard_name, long_name or cell_methods call it a
# maximum, a minimum, a time mean, 2 m air temperature or another kind of
# temperature (dew point, sea, soil, skin, an anomaly).
temperature_evidence <- function(nc, name) {
  units <- nc_text(nc, name, "units")
  unit <- if (nzchar(units)) tryCatch(temperature_unit(units), error = function(e) NA_character_) else NA_character_
  standard <- nc_text(nc, name, "standard_name")
  long <- nc_text(nc, name, "long_name")
  methods <- nc_text(nc, name, "cell_methods")
  described <- grepl("temperature", paste(standard, long), ignore.case = TRUE)
  named <- grepl("^(t2m|2m_temperature|tx|tas|temp|mx2t|mn2t)$|tmax|tmin|tasm|tmm[xn]|temperature", name,
                 ignore.case = TRUE)
  # Whole words of the two descriptive names: "Near-Surface" and
  # "air_temperature" read "near surface" and "air temperature".
  text <- paste0(" ", gsub("[^a-z0-9]+", " ", tolower(paste(standard, long))), " ")
  says <- function(in_name, in_text, in_time = NULL) {
    grepl(in_name, name, ignore.case = TRUE) || grepl(in_text, text) ||
      (!is.null(in_time) && grepl(paste0("time: *", in_time), methods, ignore.case = TRUE))
  }
  # A number scales the stored values only where a factor stands: leading the
  # text ("0.1 degC", "10 K") or after *, x, the multiplication sign or /
  # ("K*10", "degC / 10"). The height in "Celsius (2 m)" or the scale in
  # "ITS-90" is no factor.
  number <- "[0-9]*[.]?[0-9]+([eE][-+]?[0-9]+)?"
  times <- "[*xX/\u00d7]"
  factors <- regmatches(units, gregexpr(paste0("^[[:space:]]*", number, "|", times, "[[:space:]]*", number), units))[[1L]]
  factors <- as.numeric(sub(paste0("^(", times, "|[[:space:]])+"), "", factors))
  # The readers take the units as they are stored and stop on text that is not valid UTF-8.
  stored <- tryCatch(ncdf4::ncatt_get(nc, name, "units"), error = function(e) list(hasatt = FALSE))
  list(units = units, unit = unit, legible = !isTRUE(stored$hasatt) || all(validUTF8(as.character(stored$value))),
       fahrenheit = grepl("fah?renheit|^(deg(ree)?s?)?f$", gsub("[^A-Za-z]", "", units, useBytes = TRUE),
                          ignore.case = TRUE),
       scaled = any(factors != 1) || grepl("tenth|hundredth", units, ignore.case = TRUE),
       temperature = !is.na(unit) || described || named,
       maximum = says("max|^tx$|tmmx|mx2t", " (max|maximum|maximal|maxima|tmax|tasmax|t2mmax|tmmx|mx2t) ", "max"),
       minimum = says("min|^tn$|tmmn|mn2t", " (min|minimum|minimal|minima|tmin|tasmin|t2mmin|tmmn|mn2t) ", "min"),
       mean = says("mean|avg|average|^tg$", " (mean|average|averaged|avg) ", "mean"),
       # CF's air_temperature is the air at any height, so that name alone does not say 2 m.
       air = says("^(t2m|2m_temperature|tas)$",
                  paste0(" 2 ?m(et(re|er)s?)? (above (the )?(ground|surface) )?(air )?temperature ",
                         "| temperature ((at|in) )?2 ?m(et(re|er)s?)? ",
                         "|surface air temperature ")),
       other = tolower(standard) == "surface_temperature" ||
         says("dew|sst|skt|soil|skin|anom|^(d2m|tdps|ts|tos)$",
              paste0(" (dew|dewpoint|wet bulb|wetbulb|sea|soil|skin|lake|ice|potential|apparent|brightness|water|below|depth) ",
                     "|anomal| land surface temperature ")))
}

# Does the file itself identify the variable as what SCORCH analyzes? Daily
# values must be a daily maximum; sub-daily values must be 2 m air temperature,
# whose highest value of each day is the daily maximum. The names that `auto`
# reads in the scripted workflow (tmax, t2m) count unless the file objects: a
# tmax that the file calls a minimum, an anomaly or a sea temperature does not.
# What nothing calls temperature is never identified, whatever its name says.
temperature_identified <- function(evidence, variable, format, per_day) {
  if (!evidence$temperature) return(FALSE)
  highest <- evidence$maximum && !evidence$minimum
  if (format == "daily_tmax") return(highest && !evidence$other)
  known <- variable %in% reader_names("era5")$variable
  if (per_day > 1L) return((known || evidence$air || highest) && !evidence$minimum && !evidence$other)
  (known || highest) && !evidence$minimum && !evidence$mean && !evidence$other
}

# Is a dimension of length one a level above the surface? A pressure, sigma
# or model level is; the height of a few metres that some files carry is not.
upper_level <- function(nc, name) {
  dim <- nc$dim[[name]]
  units <- tolower(trimws(paste(dim$units, collapse = "")))
  near_surface <- grepl("^(m|met(er|re)s?)$", units) && isTRUE(dim$create_dimvar) && all(abs(dim$vals) <= 10)
  !near_surface && (grepl("^(pa|hpa|mb|mbar|millibars?)$", units) || tolower(nc_text(nc, name, "axis")) == "z" ||
                      nzchar(nc_text(nc, name, "positive")) ||
                      grepl("lev|pressure|isobaric|sigma|height|altitude", name, ignore.case = TRUE))
}

# One decision the file leaves open, for the setup window to ask.
input_question <- function(id, title, detail, choices, recommended = NULL) {
  list(id = id, title = title, detail = detail, choices = choices, recommended = recommended)
}

# A temperature variable that the file does not identify is confirmed by the
# user, never assumed: a minimum, a mean or a dew point gives other heatwaves.
variable_question <- function(nc, label, variable, per_day) {
  facts <- c("long name" = nc_text(nc, variable, "long_name"), "standard name" = nc_text(nc, variable, "standard_name"),
             "cell methods" = nc_text(nc, variable, "cell_methods"))
  facts <- facts[nzchar(facts)]
  hourly <- per_day > 1L
  input_question("variable", "Is this the temperature to analyze?",
    paste0(sprintf("%s holds the variable '%s'%s, with %s. ", label, variable,
                   if (length(facts)) sprintf(" (%s)", paste(sprintf("%s '%s'", names(facts), facts), collapse = ", ")) else "",
                   if (hourly) sprintf("%d values per day", per_day) else "one value per day"),
           if (hourly) paste("The file does not identify it as 2 m air temperature. SCORCH detects heatwaves in daily",
                             "maximum temperature, which it would take from the highest value of each day.")
           else paste("The file does not identify it as daily maximum temperature, which is what SCORCH analyzes",
                      "to detect heatwaves."),
           " If this is not the variable you meant, choose other data."),
    stats::setNames(variable, if (hourly) sprintf("Analyze the daily maximum of '%s'", variable)
                              else sprintf("Analyze '%s' as the daily temperature", variable)),
    NA_character_)
}

# The refusal for coordinates that the readers find unevenly spaced (`reason`
# is their sentence). What to do depends on the cause: a regular grid whose
# coordinates carry the rounding of single precision, a region stored across
# 180 degrees with negative longitudes, or a grid that really is uneven.
# `values` are the coordinates as the reader sees them and `size` is the
# largest of them as the file stores them.
uneven_spacing <- function(label, axis, values, reason) {
  regular <- function(v) !inherits(tryCatch(grid_step(v, axis), error = identity), "error")
  # Coordinates that are regular to the rounding of single precision are read
  # (even_coordinates()); what reaches this point is uneven beyond that.
  # Negative longitudes on one side of 180 degrees and positive ones on the
  # other are neighbours only when all are counted from 0 to 360.
  if (axis == "longitude" && any(values < 0) && any(values >= 0) && regular(sort(values %% 360))) {
    return(sprintf(paste("%s covers a region across 180 degrees longitude, with its longitudes stored as -180 to",
                         "180. %s SCORCH needs the same spacing between all longitudes: store them as 0 to 360 in",
                         "ascending order (%s to %s), then choose that file."),
                   label, reason, format(min(values %% 360)), format(max(values %% 360))))
  }
  sprintf(paste("%s is not on a regular longitude-latitude grid. %s SCORCH needs the same spacing between all",
                "%ss; regrid a Gaussian or otherwise uneven grid first."), label, reason, axis)
}

# Read coordinate arrays, attributes and one time step only. No temperature
# cubes are loaded until the user presses Run analysis. Decisions that the
# metadata leaves open come back as `questions` instead of a guess.
temperature_header <- function(file, input) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  # The units of a coordinate are text like any attribute: UTF-8, or Latin-1
  # in older files, where a degree sign would otherwise stop the reading.
  for (d in names(nc$dim)) {
    stored <- nc$dim[[d]]$units
    if (!is.character(stored)) next
    if (all(validUTF8(stored))) Encoding(stored) <- "UTF-8" else stored <- iconv(stored, "latin1", "UTF-8")
    nc$dim[[d]]$units <- stored
  }
  label <- basename(file)
  mapped <- mapped_dimensions(nc)
  roles <- dimension_roles(nc, input, mapped)
  grids <- grid_variables(nc, roles, mapped)
  raw <- reader_names("era5")$variable
  questions <- list()
  variable <- input$variable
  several <- function(found) {
    input_question("variable", "Which variable holds the temperature?",
      sprintf("%s has %d gridded temperature variables.", label, length(found)),
      stats::setNames(found, vapply(found, function(v) {
        long <- nc_text(nc, v, "long_name")
        if (nzchar(long)) sprintf("%s (%s)", v, long) else v
      }, "")))
  }
  # The variables among which one was preferred, to ask about them all when
  # the file does not identify the preferred one after all.
  among <- character(0)
  if (identical(variable, "auto")) {
    names_found <- names(grids)
    clues <- lapply(names_found, temperature_evidence, nc = nc)
    warm <- names_found[vapply(clues, function(e) e$temperature, logical(1))]
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
      highest <- warm[vapply(clues[match(warm, names_found)], function(e) e$maximum && !e$minimum && !e$other,
                             logical(1))]
      if (length(highest) == 1L) {
        variable <- highest
        among <- warm
      } else questions$variable <- several(warm)
    } else if (length(names_found)) {
      # Nothing calls itself temperature. A variable recorded in other units
      # (mm, m s-1) is not one; a variable without units may be.
      other <- vapply(clues, function(e) nzchar(e$units), logical(1))
      if (all(other)) {
        fail(sprintf("%s does not appear to hold temperature. Its gridded variables are: %s.", label,
                     paste(sprintf("%s (%s)", names_found, vapply(clues, function(e) e$units, "")), collapse = ", ")))
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
    fail(sprintf(paste("%s is not on a regular longitude-latitude grid: '%s' uses the dimensions %s, %s.",
                       "SCORCH needs longitudes and latitudes in degrees."),
                 label, variable, paste(grid$projected, collapse = " and "),
                 if (isTRUE(grid$beyond)) "and both have values beyond 90, so that neither can be the latitude"
                 else "which are projected or rotated coordinates"))
  }
  if (!isTRUE(nc$dim[[grid$time]]$create_dimvar)) {
    fail(sprintf("The dimension '%s' in %s has no coordinate values.", grid$time, label))
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

  # read_tmax() takes exactly three dimensions; only the ERA5 reader drops
  # dimensions of length one and merges expver.
  dims <- vapply(nc$var[[variable]]$dim, function(d) d$name, "")
  extra <- setdiff(dims, c(grid$time, grid$latitude, grid$longitude, grid$unknown))
  if (format == "daily_tmax" && length(dims) != 3L) {
    fail(sprintf(paste("'%s' in %s has %d dimensions (%s). Prepared daily Tmax must have exactly three: time, latitude",
                       "and longitude. Save the variable without the extra dimension %s, then choose that file.%s"),
                 variable, label, length(dims), paste(dims, collapse = ", "), paste0("'", extra, "'", collapse = " and "),
                 if ("expver" %in% extra) paste(" For ERA5 data, setting the data format to ERA5 temperature under",
                                                "Advanced settings merges 'expver' instead.") else ""))
  }

  # Units that no answer could make readable stop the selection before any question.
  evidence <- temperature_evidence(nc, variable)
  if (evidence$fahrenheit) {
    fail(sprintf("%s stores temperature in Fahrenheit. Provide degrees Celsius or Kelvin.", label))
  }
  if (evidence$scaled && identical(input$units, "auto")) {
    fail(sprintf(paste("%s records the units of '%s' as '%s', so its numbers are not whole degrees.",
                       "Provide degrees Celsius or Kelvin. If the numbers are whole degrees after all, choose the",
                       "temperature units under Advanced settings."), label, variable, evidence$units))
  }

  # A variable that detection found is analyzed only when the file identifies
  # it. Otherwise the user confirms it, which names it like a configuration.
  # Temperature on a pressure or model level is not the 2 m temperature.
  identified <- temperature_identified(evidence, variable, format, per_day) &&
    !any(vapply(extra, upper_level, logical(1), nc = nc))
  if (identical(input$variable, "auto") && !identified) {
    return(list(file = file, questions = list(variable =
      if (length(among) > 1L) several(among) else variable_question(nc, label, variable, per_day))))
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
      stats::setNames(grid$unknown, span)[grid$unknown %in% (if (is.null(grid$offered)) grid$unknown else grid$offered)]))))
  }

  # The readers need constant spacing. Check the coordinates as they will see
  # them: the ERA5 reader wraps the longitudes and sorts both coordinates.
  coordinates <- list(longitude = grid$longitude, latitude = grid$latitude)
  for (axis in names(coordinates)) {
    if (!isTRUE(nc$dim[[coordinates[[axis]]]]$create_dimvar)) {
      fail(sprintf("The dimension '%s' in %s has no coordinate values.", coordinates[[axis]], label))
    }
    coordinates[[axis]] <- as.numeric(nc$dim[[coordinates[[axis]]]]$vals)
    if (length(coordinates[[axis]]) < 2L || anyNA(coordinates[[axis]])) {
      fail(sprintf("%s needs at least two %s values, without missing values.", label, axis))
    }
  }
  # Coordinates that are evenly spaced only to the rounding of single
  # precision are made even first, as the readers do (even_coordinates()).
  if (format == "era5") {
    axes <- raw_coordinates(coordinates$longitude, coordinates$latitude)
    coordinates <- list(longitude = axes$lon, latitude = axes$lat)
  } else {
    coordinates <- lapply(coordinates, function(values) even_coordinates(values)$values)
  }
  for (axis in names(coordinates)) {
    tryCatch(grid_step(coordinates[[axis]], axis), error = function(e) {
      fail(uneven_spacing(label, axis, coordinates[[axis]], conditionMessage(e)))
    })
  }

  # One time step shows whether the stored numbers can be air temperature in
  # the stated units: between -100 and 70 degrees Celsius.
  sample <- tryCatch(suppressWarnings(range(ncdf4::ncvar_get(nc, variable, start = rep(1, length(dims)),
    count = ifelse(dims == grid$time, 1, -1)), na.rm = TRUE)), error = function(e) c(NA_real_, NA_real_))
  if (!all(is.finite(sample))) sample <- c(NA_real_, NA_real_)
  celsius <- list(K = sample - 273.15, degC = sample)
  fits <- vapply(celsius, function(range) !anyNA(range) && range[1] >= -100 && range[2] <= 70, logical(1))
  implausible <- function(unit) {
    span <- sprintf("%s to %s", format(round(sample[1], 1)), format(round(sample[2], 1)))
    fail(sprintf("The first time step of %s ranges from %s", label, span),
         if (is.na(unit)) {
           sprintf(" and %s. These numbers cannot be air temperature in degrees Celsius or in Kelvin.",
                   if (nzchar(evidence$units)) sprintf("the file records the units as '%s'", evidence$units)
                   else "the file records no units")
         } else if (unit == "K") {
           sprintf(" K (%s to %s \u00b0C), which cannot be air temperature.",
                   format(round(celsius$K[1], 1)), format(round(celsius$K[2], 1)))
         } else " \u00b0C, which cannot be air temperature.",
         " The file may mark missing data with a number that it does not declare as missing (such as 9999 or 1e20),",
         " or its values may be in other units, such as Fahrenheit or tenths of a degree.")
  }
  units <- input$units
  # Units that only this preview can read are settled by a question, whose
  # answer the configuration then carries to the readers.
  stated <- if (evidence$legible) evidence$unit else NA_character_
  if (identical(units, "auto")) {
    seen <- if (is.na(sample[1])) "" else sprintf(" The first time step ranges from %s to %s.",
                                                  format(round(sample[1], 1)), format(round(sample[2], 1)))
    choices <- c("Kelvin" = "K", "Degrees Celsius" = "degC")
    # At most one unit can fit. It is suggested; a question that neither
    # answer could satisfy is not asked.
    likely <- if (any(fits)) names(fits)[fits] else NA_character_
    # Above 60 the numbers are hotter than air has been measured in degrees
    # Celsius and may be Fahrenheit: that answer is offered, not suggested.
    suggested <- if (identical(likely, "degC") && sample[2] > 60) NA_character_ else likely
    if (!is.na(stated) && (is.na(sample[1]) || fits[[stated]])) {
      units <- stated
    } else if (!is.na(sample[1]) && is.na(likely)) {
      implausible(stated)
    } else if (is.na(stated)) {
      recorded <- sprintf("%s records the units of '%s' as '%s', ", label, variable, evidence$units)
      questions$units <- input_question("units", "Which units are these temperatures in?",
        paste0(if (!evidence$legible) paste0(recorded, "in a text encoding that the analysis cannot read.")
               else if (nzchar(evidence$units)) paste0(recorded, "which SCORCH does not recognize.")
               else sprintf("%s does not record the units of '%s'.", label, variable), seen),
        choices, suggested)
    } else {
      spelled <- c(K = "Kelvin", degC = "degrees Celsius")
      questions$units <- input_question("units", "Which units are these temperatures in?",
        sprintf("%s records '%s' for '%s', but the numbers %s.%s", label, evidence$units, variable,
                if (is.na(suggested)) paste("cannot be air temperature in", spelled[[stated]])
                else paste("look like", spelled[[likely]]), seen),
        choices, suggested)
    }
  } else units <- temperature_unit(units)
  if (length(questions)) return(list(file = file, questions = questions))
  if (!is.na(sample[1]) && !fits[[units]]) implausible(units)

  list(file = file, format = format, variable = variable, longitude = grid$longitude,
       latitude = grid$latitude, time = grid$time, units = units, stated_units = evidence$units,
       identified = identified, dates = dates, per_day = per_day,
       lon = nc$dim[[grid$longitude]]$vals, lat = nc$dim[[grid$latitude]]$vals,
       sample_c = celsius[[units]], questions = list())
}

inspect_temperature_source <- function(source, input) {
  first <- temperature_header(source$files[[1L]], input)
  if (length(first$questions)) return(first)
  headers <- c(list(first), lapply(source$files[-1L], function(file) {
    header <- tryCatch(temperature_header(file, input), error = identity)
    if (inherits(header, "error") || length(header$questions)) {
      fail(sprintf(paste("%s does not hold temperature in the same way as %s%s.",
                         "Select the files of one temperature dataset only."), file_label(source, file), file_label(source, first$file),
                   if (inherits(header, "error")) paste0(" (", conditionMessage(header), ")")
                   else sprintf(" (on its own it raises the question: %s)", header$questions[[1L]]$title)))
    }
    header
  }))
  if (any(vapply(headers, function(h) h$format != first$format, logical(1))))
    fail("This selection mixes ERA5 and prepared daily Tmax. Choose one dataset.")
  folder_selected <- length(input$file) > 0L && any(dir.exists(input$file))
  if (first$format == "daily_tmax" && (length(headers) != 1L || folder_selected)) {
    # Several files of daily values (one per day or year, as MERRA-2 gives
    # them) are read together by the ERA5 reader, which takes daily values as
    # they are; it is chosen under Advanced settings.
    fail("For prepared daily Tmax, select one NetCDF file or a ZIP containing one prepared file.",
         if (length(headers) > 1L) paste(" To read these", length(headers), "files of daily values together, set",
                                         "'Data format' to 'ERA5 temperature' under Advanced settings."))
  }
  # The readers apply one set of names to all files. Names that differ are
  # read correctly only when `auto` finds each of them by itself.
  expected <- reader_names(first$format)
  what <- c(variable = "temperature variable", longitude = "longitude dimension", latitude = "latitude dimension",
            time = "time dimension")
  for (h in headers[-1L]) {
    for (key in names(what)) {
      if (h[[key]] != first[[key]] && !all(c(h[[key]], first[[key]]) %in% expected[[key]])) {
        fail(sprintf(paste("%s and %s name the %s differently ('%s' and '%s'). SCORCH reads all files of a selection",
                           "with the same names. Select the files of one temperature dataset only%s."),
                     file_label(source, first$file), file_label(source, h$file), what[[key]], first[[key]], h[[key]],
                     if (key == "variable" && identical(input$variable, "auto"))
                       paste("; if every file holds the variable you want, name it as the temperature variable",
                             "under Advanced settings") else ""))
      }
    }
  }
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
  first$identified <- all(vapply(headers, function(h) isTRUE(h$identified), logical(1)))
  first$files <- length(headers)
  first
}
