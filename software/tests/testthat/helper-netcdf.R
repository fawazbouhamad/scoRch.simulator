# A small NetCDF file of gridded values for the input tests. `values` is
# time x latitude x longitude, or a named list of such arrays, one variable
# each. Units "" leave a variable or coordinate without a units attribute.
# `attributes` are named by variable or by coordinate. `coordinates = "float"`
# stores the longitudes and latitudes in single precision, as many files do.
write_grid <- function(path, values, time, lon, lat, units = "degC", time_units = "days since 2000-01-01",
                       dims = c("lon", "lat", "time"), dim_units = c("degrees_east", "degrees_north"),
                       attributes = list(), coordinates = "double") {
  if (!is.list(values)) values <- list(tmax = values)
  units <- rep_len(units, length(values))
  # ncdf4 writes the values of a dimension in double precision. In single
  # precision they are a variable with the name of a dimension without values.
  single <- identical(coordinates, "float")
  axis <- function(i, values) {
    if (single) ncdf4::ncdim_def(dims[i], "", seq_along(values), create_dimvar = FALSE)
    else ncdf4::ncdim_def(dims[i], dim_units[i], values)
  }
  axes <- list(axis(1, lon), axis(2, lat), ncdf4::ncdim_def(dims[3], time_units, time))
  variables <- Map(function(name, unit) ncdf4::ncvar_def(name, unit, axes, missval = NA, prec = "double"),
                   names(values), units)
  stored <- if (single) lapply(1:2, function(i) ncdf4::ncvar_def(dims[i], dim_units[i], axes[i], prec = "float"))
  nc <- ncdf4::nc_create(path, c(unname(variables), stored))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  for (name in names(values)) ncdf4::ncvar_put(nc, variables[[name]], aperm(values[[name]], c(3, 2, 1)))
  if (single) {
    ncdf4::ncvar_put(nc, stored[[1]], lon)
    ncdf4::ncvar_put(nc, stored[[2]], lat)
  }
  for (name in names(attributes)) {
    for (key in names(attributes[[name]])) ncdf4::ncatt_put(nc, name, key, attributes[[name]][[key]])
  }
  path
}

# Any other layout, for files write_grid() cannot describe: an extra
# dimension, two-dimensional coordinates, a grid mapping. `dims` is a named
# list of list(units, values, attributes) and `variables` a named list of
# list(dims, units, values, attributes), with values in the order of `dims`.
# A variable without dimensions is a scalar that only carries attributes.
write_layout <- function(path, dims, variables) {
  text <- function(x) if (is.null(x)) "" else x
  axes <- Map(function(name, dim) ncdf4::ncdim_def(name, text(dim$units), dim$values), names(dims), dims)
  defined <- Map(function(name, variable) {
    if (!length(variable$dims)) return(ncdf4::ncvar_def(name, "", list(), prec = "integer"))
    ncdf4::ncvar_def(name, text(variable$units), unname(axes[variable$dims]), missval = NA, prec = "double")
  }, names(variables), variables)
  nc <- ncdf4::nc_create(path, unname(defined))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  for (name in names(variables)) {
    if (!is.null(variables[[name]]$values)) ncdf4::ncvar_put(nc, defined[[name]], variables[[name]]$values)
  }
  described <- c(dims, variables)
  for (name in names(described)) {
    for (key in names(described[[name]]$attributes)) {
      ncdf4::ncatt_put(nc, name, key, described[[name]]$attributes[[key]])
    }
  }
  path
}

# What the setup window learns from a selection, with the shipped defaults
# and any setting given in `...`.
detect_input <- function(path, ...) {
  input <- utils::modifyList(read_config(input_required = FALSE)$input, list(...))
  input["file"] <- list(path)
  source <- open_temperature_source(path)
  on.exit(close_temperature_source(source), add = TRUE)
  inspect_temperature_source(source, input)
}

# A 12 by 10 cell window of the bundled example for June and July 2012,
# small enough to write in several forms and large enough to hold events.
example_window <- function() {
  full <- read_tmax(system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator"),
                    months = 6:7, start = "2012-06-01", end = "2012-07-31")
  keep <- which(full$cells$lon > -100 & full$cells$lon < -88 & full$cells$lat > 32 & full$cells$lat < 42)
  lon <- sort(unique(full$cells$lon[keep]))
  lat <- sort(unique(full$cells$lat[keep]))
  celsius <- array(NA_real_, c(length(full$dates), length(lat), length(lon)))
  for (k in keep) celsius[, match(full$cells$lat[k], lat), match(full$cells$lon[k], lon)] <- full$tmax[, k]
  kelvin <- celsius + 273.15
  # Every route below converts Kelvin once, so this is the common reference.
  list(dates = full$dates, lon = lon, lat = lat, kelvin = kelvin, celsius = kelvin - 273.15)
}
