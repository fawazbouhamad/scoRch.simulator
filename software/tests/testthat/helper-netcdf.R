# A small NetCDF file of gridded values for the input tests. `values` is
# time x latitude x longitude, or a named list of such arrays, one variable
# each. Units "" leave a variable or coordinate without a units attribute.
write_grid <- function(path, values, time, lon, lat, units = "degC", time_units = "days since 2000-01-01",
                       dims = c("lon", "lat", "time"), dim_units = c("degrees_east", "degrees_north"),
                       attributes = list()) {
  if (!is.list(values)) values <- list(tmax = values)
  units <- rep_len(units, length(values))
  axes <- list(ncdf4::ncdim_def(dims[1], dim_units[1], lon), ncdf4::ncdim_def(dims[2], dim_units[2], lat),
               ncdf4::ncdim_def(dims[3], time_units, time))
  variables <- Map(function(name, unit) ncdf4::ncvar_def(name, unit, axes, missval = NA, prec = "double"),
                   names(values), units)
  nc <- ncdf4::nc_create(path, unname(variables))
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  for (name in names(values)) {
    ncdf4::ncvar_put(nc, variables[[name]], aperm(values[[name]], c(3, 2, 1)))
    for (key in names(attributes[[name]])) ncdf4::ncatt_put(nc, name, key, attributes[[name]][[key]])
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
