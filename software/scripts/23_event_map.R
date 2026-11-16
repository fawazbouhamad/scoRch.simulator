# What the setup window draws: the heatwave cells, clusters and ellipses of
# every day of a compound event, as plain lists that the browser reads. The
# same functions prepare the bundled demonstration and the events of a run.

# The four types in plain language. Each sentence restates classify_event().
TYPE_MEANINGS <- c(
  "One regionally extensive day whose heatwave cells form a single cluster, so SCORCH fits one ellipse.",
  "One regionally extensive day whose heatwave cells form several separate clusters, each with its own ellipse.",
  "Several consecutive days with the same pattern every day: always one cluster, or always several clusters.",
  "Several consecutive days whose pattern changes: one-cluster and several-cluster days are mixed, or a day forms no cluster.")

# Why an event has its type, from the number of clusters on each of its days.
event_type_reason <- function(counts) {
  type <- classify_event(counts)
  n <- length(counts)
  if (n == 1L) {
    return(sprintf("One regionally extensive day with %s",
                   if (type == 1L) "one cluster" else sprintf("%d separate clusters", counts)))
  }
  pattern <- if (type == 3L) {
    if (all(counts == 1)) "one cluster on every day" else "several clusters on every day"
  } else {
    kinds <- c(if (any(counts == 1)) "one-cluster", if (any(counts > 1)) "several-cluster")
    none <- sum(counts == 0)
    paste(c(if (length(kinds) == 2L) "a mix of one-cluster and several-cluster days" else paste(kinds, "days"),
            if (none) sprintf("%d day%s without a cluster", none, plural(none))), collapse = " and ")
  }
  sprintf("%d consecutive regionally extensive days with %s", n, pattern)
}

# Land, lakes and (near the United States) state lines of the maps package as
# flat longitude-latitude vectors. Whole outlines are sent; the browser clips
# them to the grid. Coordinates are rounded to what a window-sized map shows.
basemap_layers <- function(extent) {
  width <- extent[2] - extent[1]
  digits <- if (width >= 50) 1L else if (width >= 5) 2L else 3L
  layer <- function(database, fill = TRUE) {
    if (extent[2] > 180) {
      if (database != "world") return(list())
      database <- "world2"
    }
    shape <- tryCatch(suppressWarnings(maps::map(database, xlim = extent[1:2], ylim = extent[3:4],
                                                 plot = FALSE, fill = fill)), error = function(e) NULL)
    if (is.null(shape) || !length(shape$x)) return(list())
    ring <- cumsum(is.na(shape$x))
    keep <- !is.na(shape$x)
    lines <- lapply(split(seq_along(shape$x)[keep], ring[keep]), function(i) {
      xy <- round(cbind(shape$x[i], shape$y[i]), digits)
      xy <- xy[c(TRUE, rowSums(abs(diff(xy))) > 0), , drop = FALSE]
      if (nrow(xy) < 2L) NULL else I(as.vector(t(xy)))
    })
    unname(Filter(Negate(is.null), lines))
  }
  list(land = layer("world"), lakes = layer("lakes"),
       states = if (overlaps_us_states(extent)) layer("state", fill = FALSE) else list())
}

# Daily maximum temperature of every heatwave cell of every selected day, in
# the row order of the cluster-cell table.
cluster_cell_tmax <- function(result, tmax) {
  cells <- result$clusters$cluster_cells
  tmax$tmax[cbind(match(cells$date, tmax$dates), match(cells$cell_id, tmax$cells$cell_id))]
}

# The days of one compound event. Cells are numbered west to east within
# each latitude row, rows south to north, from zero; cluster -1 marks a
# heatwave cell that DBSCAN left outside every cluster.
event_frames <- function(result, cell_tmax, id) {
  grid <- result$input$grid
  cells <- result$clusters$cluster_cells
  days <- result$events$event_days
  daily <- result$regional$daily
  ellipses <- result$ellipses
  flat <- function(points) I(round(as.vector(t(points)), 3))
  lapply(sort(days$date[days$compound_event_id == id]), function(date) {
    rows <- which(cells$date == date)
    e <- ellipses[ellipses$date == date, ]
    column <- round((cells$lon[rows] - grid$lon[1]) / grid$lon_step)
    row <- round((cells$lat[rows] - grid$lat[1]) / grid$lat_step)
    list(date = format(date),
         heatwave_cells = daily$n_heatwave_cells[match(date, daily$date)],
         cell = I(as.integer(column + length(grid$lon) * row)),
         cluster = I(as.integer(cells$dbscan_label[rows])),
         tmax = I(round(cell_tmax[rows], 1)),
         ellipses = lapply(seq_len(nrow(e)), function(i) {
           axes <- ellipse_axes(e[i, ])
           list(outline = flat(ellipse_outline(e[i, ], n = 73)), major = flat(axes$major), minor = flat(axes$minor),
                centroid = I(round(c(e$centroid_lon_weighted[i], e$centroid_lat_weighted[i]), 3)),
                cells = e$n_member_cells[i], area_km2 = round(e$area_km2[i]),
                major_km = round(e$major_axis_km[i]), minor_km = round(e$minor_axis_km[i]))
         }))
  })
}

# Everything the event map needs except the daily frames: the grid, basemap,
# heat classes, the four types with their counts, and one line per event.
event_overview <- function(result, cell_tmax, title, source, demonstration = FALSE) {
  cfg <- result$config
  grid <- result$input$grid
  events <- result$events$events
  days <- result$events$event_days
  extent <- map_extent(result)
  # The map prints the coordinate of a cell as first + column * step. The step
  # is therefore the mean over the whole axis: the first difference of
  # coordinates stored in single precision is off in the sixth digit, which
  # would show 121.7001 for 121.7 some thirty columns on.
  mean_step <- function(values) (values[length(values)] - values[1]) / (length(values) - 1)
  list(
    title = title, source = source, demonstration = demonstration,
    grid = list(lon = grid$lon[1], lat = grid$lat[1], lon_step = mean_step(grid$lon), lat_step = mean_step(grid$lat),
                columns = length(grid$lon), rows = length(grid$lat)),
    extent = I(extent), basemap = basemap_layers(extent),
    domain_cells = nrow(result$input$cells), threshold_cells = result$regional$threshold_cells,
    breaks = I(pretty(range(cell_tmax), n = 7)),
    types = lapply(1:4, function(type) {
      list(type = type, name = TYPE_NAMES[type], meaning = TYPE_MEANINGS[type], colour = TYPE_COLOURS[type],
           events = sum(events$event_type == type), days = sum(events$n_selected_days[events$event_type == type]))
    }),
    events = lapply(seq_len(nrow(events)), function(i) {
      counts <- days$n_clusters[days$compound_event_id == events$compound_event_id[i]]
      list(id = events$compound_event_id[i], type = events$event_type[i],
           start = format(events$start_date[i]), end = format(events$end_date[i]),
           counts = I(as.integer(counts)), ellipses = events$total_clusters[i],
           reason = event_type_reason(counts))
    }),
    terms = list(
      cell = sprintf(paste("Heatwave cell: a grid cell inside a local heatwave that day, a run of at least %g days",
                           "with at least %g of them at or above the cell's own %gth percentile of daily maximum temperature."),
                     cfg$heatwaves$min_duration_days, cfg$heatwaves$min_exceeding_days, cfg$heatwaves$threshold_percentile),
      day = sprintf(paste("Regionally extensive day: at least %d of the %d cells are heatwave cells,",
                          "the %gth percentile of all analyzed days."),
                    result$regional$threshold_cells, nrow(result$input$cells), cfg$regional_days$percentile),
      # The map prints dbscan_label as it is, so a cell can be looked up in the catalogs.
      cluster = paste("Cluster: neighbouring heatwave cells grouped by DBSCAN. Cells it leaves out stay grey.",
                      "Point at a cell to see its cluster number: it is the number in Catalogs 3 and 4",
                      "(dbscan_label, cluster_number_in_day), which count each day's clusters from 0."),
      ellipse = sprintf(paste("Ellipse: the principal axes of a cluster, each %g standard deviations long from the centre,",
                              "drawn around the temperature-weighted centroid."), cfg$ellipses$scale_factor),
      event = "Compound event: consecutive regionally extensive days. Its type follows the number of clusters on each day."))
}

# Stages 1-5 on prepared temperature, arranged like the result of a run. Used
# for the bundled demonstration, which needs no models, workbooks or figures.
event_stages <- function(tmax, cfg) {
  heatwaves <- detect_heatwaves(tmax, cfg$heatwaves$threshold_percentile,
                                cfg$heatwaves$min_duration_days, cfg$heatwaves$min_exceeding_days)
  regional <- select_regional_days(heatwaves, cfg$regional_days$percentile)
  clusters <- cluster_heatwaves(regional, cfg$clustering$eps_values, cfg$clustering$min_samples_values)
  events <- classify_events(clusters, fit_ellipses(clusters, tmax, cfg$ellipses$scale_factor))
  list(config = cfg, input = list(cells = tmax$cells, grid = tmax$grid, dates = tmax$dates),
       regional = regional[c("daily", "selected_days", "threshold_cells")],
       clusters = clusters[c("clusters", "cluster_cells", "daily_settings")],
       ellipses = events$ellipses, events = events[c("events", "event_days")])
}

# The demonstration shown before any run: one event of each type from the
# bundled North America example, analysed with the study defaults.
showcase_events <- function() c(type_1 = 6L, type_2 = 9L, type_3 = 7L, type_4 = 3L)

build_showcase <- function(ids = showcase_events()) {
  cfg <- read_config(input_required = FALSE)
  tmax <- read_tmax(scorch_bundled_file(), months = cfg$period$warm_season_months)
  result <- event_stages(tmax, cfg)
  cell_tmax <- cluster_cell_tmax(result, tmax)
  overview <- event_overview(result, cell_tmax, "Demonstration",
    "Bundled North America example: ERA5 daily maximum temperature, April\u2013September 1996\u20132025",
    demonstration = TRUE)
  shown <- vapply(overview$events, function(event) event$id %in% ids, logical(1))
  overview$events <- overview$events[shown]
  overview$frames <- stats::setNames(lapply(ids, function(id) event_frames(result, cell_tmax, id)), ids)
  overview
}
