test_that("the plain-language types say what classify_event() does", {
  classify <- scoRch.simulator:::classify_event
  reason <- scoRch.simulator:::event_type_reason
  expect_length(scoRch.simulator:::TYPE_MEANINGS, 4L)
  counts <- c(lapply(1:3, c), unlist(lapply(2:4, function(n) {
    grid <- as.matrix(expand.grid(rep(list(0:3), n)))
    lapply(which(rowSums(grid) > 0), function(i) unname(grid[i, ]))
  }), recursive = FALSE))
  expect_gt(length(counts), 300L)
  for (days in counts) {
    type <- classify(days)
    text <- reason(days)
    label <- paste(days, collapse = "-")
    expect_identical(grepl("^One regionally extensive day", text), length(days) == 1L, info = label)
    expect_identical(grepl("with one cluster$", text), type == 1L, info = label)
    expect_identical(grepl("separate clusters$", text), type == 2L, info = label)
    expect_identical(grepl("on every day$", text), type == 3L, info = label)
    expect_identical(grepl("a mix of|without a cluster", text), type == 4L, info = label)
    expect_identical(grepl("without a cluster", text), length(days) > 1L && any(days == 0), info = label)
    if (length(days) > 1L) expect_match(text, sprintf("^%d consecutive", length(days)), info = label)
  }
  expect_identical(reason(c(1, 1, 1)), "3 consecutive regionally extensive days with one cluster on every day")
  expect_identical(reason(c(2, 0, 3)), "3 consecutive regionally extensive days with several-cluster days and 1 day without a cluster")
  expect_identical(reason(4), "One regionally extensive day with 4 separate clusters")
})

test_that("the event map shows the cells, clusters and ellipses of the run", {
  window <- example_window()
  tmax <- tmax_from_array(window$celsius, window$dates, window$lon, window$lat, months = 6:7)
  result <- scoRch.simulator:::event_stages(tmax, scoRch.simulator:::read_config(input_required = FALSE))
  cell_tmax <- scoRch.simulator:::cluster_cell_tmax(result, tmax)
  cells <- result$clusters$cluster_cells
  expect_length(cell_tmax, nrow(cells))
  expect_equal(cell_tmax, tmax$tmax[cbind(match(cells$date, tmax$dates), cells$cell_id)])

  overview <- scoRch.simulator:::event_overview(result, cell_tmax, "Events", "window")
  events <- result$events$events
  expect_false(overview$demonstration)
  expect_equal(vapply(overview$types, `[[`, 1, "events"), tabulate(events$event_type, 4))
  expect_identical(vapply(overview$types, `[[`, "", "name"), scoRch.simulator:::TYPE_NAMES)
  expect_equal(vapply(overview$events, `[[`, 1, "id"), events$compound_event_id)
  expect_identical(vapply(overview$events, function(e) paste(e$counts, collapse = "-"), ""), events$daily_cluster_counts)
  # The event panel shows the catalog's own values, the axial mean of the azimuths among them.
  for (column in scoRch.simulator:::EVENT_PANEL_COLUMNS) {
    shown <- vapply(overview$events, function(e) as.numeric(e$stats[[column]]), 1)
    expect_identical(shown, ifelse(is.finite(events[[column]]), as.numeric(events[[column]]), NA_real_), info = column)
  }
  expect_equal(unclass(overview$extent), c(min(window$lon) - 0.5, max(window$lon) + 0.5, min(window$lat) - 0.5, max(window$lat) + 0.5))
  # The map shows a margin of context around the region, more above and below; the region is unchanged.
  view <- unclass(overview$view)
  e <- unclass(overview$extent)
  expect_true(view[1] < e[1] && view[2] > e[2] && view[3] < e[3] && view[4] > e[4])
  expect_gt((e[3] - view[3]) / (e[4] - e[3]), (e[1] - view[1]) / (e[2] - e[1]))
  # A grid with exact coordinates is sent the step the reader found, and where it starts.
  expect_identical(overview$grid, list(lon = min(window$lon), lat = min(window$lat), lon_step = tmax$grid$lon_step,
                                       lat_step = tmax$grid$lat_step, columns = 12L, rows = 10L))
  expect_true(all(diff(unclass(overview$breaks)) > 0))
  expect_true(min(overview$breaks) <= min(cell_tmax) && max(overview$breaks) >= max(cell_tmax))
  # A wider range keeps its classes, here ten of one degree. The map script blends the heat ramp
  # so that each has its own colour; the classes are not coarsened to fit eight colours.
  wide <- 35.4 + (cell_tmax - min(cell_tmax)) / diff(range(cell_tmax)) * 8.8
  expect_equal(unclass(scoRch.simulator:::event_overview(result, wide, "Events", "window")$breaks), 35:45)
  expect_match(overview$terms$cell, "95th percentile")
  expect_match(overview$terms$day, sprintf("at least %d of the 120 cells", result$regional$threshold_cells))
  # The map prints the cluster number it is sent, and the text beside it says which number that is.
  expect_match(overview$terms$cluster, "Catalogs 3 and 4 (dbscan_label, cluster_number_in_day)", fixed = TRUE)
  expect_match(overview$terms$cluster, "from 0", fixed = TRUE)

  days <- result$events$event_days
  catalog <- result$clusters$clusters
  expect_identical(catalog$cluster_number_in_day, catalog$dbscan_label)
  for (id in events$compound_event_id) {
    frames <- scoRch.simulator:::event_frames(result, cell_tmax, id)
    dates <- sort(days$date[days$compound_event_id == id])
    expect_identical(vapply(frames, `[[`, "", "date"), format(dates))
    for (k in seq_along(frames)) {
      frame <- frames[[k]]
      here <- cells[cells$date == dates[k], ]
      # A cell number gives back the longitude and latitude of its grid cell.
      column <- unclass(frame$cell) %% length(window$lon)
      row <- unclass(frame$cell) %/% length(window$lon)
      expect_equal(window$lon[column + 1L], here$lon)
      expect_equal(window$lat[row + 1L], here$lat)
      expect_identical(unclass(frame$cluster), as.integer(here$dbscan_label))
      expect_equal(unclass(frame$tmax), round(cell_tmax[cells$date == dates[k]], 1))
      expect_equal(frame$heatwave_cells, nrow(here))
      ellipses <- result$ellipses[result$ellipses$date == dates[k], ]
      expect_length(frame$ellipses, days$n_clusters[days$date == dates[k]])
      # A cell's cluster number on the map is its number in Catalog 3 (clusters) and Catalog 4 (ellipses):
      # -1 outside every cluster, otherwise counted from 0 on each day.
      numbers <- sort(setdiff(unique(unclass(frame$cluster)), -1L))
      expect_identical(numbers, sort(catalog$cluster_number_in_day[catalog$date == dates[k]]))
      expect_identical(numbers, sort(ellipses$cluster_number_in_day))
      if (length(numbers)) expect_identical(numbers, seq_along(numbers) - 1L)
      for (i in seq_along(frame$ellipses)) {
        drawn <- frame$ellipses[[i]]
        expect_equal(sum(unclass(frame$cluster) == ellipses$cluster_number_in_day[i]), drawn$cells)
        outline <- matrix(unclass(drawn$outline), ncol = 2, byrow = TRUE)
        expect_equal(outline[1, ], outline[nrow(outline), ], tolerance = 1e-3)
        expect_equal(unclass(drawn$centroid), c(ellipses$centroid_lon_weighted[i], ellipses$centroid_lat_weighted[i]),
                     tolerance = 1e-3)
        expect_equal(colMeans(outline[-1, ]), unclass(drawn$centroid), tolerance = 1e-2)
        expect_equal(drawn$area_km2, round(ellipses$area_km2[i]))
        expect_equal(drawn$cells, ellipses$n_member_cells[i])
        # The window calls it "Heatwave <number>", counted from 1 in catalog order on this day, and
        # shows the ellipse's own values in Catalog 4 unchanged.
        expect_identical(drawn$number, i)
        expect_identical(drawn$label, ellipses$cluster_number_in_day[i])
        expect_identical(drawn$stats$axis_ratio, ellipses$axis_ratio[i])
        expect_identical(drawn$stats$area_km2, ellipses$area_km2[i])
        expect_identical(drawn$stats$azimuth_north_cw_deg, ellipses$azimuth_north_cw_deg[i])
      }
    }
  }
})

test_that("the map names the cells of a grid whose coordinates are stored in single precision", {
  # A 0.1-degree grid as many files hold it. The first difference of its longitudes is 0.09999847,
  # and first + column * that step would print 121.7001 for 121.7 some thirty columns on.
  single <- function(x) readBin(writeBin(x, raw(), size = 4L), "numeric", size = 4L, n = length(x))
  full <- read_tmax(system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator"),
                    months = 6:7, start = "2012-06-01", end = "2012-07-31")
  columns <- length(full$grid$lon)
  rows <- length(full$grid$lat)
  lon <- (-1250 + seq_len(columns) - 1) / 10
  lat <- (300 + seq_len(rows) - 1) / 10
  # The example's cells run west to east within each latitude row.
  celsius <- aperm(array(full$tmax, c(length(full$dates), columns, rows)), c(1, 3, 2))
  tmax <- tmax_from_array(celsius, full$dates, single(lon), single(lat), months = 6:7)
  result <- scoRch.simulator:::event_stages(tmax, scoRch.simulator:::read_config(input_required = FALSE))
  expect_gt(nrow(result$events$events), 0L)
  grid <- scoRch.simulator:::event_overview(result, scoRch.simulator:::cluster_cell_tmax(result, tmax),
                                            "Events", "single precision")$grid
  expect_identical(c(grid$columns, grid$rows), c(columns, rows))
  # The map script prints first + column * step to four decimals.
  printed <- function(first, step, n) round((first + (seq_len(n) - 1) * step) * 1e4) / 1e4
  expect_identical(printed(grid$lon, grid$lon_step, columns), lon)
  expect_identical(printed(grid$lat, grid$lat_step, rows), lat)
  # The step the reader keeps, the first difference, would misname more than half of the columns.
  expect_gt(sum(printed(grid$lon, tmax$grid$lon_step, columns) != lon), columns / 2)
})

test_that("the bundled demonstration is what the package computes from the example", {
  skip_if_not_installed("jsonlite")
  parse <- function(text) jsonlite::fromJSON(text, simplifyVector = FALSE)
  shipped <- parse(paste(readLines(system.file("launcher", "showcase.json", package = "scoRch.simulator"),
                                   warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  fresh <- parse(as.character(jsonlite::toJSON(scoRch.simulator:::build_showcase(), auto_unbox = TRUE, digits = NA,
                                               null = "null")))
  expect_true(shipped$demonstration)
  expect_match(shipped$source, "Bundled North America example")
  expect_identical(sort(vapply(shipped$events, `[[`, 1L, "type")), 1:4)
  expect_identical(vapply(shipped$types, `[[`, 1L, "events"), c(1L, 5L, 20L, 4L))
  # The outlines of land come from the maps package and may change with it.
  expect_gt(length(shipped$basemap$land), 0L)
  shipped$basemap <- fresh$basemap <- NULL
  # Ellipse geometry is compared to the rounding of the file, everything else exactly.
  ellipses <- function(x, fields) unlist(lapply(x$frames, function(days) lapply(days, function(day) {
    lapply(day$ellipses, function(e) unlist(e[fields]))
  })))
  expect_lt(max(abs(ellipses(shipped, c("outline", "major", "minor", "centroid")) -
                      ellipses(fresh, c("outline", "major", "minor", "centroid")))), 0.01)
  expect_lte(max(abs(ellipses(shipped, c("area_km2", "major_km", "minor_km")) -
                       ellipses(fresh, c("area_km2", "major_km", "minor_km")))), 1)
  counted <- function(x) {
    x$frames <- lapply(x$frames, function(days) lapply(days, function(day) {
      day$ellipses <- lapply(day$ellipses, `[[`, "cells")
      day
    }))
    x
  }
  expect_identical(counted(shipped), counted(fresh))
})

test_that("the base map follows the grid and survives regions without land", {
  layers <- scoRch.simulator:::basemap_layers(c(-130, -58, 25, 50))
  expect_named(layers, c("land", "lakes", "states"))
  expect_gt(length(layers$land), 10L)
  expect_gt(length(layers$states), 10L)
  expect_true(all(lengths(layers$land) %% 2L == 0L))
  expect_length(scoRch.simulator:::basemap_layers(c(19.5, 70.5, 9.5, 45.5))$states, 0L)
  ocean <- scoRch.simulator:::basemap_layers(c(-40, -30, 20, 30))
  expect_identical(lengths(ocean), c(land = 0L, lakes = 0L, states = 0L))
  pacific <- scoRch.simulator:::basemap_layers(c(120, 290, -10, 60))
  expect_gt(length(pacific$land), 10L)
  expect_length(pacific$lakes, 0L)
})
