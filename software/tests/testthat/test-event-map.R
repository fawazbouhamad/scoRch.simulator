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
  expect_equal(unclass(overview$extent), c(min(window$lon) - 0.5, max(window$lon) + 0.5, min(window$lat) - 0.5, max(window$lat) + 0.5))
  expect_true(all(diff(unclass(overview$breaks)) > 0))
  expect_true(min(overview$breaks) <= min(cell_tmax) && max(overview$breaks) >= max(cell_tmax))
  expect_match(overview$terms$cell, "95th percentile")
  expect_match(overview$terms$day, sprintf("at least %d of the 120 cells", result$regional$threshold_cells))

  days <- result$events$event_days
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
      for (i in seq_along(frame$ellipses)) {
        drawn <- frame$ellipses[[i]]
        outline <- matrix(unclass(drawn$outline), ncol = 2, byrow = TRUE)
        expect_equal(outline[1, ], outline[nrow(outline), ], tolerance = 1e-3)
        expect_equal(unclass(drawn$centroid), c(ellipses$centroid_lon_weighted[i], ellipses$centroid_lat_weighted[i]),
                     tolerance = 1e-3)
        expect_equal(colMeans(outline[-1, ]), unclass(drawn$centroid), tolerance = 1e-2)
        expect_equal(drawn$area_km2, round(ellipses$area_km2[i]))
        expect_equal(drawn$cells, ellipses$n_member_cells[i])
      }
    }
  }
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
