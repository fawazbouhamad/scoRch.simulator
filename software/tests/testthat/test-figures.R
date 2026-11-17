test_that("figure headings and labels draw on a base PDF device", {
  grDevices::pdf(tempfile(fileext = ".pdf"))
  tryCatch(expect_warning({
    graphics::par(family = scoRch.simulator:::figure_font("single_day_events"))
    scoRch.simulator:::type_heading(1)
    scoRch.simulator:::row_heading("Area")
    graphics::plot.new()
    scoRch.simulator:::plot_heading("Ellipse area")
  }, NA), finally = grDevices::dev.off())
})

test_that("state outlines are drawn only on maps over the United States", {
  overlaps <- scoRch.simulator:::overlaps_us_states
  expect_true(overlaps(c(-104, -94, 34, 44)))       # central Great Plains
  expect_true(overlaps(c(-70, -60, 40, 50)))        # touches the north-east corner
  expect_false(overlaps(c(20, 70, 10, 46)))         # the SCORCH study region
  expect_false(overlaps(c(-60, -30, -40, -10)))     # South America
  expect_false(overlaps(c(-104, -94, 50, 60)))      # north of the database's extent
})

test_that("event snapshot figures size their canvas by the panels they draw", {
  specs <- scoRch.simulator:::figure_specs()
  # A tiny result with one Type 3 event of two days and nothing else.
  dates <- as.Date(c("2000-07-01", "2000-07-02"))
  result <- list(
    input = list(grid = list(lon = 1:4, lat = 1:2, lon_step = 1, lat_step = 1)),
    config = list(figures = list(representative_events = list())),
    events = list(events = data.frame(compound_event_id = 1L, start_date = dates[1], end_date = dates[2],
                                      duration_days = 2L, event_type = 3L, largest_ellipse_area_km2 = 1),
                  event_days = data.frame(date = dates, compound_event_id = 1L, n_clusters = 1L)))
  ratio <- scoRch.simulator:::extent_ratio(result)
  expect_equal(specs$consistent_multiday_events$size(result)[2], 0.9 + 0.5 + 1 * (4.6 * ratio + 0.3 + 0.45))
  expect_equal(specs$single_day_events$size(result)[1], 1 * (5.3 + 0.45 + 0.1))   # no Type 1 or 2: one column
  expect_equal(specs$mixed_multiday_events$size(result)[2], 0.9 + 0.5 + 1 * (4.4 * ratio + 0.3 + 0.45))
  two_rows <- result
  two_rows$events$events <- rbind(two_rows$events$events,
                                  data.frame(compound_event_id = 2L, start_date = dates[1] + 10, end_date = dates[2] + 10,
                                             duration_days = 2L, event_type = 3L, largest_ellipse_area_km2 = 1))
  two_rows$events$event_days <- rbind(two_rows$events$event_days,
                                      data.frame(date = dates + 10, compound_event_id = 2L, n_clusters = 1L))
  expect_equal(specs$consistent_multiday_events$size(two_rows)[2], 0.9 + 0.5 + 2 * (4.6 * ratio + 0.3 + 0.45))
  expect_equal(specs$consistent_multiday_events$dpi, 600)
  expect_equal(specs$ellipse_spatial_patterns$dpi, 500)
})


test_that("every event has one global filename and every day in one canvas", {
  dates <- as.Date("2000-07-01") + 0:27
  result <- list(input = list(grid = list(lon = 1:4, lat = 1:2, lon_step = 1, lat_step = 1)),
    events = list(events = data.frame(compound_event_id = c(1L, 2L, 3L), event_type = c(1L, 3L, 4L)),
      event_days = data.frame(compound_event_id = c(1L, rep(2L, 28), rep(3L, 2)), date = c(dates[1] - 10, dates, dates[1:2] + 40))),
    figure_event_id = 2L)
  plan <- scoRch.simulator:::figure_output_plan(result)
  expect_equal(plan$path[1:3], c("events/Type_1/Event_001.png", "events/Type_3/Event_002.png", "events/Type_4/Event_003.png"))
  expect_equal(plan$event_id[1:3], 1:3)
  expect_equal(nrow(plan), 14L)
  expect_equal(scoRch.simulator:::event_figure_dates(result), dates)
  grid <- scoRch.simulator:::event_figure_grid(result)
  expect_gte(grid$rows * grid$cols, 28L)
  expect_gt(scoRch.simulator:::event_figure_size(result)[2], 10)
  expect_equal(scoRch.simulator:::event_figure_size(result)[2],
    grid$rows * (grid$width * scoRch.simulator:::extent_ratio(result) + 0.3 + 0.1) + 0.5)
  drawn <- as.Date(character())
  headings <- character()
  testthat::local_mocked_bindings(draw_snapshot = function(result, date, time_label = NULL, mai) {
    expect_null(time_label)
    drawn <<- c(drawn, date)
    graphics::plot.new()
  }, snapshot_legend = function() graphics::plot.new(), .package = "scoRch.simulator")
  original_text <- graphics::text.default
  testthat::local_mocked_bindings(text.default = function(x, y = NULL, labels = seq_along(x), ...) {
    headings <<- c(headings, as.character(labels))
    original_text(x, y, labels, ...)
  }, .package = "graphics")
  grDevices::pdf(tempfile(fileext = ".pdf"), width = 16, height = 20)
  graphics::par(mai = rep(0, 4))
  tryCatch(scoRch.simulator:::draw_event(result), finally = grDevices::dev.off())
  expect_identical(drawn, dates)
  expect_length(headings, 0L)
  expect_false(any(grepl("^\\(?[a-eA-E]\\)?$", headings)))
  result$figure_event_id <- 99L
  expect_error(scoRch.simulator:::event_figure_dates(result), "event_id")
})

test_that("publication retires only recognized generated event images", {
  out <- tempfile("scorch-publish-")
  stage <- scoRch.simulator:::prepare_run_output(out)
  dir.create(file.path(out, "figures/events/Type_3"), recursive = TRUE)
  writeLines("old", file.path(out, "figures/events/Type_3/Event_027.png"))
  writeLines("personal", file.path(out, "figures/events/Type_3/My_event.png"))
  writeLines("new", file.path(stage, "figures/events/Type_3/Event_001.png"))
  scoRch.simulator:::publish_run_output(stage, out)
  expect_false(file.exists(file.path(out, "figures/events/Type_3/Event_027.png")))
  expect_equal(readLines(file.path(out, "figures/events/Type_3/Event_001.png")), "new")
  expect_equal(readLines(file.path(out, "figures/events/Type_3/My_event.png")), "personal")
  expect_true(all(dir.exists(file.path(out, paste0("figures/events/Type_", 1:4)))))
})


test_that("publication restores previous output if moving a new event image fails", {
  out <- tempfile("scorch-rollback-")
  stage <- scoRch.simulator:::prepare_run_output(out)
  dir.create(file.path(out, "figures/events/Type_3"), recursive = TRUE)
  writeLines("previous", file.path(out, "figures/events/Type_3/Event_001.png"))
  writeLines("next", file.path(stage, "figures/events/Type_3/Event_001.png"))
  writeLines("next", file.path(stage, "figures/events/Type_3/Event_002.png"))
  rename <- base::file.rename
  testthat::local_mocked_bindings(file.rename = function(from, to) {
    if (identical(from, file.path(stage, "figures/events/Type_3/Event_002.png"))) return(FALSE)
    rename(from, to)
  }, .package = "base")
  expect_error(scoRch.simulator:::publish_run_output(stage, out), "Could not publish output")
  expect_equal(readLines(file.path(out, "figures/events/Type_3/Event_001.png")), "previous")
  expect_false(file.exists(file.path(out, "figures/events/Type_3/Event_002.png")))
})
