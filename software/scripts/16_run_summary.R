# Run summary: input, settings, stage counts, models, files and warnings.
# Write UTF-8 so coordinate symbols survive changes in the session locale.
write_text_file <- function(lines, path) {
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

run_readme <- function(result, books) {
  cfg <- result$config
  complete <- !is.null(result$events)
  c(readme_heading(result, complete),
    "", "INPUT", readme_input(result, cfg),
    "", "MAIN SETTINGS", readme_settings(result, cfg),
    "", "RESULTS", readme_results(result),
    if (complete) c("", "MODELS", readme_models(result)),
    if (complete) c("", "FIGURES (figures/)", readme_figures(result)),
    "", "FILES", readme_files(books),
    "", "WARNINGS", readme_warnings(result),
    if (!is.null(result$trace)) c("", "TRACED EVENT", ifelse(nzchar(result$trace), paste0("  ", result$trace), "")),
    "", "SETTINGS OF THIS RUN (YAML)",
    paste0("  ", strsplit(yaml::as.yaml(cfg[setdiff(names(cfg), "output")]), "\n")[[1]]))
}

readme_heading <- function(result, complete) {
  started <- format(result$started, "%Y-%m-%d %H:%M:%S")
  if (!complete) {
    return(c(sprintf("scoRch.simulator %s results: INCOMPLETE RUN", result$package_version),
             sprintf("Run started %s and stopped: %s", started, result$incomplete),
             "Only Catalogs 1-3, the cell thresholds and the daily heatwave extent are available.",
             "No ellipses, compound events, models or figures were produced."))
  }
  seconds <- result$elapsed_minutes * 60
  c(sprintf("scoRch.simulator %s results", result$package_version),
    sprintf("Run started %s, finished in %s.", started,
            if (seconds < 120) sprintf("%.0f seconds", seconds) else sprintf("%.1f minutes", seconds / 60)))
}

readme_input <- function(result, cfg) {
  input <- result$input
  c(sprintf("  Source: %s", input$source),
    if (!is.null(input$preprocessing)) sprintf("  Preprocessing: %s", input$preprocessing),
    sprintf("  Area: %d cells on a %g x %g degree grid, %s to %s and %s to %s", nrow(input$cells),
            input$grid$lon_step, input$grid$lat_step,
            format_degrees(min(input$cells$lon), c("E", "W")), format_degrees(max(input$cells$lon), c("E", "W")),
            format_degrees(min(input$cells$lat), c("N", "S")), format_degrees(max(input$cells$lat), c("N", "S"))),
    sprintf("  Period: %d dates from %s to %s, warm-season months %s", length(input$dates),
            format(min(input$dates)), format(max(input$dates)), paste(cfg$period$warm_season_months, collapse = " ")))
}

readme_settings <- function(result, cfg) {
  overrides <- cfg$figures$representative_events
  overrides <- overrides[!vapply(overrides, is.null, logical(1))]
  c(sprintf("  Local heatwave: at least %g consecutive days with at least %g of them at or above the cell's %gth percentile of daily maximum temperature (days below it inside the run are bridged).",
            cfg$heatwaves$min_duration_days, cfg$heatwaves$min_exceeding_days, cfg$heatwaves$threshold_percentile),
    sprintf("  Regionally extensive day: a day whose number of heatwave cells reaches the %gth percentile of all days%s.",
            cfg$regional_days$percentile,
            if (!is.null(result$regional$threshold_cells)) sprintf(" (%d cells in this run)", result$regional$threshold_cells) else ""),
    sprintf("  Clustering: DBSCAN of the heatwave cells of each selected day; eps candidates %s grid cells, min_samples candidates %s; one pair is chosen for each run of consecutive days.",
            paste(cfg$clustering$eps_values, collapse = ", "), paste(cfg$clustering$min_samples_values, collapse = ", ")),
    sprintf("  Ellipses: principal axes of each cluster scaled by %g, centroids weighted by temperature.", cfg$ellipses$scale_factor),
    sprintf("  Power law: %d bootstrap repetitions (seeds %s for daily maxima, %s for event maxima).",
            as.integer(cfg$power_law$bootstrap_repetitions), format(cfg$power_law$seed_daily_maxima, scientific = FALSE),
            format(cfg$power_law$seed_event_maxima, scientific = FALSE)),
    sprintf("  LGCP: covariates %s; %g km raster; %g km window margin.", paste(cfg$lgcp$covariates, collapse = ", "),
            cfg$lgcp$raster_km, cfg$lgcp$window_margin_km),
    if (length(overrides)) {
      sprintf("  Figures: representative events chosen in the configuration: %s.",
              paste(sprintf("%s %s", sub("type_", "Type ", names(overrides)),
                            vapply(overrides, function(x) paste(x, collapse = ", "), "")), collapse = "; "))
    })
}

readme_results <- function(result) {
  lines <- c(sprintf("  Stage 1  Local heatwaves: %d", nrow(result$heatwaves$events)),
             sprintf("  Stage 2  Regionally extensive days: %d of %d days (regional threshold %d heatwave cells)",
                     nrow(result$regional$selected_days), nrow(result$regional$daily), result$regional$threshold_cells),
             sprintf("  Stage 3  Clusters: %d", nrow(result$clusters$clusters)))
  if (is.null(result$events)) return(lines)
  types <- result$events$table_1
  c(lines,
    sprintf("  Stage 4  Ellipses: %d", nrow(result$ellipses)),
    sprintf("  Stage 5  Compound heatwave events: %d", nrow(result$events$events)),
    sprintf("           %s: %d event%s over %d day%s, longest %d day%s, %s ellipse%s per event on average",
            types$compound_heatwave_type, types$number_of_compound_events, plural(types$number_of_compound_events),
            types$selected_event_days, plural(types$selected_event_days), types$longest_event_days,
            plural(types$longest_event_days), fmt(types$mean_ellipses_per_event, 4),
            ifelse(types$mean_ellipses_per_event == 1, "", "s")))
}

plural <- function(n) ifelse(n == 1, "", "s")

# Each number formatted on its own, without the padding of a common width.
fmt <- function(x, digits) vapply(x, function(v) format(v, digits = digits), "")

readme_models <- function(result) {
  pl <- result$power_law$summary
  lines <- c("  Power law of the largest ellipse areas (modeling/Power_Law_Model.xlsx):",
             sprintf("    %s: n = %d, tail n = %s, A_min = %s km2, alpha = %s, bootstrap p = %s (%s repetitions)%s",
                     pl$dataset, pl$n, fmt(pl$n_tail, 7), fmt(pl$xmin_km2, 7), fmt(pl$alpha, 4),
                     fmt(pl$p_value, 3), fmt(pl$reps, 7),
                     ifelse(pl$status == "ok", "", paste0(" [", pl$status, "]"))))
  if (is.null(result$lgcp)) {
    return(c(lines, sprintf("  Log-Gaussian Cox process: not fitted. Reason: %s", result$lgcp_error)))
  }
  lgcp <- result$lgcp
  zones <- lgcp$concentration
  groups <- c(all_centroids = "all centroids", daily_largest = "daily largest", event_largest = "event largest")
  zone_lines <- vapply(unique(zones$zone_area_fraction), function(fraction) {
    z <- zones[zones$zone_area_fraction == fraction, ]
    sprintf("    Centroids inside the top-%d%% intensity zone: %s", round(100 * fraction),
            paste(sprintf("%s %d of %d (%.1f%%)", ifelse(z$group %in% names(groups), groups[z$group], z$group),
                          z$n_in_zone, z$n_centroids_in_group, z$percent_in_zone), collapse = "; "))
  }, "")
  c(lines,
    sprintf("  Log-Gaussian Cox process of the %d ellipse centroids (modeling/Spatial_Risk_Model.xlsx):", nrow(result$ellipses)),
    sprintf("    %s = %s", names(lgcp$parameters), fmt(lgcp$parameters, 6)),
    zone_lines,
    if (length(lgcp$warnings)) sprintf("    Warning: %s", lgcp$warnings) else "    No warnings from the fit.")
}

figure_descriptions <- function() c(
  "ellipse_spatial_patterns" = "maps of all ellipses: centroids with their axes, footprint density, axis ratio, area and orientation",
  "single_day_events" = "snapshots of representative single-day events (Types 1 and 2)",
  "consistent_multiday_events" = "the first two days of representative Type 3 events",
  "mixed_multiday_events" = "six consecutive days of a representative Type 4 event",
  "ellipse_geometry_by_type" = "ellipse geometry by event type",
  "event_geometry_distributions" = "distributions of event geometry by type",
  "annual_event_counts_and_duration" = "annual event counts and mean durations by type",
  "power_law_model" = "the power-law fit of the largest ellipse areas",
  "spatial_risk_model" = "the LGCP intensity with the centroids and the concentration zones")

readme_figures <- function(result) {
  plots <- result$plots
  descriptions <- figure_descriptions()
  sprintf("  %s.png: %s%s", plots$name, ifelse(plots$name %in% names(descriptions), descriptions[plots$name], ""),
          ifelse(plots$status == "generated", "", paste0(" [not drawn: ", plots$reason, "]")))
}

readme_files <- function(books) {
  descriptions <- sheet_descriptions()
  lines <- character()
  for (path in names(books)) {
    lines <- c(lines, sprintf("  %s", path))
    for (sheet in names(books[[path]])) {
      description <- if (sheet %in% names(descriptions)) descriptions[[sheet]] else ""
      lines <- c(lines, sprintf("    %s (%d rows): %s", sheet, nrow(books[[path]][[sheet]]), description))
    }
  }
  c(lines,
    "  figures/: the figures listed above (PNG, 600 dpi; 500 dpi for the two largest)",
    "  Run_Summary.txt: this file",
    "  Numbers are stored with the 15 significant digits of the XLSX format; the display formats are rounding for reading only. Empty cells are missing values, not zeros.")
}

readme_warnings <- function(result) {
  warnings <- character()
  if (!is.null(result$incomplete)) warnings <- c(warnings, sprintf("  The run is incomplete: %s", result$incomplete))
  if (!is.null(result$lgcp_error)) warnings <- c(warnings, sprintf("  The LGCP was not fitted: %s", result$lgcp_error))
  if (!is.null(result$lgcp) && length(result$lgcp$warnings)) {
    warnings <- c(warnings, sprintf("  LGCP fit: %s", result$lgcp$warnings))
  }
  if (!is.null(result$power_law)) {
    pl <- result$power_law$summary
    bad <- pl$status != "ok"
    if (any(bad)) warnings <- c(warnings, sprintf("  Power law (%s): %s", pl$dataset[bad], pl$status[bad]))
  }
  if (!is.null(result$plots)) {
    skipped <- result$plots[result$plots$status != "generated", ]
    if (nrow(skipped)) warnings <- c(warnings, sprintf("  Figure %s.png was not drawn: %s", skipped$name, skipped$reason))
  }
  if (length(warnings)) warnings else "  None."
}


trace_event <- function(result, tmax, date) {
  date <- as.Date(date)
  events <- result$events$events
  event <- events[events$start_date <= date & events$end_date >= date, ]
  if (nrow(event) != 1) stop("No compound event contains ", format(date))
  old_width <- options(width = 110)                 # the tables below fit on one line
  on.exit(options(old_width), add = TRUE)
  lines <- character()
  say <- function(...) lines <<- c(lines, sprintf(...))
  show <- function(df) lines <<- c(lines, paste0("  ", utils::capture.output(print(df, row.names = FALSE))))
  n_cells <- nrow(result$input$cells)
  say("Compound event %d (Catalog 5): %s to %s, %d days, %s = Type %d, clusters per day %s",
      event$compound_event_id, format(event$start_date), format(event$end_date), event$duration_days,
      event$event_type_name, event$event_type, event$daily_cluster_counts)
  # Stage 2: the days were selected because their heatwave cells reached the regional threshold.
  days <- result$events$event_days[result$events$event_days$compound_event_id == event$compound_event_id, ]
  daily <- result$regional$daily[match(days$date, result$regional$daily$date), ]
  say("")
  say("Stage 2 (Catalog 2): every day of the event had at least %d of the %d cells in a local heatwave,",
      result$regional$threshold_cells, n_cells)
  say("the regional threshold (the 97.5th percentile of the daily heatwave-cell counts):")
  show(data.frame(date = format(days$date), heatwave_cells = daily$n_heatwave_cells,
                  exceeding_cells = daily$n_exceeding_cells, clusters = days$n_clusters))
  # Stage 3: DBSCAN clusters of the heatwave cells on those days.
  clusters <- result$clusters$clusters[result$clusters$clusters$date %in% days$date, ]
  say("")
  say("Stage 3 (Catalog 3): DBSCAN grouped the heatwave cells of each day into these clusters")
  say("(eps in grid cells and min_samples cells are the settings chosen for the run of days):")
  show(data.frame(date = format(clusters$date), cluster_id = clusters$cluster_id,
                  member_cells = clusters$n_member_cells, core_cells = clusters$n_core_cells,
                  eps = round(clusters$eps_applied, 3), min_samples = clusters$min_samples_applied,
                  noise_cells = clusters$n_noise_cells_on_day))
  # Stage 4: the largest ellipse of the event.
  e <- result$ellipses[result$ellipses$ellipse_id == event$largest_ellipse_id, ]
  say("")
  say("Stage 4 (Catalog 4): the largest ellipse of the event is ellipse %d, fitted to cluster %d on %s:",
      e$ellipse_id, e$cluster_id, format(e$date))
  direction <- if (abs(e$azimuth_north_cw_deg) > 67.5) "nearly east-west" else if (abs(e$azimuth_north_cw_deg) < 22.5) "nearly north-south" else if (e$azimuth_north_cw_deg > 0) "northeast-southwest" else "northwest-southeast"
  say("  %d member cells; axes %.0f km x %.0f km; area %.0f km2; major axis %.0f degrees from north (%s);",
      e$n_member_cells, e$major_axis_km, e$minor_axis_km, e$area_km2, e$azimuth_north_cw_deg, direction)
  say("  Tmax-weighted centroid %.2f N, %.2f W; member Tmax %.1f to %.1f C.",
      e$centroid_lat_weighted, -e$centroid_lon_weighted, e$tmax_min_c, e$tmax_max_c)
  # Stage 1 and the data: the hottest member cell of that ellipse.
  members <- result$clusters$cluster_cells[which(result$clusters$cluster_cells$cluster_id == e$cluster_id), ]
  column <- match(members$cell_id, tmax$cells$cell_id)
  row <- match(e$date, tmax$dates)
  hottest <- members[which.max(tmax$tmax[row, column]), ]
  threshold <- result$heatwaves$thresholds[result$heatwaves$thresholds$cell_id == hottest$cell_id, ]
  heatwave <- result$heatwaves$events[result$heatwaves$events$local_event_id == hottest$local_event_id, ]
  say("")
  say("Stage 1 (Catalog 1): the hottest of those cells is cell %d (%.1f N, %.1f W). Its threshold is %.2f C,",
      hottest$cell_id, hottest$lat, -hottest$lon, threshold$threshold_tmax_c)
  say("the 95th percentile of its %d daily maxima. Local heatwave %d of that cell ran from %s to %s:",
      length(tmax$dates), heatwave$local_event_id, format(heatwave$start_date), format(heatwave$end_date))
  say("%d days, %d of them at or above the threshold, peak %.2f C on %s.",
      heatwave$duration_days, heatwave$exceeding_days, heatwave$peak_tmax_c, format(heatwave$peak_date))
  say("")
  say("The temperature data of that cell around the heatwave (daily maximum 2-m temperature, degrees C):")
  window <- seq(heatwave$start_date - 2, heatwave$end_date + 2, by = "day")
  window <- window[window %in% tmax$dates]
  series <- tmax$tmax[match(window, tmax$dates), match(hottest$cell_id, tmax$cells$cell_id)]
  show(data.frame(date = format(window), tmax_c = sprintf("%.2f", series),
                  exceeds_threshold = ifelse(series >= threshold$threshold_tmax_c, "yes", "no"),
                  local_heatwave = ifelse(window >= heatwave$start_date & window <= heatwave$end_date, "yes", "no"),
                  selected_day = ifelse(window %in% result$regional$selected_days$date, "yes", "no"),
                  in_event = ifelse(window >= event$start_date & window <= event$end_date, "yes", "no")))
  figure <- c("single_day_events", "single_day_events", "consistent_multiday_events", "mixed_multiday_events")[event$event_type]
  say("")
  say("Figure for this event type: %s/figures/%s.png (selected under figures: representative_events).", result$output_dir, figure)
  say("Orange cells are the clustered heatwave cells, grey cells the")
  say("heatwave cells DBSCAN left out, the black outline is the ellipse and the dot its weighted centroid.")
  lines
}
