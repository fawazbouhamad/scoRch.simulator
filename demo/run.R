# One launcher for prepared daily Tmax or local hourly ERA5.
# From the repository root:
# Rscript demo/run.R [config.yaml] [input_file_or_folder] [output_folder]
# In R: source("demo/run.R"); result <- run_demo()

run_demo <- function(config_file = "demo/config/prepared.yaml", input = NULL, output = NULL) {
  if (!file.exists(config_file)) stop("The demo configuration does not exist: ", config_file, call. = FALSE)
  demo_config <- yaml::read_yaml(config_file)
  if (!is.null(input)) demo_config$input$file <- input
  if (!is.null(output)) demo_config$output$directory <- output
  hourly <- identical(demo_config$input$format, "era5")
  if (hourly) {
    demo_config$input$processed_file <- file.path(demo_config$output$directory, "prepared_tmax.nc")
  }

  # The package performs input preparation, all five stages, both models and
  # every export. The launcher does not implement a second analysis pipeline.
  result <- scoRch.simulator::run_scorch(demo_config)
  if (!is.null(result$incomplete)) return(invisible(result))
  prepared <- if (hourly) demo_config$input$processed_file else demo_config$input$file
  tmax <- scoRch.simulator::read_tmax(prepared,
      months = result$config$period$warm_season_months,
      start = result$config$period$start_date, end = result$config$period$end_date)
  print(tmax)
  cat("\nCompound heatwave events by type:\n")
  print(result$events$table_1, row.names = FALSE)
  cat("\nPower-law fits:\n")
  print(result$power_law$summary[, c("dataset", "n", "n_tail", "xmin_km2", "alpha", "p_value")], row.names = FALSE)

  # Follow an observed event within the complete 30-year dataset. These dates
  # are scientific observations, separate from any preprocessing demonstration.
  trace_date <- as.Date("2012-06-30")
  contains <- result$events$events$start_date <= trace_date & result$events$events$end_date >= trace_date
  if (sum(contains) == 1L && trace_date %in% tmax$dates) {
    trace <- trace_event(result, tmax, trace_date, config_file)
    cat("\n", paste(trace, collapse = "\n"), "\n", sep = "")
    readme_file <- file.path(result$output_dir, "README.txt")
    readme <- readLines(readme_file, encoding = "UTF-8")
    settings_at <- grep("^SETTINGS OF THIS RUN", readme)[1]
    indented <- ifelse(nzchar(trace), paste0("  ", trace), "")
    writeLines(enc2utf8(c(readme[seq_len(settings_at - 1)], "TRACED EVENT (demo/run.R)", indented, "",
                          readme[settings_at:length(readme)])), readme_file, useBytes = TRUE)
  }
  cat("\nFiles written:\n")
  cat(paste0("  ", result$output_dir, "/", list.files(result$output_dir, recursive = TRUE)), sep = "\n")
  if (hourly) cat("\nReusable daily Tmax: ", prepared, "\n", sep = "")
  invisible(result)
}

trace_event <- function(result, tmax, date, config_file) {
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
  say("Figure: %s/figures/%s.png shows this event (named in %s", result$output_dir, figure, config_file)
  say("under figures: representative_events): orange cells are the clustered heatwave cells, grey cells the")
  say("heatwave cells DBSCAN left out, the black outline is the ellipse and the dot its weighted centroid.")
  lines
}


if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 3L) stop("Use: Rscript demo/run.R [config.yaml] [input_file_or_folder] [output_folder]", call. = FALSE)
  config <- if (length(args) >= 1L) args[1] else "demo/config/prepared.yaml"
  input <- if (length(args) >= 2L) args[2] else NULL
  output <- if (length(args) >= 3L) args[3] else NULL
  result <- run_demo(config, input, output)
}
