# The complete workflow: read, analyse, model, write and draw.

#' Run the complete SCORCH workflow
#'
#' Reads the temperature input named in the configuration (raw ERA5 2-m
#' temperature, reduced to daily maxima as described in [read_era5()], or
#' daily maximum temperature, see [read_tmax()]), runs the five
#' catalog stages, fits the power-law and LGCP models, and writes the
#' results to the output folder as XLSX workbooks, PNG figures and one
#' `Run_Summary.txt`: `catalogs/` (`01_heatwave_events.xlsx`,
#' `02_regionally_extensive_heatwaves.xlsx`, `03_extreme_heatwave_clusters.xlsx`,
#' `04_extreme_heatwave_ellipses.xlsx` and `05_compound_heatwave_events.xlsx`: Catalogs 1-5 with
#' their day and cell tables), `modeling/` (`Power_Law_Model.xlsx` and
#' `Spatial_Risk_Model.xlsx`: fits, parameters, intensity and centroid concentration),
#' `Heatwave_Statistics.xlsx` (event type summary, cell
#' thresholds, daily heatwave extent, duration trends), `figures/` (nine
#' descriptively named PNG files) and `Run_Summary.txt`, which describes the
#' input, the main settings, the counts, the models, every file and sheet,
#' the run time and the warnings in plain language. Every sheet holds all
#' rows and columns of its table as date and numeric cells with a frozen,
#' filtered header and empty cells for missing values; a table longer than
#' an XLSX sheet or a file that cannot be written stops the run with an
#' error. Package-generated files are replaced after the new run is written;
#' other files in those folders are preserved.
#' A valid run can have an unavailable LGCP fit or a plot whose event type is
#' absent. Their status and reasons are written to `Run_Summary.txt`; an
#' unexpected plot error stops the run without publishing its partial output.
#' When no selected day forms a cluster, only Catalogs 1-3, the cell
#' thresholds and the daily extent are published, and `Run_Summary.txt` marks the
#' run incomplete.
#'
#' The package ships with the configuration of the SCORCH study, and its
#' settings are the defaults. A run can use them as they are or change
#' individual settings: any setting that `config` leaves out keeps its study
#' value.
#'
#' @param config Path of a YAML configuration file (an edited copy from
#'   [write_scorch_config()] or [scorch_template()]), or a list with the same
#'   structure. With no arguments, reads `config.yaml` in the working folder
#'   and creates it from the installed configuration if absent. A YAML file
#'   with a null input runs the bundled US example. Explicit `NULL` uses the
#'   study defaults and requires temperature supplied as `tmax`.
#' @param tmax Optional `scorch_tmax` object (from [read_tmax()] or
#'   [tmax_from_array()]) to analyse instead of reading the file named in the
#'   configuration. The configured warm-season months and date limits also
#'   apply to this object.
#' @param trace_date Optional date to explain in the run summary. A trace is
#'   included if the date belongs to one of the compound events in this run.
#' @return A `scorch_result` list (invisibly) with the configuration, the
#'   input description and the tables of every stage: `heatwaves`, `regional`,
#'   `clusters`, `ellipses`, `events`, `power_law` and `lgcp`. Use it with
#'   [scorch_figure()] to redraw a result plot.
#' @examples
#' \donttest{
#' old <- setwd(tempdir())                              # results go to tempdir()/output
#' scorch_template("config.yaml", fast = TRUE)   # fast demonstration for the example data
#' result <- run_scorch("config.yaml")
#' result$events$table_1
#' setwd(old)
#' }
#' @export
run_scorch <- function(config = NULL, tmax = NULL, trace_date = NULL) {
  if (!is.null(trace_date)) {
    trace_date <- tryCatch(as.Date(trace_date), error = function(e) as.Date(NA))
    if (length(trace_date) != 1L || is.na(trace_date)) fail("trace_date must be one calendar date, e.g. '2012-06-30'.")
  }
  started <- Sys.time()
  if (missing(config) && is.null(tmax)) {
    if (!file.exists("config.yaml")) write_scorch_config("config.yaml")
    config <- "config.yaml"
  }
  if (is.character(config) && length(config) == 1L && is.null(tmax)) {
    config <- demo_settings(config)
    example <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
    if (is.null(trace_date) && identical(config$input$file, example)) trace_date <- as.Date("2012-06-30")
  }
  cfg <- read_config(config, input_required = is.null(tmax))
  out <- cfg$output$directory
  published <- FALSE
  on.exit(if (!published && dir.exists(out)) {
    message("This run did not publish output; files in ", out, " belong to an earlier run.")
  }, add = TRUE)
  step <- function(...) message(format(Sys.time(), "%H:%M:%S "), sprintf(...))

  if (is.null(tmax)) {
    step("Reading %s", paste(cfg$input$file, collapse = ", "))
    tmax <- read_configured_input(cfg)
  } else if (!inherits(tmax, "scorch_tmax")) {
    fail("`tmax` must come from read_tmax() or tmax_from_array().")
  } else {
    tmax <- subset_tmax_period(tmax, months = cfg$period$warm_season_months,
                               start = cfg$period$start_date, end = cfg$period$end_date)
  }
  step("%d cells, %d dates from %s to %s", nrow(tmax$cells), length(tmax$dates),
       format(tmax$dates[1]), format(tmax$dates[length(tmax$dates)]))
  input <- c(list(source = tmax$source, cells = tmax$cells, grid = tmax$grid, dates = tmax$dates),
             if (!is.null(tmax$preprocessing)) list(preprocessing = tmax$preprocessing))
  step("Stage 1: local heatwaves")
  heatwaves <- detect_heatwaves(tmax, cfg$heatwaves$threshold_percentile,
                                cfg$heatwaves$min_duration_days, cfg$heatwaves$min_exceeding_days)
  step("Stage 2: regionally extensive days")
  regional <- select_regional_days(heatwaves, cfg$regional_days$percentile)
  heatwaves <- heatwaves[c("events", "thresholds")]
  step("Stage 3: DBSCAN clustering of %d selected days", nrow(regional$selected_days))
  clusters <- cluster_heatwaves(regional, cfg$clustering$eps_values, cfg$clustering$min_samples_values)
  stage <- prepare_run_output(out)
  on.exit(if (dir.exists(stage)) unlink(stage, recursive = TRUE), add = TRUE)
  if (!any(clusters$daily_settings$n_clusters > 0)) {
    # Publish only the partial results of this run, so old downstream results
    # cannot be mistaken for output from the current attempt.
    books <- partial_workbooks(heatwaves, regional, clusters)
    write_workbooks(stage, books)
    partial <- list(config = cfg, input = input, heatwaves = heatwaves, regional = regional, clusters = clusters,
                    started = started, package_version = as.character(utils::packageVersion("scoRch.simulator")),
                    incomplete = "no selected day formed a cluster.")
    write_text_file(run_readme(partial, books), file.path(stage, "Run_Summary.txt"))
    publish_run_output(stage, out)
    published <- TRUE
    fail(paste("No selected day formed a cluster, so there are no ellipses or compound events.",
               "Partial workbooks were written to", out, "."))
  }
  step("Stage 4: ellipses of %d clusters", nrow(clusters$clusters))
  ellipses <- fit_ellipses(clusters, tmax, cfg$ellipses$scale_factor)
  step("Stage 5: compound events")
  events <- classify_events(clusters, ellipses)
  validate_representative_event_overrides(cfg, events)
  step("Power-law fits with %d bootstrap repetitions", cfg$power_law$bootstrap_repetitions)
  power_law <- fit_power_law(events, cfg$power_law$bootstrap_repetitions,
                             cfg$power_law$seed_daily_maxima, cfg$power_law$seed_event_maxima)
  step("Log-Gaussian Cox process of %d centroids", nrow(events$ellipses))
  lgcp_error <- NULL
  lgcp <- tryCatch(fit_lgcp(events, tmax, cfg$lgcp$covariates, cfg$lgcp$raster_km, cfg$lgcp$window_margin_km),
                   error = function(e) {
                     lgcp_error <<- conditionMessage(e)
                     message("The LGCP could not be fitted: ", lgcp_error)
                     NULL
                   })

  result <- structure(list(
    config = cfg,
    input = input,
    heatwaves = heatwaves,
    regional = regional[c("daily", "selected_days", "selected_cells", "threshold_cells")],
    clusters = clusters[c("clusters", "cluster_cells", "daily_settings", "sequences")],
    ellipses = events$ellipses,
    events = events[c("events", "event_days", "table_1", "annual_counts", "annual_duration", "trends")],
    power_law = power_law, lgcp = lgcp, lgcp_error = lgcp_error, output_dir = stage,
    package_version = as.character(utils::packageVersion("scoRch.simulator")),
    started = started), class = "scorch_result")

  step("Writing the result workbooks to %s", out)
  books <- result_workbooks(result)
  write_workbooks(stage, books)
  plot_names <- names(figure_specs())
  plots <- data.frame(name = plot_names, status = rep("skipped", length(plot_names)),
                      reason = rep("", length(plot_names)))
  for (i in seq_along(plot_names)) {
    figure <- plot_names[i]
    step("Plot %s", figure)
    file <- file.path(stage, "figures", paste0(figure, ".png"))
    reason <- character()
    drawn <- withCallingHandlers(scorch_figure(result, figure, file),
                                 message = function(m) reason <<- c(reason, trimws(conditionMessage(m))))
    if (isTRUE(drawn)) {
      plots$status[i] <- "generated"
    } else {
      plots$reason[i] <- if (length(reason)) paste(reason, collapse = " ") else "Not available for this dataset."
    }
  }
  result$plots <- plots
  if (!is.null(trace_date) && trace_date %in% tmax$dates) {
    matches <- events$events$start_date <= trace_date & events$events$end_date >= trace_date
    if (sum(matches) == 1L) {
      traced <- result
      traced$output_dir <- out
      result$trace <- trace_event(traced, tmax, trace_date)
    }
  }
  result$elapsed_minutes <- as.numeric(difftime(Sys.time(), started, units = "mins"))
  write_text_file(run_readme(result, books), file.path(stage, "Run_Summary.txt"))
  publish_run_output(stage, out)
  published <- TRUE
  result$output_dir <- out
  if (is.null(lgcp)) {
    step("Finished with the LGCP unavailable in %.1f minutes; see Run_Summary.txt", result$elapsed_minutes)
  } else {
    step("Done in %.1f minutes", result$elapsed_minutes)
  }
  print(result)
  message("Open your results: ", normalizePath(out, winslash = "/"))
  invisible(result)
}

# Check a date against the actual catalog before fitting models or drawing
# plots. Invalid manual choices should never be hidden as a skipped plot.
validate_representative_event_overrides <- function(cfg, events) {
  context <- list(config = cfg, events = events)
  for (type in 1:4) {
    if (!is.null(cfg$figures$representative_events[[paste0("type_", type)]])) {
      representative_events(context, type)
    }
  }
  invisible(TRUE)
}

#' @export
print.scorch_result <- function(x, ...) {
  types <- tabulate(x$events$events$event_type, 4)
  cat(sprintf(paste0("<scorch_result> %d cells, %d dates; %d local heatwaves; regional threshold %d cells;\n",
                     "  %d selected days, %d clusters/ellipses, %d compound events (Type 1-4: %s)\n",
                     "  outputs in %s\n"),
              nrow(x$input$cells), length(x$input$dates), nrow(x$heatwaves$events),
              x$regional$threshold_cells, nrow(x$regional$selected_days), nrow(x$ellipses),
              nrow(x$events$events), paste(types, collapse = "/"), x$output_dir))
  invisible(x)
}

# Build each run away from the published output. Package-generated files are
# moved into place only after they have all been written.
prepare_run_output <- function(out) {
  parent <- dirname(out)
  if (!dir.exists(parent) && !dir.create(parent, recursive = TRUE, showWarnings = FALSE)) {
    fail("Could not create the parent of the output directory: ", parent)
  }
  stage <- tempfile(pattern = ".scorch-stage-", tmpdir = parent)
  if (!dir.create(stage, showWarnings = FALSE)) fail("Could not create the temporary output directory: ", stage)
  for (folder in c("catalogs", "modeling", "figures")) {
    if (!dir.create(file.path(stage, folder), showWarnings = FALSE)) {
      unlink(stage, recursive = TRUE)
      fail("Could not prepare the output folder: ", folder)
    }
  }
  stage
}

publish_run_output <- function(stage, out) {
  if (!dir.exists(out) && !dir.create(out, recursive = TRUE, showWarnings = FALSE)) {
    fail("Could not create the output directory: ", out)
  }
  backup <- tempfile(pattern = ".scorch-backup-", tmpdir = dirname(out))
  if (!dir.create(backup, showWarnings = FALSE)) fail("Could not prepare a backup of existing output: ", backup)
  managed <- managed_output_files()
  old_moved <- new_moved <- character()
  problem <- tryCatch({
    for (item in managed) {
      target <- file.path(out, item)
      if (file.exists(target)) {
        dir.create(dirname(file.path(backup, item)), recursive = TRUE, showWarnings = FALSE)
        if (!file.rename(target, file.path(backup, item))) fail("Could not back up existing output: ", target)
        old_moved <- c(old_moved, item)
      }
    }
    for (item in managed) {
      source <- file.path(stage, item)
      if (file.exists(source)) {
        dir.create(dirname(file.path(out, item)), recursive = TRUE, showWarnings = FALSE)
        if (!file.rename(source, file.path(out, item))) fail("Could not publish output: ", source)
        new_moved <- c(new_moved, item)
      }
    }
    NULL
  }, error = identity)
  if (!is.null(problem)) {
    returned_new <- vapply(rev(new_moved), function(item) {
      file.rename(file.path(out, item), file.path(stage, item))
    }, logical(1))
    restored_old <- vapply(rev(old_moved), function(item) {
      file.rename(file.path(backup, item), file.path(out, item))
    }, logical(1))
    if (!all(returned_new) || !all(restored_old)) {
      fail("Publishing failed and some previous files could not be restored. The backup remains at ", backup, ".")
    }
    unlink(backup, recursive = TRUE)
    stop(problem)
  }
  unlink(backup, recursive = TRUE)
  # Remove only the now-empty former package folder, never user contents.
  for (folder in c("Additional_Results", "Heatwave_Catalogs", "Modelling")) {
    old_folder <- file.path(out, folder)
    if (dir.exists(old_folder) && !length(list.files(old_folder, all.files = TRUE, no.. = TRUE))) {
      root <- normalizePath(out, winslash = "/", mustWork = TRUE)
      target <- normalizePath(old_folder, winslash = "/", mustWork = TRUE)
      if (identical(dirname(target), root)) unlink(target, recursive = TRUE)
    }
  }
  invisible(out)
}

# Include all optional outputs so a rerun cannot leave a stale model or plot.
managed_output_files <- function() {
  c(
    "catalogs/01_heatwave_events.xlsx",
    "catalogs/02_regionally_extensive_heatwaves.xlsx",
    "catalogs/03_extreme_heatwave_clusters.xlsx",
    "catalogs/04_extreme_heatwave_ellipses.xlsx",
    "catalogs/05_compound_heatwave_events.xlsx",
    "modeling/Power_Law_Model.xlsx",
    "modeling/Spatial_Risk_Model.xlsx",
    "Heatwave_Statistics.xlsx",
    "Run_Summary.txt",
    file.path("figures", paste0(names(figure_specs()), ".png")),
    # Retire earlier generated paths only after a successful run.
    "Heatwave_Catalogs/Heatwaves.xlsx",
    "Heatwave_Catalogs/Regional_Heatwaves.xlsx",
    "Heatwave_Catalogs/Heatwave_Clusters.xlsx",
    "Heatwave_Catalogs/Heatwave_Ellipses.xlsx",
    "Heatwave_Catalogs/Compound_Heatwaves.xlsx",
    "Modelling/Power_Law_Model.xlsx",
    "Modelling/Spatial_Risk_Model.xlsx",
    "README.txt",
    "Additional_Results/Analysis_Summary.xlsx",
    "Modelling/Power_Law.xlsx",
    "Modelling/LGCP.xlsx",
    "figures/ellipse_area_power_law.png",
    "figures/centroid_concentration.png"
  )
}

# Degrees with the hemisphere letter, for the run summary and print methods.
format_degrees <- function(value, letters) {
  paste0(abs(value), "\u00b0", ifelse(value < 0, letters[2], letters[1]))
}
