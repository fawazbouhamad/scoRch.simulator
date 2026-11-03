#' Run the US demonstration
#'
#' Runs the installed prepared example without a repository download or YAML
#' editing. Use "hourly" with local ERA5 files to demonstrate preprocessing.
#' Both profiles retain the study settings and the same representative events.
#' The full hourly archive is distributed separately when available.
#'
#' @param config_file "prepared" (default), "hourly", or a custom YAML path.
#' @param input Optional prepared NetCDF file or hourly file/folder. Required
#'   for the hourly profile. The prepared profile uses the installed example.
#' @param output Optional destination. Defaults to output/ or output/hourly/.
#' @return The same invisible result as [run_scorch()].
#' @seealso [run_scorch()] for your own analysis and [write_scorch_config()]
#'   for editable settings.
#' @examples
#' \dontrun{
#' run_demo()
#' run_demo("hourly", input = "path/to/hourly_folder")
#' }
#' @export
run_demo <- function(config_file = "prepared", input = NULL, output = NULL) {
  cfg <- demo_settings(config_file, input, output)
  result <- run_scorch(cfg, trace_date = "2012-06-30")
  message("Open your results: ", normalizePath(result$output_dir, winslash = "/"))
  invisible(result)
}

demo_settings <- function(config_file = "prepared", input = NULL, output = NULL) {
  if (!is.character(config_file) || length(config_file) != 1L || is.na(config_file)) {
    fail("Choose 'prepared', 'hourly', or a YAML configuration file.")
  }
  profile <- config_file %in% c("prepared", "hourly")
  if (profile) {
    document <- yaml::read_yaml(scorch_config_file())
    cfg <- scorch_defaults()
    profiles <- c("prepared", if (config_file == "hourly") "hourly")
    for (name in profiles) {
      overrides <- document$demo[[name]]
      if (is.null(overrides)) fail("The configuration is missing the ", name, " demo preset.")
      read_config(overrides, input_required = FALSE)
      for (section in names(overrides)) for (key in names(overrides[[section]])) {
        cfg[[section]][key] <- list(overrides[[section]][[key]])
      }
    }
    if (config_file == "prepared") cfg$input$file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator", mustWork = TRUE)
  } else {
    if (!file.exists(config_file)) fail("The configuration file does not exist: ", config_file)
    cfg <- analysis_settings(yaml::read_yaml(config_file))
  }
  if (!is.null(input)) cfg$input$file <- input
  if (!is.null(output)) cfg$output$directory <- output
  if (identical(cfg$input$format, "era5")) {
    if (is.null(cfg$input$file)) fail("Supply your local hourly ERA5 folder: run_demo('hourly', input = 'path/to/hourly_folder').")
    cfg$input$processed_file <- file.path(if (is.null(cfg$output$directory)) "output" else cfg$output$directory, "prepared_tmax.nc")
  }
  cfg
}
