#' Run the US demonstration
#'
#' Runs the complete workflow using the local `config.yaml` when present,
#' otherwise the installed configuration. A null input uses the bundled US
#' example. Use "hourly" with local ERA5 files to demonstrate preprocessing.
#'
#' @param config_file "prepared" (default), "hourly", or a YAML path.
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
  run_scorch(cfg, trace_date = "2012-06-30")
}

demo_settings <- function(config_file = "prepared", input = NULL, output = NULL) {
  if (!is.character(config_file) || length(config_file) != 1L || is.na(config_file)) {
    fail("Choose 'prepared', 'hourly', or a YAML configuration file.")
  }
  profile <- config_file %in% c("prepared", "hourly")
  file <- if (profile) {
    if (file.exists("config.yaml")) "config.yaml" else scorch_config_file()
  } else config_file
  if (!file.exists(file)) fail("The configuration file does not exist: ", file)
  document <- yaml::read_yaml(file)
  cfg <- read_config(document, input_required = FALSE)
  example <- is.null(cfg$input$file) && is.null(input) && config_file != "hourly"
  if (example || identical(config_file, "hourly")) {
    profiles <- c("prepared", if (identical(config_file, "hourly")) "hourly")
    for (name in profiles) {
      overrides <- document$demo[[name]]
      if (is.null(overrides)) next
      read_config(overrides, input_required = FALSE)
      for (section in names(overrides)) for (key in names(overrides[[section]])) {
        value <- overrides[[section]][[key]]
        if (section == "figures" && key == "representative_events") {
          for (type in names(value)) {
            if (is.null(cfg$figures$representative_events[[type]])) {
              cfg$figures$representative_events[type] <- value[type]
            }
          }
        } else cfg[[section]][key] <- list(value)
      }
    }
  }
  if (example) {
    if (!cfg$input$format %in% c("auto", "daily_tmax")) fail("Set input: file to your local ERA5 file or folder.")
    cfg$input$file <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator", mustWork = TRUE)
    cfg$input$format <- "daily_tmax"
  }
  if (example || identical(config_file, "hourly")) {
    cfg$figures$representative_events <- Filter(function(value) !is.null(value), cfg$figures$representative_events)
  }
  if (!is.null(input)) cfg$input$file <- input
  if (!is.null(output)) cfg$output$directory <- output
  if (identical(cfg$input$format, "era5")) {
    if (is.null(cfg$input$file)) fail("Supply your local hourly ERA5 folder: run_demo('hourly', input = 'path/to/hourly_folder').")
    cfg$input$processed_file <- file.path(if (is.null(cfg$output$directory)) "output" else cfg$output$directory, "prepared_tmax.nc")
  }
  cfg
}
