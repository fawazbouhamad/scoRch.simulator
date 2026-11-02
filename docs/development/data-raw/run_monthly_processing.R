# Genuine hourly ERA5 -> daily Tmax -> Celsius -> 1-degree cells.
# Rscript docs/development/data-raw/run_monthly_processing.R path/to/processing_demo
# One month illustrates preprocessing; use run_demo.R for the full analysis.
library(scoRch.simulator)
config <- yaml::read_yaml("docs/development/data-raw/monthly_config.yaml")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L) stop("Supply the folder containing the hourly demonstration and paired prepared file.")
folder <- if (length(args)) args[1] else dirname(config$input$file)
hourly <- file.path(folder, "era5_hourly_us_2012_07.nc")
reference <- file.path(folder, "daily_tmax_us_2012_07.nc")
if (!file.exists(hourly) || !file.exists(reference)) {
  stop("Extract the optional hourly demonstration archive at ", folder,
       ". Both era5_hourly_us_2012_07.nc and daily_tmax_us_2012_07.nc are needed; see demo/README.md.",
       call. = FALSE)
}
processed <- read_era5(hourly, months = config$period$warm_season_months,
                       start = config$period$start_date, end = config$period$end_date,
                       cell_size = config$input$era5_cell_size_deg)
prepared <- read_tmax(reference, months = 7)
stopifnot(length(processed$dates) == 31L, nrow(processed$cells) == 1800L,
          identical(processed$dates, prepared$dates), identical(processed$cells, prepared$cells),
          identical(processed$tmax, prepared$tmax))
destination <- config$input$processed_file
dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
write_tmax(processed, destination, precision = "float64", overwrite = TRUE)
stopifnot(identical(read_tmax(destination, months = 7)$tmax, processed$tmax))
cat("PASS: 744 genuine hourly fields -> 31 daily Tmax fields, Celsius, 1800 one-degree cells.",
    "The paired prepared input gives exactly the same values.",
    "Use demo/run_demo.R for the complete 30-year heatwave and modelling analysis.", sep = "\n")
