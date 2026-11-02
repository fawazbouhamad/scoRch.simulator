# Full hourly input -> daily Tmax -> Celsius -> 1-degree cells -> all results.
# From the repository root:
# Rscript demo/run_hourly_demo.R path/to/hourly_folder [output_folder]
# The full optional hourly archive is required; see demo/README.md.
library(scoRch.simulator)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 2L) stop("Use: Rscript demo/run_hourly_demo.R [hourly_folder] [output_folder]")
config <- yaml::read_yaml("demo/hourly_config.yaml")
if (length(args) >= 1L) config$input$file <- args[1]
if (length(args) >= 2L) config$output$directory <- args[2]
config$input$processed_file <- file.path(config$output$directory, "prepared_tmax.nc")
if (!dir.exists(config$input$file) && !file.exists(config$input$file)) {
  stop("Supply the full 1996-2025 hourly archive locally. See demo/README.md for availability.", call. = FALSE)
}
result <- run_scorch(config)
expected <- seq(as.Date("1996-04-01"), as.Date("2025-09-30"), by = "day")
expected <- expected[as.integer(format(expected, "%m")) %in% 4:9]
stopifnot(identical(result$input$dates, expected), nrow(result$input$cells) == 1800L)
cat("Complete hourly demonstration: 5490 daily Tmax fields on 1800 one-degree cells.\n",
    "Saved reusable prepared input: ", config$input$processed_file, "\n", sep = "")
