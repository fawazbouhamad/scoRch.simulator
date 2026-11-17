# Command-line launcher for the complete workflow and editable config.yaml.
# Rscript demo/run.R [prepared|hourly|config.yaml] [input_file_or_folder] [output_folder]
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 3L) stop("Use: Rscript demo/run.R [prepared|hourly|config.yaml] [input] [output]", call. = FALSE)
  if (!length(args)) {
    scoRch.simulator::run_scorch()
  } else {
    names(args) <- c("config_file", "input", "output")[seq_along(args)]
    do.call(scoRch.simulator::run_demo, as.list(args))
  }
}
