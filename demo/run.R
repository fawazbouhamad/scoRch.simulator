# Optional command-line launcher; R users can call scoRch.simulator::run_demo().
# Rscript demo/run.R [prepared|hourly|config.yaml] [input_file_or_folder] [output_folder]
run_demo <- scoRch.simulator::run_demo
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 3L) stop("Use: Rscript demo/run.R [prepared|hourly|config.yaml] [input] [output]", call. = FALSE)
  names(args) <- c("config_file", "input", "output")[seq_along(args)]
  do.call(run_demo, as.list(args))
}
