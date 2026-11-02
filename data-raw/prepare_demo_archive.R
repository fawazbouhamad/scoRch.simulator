# Prepare paired preprocessing data; no publishing or credentials.
# Rscript data-raw/prepare_demo_archive.R path/to/hourly.nc path/to/new_folder
library(scoRch.simulator)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || !file.exists(args[1]) || dir.exists(args[1])) {
  stop("Supply the hourly ERA5 NetCDF file and a new output folder.")
}
source <- args[1]
archive <- args[2]
if (file.exists(archive)) stop("Choose a new archive folder to preserve existing files.")
check_source <- function(file) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  stopifnot("t2m" %in% names(nc$var), "valid_time" %in% names(nc$dim),
            length(nc$dim$valid_time$vals) == 744L,
            identical(nc$dim$valid_time$units, "hours since 1900-01-01 00:00:00"),
            all(diff(nc$dim$valid_time$vals) == 1),
            identical(range(nc$dim$latitude$vals), c(25, 50)),
            identical(range(nc$dim$longitude$vals), c(230, 302)),
            all(abs(diff(nc$dim$latitude$vals)) == 0.25),
            all(diff(nc$dim$longitude$vals) == 0.25))
}
check_source(source)
tmax <- read_era5(source, months = 7, start = "2012-07-01", end = "2012-07-31", cell_size = 1)
stopifnot(identical(as.character(tmax$dates), as.character(seq(as.Date("2012-07-01"), as.Date("2012-07-31"), by = "day"))),
          nrow(tmax$cells) == 1800L)
dir.create(archive, recursive = TRUE)
if (!file.copy(source, file.path(archive, "era5_hourly_us_2012_07.nc"))) stop("Could not copy the hourly input.")
prepared <- file.path(archive, "daily_tmax_us_2012_07.nc")
write_tmax(tmax, prepared, precision = "float64", title = "US July 2012 daily Tmax from the accompanying hourly ERA5",
           licence = "Contains modified Copernicus Climate Change Service information 2026 (ERA5)")
back <- read_tmax(prepared, months = 7)
stopifnot(identical(back$dates, tmax$dates), identical(back$tmax, tmax$tmax))
quick <- system.file("extdata", "north_america_tmax.nc", package = "scoRch.simulator")
provenance <- system.file("extdata", "README.md", package = "scoRch.simulator")
stopifnot(file.copy(quick, file.path(archive, "north_america_tmax_1996_2025.nc")),
          file.copy(provenance, file.path(archive, "quick_demo_provenance.txt")))
hourly_provenance <- file.path(dirname(source), "provenance.json")
if (file.exists(hourly_provenance)) {
  stopifnot(file.copy(hourly_provenance, file.path(archive, "hourly_source_provenance.json")))
}
writeLines(c("Paired ERA5 preprocessing demonstration for scoRch.simulator",
  "Domain: 25-50 N, 130-58 W. Period: every UTC hour of July 2012.",
  "era5_hourly_us_2012_07.nc: 744 genuine hourly ERA5 fields at 0.25 degrees, kelvin.",
  "daily_tmax_us_2012_07.nc: 31 UTC daily maxima averaged to 1800 one-degree cells, Celsius, float64.",
  "north_america_tmax_1996_2025.nc: separate 30-year prepared quick-demo data, April-September, 1800 cells.",
  "quick_demo_provenance.txt describes that historical dataset's source, processing and licence.",
  "The prepared file was derived from this exact hourly file using read_era5().",
  "The prepared temperatures read back identically to the computed hourly-route temperatures.",
  "Run from the repository root: Rscript demo/run_hourly_demo.R path/to/this_folder",
  "This month demonstrates preprocessing. It is not a heatwave climatology.",
  "Use the separate bundled 1996-2025 prepared dataset for the full catalog/model demonstration.",
  "The historical bundled dataset rounds values to 0.01 C and is not the exact-value reference for this pair.",
  "Source: Google ARCO-ERA5 public archive; https://github.com/google-research/arco-era5",
  "ERA5 hourly single-level 2-m temperature: DOI 10.24381/cds.adbb2d47; cite Hersbach et al. (2020).",
  "Contains modified Copernicus Climate Change Service information 2026. Data: CC BY 4.0.",
  "No Zenodo record or DOI has been created."), file.path(archive, "README.txt"))
files <- list.files(archive, full.names = TRUE)
write.table(data.frame(file = basename(files), bytes = file.size(files), md5 = unname(tools::md5sum(files))),
            file.path(archive, "manifest.tsv"), sep = "\t", quote = FALSE, row.names = FALSE)
cat("Prepared paired dataset at ", archive, "\n", sep = "")
