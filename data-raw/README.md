# Rebuilding the demonstration data

`make_demo_data.R` and `arco_na_manifest.txt` record the existing compact
1996-2025 prepared dataset. Its 0.01 C packing is retained so that the
published numerical references remain reproducible.

The optional hourly example covers July 2012 over the same full US box,
25-50 N and 130-58 W. Rebuild it from the repository root:

```text
python data-raw/fetch_hourly_demo.py path/to/source_cache
Rscript data-raw/prepare_demo_archive.R path/to/source_cache/era5_hourly_us_2012_07.nc path/to/new_archive
python data-raw/validate_hourly_demo.py path/to/new_archive
Rscript demo/run_hourly_demo.R path/to/new_archive
```

The Python maintainer scripts require `numpy`, `netCDF4` and `numcodecs`.
They are not R package dependencies. Fetching uses the public Google
ARCO-ERA5 archive without an account. Its global chunks require about
1.73 GB of source transfer to retain a 47.1 MB US subset; users of the
prepared archive download only the approximately 59.8 MB demonstration
data. The fetch preserves original hourly values and makes no synthetic data.

The R recipe validates the sampling and domain, derives daily UTC Tmax,
converts to Celsius, averages to one-degree cells and writes a paired
double-precision prepared file. It also copies the separate 30-year quick
dataset and its provenance. The independent Python check calculates daily
maxima and spatial block means without using the R implementation.

Keep the source provenance and validation report with any deposit, and
refresh its checksum manifest after adding metadata. These scripts neither
publish a Zenodo record nor mint a DOI. The one-month example demonstrates
preprocessing; the 30-year dataset supports the full heatwave/model analysis.
