# User guide

scoRch.simulator prepares local temperature data, builds five heatwave
catalogs, fits the power-law and LGCP models, and writes the workbooks and
figures. This is the complete user guide for both input routes.

- [Install](#install)
- [Run the demo](#run-the-demo)
- [Use your own data](#use-your-own-data)
- [Choose settings](#choose-settings)
- [Follow the workflow](#follow-the-workflow)
- [Find the outputs](#find-the-outputs)
- [Trace one observed event](#trace-one-observed-event)
- [Software structure](#software-structure)
- [Verification, citation and support](#verification-citation-and-support)

## Install

R 4.1 or newer is required. The package is pure R and supports Windows,
macOS and Linux. From R:

```r
install.packages("remotes")
remotes::install_github("fawazbouhamad/scoRch.simulator", subdir = "src")
```

Dependencies install automatically. From a local clone, `R CMD INSTALL src`
installs the package. The current main-branch version is 1.0.1, pending a
tagged release.

## Run the demo

Download or clone the repository and run commands from its top folder.
Both configurations use the study settings, including 5,000 power-law
bootstrap repetitions and fixed seeds.

**Prepared input:**

```text
Rscript demo/run.R
```

This uses [prepared.yaml](../demo/config/prepared.yaml) and the bundled
`src/inst/extdata/north_america_tmax.nc`: 1,800 one-degree cells over
25–50°N, 130–58°W, every April–September day of 1996–2025 (5,490 dates,
about 12 MB). The region covers the contiguous US and its surroundings,
including southern Canada, northern Mexico and nearby waters. Output goes
to `results/`.

Expected results are **65,790 local heatwaves, 140 selected days,
376 ellipses and 29 compound events** (Types 1–4: 1/4/20/4).

**Hourly input:**

```text
Rscript demo/run.R demo/config/hourly.yaml path/to/hourly_folder
```

The [hourly configuration](../demo/config/hourly.yaml) names the same
30-year domain and season. The package processes local hourly ERA5 NetCDF
files before running the same catalog and modelling workflow. It saves a
reusable double-precision `results/hourly/prepared_tmax.nc`, with results
in `results/hourly/`.

The full 30-year hourly archive and its directly derived prepared counterpart
are being acquired and validated separately for a data deposit. No new DOI
is published yet. The hourly example requires those local files; package
use does not download data or request an API key.

To select another prepared file and output folder:

```text
Rscript demo/run.R demo/config/prepared.yaml path/to/prepared.nc results/paired_prepared
```

The optional arguments are always `configuration`, `input`, `output`.
In an R session:

```r
source("demo/run.R")
result <- run_demo()
```

The bundled compact input is packed to 0.01°C. Its numerical references
remain fixed. Exact hourly-versus-prepared comparisons must use the
double-precision prepared file derived from those same hourly inputs:
rounding can change percentile ties. A newly derived pair is checked
separately from the compact demonstration baseline.

## Use your own data

```r
library(scoRch.simulator)
write_scorch_config("scorch_config.yaml")
# Set input.file and the required period/grid settings in the YAML.
result <- run_scorch("scorch_config.yaml")
```

### Prepared daily Tmax

Set `input.format: daily_tmax` and `input.file` to one NetCDF file.
Defaults are variable `tmax`, dimensions `lon`, `lat`, `time`, and degrees
Celsius; the matching variable/dimension names can be configured. The grid
is regular and keeps its supplied spacing. Kelvin is also supported.
Dates must have CF-style time units and a supported calendar. See
`?read_tmax` for the complete interface. Data already in R can enter through
`tmax_from_array()` and `run_scorch(config, tmax = my_tmax)`.

### Local ERA5 NetCDF

```yaml
input:
  file: 'path/to/era5_folder'
  format: era5
  era5_cell_size_deg: 1
  processed_file: 'results/prepared_tmax.nc'
```

Use local ERA5 2-m temperature (`t2m`) files, downloaded in NetCDF format.
Input can be one file, a list of files, or a folder containing only the
relevant `.nc` files. The reader supports hourly/subdaily temperature and
daily maximum temperature on regular grids. GRIB is not supported.

Files must share the same grid, contain complete UTC days at a consistent
sampling interval, and have no duplicated dates across files. Download the
region with its boundary grid points included. Packed variables and the
`expver` dimension are handled. See `?read_era5` for supported dimensions,
units and calendars. `format: auto` can identify either input format.

The configured months and date limits select which days are processed.
Temperature preparation follows this order:

1. Compute each UTC day's maximum at every native grid point. Hourly data
   require all 24 hours of the day.
2. Convert kelvin to Celsius.
3. Average the daily maxima over complete spatial blocks. For 0.25° input
   and 1° output, each cell averages 4×4 points. This is an arithmetic mean.
4. Validate the resulting temperature input for the catalog stages.

`input.processed_file` optionally saves reusable daily Tmax in double
precision. Set it to `null` to omit that saved copy. Saving must not replace
the input file. The regular scientific calculations use the same prepared
temperature values whichever input route supplied them.

## Choose settings

`write_scorch_config()` writes the full commented configuration. Settings
omitted from a user configuration retain the study defaults; users edit
the YAML rather than the R source.

| Section | Controls | Default |
|---|---|---|
| `period` | Months and date limits | April–September; no date limits |
| `heatwaves` | Cell threshold and duration/gap rules | 95th percentile; 3-day requirements |
| `regional_days` | Extensive-day threshold | 97.5th percentile |
| `clustering` | DBSCAN candidate distances and minimum membership | Study search grid |
| `ellipses` | Principal-axis scale | 1.25 |
| `power_law` | Bootstrap repetitions and seeds | 5,000; fixed daily/event seeds |
| `lgcp` | Covariates, raster and window margin | Lon, lat, mean and SD of Tmax; 25 km; 80 km |
| `figures` | Representative events to display | Automatic unless overridden |
| `output` | Results folder | `results/` |

The [installed default configuration](../src/inst/scorch_config.yaml) gives
every parameter and its explanation.

## Follow the workflow

`run_scorch()` is the controller. Data preparation happens before the five
catalog stages; modelling and export happen after them.

| Part | Operation | Function |
|---|---|---|
| Input | Read prepared data or process local ERA5 | `read_tmax()`, `read_era5()`, `write_tmax()` |
| Catalog 1 | Detect heatwaves in each grid cell | `detect_heatwaves()` |
| Catalog 2 | Select regionally extensive days | `select_regional_days()` |
| Catalog 3 | Cluster each day's heatwave cells with DBSCAN | `cluster_heatwaves()` |
| Catalog 4 | Fit principal-component ellipses with temperature-weighted centroids | `fit_ellipses()` |
| Catalog 5 | Group consecutive selected days and assign compound-event types | `classify_events()` |
| Models | Fit area-distribution tails and spatial centroid intensity | `fit_power_law()`, `fit_lgcp()` |
| Export | Write workbooks, figures and the run README | Managed by `run_scorch()` |

The four event types are: one day/one cluster; one day/several clusters;
several days with a consistent cluster-count category; and several days
mixing categories. The [typology artwork](../src/man/figures/compound_event_types.png)
illustrates them. The [workflow artwork](../src/man/figures/scorch_workflow.png)
shows the catalog sequence.

To inspect the stages individually:

```r
tmax <- read_tmax("my_prepared.nc")
heatwaves <- detect_heatwaves(tmax)
regional <- select_regional_days(heatwaves)
clusters <- cluster_heatwaves(regional)
ellipses <- fit_ellipses(clusters, tmax)
events <- classify_events(clusters, ellipses)
```

These calls use the study defaults. The YAML-driven controller passes any
user overrides to the same functions.

## Find the outputs

The existing reproducibility-style grouping is preserved: separate catalog
workbooks, a modelling folder and additional results. Browse the
[reference outputs](../demo/results/) directly.

| File | Worksheets |
|---|---|
| `Heatwave_Catalogs/Heatwaves.xlsx` | Heatwave Events |
| `Heatwave_Catalogs/Regional_Heatwaves.xlsx` | Regional Days; Regional Day Cells |
| `Heatwave_Catalogs/Heatwave_Clusters.xlsx` | Clusters; Cluster Cells |
| `Heatwave_Catalogs/Heatwave_Ellipses.xlsx` | Ellipses |
| `Heatwave_Catalogs/Compound_Heatwaves.xlsx` | Compound Events; Event Days |
| `Modelling/Power_Law.xlsx` | Power Law Fits |
| `Modelling/LGCP.xlsx` | Parameters; Intensity; Centroid Concentration |
| `Additional_Results/Analysis_Summary.xlsx` | Event Type Summary; Cell Thresholds; Daily Heatwave Extent; Duration Trends |

All rows and columns are retained, with typed dates/numbers, frozen and
filtered headers, and empty cells for missing values. A table exceeding
Excel's sheet limit stops with an error. Stored values are distinct from
the display's rounded formatting.

The nine PNG files remain in `figures/`: `ellipse_spatial_patterns`,
`single_day_events`, `consistent_multiday_events`, `mixed_multiday_events`,
`ellipse_geometry_by_type`, `event_geometry_distributions`,
`annual_event_counts_and_duration`, `ellipse_area_power_law`, and
`centroid_concentration`. The run's `README.txt` explains settings, counts,
models, all output sheets and any unavailable results. Use
`scorch_figure(result, name = "mixed_multiday_events")` to redraw a figure.

Sparse datasets can have an unavailable LGCP fit or absent event types;
the README records why their outputs are unavailable. A run with no
clusters writes partial upstream results and is marked incomplete.
Package-managed outputs are replaced only after a successful write, and
unrelated files in the output folder are preserved.

## Trace one observed event

The default full-period demo follows the observed event of 28 June–4 July
2012 through the five catalogs and writes the trace into `results/README.txt`.
This is an event within the 30-year record. Its temperatures and plotted
panels remain part of the scientific results.

For this input, each of the seven days has one spatial cluster. The largest
ellipse is on 1 July 2012, and the trace follows its hottest cell through
the local threshold, heatwave dates and surrounding daily temperatures.
The Type 3 figure also shows an August 2023 event. Their choices are in
`figures.representative_events` in the example configurations.

The common demo launcher adds this trace when the event exists in the
supplied input. It does not add a second catalogue or modelling pipeline.

## Software structure

| Location | Responsibility |
|---|---|
| `src/R/run.R`, `config.R` | Controller and configuration |
| `src/R/input.R`, `era5.R`, `write_tmax.R` | Temperature reading, preparation and saving |
| `src/R/heatwaves.R`, `regional_days.R`, `clustering.R`, `ellipses.R`, `events.R` | Five catalog stages |
| `src/R/power_law.R`, `lgcp.R` | Statistical modelling |
| `src/R/figures.R`, `figures_stats.R`, `figure_style.R` | Figure generation and shared styling |
| `src/R/workbooks.R`, `readme.R` | Table exports and run description |
| `src/inst/`, `src/man/`, `src/tests/` | Installed data/defaults, help and automated checks |
| `demo/` | Common launcher, two configurations and reference results |
| `docs/verification/`, `docs/development/` | Current verification and maintainer material |

## Verification, citation and support

[Verification](verification/README.md) explains comparisons with preserved
results and the [research reproducibility repository](https://github.com/fawazbouhamad/compound-heatwave-typologies-SCORCH).
[Maintainer notes](development/README.md) cover data recipes, testing,
release preparation and software-paper metadata.

Use [CITATION.cff](../CITATION.cff) or `citation("scoRch.simulator")` for the
software citation. The study's processed input is a separate dataset,
[doi:10.5281/zenodo.21717874](https://doi.org/10.5281/zenodo.21717874).
Code is [GPL-3.0-or-later](../LICENSE.txt). The
[data provenance and licence](../src/inst/extdata/README.md) record ERA5 and
artwork attribution. Report problems through the
[issue tracker](https://github.com/fawazbouhamad/scoRch.simulator/issues),
including package/R versions, configuration and the error message.
