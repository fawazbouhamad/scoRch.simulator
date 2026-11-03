# scoRch.simulator

**Find, describe and model regional compound heatwaves from temperature data.**
Developed by Fawaz Bouhamad and Nasser Najibi using SCORCH: Spatiotemporal
Classification of Regional Compound Heatwaves.

[Install](#install) · [Run](#run) · [Find your results](#find-your-results) · [Use your own data](#use-your-own-data)

## Install

R 4.1 or newer is required. In R:

```r
install.packages("remotes")
remotes::install_github("fawazbouhamad/scoRch.simulator", subdir = "src")
```

Dependencies install automatically. Windows, macOS and Linux are supported.
The current version is 1.0.1, pending a tagged release.

## Run

Download or clone this repository. From its top folder:

| Input | Command |
|---|---|
| Prepared daily Tmax — ready to run | `Rscript demo/run.R` |
| Hourly ERA5 — supply local files | `Rscript demo/run.R demo/config/hourly.yaml path/to/hourly_folder` |

The prepared demo covers the contiguous US and its surroundings
(25–50°N, 130–58°W), every April–September day of 1996–2025: 1,800 one-degree
cells and 5,490 dates. Its bundled input is about 12 MB. Both configurations
use the study settings, including 5,000 bootstrap repetitions and fixed seeds.

The full 30-year hourly archive and its matching prepared file are being
acquired and validated for a separate data deposit; no new DOI is published
yet. Hourly processing uses files already on your computer and saves a
reusable prepared file. It does not request an API key.

## Find your results

Browse the [example results](demo/results/). A prepared demo run writes the
same layout to `results/`; the hourly configuration writes to `results/hourly/`.

```text
results/
├── Heatwave_Catalogs/       Five catalogs: events and their characteristics
├── Modelling/              Power-law and spatial risk model workbooks
├── figures/                Nine figures
├── Heatwave_Statistics.xlsx Thresholds, daily extent, summaries and trends
└── Run_Summary.txt          Input, settings, counts, models and warnings
```

| Find | Open | What it contains |
|---|---|---|
| Local heatwaves | [Heatwaves.xlsx](demo/results/Heatwave_Catalogs/Heatwaves.xlsx) | Cell-level event dates, durations, peaks and bridged days |
| Regionally extensive days | [Regional_Heatwaves.xlsx](demo/results/Heatwave_Catalogs/Regional_Heatwaves.xlsx) | Selected days, cell counts and the selected cells |
| Spatial clusters | [Heatwave_Clusters.xlsx](demo/results/Heatwave_Catalogs/Heatwave_Clusters.xlsx) | DBSCAN clusters and membership/core/noise labels |
| Ellipse characteristics | [Heatwave_Ellipses.xlsx](demo/results/Heatwave_Catalogs/Heatwave_Ellipses.xlsx) | Area, axes, orientation, weighted centroids and temperatures |
| Compound-event characteristics | [Compound_Heatwaves.xlsx](demo/results/Heatwave_Catalogs/Compound_Heatwaves.xlsx) | Event type, dates, duration, cluster counts and geometry; event-day details |
| Power-law model | [Power_Law_Model.xlsx](demo/results/Modelling/Power_Law_Model.xlsx) | Area cutoffs, exponents, fit statistics and bootstrap results |
| Spatial risk model | [Spatial_Risk_Model.xlsx](demo/results/Modelling/Spatial_Risk_Model.xlsx) | LGCP parameters, fitted spatial intensity and centroid concentration |
| Thresholds and trends | [Heatwave_Statistics.xlsx](demo/results/Heatwave_Statistics.xlsx) | **Event Type Summary**, **Cell Thresholds**, **Daily Heatwave Extent**, **Duration Trends** tabs |

The [figures folder](demo/results/figures/) contains the maps and plots,
including [power_law_model.png](demo/results/figures/power_law_model.png) and
[spatial_risk_model.png](demo/results/figures/spatial_risk_model.png).
The spatial risk model depicts the fitted spatial occurrence intensity of
heatwave centroids; exposure and vulnerability are not modelled.

`Run_Summary.txt` is a generated record of that particular run, including
its settings and warnings. All instructions are on this page.

## Use your own data

```r
library(scoRch.simulator)
write_scorch_config("scorch_config.yaml")
# Set input.file and the period/grid settings in that YAML.
result <- run_scorch("scorch_config.yaml")
```

<details>
<summary><strong>Input requirements and processing</strong></summary>

Choose either a prepared daily maximum temperature NetCDF file or local
ERA5 NetCDF files. `input.format` is `daily_tmax`, `era5`, or `auto` for
format detection. A prepared file keeps its regular grid. ERA5 input may
be a file, list of files or a folder containing the relevant `.nc` files.
Files share a grid, contain complete UTC days and have no overlapping dates.

```yaml
input:
  file: 'path/to/era5_folder'
  format: era5
  era5_cell_size_deg: 1
  processed_file: 'results/prepared_tmax.nc'
```

For hourly ERA5, the package takes the maximum of all 24 hours at each
native point, converts kelvin to Celsius, and averages complete spatial
blocks. At 0.25° input and 1° output, each block contains 4×4 points.
Configured months/date limits select the days to process. Packed variables
and `expver` are handled; GRIB is not supported. Use `?read_era5` and
`?read_tmax` for variable names, dimensions, units and calendars.

Saved prepared input uses float64 so that re-reading it preserves the
processed values. The compact bundled input uses 0.01°C packing; its
reference results stay fixed. Exact comparisons between the hourly and
prepared routes use the prepared file derived from those same hourly
inputs, since rounding can alter percentile ties.

The common launcher accepts `configuration`, `input`, `output` arguments:

```text
Rscript demo/run.R demo/config/prepared.yaml path/to/prepared.nc results/paired_prepared
```

In R, `source("demo/run.R"); result <- run_demo()` runs the prepared demo.
For your own analysis, `run_scorch()` reads the complete configuration.
Data already in R can enter through `tmax_from_array()`.

</details>

<details>
<summary><strong>How the analysis works and what you can configure</strong></summary>

The controller `run_scorch()` connects temperature preparation, five catalog
stages, both models, figure generation and workbook export.

| Stage | Function | Study default |
|---|---|---|
| Local heatwaves | `detect_heatwaves()` | 95th-percentile Tmax; 3-day requirements |
| Extensive days | `select_regional_days()` | 97.5th percentile of daily heatwave-cell counts |
| Spatial clusters | `cluster_heatwaves()` | DBSCAN parameter search pooled over consecutive selected days |
| Ellipses | `fit_ellipses()` | Principal axes scaled by 1.25; temperature-weighted centroid |
| Compound events | `classify_events()` | Consecutive selected days classified into four space-time types |
| Area model | `fit_power_law()` | 5,000 bootstrap repetitions, fixed seeds |
| Spatial model | `fit_lgcp()` | Lon, lat, mean/SD of Tmax; 25 km raster; 80 km margin |

`write_scorch_config()` creates the full commented YAML. Omitted settings
retain the defaults; `period`, `figures` and `output` control the season,
representative events and destination. Edit the YAML to configure an analysis.

The [workflow](src/man/figures/scorch_workflow.png) and
[event-type diagram](src/man/figures/compound_event_types.png) illustrate the
method. The demo's run summary traces an observed June–July 2012 event
within the 30-year record. Those observations and event panels are part of
the retained scientific results.

Workbooks retain all rows and columns, numeric/date types and missing cells.
Display formatting does not change stored values. A table exceeding Excel's
sheet limit stops with an error. Missing event types or unavailable LGCP
fits are explained in the run summary. A run with no clusters is marked
incomplete and writes only available upstream tables. Unrelated user files
in an output directory are preserved.

</details>

<details>
<summary><strong>What the repository folders and R files mean</strong></summary>

There are **two working folders**: [demo/](demo/) and [src/](src/).
[.github/](.github/) is GitHub's technical folder for automatic checks and
maintenance scripts; users do not run it as an extra software step.

| Item inside `src/` | Meaning |
|---|---|
| `R/` | The calculation, processing, plotting and export code |
| `inst/` | Data, default configuration and citation installed with the package |
| `man/` | R's help pages, shown by commands such as `?run_scorch` |
| `tests/` | Automated checks of calculations, input handling and exports |
| `DESCRIPTION` | Package name/version, authors, licence and required dependencies |
| `NAMESPACE` | Functions and methods R makes available when loading the package |

These follow [R's standard package layout](https://stat.ethz.ch/R-manual/R-devel/doc/manual/R-exts.html#Package-structure). The package installs from `src/`:
`R CMD INSTALL src` for a local clone, or `subdir = "src"` on GitHub.
The main controller is `src/R/run.R`; preparation is in `input.R` and
`era5.R`; catalogs are in `heatwaves.R`, `regional_days.R`, `clustering.R`,
`ellipses.R`, `events.R`; models are in `power_law.R` and `lgcp.R`.

The root `LICENSE.txt` licenses the source. `CITATION.cff` provides the
software citation, `.zenodo.json` supplies archive metadata, and `.gitignore`
excludes generated local files. They have distinct technical roles.

</details>

<details>
<summary><strong>Verification and maintenance</strong></summary>

The preserved prepared-demo baseline has **65,790 local heatwaves,
140 selected days, 376 ellipses and 29 compound events** (Types 1–4:
1/4/20/4). Comparisons cover every worksheet in all eight workbooks.

```text
Rscript demo/run.R
Rscript .github/scripts/check_input_options.R
Rscript .github/scripts/compare_output.R
```

The portable comparator uses 1e-6 tolerance, or 1e-3 for LGCP fits across
platforms. Same-system preservation checks compare all worksheet values
and catalog/power-law objects with zero tolerance. A genuine completed
hourly season and its saved float64 counterpart are also checked through
the common launcher. Full 30-year hourly-pair validation follows acquisition.

[GitHub Actions](https://github.com/fawazbouhamad/scoRch.simulator/actions)
checks the package on Windows, macOS and Linux and runs the processed demo.
The [reference hashes](.github/scripts/reference_hashes.json) record protected
inputs, numerical references, figure pixels and calculation source.
The independent [research repository](https://github.com/fawazbouhamad/compound-heatwave-typologies-SCORCH)
contains the study scripts and outputs. To enable the full-study test, set
`SCORCH_REFERENCE_DIR` to its clone with the archived input at
`input/tmax_processed.nc`. `SCORCH_LONG_TESTS=1` enables the long bootstrap test.

Data-build and fixture-build scripts are under `.github/scripts/`; run them
from the repository root. They preserve source checksums and the compact
demo's fixed packing. Before release, synchronize version/citation/archive
metadata, run `devtools::check("src")`, confirm the demo and reference
comparisons, tag the tested commit and archive the software separately from
the input data. Review the assigned DOI and the journal's current submission
requirements before publishing.

The authors developed the method, R implementation and study tests, and
reported AI coding assistance in their development. Additional Claude Code
and Codex assistance covered packaging, documentation, input-interface work,
figure-style alignment and validation. The authors retain responsibility
for the final software and manuscript.

</details>

Use [CITATION.cff](CITATION.cff) or `citation("scoRch.simulator")` to cite the
software. Source code is [GPL-3.0-or-later](LICENSE.txt). The
[data provenance](src/inst/extdata/PROVENANCE.txt) records ERA5 attribution,
CC BY 4.0 processing/artwork and the internal test fixture. The study input
is a separate [Zenodo dataset](https://doi.org/10.5281/zenodo.21717874).
For support or contributions, open an
[issue](https://github.com/fawazbouhamad/scoRch.simulator/issues) with the
package/R versions, configuration and error. Calculation or default changes
should retain the study-reference tests and numerical verification.
