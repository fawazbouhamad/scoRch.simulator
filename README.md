# scoRch.simulator

[![License: GPL-3.0-or-later](https://img.shields.io/badge/license-GPL--3.0--or--later-orange)](LICENSE.txt)
[![R: 4.1 or newer](https://img.shields.io/badge/R-%E2%89%A54.1-blue)](software/setup/DESCRIPTION)
[![Version: 1.2.0](https://img.shields.io/badge/version-1.2.0-blue)](https://github.com/fawazbouhamad/scoRch.simulator/releases/tag/v1.2.0)
[![Software checks](https://github.com/fawazbouhamad/scoRch.simulator/actions/workflows/checks.yaml/badge.svg?branch=main)](https://github.com/fawazbouhamad/scoRch.simulator/actions/workflows/checks.yaml)

An R package implementing **SCORCH: Spatiotemporal Classification of Regional
Compound Heatwaves**, developed by **Fawaz Bouhamad and Nasser Najibi**.

The package applies the SCORCH framework to local ERA5 or prepared daily maximum
temperature data, producing heatwave catalogs, statistical models, tables and
figures. A complete North America example is included.

<img src="software/figures/scorch_workflow.png" width="720" alt="SCORCH workflow from temperature data through detection, clustering and classification to catalogs, models and figures">

## Installation

You need **R 4.1 or newer**. In R or RStudio, run:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("remotes")
source("https://raw.githubusercontent.com/fawazbouhamad/scoRch.simulator/v1.2.0/software/setup/install.R")
install_scorch()
```

To update, restart R and run the same installation block.

## Running the software

Open a new RStudio project and run the included example:

```r
scoRch.simulator::run_scorch()
```

The software creates [config.yaml](config.yaml) in your project folder and runs
the full analysis. No separate data download is needed.

For **your own data**:

1. Open the example's config.yaml, or create one in a new project with
   `scoRch.simulator::write_scorch_config()`.
2. Set your input path and format under `input`, results folder under `output`,
   and months and dates under `period`.
3. Save the file and run `scoRch.simulator::run_scorch()` again.

Use the configuration and input instructions below, or watch the
[animated walkthrough](software/figures/how_to_run.gif). To keep the result in R
for later plotting, use `result <- scoRch.simulator::run_scorch()`.

## Use your own data

In RStudio, choose **File > New Project > New Directory > New Project** and
create a folder for your analysis. If you have not run the example there, run:

```r
scoRch.simulator::write_scorch_config()
```

Open `config.yaml` from RStudio's Files pane. Edit your input, output and period,
save the file, and run `scoRch.simulator::run_scorch()`. You do not need to clone
the repository or run the numbered scripts individually.

For **prepared daily Tmax**, use:

```yaml
input:
  file: "C:/data/my_daily_tmax.nc"
  format: daily_tmax
output:
  directory: "output/my_analysis"
period:
  warm_season_months: [4, 5, 6, 7, 8, 9]
  start_date: "1996-04-01"
  end_date: "2025-09-30"
```

For **local ERA5 temperature**, replace the input section with:

```yaml
input:
  file: "C:/data/my_era5_folder"
  format: era5
  processed_file: "C:/data/my_prepared_tmax.nc"
```

Keep the other configuration sections. Quote paths, use forward slashes and
preserve YAML indentation. Relative paths use R's working folder (`getwd()`).
Set `processed_file: null` if you do not want to save prepared input. Save it
outside the ERA5 source folder so later runs do not read it as a source file.
To use another configuration, run
`scoRch.simulator::run_scorch("C:/analysis/config.yaml")`.

**Accepted input:** a regular longitude-latitude NetCDF grid, in Celsius or
Kelvin, with complete daily Tmax in every retained cell. ERA5 input can be one
NetCDF file, a folder of NetCDF files or a YAML list of files. Hourly files need
complete UTC days on the same grid without overlapping dates. Daily ERA5 maxima
are also accepted; daily means are not daily Tmax. Obtain your local files
first; the software does not download ERA5 or require an API key.

Hourly processing takes each native point's maximum over 24 UTC hours, converts
Kelvin to Celsius, then averages complete blocks into the configured cell size.
For the 0.25-degree demonstration input, each 1-degree cell is the equal-weight
mean of 16 daily maxima, with weight 1/16 per point. This temperature aggregation
does not use area, latitude or temperature weights. Prepared Tmax retains its
existing grid; output can be saved at float64 precision. Temperature weighting
is used separately for ellipse centroids.

## Configuration

Start with `input`, `output` and `period`. Other settings retain the study
defaults unless you change them. A setting omitted from your file uses its
default; `null` dates use the available date range.

| Section | Settings and purpose |
|---|---|
| `input` | `file`, `format` (`auto`, `daily_tmax`, `era5`) and optional `processed_file`; `file: null` uses the example. |
| Input overrides | `variable`, `longitude`, `latitude`, `time` and `units` normally stay `auto`; use file names or `degC`/`K` when needed. `era5_cell_size_deg: 1` averages complete ERA5 blocks into 1-degree cells; `null` retains the native grid. |
| `output` | `directory` receives all results. |
| `period` | `warm_season_months`, `start_date`, `end_date`; months must form one contiguous season, which may cross New Year. |
| `heatwaves` | `threshold_percentile`, `min_duration_days`, `min_exceeding_days` control local detection. |
| `regional_days` | `percentile` selects days with extensive heatwave coverage. |
| `clustering` | `eps_values` and `min_samples_values` are the candidate DBSCAN settings. Distances are in grid-cell units. |
| `ellipses` | `scale_factor` sets the principal-component ellipse semi-axes. |
| `power_law` | `bootstrap_repetitions` and the two sample seeds control goodness-of-fit resampling. The study uses 5,000 repetitions; fewer are for exploratory checks. |
| `lgcp` | `covariates` selects from `lon`, `lat`, `mean_tmax`, `std_tmax`; `raster_km` and `window_margin_km` control the spatial model grid and window. |

Review scientific defaults when changing region, period, season or resolution.
Local thresholds are recalculated from each cell's Tmax over all analyzed dates.
The study detector bridges isolated below-threshold days and separates runs at
two consecutive below-threshold days. Temperature-weighted centroids require
positive Celsius Tmax in cluster cells. Model/window distances chosen for
1-degree data may need adjustment for another grid.

Automatic output always includes every event. Older representative-event
settings are accepted for manual comparison figures; they are absent from new
configuration files and do not select or block automatic event output.

## Demonstration data

The North America example covers **April–September 1996–2025**, with **1,800
one-degree cells** and **5,490 dates** between 25–50°N and 130–58°W, including
adjacent waters. Its data are archived separately on Zenodo:

[![Demo data DOI: 10.5281/zenodo.24232552](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.24232552-blue?style=flat-square)](https://doi.org/10.5281/zenodo.24232552)

| Download | Approximate size | Use |
|---|---|---|
| [Processed NetCDF](https://zenodo.org/records/24232552/files/SCORCH_NA_processed_1996_2025.nc?download=1) | 34 MB | Ready to analyze; the same data are included with the package. |
| [Unprocessed ERA5 ZIP](https://zenodo.org/records/24232552/files/SCORCH_NA_unprocessed_1996_2025.zip?download=1) | 6.33 GB | Extract the 30 yearly NetCDF files and use their folder as ERA5 input. |

Processing details, checksums and attribution are recorded in the
[data provenance](demo/PROVENANCE.txt). The two Zenodo files can be downloaded
independently. Use the processed file as `daily_tmax`, or extract the ZIP once
and point ERA5 input at its folder containing the 30 yearly NetCDF files.

## Results

Results go to `output/` in your project unless configured otherwise. **Open
Summary.txt first** for the completion status, warnings and main results.

| Example output | Contents |
|---|---|
| [Summary.txt](demo/output/Summary.txt) | Input, results, settings and file descriptions |
| [Heatwave statistics](demo/output/Heatwave_Statistics.xlsx) | Event types, thresholds, daily extent, duration trends and their annual data |
| [Catalogs 1–5](demo/output/catalogs/) | Local heatwaves, regional days, clusters, ellipses and compound events |
| [Models](demo/output/modeling/) | Power-law fits and the spatial model of ellipse centroids |
| [Figures](demo/output/figures/) | Every event, plus geometry, modeling, temporal and spatial plots |

The example produces **30 event images and 11 summary figures**. In
`figures/events/`, each type has its own folder and every event has one PNG
containing all its daily snapshots. Event IDs match Catalog 5 across all types;
`Event_001.png` means Event 1.

| Figure folder | Contents |
|---|---|
| `events/Type_1/` through `events/Type_4/` | One PNG per event, containing all dated daily snapshots and a shared legend; empty types retain an empty folder. |
| `geometry/` | Ellipse geometry by type and event distributions. |
| `modeling/` | Power-law fit and spatial centroid model. |
| `temporal/` | Annual event frequency and mean duration. |
| `spatial/` | Centroids and axes, overlap, axis ratio, area and orientation. |

Figures have no paper panel letters. Event numbering is chronological across the
entire catalog, not restarted by type. Leading zeros keep filenames sorted.
To redraw a saved result, use
`scoRch.simulator::scorch_figure(result, "event", event_id = 7, file = "Event_007.png")`.

### Reading the model results

Excel files contain structured tables with numeric values, dates and necessary
identifiers. Explanations and calculation status are in `Summary.txt`. Empty
cells mean missing or inapplicable values, not zero. Display rounding does not
change the stored numbers (up to Excel's 15 significant digits).

**Spatial model:** `Parameters` retains the original coefficients, spatial-field
variance and correlation scale in km. The numeric columns
`intensity_change_pct_per_1C` and `intensity_change_pct_per_1_degree` report
`100 * (exp(coefficient) - 1)` for the applicable predictors, holding other
predictors constant; they are blank for the intercept, variance and scale.
For example, a value of -0.958 means 0.958% lower fitted centroid intensity for
a +1 C increase in that predictor. `mean_tmax` is a cell's mean daily Tmax;
`std_tmax` is its temporal standard deviation, both over the analyzed dates.
These are spatial associations, not causal future-warming predictions.

In `Intensity`, `mean_intensity_per_km2` is the fitted expected number of
centroids per square kilometer over the analyzed catalog. It is not an annual
rate or heatwave probability. `relative_intensity_rank_0_1` is the cell's
low-to-high rank divided by the number of cells, with averaged ties: 0.9 is
approximately the 90th percentile, not a 90% chance. `Centroid Concentration`
counts all, daily-largest and event-largest centroids inside the highest
intensity zones covering 20% or 50% of the analyzed grid area, including waters
where present. These area-based zones need not equal rank cutoffs.

**Power law:** `Power Law Fits` reports the total sample size, observations in
the fitted upper tail, cutoff in km2, exponent, KS distance and bootstrap
goodness-of-fit p-value, separately for daily maxima and event maxima. A
completed calculation does not establish a good fit. Smaller KS distances mean
closer agreement; the test rejects the power law at p <= 0.1. A larger p-value
does not prove a perfect fit or establish that a power law is better than other
distributions. Results concern only areas at or above the fitted cutoff.

`Bootstrap Results` retains repetitions, undefined fits, exceedance counts,
seeds, p-value bounds, Monte Carlo standard error of p and exponent quantiles.
The lower/upper p-value bounds account for undefined bootstrap fits; they are
not confidence intervals. If any refit is undefined, the p-value is blank and
the summary marks the test unavailable. Exponent quantiles describe the
bootstrap distribution. They are not automatically a confidence interval.
The cutoff search follows the SCORCH reproducibility repository: every distinct
area except the largest is tried, with the full two-sided KS distance and the
smaller cutoff retained on a tie. There is no five-observation tail minimum.

**Duration trends:** `Annual Mean Duration` contains the Type 3 and Type 4 data
used for the trends, grouped by event start-season year. For a season crossing
New Year, the year is the year the season starts. Years without that event type
are omitted, not assigned zero duration. `years_with_events` counts the years
used, not the length of the study period. OLS is linear regression and Sen is
the median pairwise slope, both in days per decade. The Mann-Kendall test
assesses a monotonic trend at the 5% level; its S and Z statistics are retained.
At least three years with events are required. A nonsignificant result does
not establish zero change. `Summary.txt` reports the conclusions.

### Event types

Consecutive regionally extensive days form a compound event. A cluster has one
ellipse, so the type follows its daily cluster counts:

| Type | Pattern |
|---|---|
| 1 | One day with one cluster. |
| 2 | One day with several clusters. |
| 3 | Several days, each with one cluster, or each with several clusters. |
| 4 | Several days mixing those patterns, or including a day without a cluster. |

<img src="software/figures/compound_event_types.png" width="720" alt="The four SCORCH compound-event types">

Reruns replace package-generated files in the selected results folder. Choose a
different output directory to keep separate runs.

## Study and verification

The [SCORCH research repository](https://github.com/fawazbouhamad/compound-heatwave-typologies-SCORCH)
contains the original Eastern Mediterranean and Middle East study, 1940–2025.
This package provides the reusable software and is verified against its catalogs,
tables and model results. Checks run on Windows, macOS and Linux; reference
counts and developer instructions follow.

The North America reference has 65,409 local heatwaves, 140 selected days,
379 ellipses and 30 compound events (Types 1-4: 1, 5, 20 and 4). The bundled
processed input is byte-for-byte identical to the Zenodo float64 file. Full
30-year validation of the hourly archive reproduced its processed input and
scientific results. No additional rounding or integer packing is used.

<details>
<summary>Repository maintenance and verification</summary>

The installer assembles `software/scripts/` as R code and `software/help/` as R
help pages, with package metadata from `software/setup/`. These native R help
pages remain available through `?run_scorch`, `?read_tmax` and `?read_era5`.

From a downloaded repository, source `software/setup/install.R` and call
`install_scorch(path = ".")`. Run `Rscript demo/run.R` for the demonstration
and `Rscript software/setup/maintenance.R compare` to compare it with
`demo/output/`. All 18 worksheets are compared at portable tolerances of 1e-6,
or 1e-3 for LGCP results; elapsed time and local paths are normalized in the
summary comparison. Full-study checks require the separate study input.

Assemble a package with `Rscript software/setup/install.R .package`, then use
`devtools::check(".package")`. The installer defaults to stable `v1.2.0`;
developers can request `install_scorch(ref = "main")`. Dependency upgrades are
disabled unless requested. To restore the exact published demo input, run
`Rscript software/setup/maintenance.R make-demo path/to/SCORCH_NA_processed_1996_2025.nc`;
the command verifies the published checksum. Maintenance `--help` lists the
other checks. Data sources, checksums and licence terms remain in
[demo/PROVENANCE.txt](demo/PROVENANCE.txt).

The authors developed the method and study implementation. Claude Code and
Codex assisted with packaging, documentation, input interfaces, figures and
validation. The authors retain responsibility for the software and manuscript.

</details>

## How to cite

Use [CITATION.cff](CITATION.cff) or `citation("scoRch.simulator")` for the software
and study references. When using the North America example, also cite its
[Zenodo dataset](https://doi.org/10.5281/zenodo.24232552). The dataset DOI is
separate from the software release.

## Issues and comments

- For installation or usage problems, open an
  [issue](https://github.com/fawazbouhamad/scoRch.simulator/issues) with the error,
  R/package versions and configuration.
- For scientific questions, contact Fawaz Bouhamad or
  [Nasser Najibi](https://nassernajibi.com/).

## License

Code is licensed under [GPL-3.0-or-later](LICENSE.txt) and provided **AS IS**,
without warranty. Workflow artwork and the processed demonstration data are
CC BY 4.0; ERA5 attribution terms apply. See the
[data provenance](demo/PROVENANCE.txt) for sources and licence details.

---

<p align="center">
  <a href="https://nassernajibi.github.io/lab/"><img src="https://nassernajibi.github.io/lab/images/lab_graphical_abstract_1.png" height="180" align="middle" alt="Climate Resilience Lab"></a><a href="https://www.ufl.edu/"><img src="https://neurology.med.jax.ufl.edu/wordpress/files/2025/06/card-logo-uf-processed.png" height="180" align="middle" alt="University of Florida"></a>
</p>
<p align="center">
  <a href="https://nassernajibi.github.io/lab/">Climate Resilience Lab</a><br>
  Department of Agricultural and Biological Engineering<br>
  University of Florida, Gainesville, Florida
</p>
