# scoRch.simulator

**Turn temperature data into regional heatwave catalogs, models and figures.**
Developed by Fawaz Bouhamad and Nasser Najibi using SCORCH: Spatiotemporal Classification of Regional Compound Heatwaves.

## Start here

You need **R 4.1 or newer**. Open R or RStudio and follow these three steps.

**1. Install once.** Paste this into the R console:

```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("remotes")
source("https://raw.githubusercontent.com/fawazbouhamad/scoRch.simulator/main/.github/scripts/package.R")
install_scorch()
```

**2. Run the example.** Paste:

```r
scoRch.simulator::run_demo()
```

The package includes the prepared US data. You do not need to download this repository, choose a configuration or edit any files.
The example covers April–September 1996–2025: 1,800 one-degree cells and 5,490 dates, in a file of about 12 MB.
It uses the study settings, including 5,000 bootstrap repetitions and fixed seeds.

**3. Open your results.** Wait for the run to finish. R prints the full folder path to open.
By default, the folder is called `output` inside your current working folder. To choose a destination:

```r
scoRch.simulator::run_demo(output = "C:/path/to/my_results")
```

## Find your results

You can also browse the [supplied example output](demo/output/) before running anything.
The catalog names and numbering follow the [reproducibility repository](https://github.com/fawazbouhamad/compound-heatwave-typologies-SCORCH).

| Open the result | What you will find |
|---|---|
| [Catalog 1: heatwave events](demo/output/catalogs/01_heatwave_events.xlsx) | Dates, duration, peaks and bridged days for every cell |
| [Catalog 2: regionally extensive heatwaves](demo/output/catalogs/02_regionally_extensive_heatwaves.xlsx) | Selected days, cell counts and selected cells |
| [Catalog 3: extreme heatwave clusters](demo/output/catalogs/03_extreme_heatwave_clusters.xlsx) | Cluster characteristics and cell membership |
| [Catalog 4: extreme heatwave ellipses](demo/output/catalogs/04_extreme_heatwave_ellipses.xlsx) | Area, axes, orientation, centroids and temperatures |
| [Catalog 5: compound heatwave events](demo/output/catalogs/05_compound_heatwave_events.xlsx) | Event type, dates, duration, geometry and event-day details |
| [Power-law model](demo/output/modeling/Power_Law_Model.xlsx) | Fits, exponents and bootstrap results |
| [Spatial risk model](demo/output/modeling/Spatial_Risk_Model.xlsx) | LGCP parameters, spatial intensity and centroid concentration |
| [Heatwave statistics](demo/output/Heatwave_Statistics.xlsx) | **Event Type Summary** (study Table 1), **Cell Thresholds**, **Daily Heatwave Extent**, **Duration Trends** |
| [Figures](demo/output/figures/) | Nine figures, including the power-law and spatial risk models |
| [Run summary](demo/output/Run_Summary.txt) | Input, counts, models, warnings and a walkthrough of an observed event |

The spatial risk model describes heatwave-centroid occurrence intensity; exposure and vulnerability are not modelled.
The run summary records unavailable models or figures and incomplete runs. Workbooks retain their full tables and stored values.

## Try hourly processing

If you have the matching US hourly ERA5 NetCDF files, run:

```r
scoRch.simulator::run_demo("hourly", input = "C:/path/to/hourly_folder")
```

The package converts **hourly temperature → daily Tmax → one-degree cells**, then runs the same analysis.
At 0.25° input, daily maxima from each complete 4×4 block are averaged into a 1° cell.
Results go to `output/hourly/`, including a reusable `prepared_tmax.nc` file. It uses local files and requires no API key.

**The prepared example is available now.** The complete 30-year hourly archive and its matching prepared file are still being acquired and validated for a separate data deposit.
The bundled compact file is rounded to 0.01°C; an exact hourly/prepared comparison uses the float64 prepared file saved from those same hourly inputs.

## Use your own data

For your own region and period, create an editable configuration:

```r
library(scoRch.simulator)
write_scorch_config("scorch_config.yaml")
# Open the YAML: set input.file, input.format and your period/grid settings.
result <- run_scorch("scorch_config.yaml")
```

Use `input.format: daily_tmax` for prepared daily Tmax or `input.format: era5` for local ERA5 NetCDF files.
Hourly input must contain complete UTC days on a shared regular grid, without overlapping dates.
Omitted settings retain the study defaults. `?run_scorch`, `?read_tmax` and `?read_era5` give the detailed options in R.

<details>
<summary><strong>See the workflow</strong></summary>

Temperature preparation → local heatwaves → regionally extensive days → clusters → ellipses → compound events → models, workbooks and figures.

<img src="software/helpers/figures/scorch_workflow.png" width="600" alt="SCORCH software workflow">

</details>

<details>
<summary><strong>For developers: source and verification</strong></summary>

`software/` contains `scripts`, `helpers` (function documentation), `supporting_files` and `tests`.
The installer assembles these into R's standard package layout in a temporary folder; `demo/run.R` calls the same installed `run_demo()` function.
To install a downloaded repository, source `.github/scripts/package.R` and call `install_scorch(path = ".")`.
From a local clone, run `Rscript demo/run.R`, then `Rscript .github/scripts/check_input_options.R` and `Rscript .github/scripts/compare_output.R`.
[GitHub Actions](https://github.com/fawazbouhamad/scoRch.simulator/actions) checks Windows, macOS and Linux and compares the demo output with `demo/output/`.
The fixed prepared reference has 65,790 local heatwaves, 140 selected days, 376 ellipses and 29 compound events.
Comparisons cover all 16 worksheets; portable tolerances are 1e-6, or 1e-3 for LGCP fits. Same-system preservation checks use zero tolerance.
Source/fixture build scripts and their manifest are in `.github/scripts/`; run them from the repository root. `Rscript .github/scripts/package.R .package` assembles the package for `devtools::check(".package")`.
GitHub and Zenodo read `CITATION.cff`.
Before release, synchronize version/citation/archive metadata, verify the reference results and tag the tested commit. Version 1.0.1 is not yet a tagged release.
The authors developed the method and study implementation; Claude Code and Codex assisted with packaging, documentation, input interfaces, figure alignment and validation. The authors retain responsibility for the software and manuscript.

</details>

## Citation and support

Use [CITATION.cff](CITATION.cff) or `citation("scoRch.simulator")`. Code is [GPL-3.0-or-later](LICENSE.txt); [data provenance and attribution](software/supporting_files/extdata/PROVENANCE.txt) are retained with the inputs.
The EMME study dataset is archived separately on [Zenodo](https://doi.org/10.5281/zenodo.21717874).
For help, open an [issue](https://github.com/fawazbouhamad/scoRch.simulator/issues) with your R/package versions, configuration and error.
