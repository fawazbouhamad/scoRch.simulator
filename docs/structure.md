# Software structure

`src/` is the complete installable R package. The repository's three main
folders separate software, demonstrations and supporting material.

| Step | What happens | Main source |
|---|---|---|
| Controller | Reads YAML, connects preprocessing, five stages, models and exports | [run.R](../src/R/run.R), [config.R](../src/R/config.R) |
| Temperature preparation | Reads prepared daily Tmax, or reduces local ERA5 hourly files to UTC daily Tmax and one-degree Celsius cells | [era5.R](../src/R/era5.R), [input.R](../src/R/input.R), [write_tmax.R](../src/R/write_tmax.R) |
| Catalogs 1–5 | Detects heatwaves, selects extensive days, clusters cells, fits ellipses and groups compound events | [heatwaves.R](../src/R/heatwaves.R), [regional_days.R](../src/R/regional_days.R), [clustering.R](../src/R/clustering.R), [ellipses.R](../src/R/ellipses.R), [events.R](../src/R/events.R) |
| Modelling | Fits the power-law tail and bootstrap; fits the LGCP and centroid concentration | [power_law.R](../src/R/power_law.R), [lgcp.R](../src/R/lgcp.R) |
| Workbooks | Writes all tables into the eight XLSX files and records run details | [workbooks.R](../src/R/workbooks.R), [readme.R](../src/R/readme.R) |
| Figures | Draws the nine figures from the computed results and shared styling | [figures.R](../src/R/figures.R), [figures_stats.R](../src/R/figures_stats.R), [figure_style.R](../src/R/figure_style.R) |

The five stage functions are `detect_heatwaves()`, `select_regional_days()`,
`cluster_heatwaves()`, `fit_ellipses()` and `classify_events()`. Modelling
comes after those stages. It is part of `run_scorch()`, alongside data
preparation and export; the five catalogs do not describe the whole program.

`src/inst/` contains installed configurations, data and citation information;
`src/man/` contains R help; `src/tests/` contains automated checks. These are
standard parts of an R package. Install from the repository's `src/`
subdirectory or run `R CMD INSTALL src` from a local clone.

`demo/` contains the two runners and configurations. The prepared default
uses the unchanged historical dataset. The hourly runner uses locally
supplied files and saves its exact prepared counterpart. Both call the same
analysis functions with the same defaults. See [demo availability and
precision](../demo/README.md) before comparing datasets.

`docs/verification/` holds comparison scripts and evidence.
`docs/development/data-raw/` holds maintainer data-building recipes; users
do not need those recipes to run local data through the package.

The root `.github/` folder runs automated checks, `.gitignore` excludes local
generated files, and `.zenodo.json` carries archive metadata. GitHub displays
committed dotfiles; the clean research repository has fewer of these files
because it has a different packaging and automation role.
