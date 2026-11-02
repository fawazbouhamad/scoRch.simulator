# Demonstration

Choose an input route. Both lead to the same five catalog stages, power-law
and LGCP models, eight Excel workbooks and nine figures. All commands below
run from the repository's top folder after installing the package with
`remotes::install_github("fawazbouhamad/scoRch.simulator", subdir = "src")`.

| File or folder | Purpose |
|---|---|
| [run_demo.R](run_demo.R) | Complete prepared-data run, a traced real event and a stage-by-stage explanation |
| [processed_config.yaml](processed_config.yaml) | Prepared input, study settings and output location |
| [run_hourly_demo.R](run_hourly_demo.R) | Full hourly preprocessing followed by the complete analysis |
| [hourly_config.yaml](hourly_config.yaml) | Hourly input, April–September 1996–2025 and one-degree aggregation |
| [results/](results/) | Browse the preserved processed-demo catalogs, model tables and figures |

## Option 1: prepared daily Tmax

```text
Rscript demo/run_demo.R
```

The unchanged bundled file covers 25–50°N, 130–58°W: 1,800 one-degree cells,
5,490 dates, April–September 1996–2025, about 12 MB. It is already daily
maximum temperature in Celsius and needs no hourly preprocessing. The full
run retains the study settings (5,000 bootstrap repetitions with fixed
seeds) and writes to `results/`.

Expected baseline: **65,790 local heatwaves, 140 selected days, 376 ellipses
and 29 compound events**, with 1/4/20/4 events of Types 1/2/3/4. The
[detailed walkthrough](../docs/walkthrough.md) follows the June–July 2012
event through every catalog.

## Option 2: hourly ERA5, then the complete analysis

```text
Rscript demo/run_hourly_demo.R path/to/hourly_folder
```

Use a folder containing only the full demonstration's hourly NetCDF files.
The package obtains the maximum of all 24 hours of each UTC day at each
0.25-degree grid point, converts kelvin to Celsius, and averages complete
4×4 blocks into one-degree cells. It then runs all catalog stages and both
models with the same settings. Output goes to `results/hourly/`, including
`prepared_tmax.nc`, saved in double precision for reuse.

**Availability:** the genuine 30-year hourly archive and its directly derived
prepared counterpart are being acquired and checked for a separate Zenodo
deposit. No new DOI is published yet. The runner requires these local data;
it does not download them or request an API key. The completed monthly
processing example remains available as a [maintainer recipe](../docs/development/data-raw/).

When the full pair is available, run its supplied prepared counterpart with:

```text
Rscript demo/run_demo.R path/to/matching_prepared.nc results/paired_prepared
```

Optional output-directory arguments let both runs coexist. For an exact
input-route comparison, compare the hourly run with **its own matching
double-precision prepared file**. The existing compact historical input is
rounded to 0.01°C; rounding can change percentile ties, so its reference
counts must be retained separately. It will not be replaced by the new pair.

## Browse the results

- [Catalogs 01–05](results/Heatwave_Catalogs/): local heatwaves, regional days,
  clusters, ellipses and compound events.
- [Models](results/Modelling/): power-law fits and LGCP parameters and maps.
- [Additional results](results/Additional_Results/): type summaries,
  thresholds, daily extent and trends.
- [Figures](results/figures/) and the [output guide](results/README.txt).

For your own prepared or hourly files, use `write_scorch_config()` and
`run_scorch()` as shown in the [user guide](../docs/guide.md). The demonstration
scripts include domain and event choices specific to this US example.
