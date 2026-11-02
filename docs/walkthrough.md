> For the current two-input demonstration, start at [demo/](../demo/README.md). This detailed guide retains the historical compact baseline and the earlier monthly preprocessing example.

# Demonstration: the whole SCORCH analysis on North America in half a minute

This folder runs the complete analysis on the data shipped with the package,
the way it is run on any dataset: temperature data, one configuration file
and one command. It is meant as a first walk-through for students and new
users. The optional hourly preprocessing example uses genuine ERA5 NetCDF data.
Users can also provide files they have downloaded themselves. The package performs the preprocessing; it does not request data
from the Climate Data Store and does not read credentials.

| File | What it is |
|---|---|
| [`run_demo.R`](../demo/run_demo.R) | the commented demonstration: reads the data, runs `run_scorch()`, prints what was found, traces one real heat wave from the temperature data to its figure, then repeats the analysis one stage at a time |
| [`scorch_config.yaml`](../demo/processed_config.yaml) | the configuration: it names the input data, the output folder and the events to show in the figures; every other setting keeps its SCORCH study value |
| [`run_hourly_demo.R`](../docs/development/data-raw/run_monthly_processing.R) | a full-US, July 2012 preprocessing demonstration: hourly ERA5 to daily Tmax, Celsius and 1-degree cells, checked against the paired prepared file |
| [`check_input_options.R`](verification/check_input_options.R) | an offline local-file aggregation and prepared-NetCDF round-trip check using the small shipped ERA5 daily file; this file is daily, so the check is not presented as an hourly demonstration |
| [`compare_output.R`](verification/compare_output.R) | compares a run's `results/` with `demo/results/` sheet by sheet; the demo workflow runs it on every change |
| [`demo/results/`](../demo/results) | eight reference workbooks, the `README.txt` (with the traced event), and all nine figures; the numerical references are unchanged and the figure styling was aligned with the reproducibility repository on 1 November 2026 |

**The data.** `src/inst/extdata/north_america_tmax.nc`: ERA5-derived daily
maximum 2-m temperature on 25 x 72 one-degree cells between 25 N and 50 N and
between 130 W and 58 W, every April-September day of 1996-2025 (1,800 cells,
5,490 dates). The box holds the contiguous United States together with
southern Canada up to 50 N, northern Mexico, the Great Lakes and the adjacent
Pacific, Atlantic and Gulf of Mexico waters; sea cells are analysed like the
others, as in the study region of the SCORCH paper. The source, the
processing and the licence are in
[`src/inst/extdata/README.md`](../src/inst/extdata/README.md). These are real
reanalysis data for a regional sample, not an analysis of all of North
America.

## Run it

From the top folder of the repository, after installing the package
(`remotes::install_github("fawazbouhamad/scoRch.simulator", subdir = "src")` or
`devtools::install("src")`):

```
Rscript demo/run_demo.R
```

or, in R, `source("demo/run_demo.R")`. The run took 43 seconds on a laptop
(the analysis about 16 seconds with the study's 5,000 bootstrap repetitions,
the eight workbooks with their archive checks 7 seconds, the nine 500/600-dpi
figures 10 seconds, the traced event and the stage-by-stage repeat the rest).
Then:

```
Rscript docs/verification/check_input_options.R
```

## Demonstrate the hourly processing

The optional data archive contains the whole US domain (25-50 N, 130-58 W)
for **every hour of July 2012**, at 0.25 degrees, plus the prepared daily
file derived from those exact hourly values. After extracting the archive:

```text
Rscript docs/development/data-raw/run_monthly_processing.R path/to/extracted_archive
```

This calls the same `read_era5()` preprocessing used by `run_scorch()`:
744 hourly fields become 31 daily UTC maxima at the native grid points,
kelvin is converted to Celsius, and those daily maxima are averaged to
1,800 complete one-degree cells. The script compares the result with the
paired prepared file and checks an exact-value NetCDF round trip, then saves
`results/processing_demo/prepared_tmax.nc`. It reads no credentials.

**Why one month?** The broad spatial domain is retained while the optional
download stays small. A month demonstrates temporal and spatial processing;
it does not supply a long-term percentile baseline. Use `run_demo.R` and the
separate 30-year prepared file for the full heatwave catalogs and models.
For your own hourly record, point `input: file` at your NetCDF file(s) or
folder, set `format: era5`, and run `run_scorch()` normally.

**Data availability.** The 30-year prepared example is installed with the
package. The optional paired monthly archive has been prepared separately
for a Zenodo deposit; no new dataset DOI has been published yet. It contains
the hourly month, its matching prepared month, and a copy of the 30-year
quick-demo file with its provenance. The maintainer recipe
[`docs/development/data-raw/prepare_demo_archive.R`](development/data-raw/prepare_demo_archive.R)
rebuilds this deposit from the genuine hourly file. The separate maintainer
[`fetch_hourly_demo.py`](development/data-raw/fetch_hourly_demo.py) retrieves the
public ARCO-ERA5 source; ordinary package use never invokes that script.

The historical compact 30-year file rounds values to 0.01 C. The new paired
monthly file preserves the computed double-precision values, so the two
monthly input routes match exactly. Rounding can change percentile ties;
the compact historical file is not used as the exact-value reference for
the monthly processing demonstration.

## What you should see

The script prints the input, the progress of the stages and the summary:

```
<scorch_tmax> 1800 cells (129.5°W to 58.5°W, 25.5°N to 49.5°N, 1 x 1 degree grid), 5490 dates (1996-04-01 to 2025-09-30)
...
<scorch_result> 1800 cells, 5490 dates; 65790 local heatwaves; regional threshold 353 cells;
  140 selected days, 376 clusters/ellipses, 29 compound events (Type 1-4: 1/4/20/4)
  outputs in results

Compound heatwave events by type (the Event Type Summary sheet):
                   compound_heatwave_type selected_event_days number_of_compound_events longest_event_days ...
            Type 1: Widespread (Isolated)                   1                         1                  1
              Type 2: Spatially Clustered                   4                         4                  1
             Type 3: Temporally Clustered                 105                        20                 28
 Type 4: Compound Clustering (Multi-Type)                  30                         4                 16
```

How to read it: in 30 warm seasons, 140 days had an unusually large number of
cells in a heatwave (353 or more of the 1,800, the 97.5th percentile of the
daily counts). Their heatwave cells formed 376 clusters, each described by an
ellipse. Runs of consecutive selected days gave 29 compound events: one
single day with one cluster (Type 1), four single days with several clusters
(Type 2), 20 multi-day events with the same kind of structure every day
(Type 3) and four multi-day events that mixed single- and multi-cluster days
(Type 4). All four types occur in these data; that is a property of this
region and period, not a guarantee for other datasets.

## One real event, traced

Part 4 of the script follows compound event 7, the heat wave of 28 June to
4 July 2012, through the five catalogs (the TRACED EVENT section of
`results/README.txt`, copied in `demo/results/README.txt`). NOAA's national climate report for June 2012
describes this event: a large heat wave over the central United States, the
Midwest and the Southeast in the last days of June, with 94 daily records
broken on 28 June alone
([NOAA/NCEI, June 2012](https://www.ncei.noaa.gov/sotc/national/2012/6)).
In these data:

- Catalog 2: on each of the seven days, 359 to 386 of the 1,800 cells were in
  a local heatwave, above the regional threshold of 353.
- Catalog 3: DBSCAN grouped them into one cluster per day (357 to 385 cells),
  so the event is Type 3, temporally clustered.
- Catalog 4: the largest ellipse, on 1 July 2012, is 2,203 km by 1,218 km
  (2.1 million km2), nearly east-west, with its temperature-weighted
  centroid at 39.5 N, 92.1 W in northern Missouri.
- Catalog 1: the hottest cell of that ellipse, 33.5 N and 81.5 W in South
  Carolina, has a 95th-percentile threshold of 35.84 degC; its local heatwave
  ran from 28 June to 5 July with a peak of 42.20 degC on 30 June, one day
  under the threshold being bridged.
- The temperature table shows that cell's daily maxima before, during and
  after the heatwave, with the days that were selected and that belong to
  the event.

![The first two days of two Type 3 events](../demo/results/figures/consistent_multiday_events.png)

*`consistent_multiday_events.png`, laid out like Figure 6 of the study: the top row
is the traced event, 28 and 29 June 2012, one cluster a day; the bottom row is
the Type 3 event of 4-31 August 2023, several clusters a day
(`demo/processed_config.yaml` names both under `figures: representative_events`).
Orange cells are the heatwave cells that DBSCAN put in a cluster, grey cells
are heatwave cells it left out, the brown mesh marks the ellipse interior,
the black outline and lines show the ellipse and its two axes, and the dot
is the temperature-weighted centroid. The map shows the surroundings of the
analysed box; each date is written inside the upper-left corner of its map.*

## The results folder

```
results/
  Heatwave_Catalogs/    Heatwaves.xlsx, Regional_Heatwaves.xlsx, Heatwave_Clusters.xlsx,
                        Heatwave_Ellipses.xlsx, Compound_Heatwaves.xlsx: the five SCORCH catalogs
  Modelling/            Power_Law.xlsx, LGCP.xlsx: the power-law fits and the log-Gaussian Cox process
  Additional_Results/   Analysis_Summary.xlsx: event type summary, cell thresholds, daily heatwave
                        extent, duration trends
  figures/              nine descriptively named plots
  README.txt            input, settings, counts, models, every file and sheet with its row count,
                        the traced event (added by part 4 of run_demo.R), run time and warnings
```

`annual_event_counts_and_duration.png` counts the events of each type per
year, `mixed_multiday_events.png` shows six days of the longest Type 4 event
(15-30 July 2023), `single_day_events.png` the Type 1 and Type 2 examples,
and `centroid_concentration.png` maps where the ellipse centroids concentrate
according to the log-Gaussian Cox process.

## Next steps

To analyse your own data, write the complete, commented configuration with
`write_scorch_config("scorch_config.yaml")`. Either point `input: file` to a
daily maximum temperature file or to ERA5 NetCDF files you downloaded
yourself. Set `input: format` to `daily_tmax` or `era5`, or leave `auto`
to detect the format. The package does the required preprocessing locally. Then run
`run_scorch("scorch_config.yaml")`.
