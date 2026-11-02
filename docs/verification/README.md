# Verification

The current processed demonstration preserves **65,790 local heatwaves,
140 selected days, 376 ellipses and 29 compound events** (Types 1–4:
1/4/20/4). Catalog, model and summary workbook paths and worksheet names
remain unchanged.

From the repository root with the package installed:

```text
Rscript demo/run.R
Rscript docs/verification/check_input_options.R
Rscript docs/verification/compare_output.R
```

The comparator reads all 16 worksheets in the eight workbooks, compares
values, dates and text, checks the run README apart from its timestamp,
and checks that all nine figures exist. Its portable numerical tolerances
are 1e-6, or 1e-3 for LGCP fits. Same-system preservation checks also compare
all worksheet values and full scientific result objects with zero tolerance.

## Current preservation checks

- The common launcher reproduced the complete 30-year prepared-data result:
  all eight workbooks and 16 worksheets matched the preserved baseline with
  zero numerical tolerance. The catalog and full power-law/bootstrap R
  objects also matched exactly.
- A repeat through the actual command-line configuration/input/output
  arguments again matched every worksheet exactly. The run README matched
  its updated reference apart from its timestamp, and all nine figures were
  present. README edits describe the current launcher/configuration names
  and the explicit prepared-input format.
- The common launcher also processed one completed genuine 1996 hourly
  season and reran its saved float64 prepared counterpart. Both routes
  produced identical catalog/model objects and all 16 worksheet values,
  with zero tolerance (183 dates, 1,800 one-degree cells). This integration
  check used 200 bootstrap repetitions on each route; both shipped demo
  configurations retain the study's 5,000-repetition default.
- All 80 protected source/default/data/reference files retained their exact
  bytes, including the eight reference workbooks and all nine reference
  PNGs. The [reference hashes](reference_hashes.json) record those checks
  against the pre-simplification commit.
- Rebuilding the installable R package produced the same payload as the
  build already checked on Windows, macOS and Linux, apart from its
  packaging timestamp. Calculation code and default settings were not
  changed by this launcher/documentation cleanup.

The current [GitHub workflows](https://github.com/fawazbouhamad/scoRch.simulator/actions)
run the package and processed-demo checks on published changes.

The local ERA5 check uses a small daily-max test fixture to verify local
file handling, spatial aggregation and an exact float64 round trip. It is
an internal test fixture. Hourly maxima are additionally tested in
`src/tests/testthat/test-era5.R`; the full 30-year hourly/prepared pair has
its own validation before integration.

The [research reproducibility repository](https://github.com/fawazbouhamad/compound-heatwave-typologies-SCORCH)
contains the study's independent scripts and results. Set
`SCORCH_REFERENCE_DIR` to its clone, with the study input at
`input/tmax_processed.nc`, to enable the full-study comparison in
`src/tests/testthat/test-reference-study.R`. `SCORCH_LONG_TESTS=1` enables
the long bootstrap test. The study input is archived separately at
[doi:10.5281/zenodo.21717874](https://doi.org/10.5281/zenodo.21717874).
