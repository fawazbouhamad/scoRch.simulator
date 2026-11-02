# Data-building recipes

Run these maintainer recipes from the repository root after installing
scoRch.simulator.

| File | Purpose |
|---|---|
| [make_demo_data.R](make_demo_data.R) | Validate the 30 yearly daily-Tmax source files and build the compact 1996–2025 demonstration input |
| [arco_na_manifest.txt](arco_na_manifest.txt) | Source-file checksums and provenance |
| [make_test_fixtures.R](make_test_fixtures.R) | Build the study reference fixtures from the reproducibility repository |

```text
Rscript docs/development/data-raw/make_demo_data.R path/to/raw_daily_nc
Rscript docs/development/data-raw/make_test_fixtures.R path/to/reproducibility_repo
```

The compact demo uses 0.01°C packing; its existing file and numerical
references remain fixed. The full hourly archive and its double-precision
prepared counterpart are validated separately before integration. Ordinary
users supply local temperature files through the [common demo launcher](../../../demo/run.R)
or `run_scorch()`, as described in the [user guide](../../README.md).
