# Demo

One launcher, two input configurations. Run from the repository root after
installing scoRch.simulator.

| Choose | Command |
|---|---|
| Prepared daily Tmax (bundled) | `Rscript demo/run.R` |
| Hourly ERA5 (local files) | `Rscript demo/run.R demo/config/hourly.yaml path/to/hourly_folder` |

| File or folder | Purpose |
|---|---|
| [run.R](run.R) | The common launcher for both inputs |
| [config/](config/) | Prepared and hourly example configurations |
| [results/](results/) | Reference catalogs, model workbooks and figures |

The [single user guide](../docs/README.md) covers input requirements, the
complete workflow, settings, a traced event, all output files, and data
availability. The full 30-year hourly archive is being prepared separately;
the bundled prepared example is available now.
