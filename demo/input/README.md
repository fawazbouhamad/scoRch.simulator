# Prepared demo input

[north_america_tmax.nc](north_america_tmax.nc) is the preprocessed temperature file used by the default US demonstration and the supplied [example output](../output/).

| Property | Value |
|---|---|
| Variable | Daily maximum 2-m air temperature (Tmax), degrees Celsius |
| Coverage | 25–50°N, 130–58°W |
| Grid | 1° × 1°; 1,800 cells |
| Period | April–September, 1996–2025; 5,490 dates |
| File size | 12,445,463 bytes (12.45 MB; 11.87 MiB) |
| Storage | 16-bit integers with a 0.01°C scale factor |
| MD5 | `526639b3a874be74fce28cee968ef36b` |

The authors prepared the file from ARCO-ERA5 hourly temperatures by taking the maximum of each UTC calendar day at 0.25° resolution, then averaging those daily maxima over complete 4 × 4 blocks to obtain one-degree cells. The temperatures are real ERA5-derived data.

## Run the prepared example

After installing the package, run this in R or RStudio:

```r
result <- scoRch.simulator::run_demo()
```

The package includes an identical copy, so this command needs no repository download or separate input selection.

To use this repository file explicitly, run from the repository root:

```r
result <- scoRch.simulator::run_demo(
  input = "demo/input/north_america_tmax.nc"
)
```

Results are written to `output/` in R's current working directory. Add `output = "C:/path/to/my_results"` to choose a destination.

## Provenance and maintenance

See the [data provenance and licence](../../software/supporting_files/example_data/PROVENANCE.txt) and [source-year checksums](../../software/supporting_files/maintenance/demo_data_sources.txt).

Contains modified Copernicus Climate Change Service information 2026. Neither the European Commission nor ECMWF is responsible for any use of the information it contains. The authors' processing is CC BY 4.0; the underlying ERA5 data retain the applicable Copernicus/CC BY 4.0 attribution terms.

This file is byte-for-byte identical to [the package's source data](../../software/supporting_files/example_data/north_america_tmax.nc). When updating it, keep both paths synchronized and regenerate and verify the reference output.
