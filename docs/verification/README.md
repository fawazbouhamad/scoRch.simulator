# Verification

[Cleanup preservation check, 2 November 2026](cleanup_2026-11-02.md): three complete processed-data runs, zero numerical differences, and file checksums.



The [historical verification record](history.md) documents comparisons with
the research reproducibility repository and earlier package checks.

From the repository root, with the package installed:

```text
Rscript demo/run_demo.R
Rscript docs/verification/check_input_options.R
Rscript docs/verification/compare_output.R
```

The comparator reads every sheet of all eight workbooks, compares dates,
text and values, checks the run README apart from its timestamp, and checks
the presence of all nine figures. For platform portability its numerical
tolerances are 1e-6, or 1e-3 for LGCP fits. Cleanup verification on the same
Windows/R installation additionally compares all worksheet values and the
full scientific R objects with **zero tolerance**.

Expected processed-demo counts: 65,790 local heatwaves, 140 selected days,
376 ellipses and 29 compound events (Types 1–4: 1/4/20/4). The reference
workbooks in [demo/results](../../demo/results/) preserve their original
bytes. The approved power-law plot styling update changes only the figure.
