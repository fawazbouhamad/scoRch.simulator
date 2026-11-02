# scoRch.simulator 1.0.1 (unreleased)

- Local input supports prepared daily Tmax or ERA5 NetCDF files, including
  hourly-to-daily maxima, Celsius conversion and spatial aggregation.
  Reusable prepared input is saved in double precision.
- One configuration-driven workflow connects five catalog stages,
  power-law and LGCP models, eight workbooks, nine figures and a run README.
- The demonstration uses one launcher and two input configurations. The
  complete user instructions are consolidated into one guide.
- Superseded preprocessing examples and outdated instructions were removed
  from the current repository. The 30-year input, scientific source and
  numerical reference results are preserved.
- Figure styling follows the research reproducibility repository, including
  the dark red ellipse mesh and the updated power-law panels (b) and (d).
- The installable package is in src/. Install from GitHub with subdir = "src".
