# Maintainer notes

The [user guide](../README.md) is the single source of user instructions.
This folder contains only current data-building and release material.

## Data recipes

[data-raw/](data-raw/) contains the compact 30-year data builder, its source
checksum manifest and the study test-fixture builder. Run these recipes
from the repository root with the package installed. Preserve the bundled
input and reference workbooks when deriving the full hourly/prepared pair;
its independent validation belongs to that pair's deposit.

## Tests and release

1. Update version metadata consistently in `src/DESCRIPTION`, `src/NEWS.md`,
   `CITATION.cff`, `src/inst/CITATION`, `.zenodo.json` and
   [codemeta.json](codemeta.json). Set publication dates when releasing.
2. If R help changes, regenerate it with `roxygen2::roxygenise("src")`.
3. Run `devtools::check("src", args = c("--no-manual", "--run-donttest"))`,
   `Rscript demo/run.R`, the local-input check and the workbook comparator
   listed under [verification](../verification/README.md).
4. For full-study validation, set `SCORCH_REFERENCE_DIR` to the research
   repository with its archived input at `input/tmax_processed.nc`.
   `SCORCH_LONG_TESTS=1` enables the long bootstrap check.
5. Review the current input/reference hashes and exact worksheet comparison,
   then wait for GitHub's package and demo workflows on the tested commit.
6. Tag the tested commit and archive the software release separately from
   the input dataset. Review citation, licence and related identifiers
   before publishing. Add the assigned software DOI to citation metadata.

The complete R package is installed from `src/`. CI workflows stay in
`.github/workflows/`, while generated local results are excluded by
`.gitignore`. [Contribution instructions](../../.github/CONTRIBUTING.md)
describe code changes.

## SoftwareX submission material

Use the journal's current
[Guide for Authors](https://www.sciencedirect.com/journal/softwarex/publish/guide-for-authors)
and [Original Software Publication template](https://legacyfileshare.elsevier.com/promis_misc/softwarex-osp-template.tex)
for the article. The repository provides its public code, licence, purpose,
installation instructions, illustrative example, verification, citation and
support location. The installable source is in `src/`.

For the code metadata table, record: code version, permanent tested-commit
or release link, GPL-3.0-or-later licence, Git versioning, R (>=4.1),
dependencies from `src/DESCRIPTION`, documentation and support links. Use the
actual tested release and assigned DOI; the current 1.0.1 version is pending
a tagged release, and the new hourly dataset has no published DOI yet.

The working Word manuscript is maintained separately. The current
repository does not include an alternative journal-format draft.

## AI assistance

The scientific method, R implementation and study tests are attributed to
the authors, who reported using AI coding assistance in their development
work. Documented additional assistance with Claude Code and Codex covered
repository and release packaging, demo and documentation work, figure-style
alignment, input-interface work and validation.
The authors remain responsible for scientific interpretation and the final
software and manuscript. Use this record when preparing the article's
required disclosure.
