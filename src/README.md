# Software

This folder is the complete **scoRch.simulator** R package. The repository's
[main page](../README.md) links to the [demonstrations](../demo/) and
[documentation](../docs/).

Install from GitHub in R:

```r
remotes::install_github("fawazbouhamad/scoRch.simulator", subdir = "src")
```

Or install a local clone from the repository root:

```text
R CMD INSTALL src
```

| Folder | Contents |
|---|---|
| [R](R/) | Preprocessing, five catalog stages, models, exports and figures |
| [inst](inst/) | Installed configuration, example data and citation |
| [man](man/) | R help pages and explanatory artwork |
| [tests](tests/) | Automated checks and study reference fixtures |

The [structure guide](../docs/structure.md) maps each task to its source file.
