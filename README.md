# Spanish mobility data validation

This repository contains the reproducible workflow and Tartu 2026
presentation for validating Spanish mobile-phone based mobility data against official
census commuting and population benchmarks.

The repository is a focused public extract. It intentionally excludes unrelated
experiments, adjustment methods, covariates such as income, private history, and
downloaded source data.

## Contents

- `_targets.R`: analysis pipeline.
- `R/`: helper functions used by the pipeline.
- `figures/`: the three generated figures used by the presentation.
- `presentation/`: Quarto RevealJS source, media, and rendered HTML.

## Reproduce the workflow

Requirements:

- R 4.6.0 or a compatible recent R release
- Quarto for rendering the presentation
- Enough disk space and network access for approximately 1.6 GB of source data

Restore the R environment:

```r
renv::restore()
```

Run the complete workflow:

```r
targets::tar_make()
```

By default, downloaded files are cached in the gitignored `data-cache/`
directory. To use another location:

```bash
export SPANISH_OD_DATA_DIR=/absolute/path/to/cache
Rscript -e 'targets::tar_make()'
```

Render the presentation:

```bash
quarto render presentation/presentation.qmd
```

The committed figures and `presentation/presentation.html` allow the results to
be viewed without downloading the source data or rerunning the workflow.

## Data and methods

The workflow downloads municipality-level mobility data through
[`spanishoddata`](https://cran.r-project.org/package=spanishoddata), commuting
benchmarks through [`ineapir`](https://cran.r-project.org/package=ineapir), and
2022 municipal population through
[`ineAtlas`](https://cran.r-project.org/package=ineAtlas). Bias measurements use
[`debiasR`](https://github.com/de-bias/debiasR).

The analyzed period is Monday, March 6, 2023 through Sunday, March 12, 2023.
Downloaded source data and the `targets` object store are not committed.

## License

Unless otherwise noted, this repository is licensed under
[CC BY 4.0](LICENSE.md).
