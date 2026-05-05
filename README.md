# r4c

`r4c` turns the original standalone workflow into an R package for:

- range condition classification with replicated SF inputs
- carrying capacity estimation with CY and DS data
- optional CW-based validation

The package is designed to work with either Excel files or in-memory `data.frame`
objects, so it fits both template-driven field workflows and scripted analysis.

## Install locally

```r
install.packages("devtools")
devtools::install(".")
```

## Quick start

```r
library(r4c)

ex <- r4c_example_data()

classify_range_condition(
  ex$sf_replicates,
  palatability = ex$palatability,
  indicative_production = 60
)

cc <- carrying_capacity(ex$cyd, ex$dsd)
validate_methods(ex$cwd, cc)
```

## Excel workflow

```r
wb <- read_r4c_excel("my_r4c_template.xlsx")

classify_range_condition(
  wb$sf,
  palatability = c(SpeciesA = 1, SpeciesB = 2, SpeciesC = 3),
  indicative_production = 80
)

carrying_capacity(wb$cyd, wb$dsd)
```

## Notes on range condition defaults

The source files in this workspace described the six-factor workflow, but did
not include the full original scoring table. The package therefore uses:

- transparent linear scoring for each factor
- equal default factor weights
- customizable class thresholds
- `TCP` as the default proxy for present production when an indicative-state
  reference is not supplied

You can override these assumptions through function arguments.
