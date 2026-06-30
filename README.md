# r4c

`r4c` provides tools for two connected rangeland workflows:

- range condition classification from six-factor field sheets
- dry fodder production and carrying-capacity estimation from CW, CY, and DS data

The package works with Excel workbooks or in-memory `data.frame` objects. It
also includes a small example workbook in `inst/extdata/r4c_data.xlsx`.

## Install

```r
install.packages("remotes")
remotes::install_github("babaknaimi/r4c")
```

For a local checkout:

```r
install.packages("devtools")
devtools::install(".")
```

## Quick Start

```r
library(r4c)

path <- system.file("extdata", "r4c_data.xlsx", package = "r4c")
```

### Range Condition

The example workbook stores condition sheets as `sfd1` and `sfd2`. In this
example they are treated as two consecutive years.

```r
condition <- classify_sf_template_years(
  path,
  sheets = c("sfd1", "sfd2"),
  year_names = c("Year 1", "Year 2"),
  score_profile = "template_rank"
)

condition$summary
```

Expected summary for the included workbook:

| year | total_score | condition_class |
|---|---:|---|
| Year 1 | 53 | Fair |
| Year 2 | 56 | Fair |

The `template_rank` profile is explicit and overridable. If your organization
uses a different official six-factor scoring table, pass revised `score_max`,
`count_reference`, `rank_fun`, or use `score_profile = "linear"`.

### Fodder Production and Livestock Units

The production workflow uses `cyd` and `dsd` as calibration data, then applies
those models to `re_cwd`.

```r
production <- estimate_fodder_production(path)

production_summary <- production$summary[c("method", "dry_fodder_kg_ha", "livestock_units")]
production_summary$dry_fodder_kg_ha <- round(production_summary$dry_fodder_kg_ha, 1)
production_summary$livestock_units <- round(production_summary$livestock_units)
production_summary
```

For the included workbook, the package returns approximately:

| method | dry_fodder_kg_ha | livestock_units |
|---|---:|---:|
| CW | 779.6 | 1516 |
| CY | 772.5 | 1502 |
| DS | 811.1 | 1577 |

The livestock-unit values use six grazing months, 60 kg average animal-unit
weight, 2% daily intake, 30 days per month, 700 ha range area, and 60% allowable
use. These defaults can be changed in `estimate_fodder_production()`.

## Validation and Plots

```r
wb <- read_r4c_excel(path, sf_sheets = c("sfd1", "sfd2"))
cc <- carrying_capacity(wb$cyd, wb$dsd)
validation <- validate_methods(wb$cwd, cc, test = "kruskal")

plot_validation(validation, type = "boxplot")
plot_validation(validation, type = "agreement")
```

See `vignettes/get-started.Rmd` for a longer tutorial with tables, plots, and a
discussion of the scoring assumptions.
