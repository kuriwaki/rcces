# rcces

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`rcces` is a set of helper functions to format, clean, and operationalize variables used in Shiro Kuriwaki's CCES-based representation research. It supports the data pipeline and analysis scripts in those replication packages.

## Functions

**Reshaping and cleaning survey data**

- `melt_cces()` — Reshape wide CCES data (one column per question) into long format (one row per respondent-question).
- `parse_qlabel()` — Parse camelCase question labels into readable text for plotting (e.g., `"RepealACA2017"` to `"Repeal ACA 2017"`).
- `std_short_party()` — Standardize party labels to short codes (`"D"`, `"R"`, etc.).
- `std_party_prcp_varname()` — Standardize party perception variable names across CCES years.

**Computing agreement and opinion**

- `dyad_agrmt()` — Compute dyadic agreement (+1/0/-1) between a constituent's survey response and their legislator's roll call vote.
- `code_threeway()` — Code agreement between any two Y/N/DK or D/R/I variables as +1, 0, or -1.
- `question_split()` / `issue_split()` — Compute proportion supporting "Yes" by question or by issue (pooling across years).
- `prop_seats()` — Compute the proportion of districts with majority support, with a probabilistic adjustment for sampling uncertainty.

**Formatting and labeling**

- `vartab()` — Recreate Stata's variable table from a `haven`-imported data frame.
- `attach_varlab()` — Attach Stata-style variable labels to columns for export.
- `cmfmtW()` — Format a number with commas and write it to a `.tex` file for LaTeX inclusion.
- `H_to_house()` / `H_to_cd()` / `H_to_rep()` — Recode `"H"`/`"S"` chamber codes to readable labels.

## Installation

The package is not on CRAN. Install from GitHub:

``` r
pak::pkg_install("kuriwaki/rcces")
```

## Related Packages

- [`ccesMRPprep`](https://www.shirokuriwaki.com/ccesMRPprep/) — Functions to download and standardize CCES data for MRP.
- [`ccesMRPrun`](https://github.com/kuriwaki/ccesMRPrun) — Functions to fit multilevel models and poststratify.
- [`cces_cumulative`](https://github.com/kuriwaki/cces_cumulative) — Code for the cumulative Common Content dataset.
