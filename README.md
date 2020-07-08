rcces
================

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

`rcces` is includes functions that could be useful in reading in and
analyzing data from the
[CCES](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/II2DB6).
Currently it is just a bundle of personal functions, not designed for
distribution. For the latter, you may be interested in the other
repositories listed below.

## Installation

You can install the package using devtools.

``` r
devtools::install_github("kuriwaki/rcces")
```

## Related Packages

  - [`ccesMRPprep`](https://www.shirokuriwaki.com/ccesMRPprep/reference/index.html)
    includes functions and documentation on how to download and
    standardized data. It is geared towards standardization for MRP, but
    some of the vignettes and documentation use common standardization
    procedures and document features of the MRP dataset.
  - [`ccesMRPrun`](https://github.com/kuriwaki/ccesMRPrun/tree/master/R)
    includes functions to fit a multilevel model and poststratify to a
    target (computed by `ccesMRPprep`).
  - [`cces_cumulative`](https://github.com/kuriwaki/cces_cumulative)
    includes code that shows how the cumulative Common Content was
    created. The cumulative dataset is a stacked version of key columns
    in the year-specific CCES, and can be used as a base dataset into
    which year-specific questions can be merged back.
