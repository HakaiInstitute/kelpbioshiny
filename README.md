
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kelpbioshiny

<!-- badges: start -->

[![R-CMD-check](https://github.com/HakaiInstitute/kelpbioshiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HakaiInstitute/kelpbioshiny/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

kelpbioshiny is a Shiny app for estimating kelp biomass. The models are
provided by the [kelpbio](https://github.com/HakaiInstitute/kelpbio)
package; see its documentation for details.

## Installation

Install the pre-built binary from the Hakai R-universe:

``` r
install.packages(
  "kelpbioshiny",
  repos = c("https://hakaiinstitute.r-universe.dev", getOption("repos"))
)
```

Or install from GitHub:

``` r
# install.packages("pak")
pak::pak("HakaiInstitute/kelpbioshiny")
```

## Usage

Run the app locally:

``` r
kelpbioshiny::run_app()
```

## Licensing

Copyright 2026 Tula Foundation.

The code is released under the [MIT License](LICENSE.md).
