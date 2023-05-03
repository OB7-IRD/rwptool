
<!-- README.md is generated from README.Rmd. Please edit that file and click on Knit button at the end. -->

# rwptool R package <a href='https://ob7-ird.github.io/rwptool'><img src='man/figures/logo_rwptool.png' align="right" /></a>

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/OB7-IRD/rwptool/workflows/R-CMD-check/badge.svg)](https://github.com/OB7-IRD/acdc/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/rwptool)](https://CRAN.R-project.org/package=rwptool)
<!-- badges: end -->

***Regional Work Plans Tool***

## Warning

Package and documentation still in construction !

## Overview

R toolbox package for the EU Regional Work Plans development.

## Installation

``` r
devtools::install_github("https://github.com/OB7-IRD/rwptool",
                         INSTALL_opts=c("--no-multiarch"))
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of rwptool from GitHub.

``` r
devtools::install_github("https://github.com/OB7-IRD/rwptool",
                         ref = "development",
                         INSTALL_opts=c("--no-multiarch"))
```

## Cheatsheet

Working in progress for this section. Be patient.

## Usage

``` r
# setup ----
library(rwptool)
reference_period_start <- as.integer(x = 2019)
reference_period_end <- as.integer(x = 2021)
# By default countries used are the 27 EU member states, you can change that through the "eu_countries" argument. 
# By default rfmo used are CCAMLR, CECAF, GFCM, IATTC, ICCAT, ICES, IOTC, NAFO, SEAFO, SPRFMO, WCPFC and WECAFC. You can change that through the "rfmo" argument.
landing_statistics <- "eurostat"
# You can switch with "rcg_stats" in the "landing_statistics" argument to use regional database source.
path_input <- "~/input"
output_path <- "~/output"

# process ----
rwp_table_2_1_template(reference_period_start = reference_period_start,
                       reference_period_end = reference_period_end,
                       landing_statistics = landing_statistics,
                       input_path = path_input,
                       output_path = output_path)
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on [GitHub issues
page](https://github.com/OB7-IRD/rwptool/issues). This link is also
available if you have any questions and improvement propositions.

## References

Working in progress for this section.
