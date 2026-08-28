[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

# timeserieshandy

**A small R package with helper functions I often use when working with economic and financial time-series data.**
The goal of timeserieshandy is to collect simple, reusable tools that come up again and again in applied work.

## What's included: 
lagging time series
log growth and year-over-year changes
rolling averages and rolling volatility
basic performance and risk metrics

## Installation
You can install the development version of timeserieshandy like so:
``` r
install from GitHub
remotes::install_github("nastarankh99/timeserieshandy")
```

## Example
This is a basic example which shows you how to solve a common problem:
``` r
library(timeserieshandy)
# log growth of inflation index:
log_growth(cpi_vector)
# rolling stats on returns:
roll_sd(returns_vector, 6)
```

## Note
These functions assume your data are ordered and regularly spaced (e.g. monthly or quarterly), date alignment is left to the user intentionally.
