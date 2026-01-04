
# timeserieshandy


**A small R package with helper functions I often use when working with economic and financial time-series data.**

The goal of timeserieshandy is to collect simple, reusable tools that come up again and again in applied work

## What's included: 
lagging time series
log growth and year-over-year changes
rolling averages and rolling volatility
basic performance and risk metrics

## Installation

You can install the development version of timeserieshandy like so:

``` r
# install from GitHub
# remotes::install_github("nastarankh99/timeserieshandy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(timeserieshandy)
# log growth (e.g. inflation or output series)
g <- log_growth(c(100, 105, 110))
# 3-period rolling mean
m <- roll_mean(1:10, 3)
```

