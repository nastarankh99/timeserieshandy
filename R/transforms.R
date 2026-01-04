#' Lag a vector or time series
#'
#' @param x Numeric vector.
#' @param k Integer lag. Positive = lag (shift right), negative = lead.
#' @return A vector the same length as x, padded with NA.
#' @export
lag_ts <- function(x, k = 1L) {
  k <- as.integer(k)
  n <- length(x)
  if (n == 0L) return(x)

  if (k == 0L) return(x)

  out <- rep(NA_real_, n)

  if (k > 0L) {
    if (k < n) out[(k + 1L):n] <- x[1L:(n - k)]
  } else {
    k2 <- abs(k)
    if (k2 < n) out[1L:(n - k2)] <- x[(k2 + 1L):n]
  }
  out
}

#' Difference a series
#' @param x Numeric vector.
#' @param k Integer difference order.
#' @return Differenced vector with NA padding.
#' @export
diff_ts <- function(x, k = 1L) {
  k <- as.integer(k)
  if (k <= 0L) stop("k must be >= 1")
  x - lag_ts(x, k)
}

#' Log growth rate
#'
#' Computes log(x_t) - log(x_{t-1}). Useful for macro series.
#'
#' @param x Numeric vector (must be positive where computed).
#' @return Vector of log growth, first element NA.
#' @export
log_growth <- function(x) {
  x <- as.numeric(x)
  x <- ifelse(x > 0, x, NA_real_)
  c(NA_real_, diff(log(x)))
}

#' Percent change
#'
#' Computes (x_t / x_{t-1}) - 1.
#'
#' @param x Numeric vector.
#' @return Vector, first element NA.
#' @export
pct_change <- function(x) {
  x <- as.numeric(x)
  x_l1 <- lag_ts(x, 1L)
  (x / x_l1) - 1
}

#' Year-over-year change
#'
#' @param x Numeric vector.
#' @param freq Seasonal frequency (12 for monthly, 4 for quarterly).
#' @return YoY change with NA padding.
#' @export
yoy <- function(x, freq = 12L) {
  freq <- as.integer(freq)
  x <- as.numeric(x)
  x - lag_ts(x, freq)
}

#' Year-over-year log growth
#'
#' @param x Numeric vector (positive where computed).
#' @param freq Seasonal frequency (12 for monthly, 4 for quarterly).
#' @return YoY log growth (in log points).
#' @export
yoy_log <- function(x, freq = 12L) {
  freq <- as.integer(freq)
  x <- as.numeric(x)
  x <- ifelse(x > 0, x, NA_real_)
  log(x) - lag_ts(log(x), freq)
}
