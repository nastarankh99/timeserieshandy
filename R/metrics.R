#' Sharpe ratio
#'
#' @param r Returns vector.
#' @param rf Risk-free rate (scalar or vector). Default 0.
#' @return Sharpe ratio (mean excess return / sd).
#' @export
sharpe_ratio <- function(r, rf = 0) {
  r <- as.numeric(r)
  rf <- as.numeric(rf)
  ex <- r - rf
  mu <- base::mean(ex, na.rm = TRUE)
  s  <- stats::sd(ex, na.rm = TRUE)
  mu / s
}

#' Annualized return (simple)
#' @param r Returns vector.
#' @param freq Periods per year (12 monthly, 252 daily).
#' @return Annualized return.
#' @export
ann_return <- function(r, freq = 12) {
  r <- as.numeric(r)
  m <- base::mean(r, na.rm = TRUE)
  (1 + m)^freq - 1
}

#' Annualized volatility
#' @param r Returns vector.
#' @param freq Periods per year.
#' @return Annualized volatility.
#' @export
ann_vol <- function(r, freq = 12) {
  r <- as.numeric(r)
  stats::sd(r, na.rm = TRUE) * sqrt(freq)
}

#' Max drawdown
#' @param r Returns vector.
#' @return Maximum drawdown (negative number).
#' @export
drawdown <- function(r) {
  r <- as.numeric(r)
  wealth <- cumprod(1 + ifelse(is.na(r), 0, r))
  peak <- cummax(wealth)
  dd <- (wealth / peak) - 1
  min(dd, na.rm = TRUE)
}
