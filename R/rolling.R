#' Rolling mean
#' @param x Numeric vector.
#' @param w Window size (integer >= 1).
#' @return Vector with NA for the first w-1 elements.
#' @export
roll_mean <- function(x, w) {
  w <- as.integer(w)
  if (w < 1L) stop("w must be >= 1")
  x <- as.numeric(x)

  n <- length(x)
  out <- rep(NA_real_, n)
  if (w > n) return(out)

  for (i in w:n) {
    out[i] <- base::mean(x[(i - w + 1L):i], na.rm = TRUE)
  }
  out
}

#' Rolling standard deviation
#' @param x Numeric vector.
#' @param w Window size.
#' @return Vector with NA for the first w-1 elements.
#' @export
roll_sd <- function(x, w) {
  w <- as.integer(w)
  if (w < 1L) stop("w must be >= 1")
  x <- as.numeric(x)

  n <- length(x)
  out <- rep(NA_real_, n)
  if (w > n) return(out)

  for (i in w:n) {
    out[i] <- stats::sd(x[(i - w + 1L):i], na.rm = TRUE)
  }
  out
}

#' Rolling z-score
#' @param x Numeric vector.
#' @param w Window size.
#' @return Vector of z-scores using rolling mean/sd.
#' @export
roll_zscore <- function(x, w) {
  mu <- roll_mean(x, w)
  s  <- roll_sd(x, w)
  (as.numeric(x) - mu) / s
}

#' Exponentially weighted moving average (EWMA)
#' @param x Numeric vector.
#' @param lambda Smoothing parameter in (0,1). Higher = smoother.
#' @return EWMA series.
#' @export
ewma <- function(x, lambda = 0.94) {
  x <- as.numeric(x)
  if (!is.finite(lambda) || lambda <= 0 || lambda >= 1) {
    stop("lambda must be between 0 and 1")
  }

  n <- length(x)
  out <- rep(NA_real_, n)
  if (n == 0L) return(out)

  out[1] <- x[1]
  for (i in 2:n) {
    out[i] <- lambda * out[i - 1L] + (1 - lambda) * x[i]
  }
  out
}
