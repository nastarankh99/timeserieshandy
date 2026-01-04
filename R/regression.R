#' OLS fit using lm.fit (fast, minimal)
#'
#' @param y Numeric vector (dependent variable).
#' @param X Matrix/data.frame of regressors (no intercept unless you add it).
#' @param add_intercept Add an intercept column automatically.
#' @return A list with coefficients, fitted, residuals, sigma2, r2, and X used.
#' @export
ols_fit <- function(y, X, add_intercept = TRUE) {
  y <- as.numeric(y)
  X <- as.matrix(X)

  if (add_intercept) {
    X <- cbind(Intercept = 1, X)
  }

  ok <- stats::complete.cases(y, X)
  y2 <- y[ok]
  X2 <- X[ok, , drop = FALSE]

  fit <- stats::lm.fit(x = X2, y = y2)
  b <- fit$coefficients
  e <- fit$residuals
  yhat <- fit$fitted.values

  tss <- sum((y2 - base::mean(y2))^2)
  rss <- sum(e^2)
  r2 <- 1 - rss / tss
  sigma2 <- rss / (length(y2) - ncol(X2))

  list(
    coef = b,
    fitted = yhat,
    resid = e,
    sigma2 = sigma2,
    r2 = r2,
    X = X2,
    y = y2
  )
}

#' Newey–West standard errors
#'
#' Computes HAC covariance matrix and returns SEs.
#'
#' @param fit Output from ols_fit().
#' @param L Lag truncation (integer). Common choice: floor(4*(T/100)^(2/9)).
#' @return Numeric vector of Newey–West standard errors.
#' @export
nw_se <- function(fit, L = NULL) {
  X <- fit$X
  e <- fit$resid
  T <- nrow(X)
  k <- ncol(X)

  if (is.null(L)) {
    L <- floor(4 * (T / 100)^(2/9))
  }
  L <- as.integer(max(0, L))

  XtX_inv <- solve(t(X) %*% X)

  # S = sum_{t} e_t^2 x_t x_t' + sum_{l=1..L} w_l sum_{t=l+1..T} e_t e_{t-l} (x_t x_{t-l}' + x_{t-l} x_t')
  S <- matrix(0, k, k)

  for (t in 1:T) {
    xt <- matrix(X[t, ], ncol = 1)
    S <- S + (e[t]^2) * (xt %*% t(xt))
  }

  if (L > 0) {
    for (l in 1:L) {
      w <- 1 - (l / (L + 1))  # Bartlett weights
      for (t in (l + 1):T) {
        xt  <- matrix(X[t, ], ncol = 1)
        xtl <- matrix(X[t - l, ], ncol = 1)
        S <- S + w * (e[t] * e[t - l]) * (xt %*% t(xtl) + xtl %*% t(xt))
      }
    }
  }

  V <- XtX_inv %*% S %*% XtX_inv
  se <- sqrt(diag(V))
  se
}

#' OLS results table (optionally with Newey–West SE)
#'
#' @param fit Output from ols_fit().
#' @param se_type "classic" or "nw"
#' @param L NW lag truncation if se_type = "nw"
#' @return data.frame with estimate, se, t, p
#' @export
ols_table <- function(fit, se_type = c("classic", "nw"), L = NULL) {
  se_type <- match.arg(se_type)
  b <- fit$coef

  if (se_type == "classic") {
    XtX_inv <- solve(t(fit$X) %*% fit$X)
    se <- sqrt(diag(fit$sigma2 * XtX_inv))
  } else {
    se <- nw_se(fit, L = L)
  }

  tval <- b / se
  df <- length(fit$y) - length(b)
  pval <- 2 * stats::pt(abs(tval), df = df, lower.tail = FALSE)

  data.frame(
    term = names(b),
    estimate = as.numeric(b),
    se = as.numeric(se),
    t = as.numeric(tval),
    p = as.numeric(pval),
    row.names = NULL
  )
}

#' Partial out controls (Frisch–Waugh style)
#'
#' Returns residuals of y after regressing on X.
#'
#' @param y Numeric vector.
#' @param X Matrix/data.frame of controls.
#' @return Residualized y.
#' @export
partial_out <- function(y, X) {
  y <- as.numeric(y)
  X <- as.matrix(X)
  ok <- stats::complete.cases(y, X)
  y2 <- y[ok]
  X2 <- X[ok, , drop = FALSE]

  fit <- stats::lm.fit(cbind(1, X2), y2)
  out <- rep(NA_real_, length(y))
  out[ok] <- fit$residuals
  out
}
