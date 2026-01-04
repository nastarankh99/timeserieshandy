
to_month_end <- function(dates) {
  d <- as.Date(dates)
  y <- as.integer(format(d, "%Y"))
  m <- as.integer(format(d, "%m"))

  # first day of next month minus 1 day
  next_m <- ifelse(m == 12L, 1L, m + 1L)
  next_y <- ifelse(m == 12L, y + 1L, y)

  as.Date(sprintf("%04d-%02d-01", next_y, next_m)) - 1
}

#' Align multiple time series by date
#'
#' Accepts named vectors (with Date names) or data frames with a Date column.
#'
#' @param ... Series to align.
#' @param how "inner" (default) keeps only overlapping dates, "outer" keeps all.
#' @return A data.frame with aligned dates and columns for each series.
#' @export
ts_align <- function(..., how = c("inner", "outer")) {
  how <- match.arg(how)
  xs <- list(...)

  to_df <- function(obj, nm) {
    if (is.data.frame(obj)) {
      if (!("date" %in% names(obj))) stop("data.frame must have a 'date' column")
      obj$date <- as.Date(obj$date)
      if (ncol(obj) != 2L) stop("data.frame must have exactly 2 columns: date and value")
      names(obj) <- c("date", nm)
      return(obj)
    }

    if (is.null(names(obj))) stop("named vector required (names must be dates)")
    d <- as.Date(names(obj))
    data.frame(date = d, value = as.numeric(obj), stringsAsFactors = FALSE) |>
      stats::setNames(c("date", nm))
  }

  nm <- names(xs)
  if (is.null(nm)) nm <- rep("", length(xs))
  nm[nm == ""] <- paste0("x", which(nm == ""))

  dfs <- Map(to_df, xs, nm)

  out <- dfs[[1L]]
  for (i in 2:length(dfs)) {
    out <- merge(out, dfs[[i]], by = "date", all = (how == "outer"))
  }
  out[order(out$date), ]
}
