# R/generate_fake_posixct_column.R

#' Generate a Fake POSIXct Column
#'
#' Create synthetic timestamps either by mimicking an existing POSIXct vector
#' (using its range and NA rate) or by sampling uniformly between `start` and `end`.
#'
#' @param like Optional POSIXct vector to mimic. If supplied, `n` defaults to `length(like)`,
#'   the output range matches `range(like, na.rm = TRUE)`, and the NA rate is copied unless
#'   you override with `na_prop`.
#' @param n Number of rows to generate. Required when `like` is `NULL`.
#' @param start,end Optional POSIXct bounds to sample between when `like` is `NULL`.
#' @param tz Timezone to use if `like` has no `tzone` (default "UTC").
#' @param na_prop Optional NA proportion to enforce in the output (0–1). If `NULL` and
#'   `like` is provided, it copies the NA rate from `like`. If `like` is `NULL`, defaults to 0.
#'
#' @return A POSIXct vector of length `n`.
#' @export
generate_fake_posixct_column <- function(like = NULL, n = NULL,
                                         start = NULL, end = NULL,
                                         tz = "UTC", na_prop = NULL) {
  `%||%` <- function(lhs, rhs) if (is.null(lhs)) rhs else lhs
  
  # Branch 1: mimic an existing POSIXct vector
  if (!is.null(like)) {
    if (!inherits(like, "POSIXct")) stop("`like` must be POSIXct.")
    n <- n %||% length(like)
    na_prop <- na_prop %||% mean(is.na(like))
    
    rng <- range(like, na.rm = TRUE)
    if (any(!is.finite(as.numeric(rng)))) {
      out <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = attr(like, "tzone") %||% tz)
      return(out)
    }
    
    lo <- as.numeric(rng[1]); hi <- as.numeric(rng[2])
    vals <- as.POSIXct(runif(n, lo, hi), origin = "1970-01-01", tz = attr(like, "tzone") %||% tz)
    
    if (!is.null(na_prop) && is.finite(na_prop) && na_prop > 0) {
      k <- floor(pmin(pmax(na_prop, 0), 1) * n)
      if (k > 0) vals[sample.int(n, k)] <- NA
    }
    return(vals)
  }
  
  # Branch 2: sample between start/end
  if (is.null(n)) stop("Please supply `n` when `like` is NULL.")
  if (is.null(start) || is.null(end)) stop("When `like` is NULL, supply both `start` and `end`.")
  if (!inherits(start, "POSIXct") || !inherits(end, "POSIXct")) {
    stop("`start` and `end` must be POSIXct.")
  }
  if (as.numeric(end) < as.numeric(start)) stop("`end` must be >= `start`.")
  
  na_prop <- na_prop %||% 0
  lo <- as.numeric(start); hi <- as.numeric(end)
  vals <- as.POSIXct(runif(n, lo, hi), origin = "1970-01-01", tz = tz)
  
  if (!is.null(na_prop) && is.finite(na_prop) && na_prop > 0) {
    k <- floor(pmin(pmax(na_prop, 0), 1) * n)
    if (k > 0) vals[sample.int(n, k)] <- NA
  }
  vals
}

