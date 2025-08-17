#' Generate a Fake POSIXct Column
#' @param col POSIXct vector to mimic.
#' @param n Number of rows.
#' @return POSIXct vector of length n.
#' @export
generate_fake_posixct_column <- function(col, n) {
  if (!inherits(col, "POSIXct")) stop("Input 'col' must be POSIXct.")
  na_prop <- mean(is.na(col))
  is_na   <- rbinom(n, 1, na_prop) == 1
  
  min_time <- suppressWarnings(min(col, na.rm = TRUE))
  max_time <- suppressWarnings(max(col, na.rm = TRUE))
  
  if (is.finite(min_time) && is.finite(max_time)) {
    fake_num <- runif(n, as.numeric(min_time), as.numeric(max_time))
    fake_col <- as.POSIXct(fake_num, origin = "1970-01-01", tz = attr(col, "tzone"))
  } else {
    fake_col <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = attr(col, "tzone"))
  }
  fake_col[is_na] <- NA
  fake_col
}
