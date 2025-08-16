#' Validate a fake dataset against the original
#'
#' Compares classes, NA/blank proportions, and simple numeric ranges.
#' @param original data.frame
#' @param fake data.frame (same columns)
#' @param tol numeric tolerance for proportion differences (default 0.15)
#' @return data.frame summary by column
#' @export
validate_fake <- function(original, fake, tol = 0.15) {
  original <- as.data.frame(original)
  fake     <- as.data.frame(fake)
  stopifnot(ncol(original) == ncol(fake))
  stopifnot(all(names(original) %in% names(fake)) || all(names(fake) %in% names(original)))
  
  cols <- intersect(names(original), names(fake))
  out <- lapply(cols, function(col) {
    a <- original[[col]]
    b <- fake[[col]]
    
    class_a <- paste(class(a), collapse = "/")
    class_b <- paste(class(b), collapse = "/")
    
    na_a <- mean(is.na(a))
    na_b <- mean(is.na(b))
    na_ok <- abs(na_a - na_b) <= tol
    
    blank_a <- if (is.character(a) || is.factor(a)) mean(trimws(as.character(a)) == "", na.rm = TRUE) else NA_real_
    blank_b <- if (is.character(b) || is.factor(b)) mean(trimws(as.character(b)) == "", na.rm = TRUE) else NA_real_
    blank_ok <- if (is.na(blank_a) || is.na(blank_b)) NA else abs(blank_a - blank_b) <= tol
    
    num_ok <- NA
    if (is.numeric(a) || inherits(a, "Date") || inherits(a, "POSIXct")) {
      amin <- suppressWarnings(min(a, na.rm = TRUE))
      amax <- suppressWarnings(max(a, na.rm = TRUE))
      bmin <- suppressWarnings(min(b, na.rm = TRUE))
      bmax <- suppressWarnings(max(b, na.rm = TRUE))
      num_ok <- is.finite(amin) && is.finite(amax) && is.finite(bmin) && is.finite(bmax) &&
        (bmin >= amin) && (bmax <= amax)
    }
    
    data.frame(
      column = col,
      class_original = class_a,
      class_fake     = class_b,
      class_match    = identical(class(a), class(b)),
      na_prop_original = na_a,
      na_prop_fake     = na_b,
      na_match         = na_ok,
      blank_prop_original = blank_a,
      blank_prop_fake     = blank_b,
      blank_match         = blank_ok,
      range_within_original = num_ok,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}
