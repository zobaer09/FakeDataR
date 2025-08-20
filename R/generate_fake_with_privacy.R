#' Generate fake data with privacy controls
#'
#' Thin wrapper over [generate_fake_data()] that forwards privacy options.
#'
#' @param data A data.frame (or coercible).
#' @param n Number of rows in the fake dataset.
#' @param level Privacy level: "low","medium","high".
#' @param seed Optional RNG seed.
#' @param category_mode,numeric_mode,column_mode Optional overrides passed to [generate_fake_data()].
#' @param sensitive Character vector of original column names treated as sensitive.
#' @param sensitive_detect Logical; detect common sensitive columns by name (id/email/phone).
#' @param sensitive_strategy One of `"fake"` or `"drop"`.
#'
#' @return A data.frame with attributes like `name_map`, `dropped_columns`, `sensitive_columns`.
#' @export
generate_fake_with_privacy <- function(
    data, n = 30,
    level = c("medium","low","high"),
    seed = NULL,
    category_mode = NULL,
    numeric_mode  = NULL,
    column_mode   = NULL,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop")
) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  # If caller doesn't supply modes, derive from level:
  if (is.null(category_mode)) category_mode <- switch(level, low="preserve", medium="generic", high="generic")
  if (is.null(numeric_mode))  numeric_mode  <- switch(level, low="range",    medium="range",   high="distribution")
  if (is.null(column_mode))   column_mode   <- switch(level, low="keep",     medium="generic", high="generic")
  
  generate_fake_data(
    data, n = n, seed = seed,
    category_mode = category_mode,
    numeric_mode  = numeric_mode,
    column_mode   = column_mode,
    sensitive = sensitive,
    sensitive_detect = sensitive_detect,
    sensitive_strategy = sensitive_strategy
  )
}
