#' Generate fake data using a privacy preset
#'
#' @param data data.frame
#' @param n rows
#' @param level 'low' | 'medium' | 'high'
#' @param seed optional seed
#' @export
generate_fake_with_privacy <- function(data, n = 30, level = c("low","medium","high"), seed = NULL) {
  level <- match.arg(level)
  cfg <- switch(level,
                low    = list(category_mode = "preserve", column_mode = "keep",    numeric_mode = "range"),
                medium = list(category_mode = "generic",  column_mode = "generic", numeric_mode = "range"),
                high   = list(category_mode = "generic",  column_mode = "generic", numeric_mode = "distribution")
  )
  do.call(generate_fake_data, c(list(data = data, n = n, seed = seed), cfg))
}
