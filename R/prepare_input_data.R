#' Prepare Input Data: Coerce to data.frame
#'
#' Takes any common tabular R object and safely converts to data.frame.
#' Returns NULL or error if unsupported.
#'
#' @param data Input object
#' @return A data.frame or error
prepare_input_data <- function(data) {
  # NULL check early
  if (is.null(data)) stop("Input is NULL. Please provide a dataset.")
  
  # Handle known object classes first
  if (is.data.frame(data)) return(data)
  if (inherits(data, c("tbl_df", "tibble", "data.table", "matrix", "table", "ftable"))) {
    return(as.data.frame(data))
  }
  if (inherits(data, c("ts", "zoo", "xts"))) {
    return(as.data.frame(data))
  }
  if (inherits(data, "grouped_df")) {
    message("Ungrouping grouped_df...")
    return(as.data.frame(dplyr::ungroup(data)))
  }
  if (inherits(data, c("lm", "glm"))) {
    message("Extracting model data using model.frame()...")
    return(as.data.frame(model.frame(data)))
  }
  
  # Try coercing from list
  if (is.list(data)) {
    try_df <- try(as.data.frame(data), silent = TRUE)
    if (!inherits(try_df, "try-error")) {
      return(try_df)
    } else {
      stop("Input is a list but not coercible to data.frame.")
    }
  }
  
  # Now reject unsafe types
  if (is.function(data) || is.language(data) || is.environment(data)) {
    stop("Unsupported object type: function, call, or environment.")
  }
  
  # Special object classes to warn about
  if (inherits(data, "tbl_spark")) {
    stop("Spark DataFrames are not supported directly. Use collect() to get a local data.frame.")
  }
  if (inherits(data, "Table")) {
    stop("Arrow Table not supported directly. Use as.data.frame() first.")
  }
  if (inherits(data, "survey.design")) {
    stop("survey.design objects not supported. Extract data.frame manually.")
  }
  
  stop("Unsupported input type. Please provide a data.frame, tibble, matrix, model, or compatible object.")
}




