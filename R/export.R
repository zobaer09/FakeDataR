#' Save a fake dataset to disk
#'
#' Save a data.frame to CSV, RDS, or Parquet based on the file extension.
#'
#' @param x A data.frame (e.g., output of \code{generate_fake_data()}).
#' @param path File path. Supported extensions: \code{.csv}, \code{.rds}, \code{.parquet}.
#'
#' @return (Invisibly) the path written.
#' @export
export_fake <- function(x, path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
         "csv" = utils::write.csv(x, path, row.names = FALSE),
         "rds" = saveRDS(x, path),
         "parquet" = {
           if (!requireNamespace("arrow", quietly = TRUE)) {
             stop("Saving .parquet requires the 'arrow' package. Install it or use .csv/.rds.")
           }
           arrow::write_parquet(x, path)
         },
         stop("Unsupported extension. Use .csv, .rds, or .parquet")
  )
  invisible(path)
}

