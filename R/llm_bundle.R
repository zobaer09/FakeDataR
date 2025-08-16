#' Create a fake-data bundle for LLM workflows
#'
#' Generates fake data, writes files (CSV/RDS/Parquet), writes a scrubbed JSON schema,
#' and optionally writes a README prompt and a single ZIP file containing everything.
#'
#' @param write_prompt Write a README_FOR_LLM.txt next to the data? Default TRUE.
#' @param zip Create a single zip archive containing data + schema + README? Default FALSE.
#' @param prompt_filename Name for the README file. Default "README_FOR_LLM.txt".
#' @param zip_filename Name for the zip file (no path). Default "<filename>.zip".
#' @inheritParams generate_fake_with_privacy
#' @inheritParams export_fake
#' @inheritParams llm_bundle
#' @return List with paths: $data_paths (named), $schema_path, $readme_path (optional), $zip_path (optional), and $fake (data.frame).
#' @export
llm_bundle <- function(data, n = 30,
                       level = c("medium","low","high"),
                       formats = c("csv","rds"),
                       path = tempdir(),
                       filename = "fake_bundle",
                       seed = NULL,
                       write_prompt = TRUE,
                       zip = FALSE,
                       prompt_filename = "README_FOR_LLM.txt",
                       zip_filename = NULL) {
  level <- match.arg(level)
  
  fake <- generate_fake_with_privacy(data, n = n, level = level, seed = seed)
  schema <- .schema_from_data(data, fake, level = level)
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Writing schema requires the 'jsonlite' package. Please install it.")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  schema_path <- file.path(path, paste0(filename, "_schema.json"))
  jsonlite::write_json(schema, schema_path,
                       pretty = TRUE, auto_unbox = TRUE,
                       na = "null", null = "null",
                       POSIXt = "ISO8601", Date = "ISO8601")
  
  # write data
  formats <- unique(tolower(formats))
  data_paths <- list()
  for (fmt in formats) {
    p <- file.path(path, paste0(filename, ".", fmt))
    export_fake(fake, p)
    data_paths[[fmt]] <- p
  }
  
  # optional README prompt
  readme_path <- NULL
  if (isTRUE(write_prompt)) {
    readme_path <- generate_llm_prompt(
      fake_path   = data_paths[[1]],
      schema_path = schema_path,
      write_file  = TRUE,
      path        = path,
      filename    = prompt_filename
    )
  }
  
  # optional zip
  zip_path <- NULL
  if (isTRUE(zip)) {
    if (is.null(zip_filename)) zip_filename <- paste0(filename, ".zip")
    zip_path <- file.path(path, zip_filename)
    bundle_files <- c(unlist(data_paths, use.names = FALSE), schema_path, readme_path)
    bundle_files <- bundle_files[!is.na(bundle_files)]
    zip_llm_bundle(bundle_files, zip_path)
  }
  
  list(
    data_paths  = data_paths,
    schema_path = schema_path,
    readme_path = readme_path,
    zip_path    = zip_path,
    fake        = fake
  )
}
