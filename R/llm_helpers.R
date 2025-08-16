#' Create a copy-paste prompt for LLMs
#'
#' @param fake_path Path to the fake data file (CSV/RDS/Parquet).
#' @param schema_path Optional path to the JSON schema.
#' @param notes Optional extra notes to append for the analyst/LLM.
#' @param write_file Write a README txt next to the files? Default TRUE.
#' @param path Output directory for the README if write_file = TRUE.
#' @param filename README file name. Default "README_FOR_LLM.txt".
#' @return The prompt string (invisibly returns the file path if written).
#' @export
generate_llm_prompt <- function(fake_path,
                                schema_path = NULL,
                                notes = NULL,
                                write_file = TRUE,
                                path = dirname(fake_path),
                                filename = "README_FOR_LLM.txt") {
  fake_bn   <- basename(fake_path)
  schema_bn <- if (!is.null(schema_path)) basename(schema_path) else NULL
  
  prompt <- paste(
    "Context:",
    "- The attached dataset is FAKE and privacy-safe. It mirrors the real data's structure (column types, factor levels, NA/blank rates, date/time classes) but contains no real values.",
    if (!is.null(schema_path))
      paste0("- A JSON schema file (", schema_bn, ") describes the FAKE data only; use it as authoritative for types/levels/ranges."),
    "",
    "What I want from you:",
    "1) Propose an analysis plan suitable for the real dataset that has the SAME structure.",
    "2) Provide reusable, parameterized R code (prefer tidyverse) for EDA, key summaries, and relevant models/tests.",
    "3) Do NOT draw conclusions from the FAKE values; they are placeholders.",
    "",
    "Files:",
    paste0("- Fake data: ", fake_bn),
    if (!is.null(schema_path)) paste0("- Schema: ", schema_bn) else NULL,
    "",
    "Important constraints:",
    "- Treat column names/types and missingness patterns as true.",
    "- If factor labels look generic (e.g., 'Category A'), treat them as placeholders.",
    "",
    if (!is.null(notes)) paste("Author notes:", notes) else NULL,
    sep = "\n"
  )
  
  if (isTRUE(write_file)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    out <- file.path(path, filename)
    writeLines(prompt, out, useBytes = TRUE)
    invisible(out)
  } else {
    prompt
  }
}

#' Zip a set of files for easy sharing
#'
#' @param files Character vector of file paths.
#' @param zipfile Path to the zip file to create.
#' @return The path to the created zip file.
#' @export
zip_llm_bundle <- function(files, zipfile) {
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  dir.create(dirname(zipfile), showWarnings = FALSE, recursive = TRUE)
  
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(zipfile, files = files)
  } else {
    # Fallback to utils::zip (may require external zip on some systems)
    utils::zip(zipfile, files)
  }
  zipfile
}
