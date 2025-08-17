#' Create a fake-data bundle for LLM workflows
#'
#' Generates fake data, writes files (CSV/RDS/Parquet), writes a scrubbed JSON schema,
#' and optionally writes a README prompt and a single ZIP file containing everything.
#'
#' @param data A data.frame (or coercible) to mirror.
#' @param n Number of rows in the fake dataset (default 30).
#' @param level Privacy level: "low", "medium", or "high". Controls stricter defaults.
#' @param formats Which data files to write: any of "csv","rds","parquet".
#' @param path Folder to write outputs. Default: \code{tempdir()}.
#' @param filename Base filename (without extension). Default: "fake_bundle".
#' @param seed Optional RNG seed for reproducibility.
#' @param write_prompt Write a README_FOR_LLM.txt next to the data? Default TRUE.
#' @param zip Create a single zip archive containing data + schema + README? Default FALSE.
#' @param prompt_filename Name for the README file. Default "README_FOR_LLM.txt".
#' @param zip_filename Name for the zip file (no path). Default "<filename>.zip".
#' @param sensitive Character vector of column names to treat as sensitive (optional).
#' @param sensitive_detect Logical, auto-detect common sensitive columns (id/email/phone). Default TRUE.
#' @param sensitive_strategy "fake" (replace with realistic fakes) or "drop". Default "fake".
#'
#' @return List with paths: $data_paths (named), $schema_path, $readme_path (optional),
#'   $zip_path (optional), and $fake (data.frame).
#' @export
llm_bundle <- function(
    data, n = 30,
    level = c("medium","low","high"),
    formats = c("csv","rds"),
    path = tempdir(),
    filename = "fake_bundle",
    seed = NULL,
    write_prompt = TRUE,
    zip = FALSE,
    prompt_filename = "README_FOR_LLM.txt",
    zip_filename = NULL,
    # --- sensitive passthrough ---
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop")
) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  # Map level -> default modes (only used if we fall back to generate_fake_data)
  .modes_from_level <- function(level) {
    switch(level,
           low    = list(category_mode="preserve",  numeric_mode="range",        column_mode="keep"),
           medium = list(category_mode="generic",   numeric_mode="range",        column_mode="generic"),
           high   = list(category_mode="generic",   numeric_mode="distribution", column_mode="generic")
    )
  }
  
  # ---- Generate fake data (prefer wrapper if present, otherwise fall back) ----
  fake <- tryCatch(
    {
      # If your generate_fake_with_privacy already forwards sensitive args, this will work.
      generate_fake_with_privacy(
        data, n = n, level = level, seed = seed,
        sensitive = sensitive,
        sensitive_detect = sensitive_detect,
        sensitive_strategy = sensitive_strategy
      )
    },
    error = function(e) {
      # Fallback: call generate_fake_data with modes derived from level
      md <- .modes_from_level(level)
      generate_fake_data(
        data, n = n, seed = seed,
        category_mode = md$category_mode,
        numeric_mode  = md$numeric_mode,
        column_mode   = md$column_mode,
        sensitive = sensitive,
        sensitive_detect = sensitive_detect,
        sensitive_strategy = sensitive_strategy
      )
    }
  )
  
  # Pull privacy/meta attributes from the fake df
  sens_cols     <- attr(fake, "sensitive_columns"); if (is.null(sens_cols)) sens_cols <- character(0)
  dropped_cols  <- attr(fake, "dropped_columns");   if (is.null(dropped_cols)) dropped_cols <- character(0)
  name_map_attr <- attr(fake, "name_map")
  if (is.null(name_map_attr)) {
    # Fallback: identity map (original -> output)
    name_map_attr <- stats::setNames(names(fake), names(fake))
  }
  # Output names corresponding to sensitive originals
  sens_out <- unname(name_map_attr[intersect(names(name_map_attr), sens_cols)])
  sens_out <- sens_out[!is.na(sens_out)]
  
  # ---- Build schema and annotate with sensitive flags/strategy ----
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Writing schema requires the 'jsonlite' package. Please install it.")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  # You already have this helper in your package:
  schema <- .schema_from_data(data, fake, level = level)
  
  # Add privacy context to schema
  schema$sensitive_strategy <- sensitive_strategy
  schema$dropped_sensitive  <- as.list(dropped_cols)
  
  # Flag column-level sensitivity in schema if a 'columns' array exists
  if (is.list(schema$columns)) {
    for (j in seq_along(schema$columns)) {
      # assume per-column name is stored under $name
      out_nm <- schema$columns[[j]]$name
      schema$columns[[j]]$sensitive <- isTRUE(out_nm %in% sens_out)
    }
  }
  
  # Write schema
  schema_path <- file.path(path, paste0(filename, "_schema.json"))
  jsonlite::write_json(
    schema, schema_path,
    pretty = TRUE, auto_unbox = TRUE,
    na = "null", null = "null",
    POSIXt = "ISO8601", Date = "ISO8601"
  )
  
  # ---- Write data files ----
  formats <- unique(tolower(formats))
  data_paths <- list()
  for (fmt in formats) {
    p <- file.path(path, paste0(filename, ".", fmt))
    export_fake(fake, p)     # your exporter switches on extension
    data_paths[[fmt]] <- p
  }
  
  # ---- README / prompt ----
  readme_path <- NULL
  if (isTRUE(write_prompt)) {
    readme_path <- file.path(path, prompt_filename)
    
    lines <- c(
      sprintf("# Fake bundle for LLM - %s", filename),
      "",
      "This bundle contains:",
      paste0("- Data files: ", paste(file.path(path, paste0(filename, ".", formats)), collapse = ", ")),
      paste0("- JSON schema: ", schema_path),
      "",
      "## Privacy summary",
      paste0("- Sensitive strategy: ", sensitive_strategy),
      if (length(sens_out)) {
        paste0("- Sensitive columns (output names): ", paste(sens_out, collapse = ", "))
      } else "- Sensitive columns (output names): none detected",
      if (length(dropped_cols)) {
        paste0("- Dropped original sensitive columns: ", paste(dropped_cols, collapse = ", "))
      } else "- Dropped original sensitive columns: none",
      "",
      "## How to use with an LLM",
      "1) Upload the schema first so the model understands dtypes and categories.",
      "2) Upload the fake data file.",
      "3) Ask questions about structure, analysis steps, and code. Do NOT expect correct statistics from fake values.",
      "4) Apply suggested code in your secure environment on the real data.",
      "",
      "## Disclaimer",
      "These data are synthetic and should not be used for real analysis or reporting."
    )
    writeLines(lines, con = readme_path)
  }
  
  # ---- Zip all files (optional) ----
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
