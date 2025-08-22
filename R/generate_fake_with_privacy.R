#' Generate fake data with privacy controls
#'
#' Generates a synthetic copy of `data`, then optionally detects/handles
#' sensitive columns by name. Detection uses the ORIGINAL column names and
#' maps to output via `attr(fake, "name_map")` if present.
#'
#' @param data A data.frame (or coercible) to mirror.
#' @param n Number of rows to generate (default: same as `nrow(data)` if NULL).
#' @param level Privacy preset: one of "low", "medium", "high".
#' @param seed Optional RNG seed for reproducibility.
#' @param sensitive Optional character vector of original column names to treat as sensitive.
#' @param sensitive_detect Logical; auto-detect common sensitive columns by name. Default TRUE.
#' @param sensitive_strategy One of "fake" (replace with tokens) or "drop". Default "fake".
#' @param normalize If TRUE (default), coerce & normalize inputs (percent → numeric,
#'   mm/dd/yyyy HH:MM → POSIXct, yes/no → factor, blanks → NA; preserves
#'   "not applicable"/"no data" as real categories).
#'
#' @return data.frame with attributes: sensitive_columns, dropped_columns, name_map
#' @export
generate_fake_with_privacy <- function(
    data,
    n = NULL,
    level = c("low","medium","high"),
    seed = NULL,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop"),
    normalize = TRUE
) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  if (!is.null(seed)) set.seed(seed)
  
  # Normalize first (or just coerce)
  data_norm <- prepare_input_data(data)
  if (isTRUE(normalize)) data_norm <- .normalize_input(data_norm)
  
  # Base fake with original names kept (so we can match sensitive by original name)
  # We ask the base faker to KEEP names (column_mode="keep") for low/medium presets.
  cfg <- switch(level,
                low    = list(category_mode = "preserve", column_mode = "keep",    numeric_mode = "range"),
                medium = list(category_mode = "generic",  column_mode = "generic", numeric_mode = "range"),
                high   = list(category_mode = "generic",  column_mode = "generic", numeric_mode = "distribution")
  )
  
  fake <- do.call(
    generate_fake_data,
    c(list(data = data_norm, n = n, seed = seed, normalize = FALSE), cfg)
  )
  
  # ensure name_map exists (original -> output)
  attr(fake, "name_map") <- attr(fake, "name_map") %||% stats::setNames(names(fake), names(fake))
  
  # ----- Privacy handling ------------------------------------------------------
  detect_rx <- "(?i)(^id$|email|e-mail|phone|tel|mobile|ssn|sin|passport|iban|account|card|name$|address)"
  sens_auto <- if (isTRUE(sensitive_detect)) {
    orig_names <- names(prepare_input_data(data))  # original names before normalization
    orig_names[grepl(detect_rx, orig_names)]
  } else character(0)
  
  sens_cols <- union(sensitive %||% character(0), sens_auto)
  dropped   <- character(0)
  
  if (length(sens_cols)) {
    if (sensitive_strategy == "drop") {
      keep_out <- setdiff(names(fake), unname(attr(fake, "name_map")[sens_cols]))
      fake <- fake[, keep_out, drop = FALSE]
      dropped <- sens_cols
    } else { # "fake" tokens, preserving NA-pattern
      out_map <- attr(fake, "name_map")
      for (orig_nm in sens_cols) {
        out_nm <- unname(out_map[orig_nm])
        if (is.na(out_nm) || !nzchar(out_nm) || !(out_nm %in% names(fake))) next
        v  <- fake[[out_nm]]
        na <- is.na(v)
        
        if (grepl("email", orig_nm, ignore.case = TRUE)) {
          v <- paste0("user", sample(100000:999999, length(v), TRUE),
                      "@example.", sample(c("com","org","net"), length(v), TRUE))
        } else if (grepl("phone|tel|mobile", orig_nm, ignore.case = TRUE)) {
          v <- vapply(seq_along(v), function(i) {
            paste0("+1-", paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                   paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                   paste0(sample(0:9,4,TRUE),collapse = ""))
          }, character(1))
        } else if (grepl("^id$", orig_nm, ignore.case = TRUE)) {
          v <- as.integer(sample(1000:9999, length(v), TRUE))
        } else if (grepl("name", orig_nm, ignore.case = TRUE)) {
          pool <- c("Alex","Sam","Jordan","Taylor","Casey","Devon","Riley","Jamie")
          v <- sample(pool, length(v), TRUE)
        } else {
          v <- vapply(seq_along(v), function(i) paste0(sample(letters, 8, TRUE), collapse = ""), character(1))
        }
        
        v[na] <- NA
        fake[[out_nm]] <- v
      }
    }
  }
  
  attr(fake, "sensitive_columns") <- sens_cols
  attr(fake, "dropped_columns")   <- dropped
  fake
}
