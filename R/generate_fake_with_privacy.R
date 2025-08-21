#' Generate fake data with privacy controls
#'
#' Generates a synthetic copy of `data`, then optionally detects/handles
#' sensitive columns by name. Detection is done on the **original column names**
#' and then mapped to the output names via `attr(fake, "name_map")` if present,
#' so it still works even if your faker renames columns.
#'
#' @param data A data.frame (or coercible) to mirror.
#' @param n Number of rows to generate (default: same as `nrow(data)` if `NULL`).
#' @param level Privacy preset: one of `"low"`, `"medium"`, `"high"`.
#'   Your faker settings (e.g., how categories/numerics are mirrored) will use
#'   stricter defaults at higher levels.
#' @param seed Optional RNG seed for reproducibility.
#' @param sensitive Optional character vector of **original** column names to
#'   treat as sensitive.
#' @param sensitive_detect Logical; auto-detect common sensitive columns
#'   (id/email/phone/name/address/etc.) from the **original** names. Default `TRUE`.
#' @param sensitive_strategy One of `"fake"` (replace with realistic tokens) or
#'   `"drop"` (remove those columns). Default `"fake"`.
#'
#' @return A `data.frame` with attributes:
#'   - `sensitive_columns` (chr; original names)
#'   - `dropped_columns` (chr; original names that were dropped)
#'   - `name_map` (named chr; original -> output)
#' @export
generate_fake_with_privacy <- function(
    data,
    n = NULL,
    level = c("low", "medium", "high"),
    seed = NULL,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake", "drop")
) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  if (!is.null(seed)) set.seed(seed)
  if (is.null(n)) n <- NROW(data)
  
  # --- Build base fake according to preset (your package already provides this) ---
  # Ensures attr(fake, "name_map") exists; if not, we create identity mapping.
  fake <- .generate_fake_with_preset(data, n = n, level = level, seed = seed)
  
  # Ensure we have a name map original -> output (identity if your faker kept names)
  name_map <- attr(fake, "name_map")
  if (is.null(name_map)) {
    name_map <- stats::setNames(names(fake), names(data))
    # If dimensions mismatch, fall back to 1:1 on fake
    if (length(name_map) != length(names(data))) {
      name_map <- stats::setNames(names(fake), names(fake))
    }
    attr(fake, "name_map") <- name_map
  }
  
  # --- Figure out sensitive ORIGINAL columns ---
  orig_names <- names(data)
  sens_user  <- sensitive %||% character(0)
  
  detect_rx <- "(?i)(^id$|email|e-mail|phone|tel|mobile|ssn|sin|passport|iban|account|card|name$|address)"
  sens_auto <- if (isTRUE(sensitive_detect)) orig_names[grepl(detect_rx, orig_names)] else character(0)
  
  sens_orig <- unique(c(sens_user, sens_auto))
  sens_orig <- sens_orig[sens_orig %in% names(name_map)]
  
  # Map to OUTPUT names present in the fake
  sens_out <- unname(name_map[sens_orig])
  sens_out <- unique(stats::na.omit(sens_out))
  sens_out <- sens_out[sens_out %in% names(fake)]
  
  dropped <- character(0)
  
  if (length(sens_out)) {
    if (identical(sensitive_strategy, "drop")) {
      fake <- fake[, setdiff(names(fake), sens_out), drop = FALSE]
      dropped <- sens_orig
    } else {
      # Replace with fake-like tokens; preserve NA pattern
      for (nm in sens_out) {
        v  <- fake[[nm]]
        na <- is.na(v)
        
        # Pick a replacement strategy based on name/type
        if (grepl("email", nm, ignore.case = TRUE)) {
          repl <- paste0("user",
                         sample(100000:999999, length(v), TRUE),
                         "@example.",
                         sample(c("com", "org", "net"), length(v), TRUE))
        } else if (grepl("phone|tel|mobile", nm, ignore.case = TRUE)) {
          repl <- vapply(seq_along(v), function(i) {
            paste0("+1-",
                   paste0(sample(0:9, 3, TRUE), collapse = ""), "-",
                   paste0(sample(0:9, 3, TRUE), collapse = ""), "-",
                   paste0(sample(0:9, 4, TRUE), collapse = ""))
          }, character(1))
        } else if (grepl("^id$", nm, ignore.case = TRUE) || is.integer(v)) {
          repl <- as.integer(sample(1000:9999, length(v), TRUE))
        } else if (grepl("name", nm, ignore.case = TRUE)) {
          pool <- c("Alex","Sam","Jordan","Taylor","Casey","Devon","Riley","Jamie")
          repl <- sample(pool, length(v), TRUE)
        } else if (is.numeric(v)) {
          # numeric: keep scale roughness
          m <- stats::median(v, na.rm = TRUE)
          s <- stats::mad(v, constant = 1, na.rm = TRUE)
          s <- ifelse(is.finite(s) && s > 0, s, abs(m) + 1)
          repl <- rnorm(length(v), mean = m, sd = s)
          # coerce back to integer if original was integer
          if (is.integer(v)) repl <- as.integer(round(repl))
        } else {
          # generic token
          repl <- vapply(seq_along(v), function(i) {
            paste0(sample(letters, 8, TRUE), collapse = "")
          }, character(1))
        }
        
        repl[na] <- NA
        fake[[nm]] <- repl
      }
    }
  }
  
  attr(fake, "sensitive_columns") <- sens_orig
  attr(fake, "dropped_columns")   <- dropped
  
  fake
}
