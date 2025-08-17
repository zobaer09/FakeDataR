#' Generate fake data with privacy controls
#'
#' @param data A data.frame to mirror
#' @param n Rows to generate
#' @param level One of "low","medium","high" (reserved for future tuning)
#' @param seed Optional RNG seed
#' @param sensitive Character vector of column names to treat as sensitive
#' @param sensitive_detect Logical, auto-detect common sensitive names (id/email/phone/...)
#' @param sensitive_strategy One of "fake" or "drop"
#' @return data.frame with attributes: sensitive_columns, dropped_columns, name_map
#' @export
generate_fake_with_privacy <- function(
    data, n = 30,
    level = c("medium","low","high"),
    seed = NULL,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop")
) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  if (!is.null(seed)) set.seed(seed)
  
  # Base fake with original names kept (so we can match sensitive by name)
  fake <- generate_fake_data(
    data,
    n = n,
    category_mode = "preserve",
    numeric_mode  = "range",
    column_mode   = "keep"
  )
  
  # If generate_fake_data() didn’t attach a map, use identity
  attr(fake, "name_map") <- attr(fake, "name_map") %||% stats::setNames(names(fake), names(fake))
  
  # Detect sensitive by output names
  detect_rx <- "(?i)(^id$|email|e-mail|phone|tel|mobile|ssn|sin|passport|iban|account|card|name$|address)"
  sens_auto <- if (isTRUE(sensitive_detect)) names(fake)[grepl(detect_rx, names(fake))] else character(0)
  sens_cols <- union(sensitive %||% character(0), sens_auto)
  
  dropped <- character(0)
  
  if (length(sens_cols)) {
    if (sensitive_strategy == "drop") {
      keep <- setdiff(names(fake), sens_cols)
      fake <- fake[, keep, drop = FALSE]
      dropped <- sens_cols
    } else {
      # Replace with fake tokens (preserving NA pattern)
      for (nm in sens_cols) {
        if (!nm %in% names(fake)) next
        v  <- fake[[nm]]
        na <- is.na(v)
        
        if (grepl("email", nm, ignore.case = TRUE)) {
          v <- paste0("user", sample(100000:999999, length(v), TRUE),
                      "@example.", sample(c("com","org","net"), length(v), TRUE))
        } else if (grepl("phone|tel|mobile", nm, ignore.case = TRUE)) {
          v <- vapply(seq_along(v), function(i) {
            paste0("+1-", paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                   paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                   paste0(sample(0:9,4,TRUE),collapse = ""))
          }, character(1))
        } else if (grepl("^id$", nm, ignore.case = TRUE) || is.integer(v)) {
          v <- as.integer(sample(1000:9999, length(v), TRUE))
        } else if (grepl("name", nm, ignore.case = TRUE)) {
          pool <- c("Alex","Sam","Jordan","Taylor","Casey","Devon","Riley","Jamie")
          v <- sample(pool, length(v), TRUE)
        } else {
          v <- vapply(seq_along(v), function(i) paste0(sample(letters, 8, TRUE), collapse = ""), character(1))
        }
        
        v[na] <- NA
        fake[[nm]] <- v
      }
    }
  }
  
  attr(fake, "sensitive_columns") <- sens_cols
  attr(fake, "dropped_columns")   <- dropped
  fake
}
