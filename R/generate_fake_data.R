#' Generate Fake Data from Real Dataset Structure
#'
#' @param data A tabular object; will be coerced via \code{prepare_input_data()}.
#' @param n Rows to generate (default 30).
#' @param category_mode One of "preserve","generic","custom".
#'   * preserve: sample observed categories by empirical frequency (keeps factors)
#'   * generic: replace categories with "Category A/B/..."
#'   * custom: use \code{custom_levels[[colname]]} if provided
#' @param numeric_mode One of "range","distribution".
#'   * range: uniform between min/max (integers stay integer-like)
#'   * distribution: sample observed values with replacement
#' @param column_mode One of "keep","generic","custom".
#'   * keep: keep original column names
#'   * generic: rename to var1..varP (mapping in attr(name_map))
#'   * custom: use \code{custom_names} named vector (old -> new)
#' @param custom_levels optional named list of allowed levels per column (for
#'   \code{category_mode="custom"}).
#' @param custom_names optional named character vector old->new (for
#'   \code{column_mode="custom"}).
#' @param seed Optional RNG seed.
#' @param verbose Logical; print progress.
#' @param sensitive Optional character vector of original column names to treat as sensitive.
#' @param sensitive_detect Logical; auto-detect common sensitive columns by name.
#' @param sensitive_strategy One of "fake","drop". Only applied if any sensitive columns exist.
#' @param normalize Logical; lightly normalize inputs (trim, %→numeric, short datetimes→POSIXct).
#' @return A data.frame of n rows with attributes:
#'   \itemize{
#'     \item \code{name_map} (named chr: original -> output)
#'     \item \code{column_mode} (chr)
#'     \item \code{sensitive_columns} (chr; original names)
#'     \item \code{dropped_columns} (chr; original names that were dropped)
#'   }
#' @export
generate_fake_data <- function(
    data,
    n = 30,
    category_mode = c("preserve","generic","custom"),
    numeric_mode  = c("range","distribution"),
    column_mode   = c("keep","generic","custom"),
    custom_levels = NULL,
    custom_names  = NULL,
    seed = NULL,
    verbose = FALSE,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop"),
    normalize = TRUE
) {
  # ---- helpers (internal) ----------------------------------------------------
  `%||%` <- function(x, y) if (is.null(x)) y else x
  .safe_map_vec <- function(map, key, default) {
    # Safe lookup in a NAMED CHARACTER vector.
    if (is.null(map)) return(default)
    nms <- names(map)
    if (is.null(nms) || !length(nms)) return(default)
    val <- unname(map[key])
    if (!length(val) || is.na(val)) default else as.character(val)
  }
  
  category_mode <- match.arg(category_mode)
  numeric_mode  <- match.arg(numeric_mode)
  column_mode   <- match.arg(column_mode)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  if (!is.null(seed)) set.seed(seed)
  
  # --- Coerce ---------------------------------------------------------------
  data <- tryCatch(prepare_input_data(data), error = function(e) {
    if (is.data.frame(data)) data else stop(e)
  })
  if (!is.data.frame(data)) stop("`data` must be (coercible to) data.frame.")
  if (is.null(n)) n <- nrow(data)
  
  # --- Optional light normalization ----------------------------------------
  if (isTRUE(normalize)) {
    # Conservative NA tokens only
    na_tokens <- c("", "NA", "N/A")
    for (nm in names(data)) if (is.character(data[[nm]])) {
      v <- trimws(data[[nm]])
      v[v %in% na_tokens] <- NA_character_
      data[[nm]] <- v
    }
    # Percent-like → numeric (only if most values look like percents)
    is_pct <- function(v) is.character(v) && mean(grepl("%", v), na.rm = TRUE) > 0.6
    pct_cols <- names(data)[vapply(data, is_pct, logical(1))]
    if (length(pct_cols)) {
      data[pct_cols] <- lapply(data[pct_cols], function(v) suppressWarnings(as.numeric(gsub("%","",v))))
    }
    # Short datetime strings → POSIXct (guard against huge strings)
    shortish <- function(v) is.character(v) && mean(nchar(v) <= 40, na.rm = TRUE) > 0.95
    looks_dt <- function(v) is.character(v) && mean(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}(?::\\d{2})?$", v), na.rm = TRUE) > 0.6
    dt_cols <- names(data)[vapply(data, function(v) shortish(v) && looks_dt(v), logical(1))]
    if (length(dt_cols)) {
      data[dt_cols] <- lapply(data[dt_cols], function(v) {
        # try several orders; silence parse errors
        fmt_try <- c("%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S", "%m/%d/%y %H:%M", "%m/%d/%y %H:%M:%S")
        out <- suppressWarnings(as.POSIXct(v, format = fmt_try[1L], tz = "UTC"))
        if (all(is.na(out))) {
          for (f in fmt_try[-1L]) {
            out <- suppressWarnings(as.POSIXct(v, format = f, tz = "UTC"))
            if (any(!is.na(out))) break
          }
        }
        out
      })
    }
  }
  
  p <- ncol(data)
  out <- vector("list", p)
  names(out) <- names(data)
  
  # Helpers
  na_mask <- function(v, n) {
    pna <- mean(is.na(v))
    if (!is.finite(pna) || pna < 0) pna <- 0
    rbinom(n, 1, pna) == 1
  }
  sample_cat <- function(v, n, keep_levels = NULL) {
    v_obs <- v[!is.na(v)]
    if (!length(v_obs)) {
      # nothing to sample -> all NA
      if (!is.null(keep_levels)) {
        return(factor(rep(NA_character_, n), levels = keep_levels))
      } else {
        return(rep(NA_character_, n))
      }
    }
    tab  <- table(v_obs, useNA = "no")    # keep names
    vals <- names(tab)                    # the categories
    probs <- as.numeric(tab) / sum(tab)   # numeric probs, names preserved above
    s <- sample(vals, size = n, replace = TRUE, prob = probs)
    if (!is.null(keep_levels)) factor(s, levels = keep_levels) else s
  }
  
  gen_generic_levels <- function(k) paste("Category", LETTERS[seq_len(max(1L, min(k, length(LETTERS))))])
  
  # --- Column-wise synthesis ------------------------------------------------
  for (j in seq_len(p)) {
    colname <- names(data)[j]
    v <- data[[j]]
    na_idx <- na_mask(v, n)
    
    # Factors & Characters
    if (is.factor(v) || is.character(v)) {
      if (category_mode == "preserve") {
        if (is.factor(v)) {
          out[[j]] <- sample_cat(as.character(v), n, keep_levels = levels(v))
        } else {
          out[[j]] <- sample_cat(v, n)
        }
      } else if (category_mode == "generic") {
        klev <- max(2L, min(10L, length(unique(stats::na.omit(v)))))
        levs <- gen_generic_levels(klev)
        out[[j]] <- sample(levs, n, replace = TRUE)
        if (is.factor(v)) out[[j]] <- factor(out[[j]], levels = levs)
      } else { # custom
        if (!is.null(custom_levels) && colname %in% names(custom_levels)) {
          levs <- custom_levels[[colname]]
          out[[j]] <- sample(levs, n, replace = TRUE)
          if (is.factor(v)) out[[j]] <- factor(out[[j]], levels = levs)
        } else {
          out[[j]] <- sample_cat(if (is.factor(v)) as.character(v) else v, n,
                                 keep_levels = if (is.factor(v)) levels(v) else NULL)
        }
      }
      out[[j]][na_idx] <- NA
      next
    }
    
    # Logical
    if (is.logical(v)) {
      p_true <- mean(v == TRUE, na.rm = TRUE); if (!is.finite(p_true)) p_true <- 0.5
      s <- rbinom(n, 1, p_true) == 1
      s[na_idx] <- NA
      out[[j]] <- s
      next
    }
    
    # Date
    if (inherits(v, "Date")) {
      if (numeric_mode == "distribution") {
        vals <- v[!is.na(v)]
        s <- if (length(vals)) sample(vals, n, replace = TRUE) else as.Date(rep(NA_real_, n), origin = "1970-01-01")
      } else {
        a <- suppressWarnings(min(v, na.rm = TRUE)); b <- suppressWarnings(max(v, na.rm = TRUE))
        if (is.finite(a) && is.finite(b) && a <= b) {
          s <- as.Date(round(runif(n, as.numeric(a), as.numeric(b))), origin = "1970-01-01")
        } else s <- as.Date(rep(NA_real_, n), origin = "1970-01-01")
      }
      s[na_idx] <- as.Date(NA)
      out[[j]] <- s
      next
    }
    
    # POSIXct
    if (inherits(v, "POSIXct")) {
      tz <- attr(v, "tzone")
      if (numeric_mode == "distribution") {
        vals <- v[!is.na(v)]
        s <- if (length(vals)) sample(vals, n, replace = TRUE) else as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz %||% "UTC")
      } else {
        a <- suppressWarnings(min(v, na.rm = TRUE)); b <- suppressWarnings(max(v, na.rm = TRUE))
        if (is.finite(a) && is.finite(b) && a <= b) {
          s_num <- runif(n, as.numeric(a), as.numeric(b))
          s <- as.POSIXct(s_num, origin = "1970-01-01", tz = tz %||% "UTC")
        } else s <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz %||% "UTC")
      }
      s[na_idx] <- as.POSIXct(NA)
      out[[j]] <- s
      next
    }
    
    # Integer
    if (is.integer(v)) {
      vals <- v[!is.na(v)]
      if (numeric_mode == "distribution" && length(vals)) {
        s <- sample(vals, n, replace = TRUE)
      } else {
        a <- suppressWarnings(min(vals)); b <- suppressWarnings(max(vals))
        if (is.finite(a) && is.finite(b) && a <= b) s <- as.integer(round(runif(n, a, b))) else s <- as.integer(rep(NA_integer_, n))
      }
      s[na_idx] <- NA_integer_
      out[[j]] <- s
      next
    }
    
    # Numeric (double)
    if (is.numeric(v)) {
      vals <- v[!is.na(v)]
      if (numeric_mode == "distribution" && length(vals)) {
        s <- sample(vals, n, replace = TRUE)
      } else {
        a <- suppressWarnings(min(vals)); b <- suppressWarnings(max(vals))
        if (is.finite(a) && is.finite(b) && a <= b) s <- runif(n, a, b) else s <- rep(NA_real_, n)
      }
      s[na_idx] <- NA_real_
      out[[j]] <- s
      next
    }
    
    # Fallback: random strings
    out[[j]] <- {
      rr <- replicate(n, paste(sample(letters, 8L, TRUE), collapse = ""), simplify = TRUE)
      rr[na_idx] <- NA_character_; rr
    }
  }
  
  # --- Names (column_mode) --------------------------------------------------
  orig_names <- names(out)
  
  if (column_mode == "generic") {
    new_names <- paste0("var", seq_along(out))
    names(out) <- new_names
    name_map <- stats::setNames(new_names, orig_names)
    
  } else if (column_mode == "custom") {
    # Accept BOTH:
    # 1) named vector old->new   (e.g., c(len="Length", supp="Supplement", dose="Dose"))
    # 2) unnamed vector by position (e.g., c("Length","Supplement","Dose"))
    if (is.null(custom_names)) {
      new_names <- orig_names
      
    } else if (is.character(custom_names) && is.null(names(custom_names))) {
      # Positional mapping: first k columns renamed; remainder keep original
      k <- length(custom_names)
      if (k == 0L) {
        new_names <- orig_names
      } else {
        if (k > length(orig_names)) {
          stop("`custom_names` (unnamed) is longer than the number of columns.")
        }
        new_names <- orig_names
        new_names[seq_len(k)] <- custom_names
      }
      
    } else {
      # Named mapping: old -> new (tolerates partial maps)
      if (!is.character(custom_names))
        stop("`custom_names` must be character (named or unnamed).")
      if (is.null(names(custom_names))) {
        stop("`custom_names` must be named, or be an unnamed vector for positional mapping.")
      }
      new_names <- vapply(
        orig_names,
        function(nm) .safe_map_vec(custom_names, nm, nm),
        character(1)
      )
    }
    
    names(out) <- new_names
    name_map <- stats::setNames(new_names, orig_names)
    
  } else {
    # keep
    name_map <- stats::setNames(orig_names, orig_names)
  }
  
  
  fake <- as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
  
  # --- Optional sensitive handling -----------------------------------------
  dropped <- character(0)
  det_rx <- "(?i)(^id$|email|e-mail|phone|tel|mobile|ssn|sin|passport|iban|account|card|name$|address)"
  sens_auto <- if (isTRUE(sensitive_detect)) orig_names[grepl(det_rx, orig_names)] else character(0)
  sens_cols <- union(sensitive %||% character(0), sens_auto)
  
  if (length(sens_cols)) {
    # map originals -> current output names safely
    sens_cols_out <- unname(name_map[sens_cols])
    sens_cols_out <- sens_cols_out[!is.na(sens_cols_out)]
    if (length(sens_cols_out)) {
      if (identical(sensitive_strategy, "drop")) {
        keep <- setdiff(names(fake), sens_cols_out)
        fake <- fake[, keep, drop = FALSE]
        dropped <- sens_cols
      } else {
        # fake sensitive tokens in-place, preserving NA masks
        for (onm in sens_cols) {
          cn <- unname(name_map[onm])
          if (!length(cn) || is.na(cn) || !(cn %in% names(fake))) next
          v <- fake[[cn]]; na <- is.na(v)
          if (grepl("email", onm, ignore.case = TRUE)) {
            v <- paste0("user", sample(100000:999999, length(v), TRUE),
                        "@example.", sample(c("com","org","net"), length(v), TRUE))
          } else if (grepl("phone|tel|mobile", onm, ignore.case = TRUE)) {
            v <- vapply(seq_along(v), function(i) {
              paste0("+1-", paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                     paste0(sample(0:9,3,TRUE),collapse = ""), "-",
                     paste0(sample(0:9,4,TRUE),collapse = ""))
            }, character(1))
          } else if (grepl("^id$", onm, ignore.case = TRUE)) {
            v <- sample(1000:9999, length(v), TRUE)
          } else if (grepl("name", onm, ignore.case = TRUE)) {
            pool <- c("Alex","Sam","Jordan","Taylor","Casey","Devon","Riley","Jamie")
            v <- sample(pool, length(v), TRUE)
          } else {
            v <- vapply(seq_along(v), function(i) paste0(sample(letters, 8, TRUE), collapse = ""), character(1))
          }
          v[na] <- NA
          fake[[cn]] <- v
        }
      }
    }
  }
  
  attr(fake, "name_map")           <- name_map
  attr(fake, "column_mode")        <- column_mode
  attr(fake, "sensitive_columns")  <- sens_cols
  attr(fake, "dropped_columns")    <- dropped
  fake
}
