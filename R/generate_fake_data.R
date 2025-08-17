#' @title Generate Fake Data from Real Dataset Structure
#'
#' @description
#' Create a synthetic dataset that mirrors the structure and key patterns of an
#' existing dataset (types, factor levels, NA/blank rates). Values are fake.
#'
#' @param data A data.frame-like object to mirror.
#' @param n Number of rows to generate. Default 30.
#' @param category_mode How to handle categorical variables: "preserve", "generic", or "custom".
#' @param numeric_mode How to generate numeric data: "range" or "distribution".
#' @param column_mode Column names: "keep", "generic", or "custom".
#' @param custom_levels Named list of custom levels (used if category_mode="custom").
#' @param custom_names Character vector of custom column names (used if column_mode="custom").
#' @param seed Optional integer seed for reproducibility. Default NULL.
#' @param verbose Logical; if TRUE, prints detected column metadata & sensitive columns.
#'
#' @param sensitive Character vector of column names to always treat as sensitive;
#'   these will be faked with special generators (or dropped if strategy = "drop").
#'   If NULL, uses detection rules below.
#' @param sensitive_detect Logical; if TRUE, detect sensitive columns by name
#'   using built-in regex (id/email/phone). Default TRUE.
#' @param sensitive_strategy "fake" (default) or "drop". If "drop", sensitive columns
#'   are removed from the output.
#'
#' @return A data.frame of fake data with \code{n} rows.
#' @export
#' @importFrom stats setNames na.omit rbinom rnorm runif sd setNames model.frame
generate_fake_data <- function(
    data,
    n = 30,
    category_mode = c("preserve","generic","custom"),
    numeric_mode  = c("range","distribution"),
    column_mode   = c("keep","generic","custom"),
    custom_levels = NULL,
    custom_names  = NULL,
    seed    = NULL,
    verbose = FALSE,
    sensitive = NULL,
    sensitive_detect = TRUE,
    sensitive_strategy = c("fake","drop")
) {
  category_mode <- match.arg(category_mode)
  numeric_mode  <- match.arg(numeric_mode)
  column_mode   <- match.arg(column_mode)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  # Safe RNG handling
  if (!is.null(seed)) {
    old <- if (exists(".Random.seed", envir = .GlobalEnv)) 
      get(".Random.seed", envir = .GlobalEnv) 
    else NULL
    on.exit({
      if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    }, add = TRUE)
    set.seed(seed)
  }
  
  # Coerce input
  data <- prepare_input_data(data)
  
  # Column metadata (for info)
  column_info <- lapply(names(data), function(colname) {
    column <- data[[colname]]
    type <- class(column)[1]
    na_count <- sum(is.na(column))
    blank_count <- sum(trimws(as.character(column)) == "", na.rm = TRUE)
    unique_vals <- if (is.factor(column) || is.character(column)) {
      unique(trimws(as.character(stats::na.omit(column))))
    } else NULL
    levels <- if (is.factor(column)) levels(column) else NULL
    list(name = colname, type = type, na_count = na_count, blank_count = blank_count,
         unique_vals = unique_vals, levels = levels)
  })
  names(column_info) <- names(data)
  
  if (verbose) {
    message("Detected column metadata:")
    print(column_info)
  }
  
  # Output names and name map
  if (column_mode == "keep") {
    new_names <- names(data)
  } else if (column_mode == "generic") {
    new_names <- paste0("var", seq_len(ncol(data)))
  } else {
    if (is.null(custom_names) || length(custom_names) != ncol(data)) {
      stop("column_mode='custom' requires custom_names of length ncol(data).")
    }
    new_names <- custom_names
  }
  
  name_map <- stats::setNames(new_names, names(data))  # original -> output
  fake_data_list <- vector("list", length(data))
  names(fake_data_list) <- new_names
  
  # Sensitive detection
  detected <- if (sensitive_detect) identify_sensitive_cols(data) else character(0)
  sens_cols <- unique(c(names(detected), sensitive))
  sens_cols <- sens_cols[!is.na(sens_cols)]
  if (verbose && length(sens_cols)) {
    message("Sensitive columns detected/specified: ",
            paste(sens_cols, collapse = ", "),
            " | strategy = ", sensitive_strategy)
  }
  
  dropped <- character(0)
  
  for (i in seq_along(data)) {
    col      <- data[[i]]
    orig_nm  <- names(data)[i]
    out_nm <- new_names[i]   # <- fixed mapping; unaffected by later removals
    is_sensitive <- orig_nm %in% sens_cols
    
    # --- Sensitive handling first ---
    if (is_sensitive) {
      if (sensitive_strategy == "drop") {
        dropped <- c(dropped, orig_nm)  # just record it
        next                            # leave element NULL; we’ll strip NULLs after the loop
      } else {
        # preserve NA rate
        na_prop <- mean(is.na(col))
        is_na   <- rbinom(n, 1, na_prop) == 1
        
        lbl <- unname(detected[orig_nm])
        if (is.na(lbl) || length(lbl) == 0) lbl <- "id"
        
        if (identical(lbl, "email")) {
          fake_col <- .fake_email(n)
        } else if (identical(lbl, "phone")) {
          fake_col <- .fake_phone_like(col, n)
        } else {
          fake_col <- .fake_id_like(col, n)
        }
        
        fake_col[is_na] <- NA
        fake_data_list[[out_nm]] <- fake_col
        next
      }
    }
    
    # --- Non-sensitive generation (your existing logic) ---
    
    # 1) character / factor
    if (is.factor(col) || is.character(col)) {
      na_prop    <- mean(is.na(col))
      blank_prop <- mean(trimws(as.character(col)) == "", na.rm = TRUE)
      is_na      <- rbinom(n, 1, na_prop) == 1
      is_blank   <- !is_na & (rbinom(n, 1, blank_prop / (1 - na_prop + 1e-8)) == 1)
      
      original_levels <- stats::na.omit(unique(trimws(as.character(col))))
      num_levels      <- length(original_levels)
      
      if (category_mode == "preserve") {
        fake_levels <- original_levels
      } else if (category_mode == "generic") {
        fake_levels <- paste("Category", LETTERS[seq_len(max(1, num_levels))])
      } else { # custom
        if (is.null(custom_levels)) stop("category_mode='custom' requires custom_levels.")
        key1 <- orig_nm; key2 <- out_nm
        if      (!is.null(custom_levels[[key1]])) fake_levels <- custom_levels[[key1]]
        else if (!is.null(custom_levels[[key2]])) fake_levels <- custom_levels[[key2]]
        else stop(sprintf("Missing custom levels for column: '%s' (also tried '%s')", key1, key2))
      }
      
      if (length(fake_levels) == 0) {
        fake_col <- rep(NA_character_, n)
      } else {
        fake_col <- sample(fake_levels, n, replace = TRUE)
      }
      fake_col[is_blank] <- ""
      fake_col[is_na]    <- NA
      if (is.factor(col)) fake_col <- factor(fake_col, levels = unique(fake_col))
      fake_data_list[[out_nm]] <- fake_col
      next
    }
    
    # 2) logical
    if (is.logical(col)) {
      na_prop <- mean(is.na(col))
      is_na   <- rbinom(n, 1, na_prop) == 1
      fake_col <- sample(c(TRUE, FALSE), n, replace = TRUE)
      fake_col[is_na] <- NA
      fake_data_list[[out_nm]] <- fake_col
      next
    }
    
    # 3) Date
    if (inherits(col, "Date")) {
      na_prop <- mean(is.na(col))
      is_na   <- rbinom(n, 1, na_prop) == 1
      min_date <- suppressWarnings(min(col, na.rm = TRUE))
      max_date <- suppressWarnings(max(col, na.rm = TRUE))
      seq_dates <- seq(min_date, max_date, by = "day")
      if (length(seq_dates) == 0L) {
        fake_col <- as.Date(rep(NA_real_, n), origin = "1970-01-01")
      } else {
        fake_col <- as.Date(sample(seq_dates, n, replace = TRUE))
      }
      fake_col[is_na] <- NA
      fake_data_list[[out_nm]] <- fake_col
      next
    }
    
    # 4) POSIXct
    if (inherits(col, "POSIXct")) {
      fake_col <- generate_fake_posixct_column(col, n)
      fake_data_list[[out_nm]] <- fake_col
      next
    }
    
    # 5) numeric/integer
    if (is.numeric(col) || is.integer(col)) {
      is_integer <- inherits(col, "integer")
      na_prop <- mean(is.na(col))
      is_na <- rbinom(n, 1, na_prop) == 1
      fake_col <- numeric(n)
      non_na_count <- sum(!is_na)
      
      if (non_na_count == 0) {
        fake_col[] <- NA
      } else if (numeric_mode == "range") {
        min_val <- suppressWarnings(min(col, na.rm = TRUE))
        max_val <- suppressWarnings(max(col, na.rm = TRUE))
        if (is.finite(min_val) && is.finite(max_val)) {
          fake_col[!is_na] <- runif(non_na_count, min_val, max_val)
        } else fake_col[] <- NA
      } else { # distribution
        mean_val <- suppressWarnings(mean(col, na.rm = TRUE))
        sd_val   <- suppressWarnings(sd(col, na.rm = TRUE))
        if (is.finite(mean_val) && is.finite(sd_val) && sd_val > 0) {
          fake_col[!is_na] <- rnorm(non_na_count, mean_val, sd_val)
        } else fake_col[] <- NA
      }
      
      if (is_integer) {
        fake_col[!is.na(fake_col)] <- round(fake_col[!is.na(fake_col)])
        fake_col <- as.integer(fake_col)
      }
      fake_col[is_na] <- NA
      fake_data_list[[out_nm]] <- fake_col
      next
    }
    
    # 6) unsupported types -> placeholder
    fake_data_list[[out_nm]] <- rep(NA_character_, n)
  }
  
  # Remove dropped columns (if any)
  keep <- !vapply(fake_data_list, is.null, logical(1))
  fake_data_list <- fake_data_list[keep]
  
  fake <- as.data.frame(fake_data_list, stringsAsFactors = FALSE)
  # --- NEW: attach mapping and sensitive metadata for downstream tools ---
  name_map_kept <- name_map[keep]           # original -> output (kept only)
  attr(fake, "name_map")          <- name_map_kept
  attr(fake, "sensitive_columns") <- sens_cols          # original names
  attr(fake, "dropped_columns")   <- dropped            # original names
  attr(fake, "column_mode")       <- column_mode
  
  fake
}
