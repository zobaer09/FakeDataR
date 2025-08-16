#' @title Generate Fake Data from Real Dataset Structure
#'
#' @description
#' Create a synthetic dataset that mirrors the structure and key patterns of an
#' existing dataset (types, factor levels, NA/blank rates). Values are fake.
#'
#' @param data A data.frame-like object to mirror.
#' @param n Number of rows to generate. Default 30.
#' @param category_mode How to handle categorical variables:
#'   "preserve", "generic", or "custom".
#' @param numeric_mode How to generate numeric data: "range" or "distribution".
#' @param column_mode Column names: "keep", "generic", or "custom".
#' @param custom_levels Named list of custom levels (used if category_mode="custom").
#' @param custom_names Character vector of custom column names (used if column_mode="custom").
#' @param seed Optional integer seed for reproducible output. Default NULL.
#' @param verbose Logical; if TRUE, prints detected column metadata. Default FALSE.
#'
#' @return A data.frame of fake data with \code{n} rows.
#' @export
#' @importFrom stats setNames
#' @importFrom stats na.omit rbinom rnorm runif sd setNames model.frame


# In generate_fake_data(...) signature:
generate_fake_data <- function(data,
                               n = 30,
                               category_mode = c("preserve","generic","custom"),
                               numeric_mode = c("range","distribution"),
                               column_mode = c("keep","generic","custom"),
                               custom_levels = NULL,
                               custom_names = NULL,
                               seed = NULL,
                               verbose = FALSE) {   # <- default FALSE for quiet tests
  category_mode <- match.arg(category_mode)
  numeric_mode  <- match.arg(numeric_mode)
  column_mode   <- match.arg(column_mode)
  
  # Optional local seeding (deterministic; safe if .Random.seed doesn't exist yet)
  if (!is.null(seed)) {
    has_old <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (has_old) old_seed <- get(".Random.seed", envir = .GlobalEnv)
    set.seed(seed)
    if (has_old) {
      on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv), add = TRUE)
    } else {
      on.exit({
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      }, add = TRUE)
    }
  }
  
  data <- prepare_input_data(data)
  
  # Compute column_info once
  column_info <- lapply(names(data), function(colname) {
    column <- data[[colname]]
    list(
      name        = colname,
      type        = class(column)[1],
      na_count    = sum(is.na(column)),
      blank_count = if (is.factor(column) || is.character(column)) sum(trimws(as.character(column)) == "", na.rm = TRUE) else 0,
      unique_vals = if (is.factor(column) || is.character(column)) unique(trimws(as.character(na.omit(column)))) else NULL,
      levels      = if (is.factor(column)) levels(column) else NULL
    )
  })
  names(column_info) <- names(data)
  
  if (isTRUE(verbose)) {
    message("Detected column metadata:")
    print(column_info)
  }


# Build output names and a name map: original -> output
  if (column_mode == "keep") {
    new_names <- names(data)
  } else if (column_mode == "generic") {
    new_names <- paste0("var", seq_len(ncol(data)))
  } else { # "custom"
    if (is.null(custom_names) || length(custom_names) != ncol(data)) {
      stop("column_mode='custom' requires custom_names of length ncol(data).")
    }
    new_names <- custom_names
  }
  
  # original -> output (used to resolve custom_levels after renaming)
  name_map <- stats::setNames(new_names, names(data))
  
  # Create named list to hold columns (using output names)
  fake_data_list <- vector("list", length(data))
  names(fake_data_list) <- new_names
  
  # ---------------------------
  # MAIN LOOP (you had this commented out)
  # ---------------------------
  for (i in seq_along(data)) {
    orig_name <- names(data)[i]
    out_name  <- name_map[[orig_name]]
    col       <- data[[i]]
    
    # 1) character / factor
    if (is.factor(col) || is.character(col)) {
      na_prop    <- mean(is.na(col))
      blank_prop <- mean(trimws(as.character(col)) == "", na.rm = TRUE)
      is_na      <- rbinom(n, 1, na_prop) == 1
      is_blank   <- !is_na & (rbinom(n, 1, blank_prop / (1 - na_prop + 1e-8)) == 1)
      
      original_levels <- na.omit(unique(trimws(as.character(col))))
      num_levels      <- length(original_levels)
      
      if (category_mode == "preserve") {
        fake_levels <- original_levels
      } else if (category_mode == "generic") {
        # safer than LETTERS (works for >26 levels)
        fake_levels <- paste("Category", seq_len(max(1, num_levels)))
      } else { # "custom"
        cl <- custom_levels
        has_orig <- !is.null(cl) && (orig_name %in% names(cl))
        has_out  <- !is.null(cl) && (out_name  %in% names(cl))
        if (has_orig) {
          fake_levels <- cl[[orig_name]]
        } else if (has_out) {
          fake_levels <- cl[[out_name]]
        } else {
          stop(sprintf("Missing custom levels for column: '%s' (also tried '%s')",
                       orig_name, out_name))
        }
      }
      
      if (length(fake_levels) == 0) {
        fake_col <- rep(NA_character_, n)
      } else {
        fake_col <- sample(fake_levels, n, replace = TRUE)
      }
      
      fake_col[is_blank] <- ""
      fake_col[is_na]    <- NA
      
      if (is.factor(col)) fake_col <- factor(fake_col, levels = unique(fake_col))
      fake_data_list[[i]] <- fake_col
      next
    }
    
    # 2) logical
    if (is.logical(col)) {
      na_prop <- mean(is.na(col))
      is_na   <- rbinom(n, 1, na_prop) == 1
      fake_col <- sample(c(TRUE, FALSE), n, replace = TRUE)
      fake_col[is_na] <- NA
      fake_data_list[[i]] <- fake_col
      next
    }
    
    # 3) Date
    if (inherits(col, "Date")) {
      na_prop <- mean(is.na(col))
      is_na   <- rbinom(n, 1, na_prop) == 1
      min_date <- suppressWarnings(min(col, na.rm = TRUE))
      max_date <- suppressWarnings(max(col, na.rm = TRUE))
      if (is.finite(min_date) && is.finite(max_date)) {
        fake_col <- as.Date(sample(seq(min_date, max_date, by = "day"), n, replace = TRUE))
      } else {
        fake_col <- as.Date(rep(NA, n))
      }
      fake_col[is_na] <- NA
      fake_data_list[[i]] <- fake_col
      next
    }
    
    # 4) POSIXct
    if (inherits(col, "POSIXct")) {
      fake_col <- generate_fake_posixct_column(col, n)
      fake_data_list[[i]] <- fake_col
      next
    }
    
    # 5) numeric / integer
    if (is.numeric(col) || is.integer(col)) {
      is_integer <- inherits(col, "integer")
      na_prop <- mean(is.na(col))
      is_na   <- rbinom(n, 1, na_prop) == 1
      fake_col <- numeric(n)
      non_na_count <- sum(!is_na)
      
      if (non_na_count == 0) {
        fake_col[] <- NA
      } else if (numeric_mode == "range") {
        min_val <- suppressWarnings(min(col, na.rm = TRUE))
        max_val <- suppressWarnings(max(col, na.rm = TRUE))
        if (is.finite(min_val) && is.finite(max_val)) {
          fake_col[!is_na] <- runif(non_na_count, min_val, max_val)
        } else {
          fake_col[] <- NA
        }
      } else { # "distribution"
        mean_val <- suppressWarnings(mean(col, na.rm = TRUE))
        sd_val   <- suppressWarnings(sd(col,   na.rm = TRUE))
        if (is.finite(mean_val) && is.finite(sd_val) && sd_val > 0) {
          fake_col[!is_na] <- rnorm(non_na_count, mean_val, sd_val)
        } else {
          fake_col[] <- NA
        }
      }
      
      if (is_integer) {
        fake_col[!is.na(fake_col)] <- round(fake_col[!is.na(fake_col)])
        fake_col <- as.integer(fake_col)
      }
      
      fake_col[is_na] <- NA
      fake_data_list[[i]] <- fake_col
      next
    }
    
    # 6) unsupported types -> placeholder
    fake_data_list[[i]] <- rep(NA_character_, n)
  }  # <- end for-loop
  
  # Return data.frame
  fake_data <- as.data.frame(fake_data_list, stringsAsFactors = FALSE)
  return(fake_data)
}  # <- end generate_fake_data()

#' Generate a Fake POSIXct Column
#' @param col POSIXct vector to mimic.
#' @param n Number of rows.
#' @return POSIXct vector of length n.
#' @keywords internal
#' @noRd
generate_fake_posixct_column <- function(col, n) {
  if (!inherits(col, "POSIXct")) stop("Input 'col' must be POSIXct.")
  na_prop <- mean(is.na(col))
  is_na   <- rbinom(n, 1, na_prop) == 1
  
  min_time <- suppressWarnings(min(col, na.rm = TRUE))
  max_time <- suppressWarnings(max(col, na.rm = TRUE))
  
  if (is.finite(min_time) && is.finite(max_time)) {
    fake_num <- runif(n, as.numeric(min_time), as.numeric(max_time))
    fake_col <- as.POSIXct(fake_num, origin = "1970-01-01", tz = attr(col, "tzone"))
  } else {
    fake_col <- as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = attr(col, "tzone"))
  }
  fake_col[is_na] <- NA
  fake_col
}
