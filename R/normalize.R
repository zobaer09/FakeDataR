# R/normalize.R

#' @keywords internal
#' @noRd
.normalize_input <- function(df) {
  stopifnot(is.data.frame(df))
  
  # Only true blanks / NA tokens -> NA. Keep "not applicable"/"no data" as categories.
  to_na  <- c("", "NA", "N/A", "na", "null", "NULL")
  
  .is_yesno <- function(x) {
    vals <- tolower(na.omit(unique(x)))
    length(vals) > 0 && all(vals %in% c("yes","no"))
  }
  .looks_pct <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(FALSE)
    mean(grepl("%", x)) >= 0.6
  }
  .looks_mdy_hms <- function(x) {
    rx <- "\\b\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}(:\\d{2})?\\b"
    x <- x[!is.na(x)]
    if (!length(x)) return(FALSE)
    mean(grepl(rx, x)) >= 0.6
  }
  .parse_mdy_hms <- function(x) {
    out <- suppressWarnings(strptime(x, "%m/%d/%Y %H:%M:%S", tz = "UTC"))
    if (all(is.na(out))) {
      out <- suppressWarnings(strptime(x, "%m/%d/%Y %H:%M", tz = "UTC"))
    }
    as.POSIXct(out, tz = "UTC")
  }
  
  for (j in seq_along(df)) {
    v <- df[[j]]
    
    # -------- NEW: Handle FACTORS by preserving class --------
    if (is.factor(v)) {
      # Optionally standardize yes/no levels but keep factor class
      if (.is_yesno(levels(v))) {
        df[[j]] <- factor(tolower(as.character(v)), levels = c("no","yes"))
      } else {
        # leave factor as-is (no character coercion)
        df[[j]] <- v
      }
      next
    }
    
    # Characters: apply full normalization
    if (is.character(v)) {
      # Normalize blanks/NA tokens
      v[trimws(v) %in% to_na] <- NA_character_
      
      # Standardize common text tokens BUT KEEP them as real categories
      tok <- tolower(v)
      v[tok %in% c("not applicable","n/a (not applicable)")] <- "not applicable"
      v[tok %in% c("no data","nodata")] <- "no data"
      
      # yes/no -> ordered factor
      if (.is_yesno(v)) {
        df[[j]] <- factor(tolower(v), levels = c("no","yes"))
        next
      }
      
      # percent -> numeric (0..100 scale)
      if (.looks_pct(v)) {
        df[[j]] <- suppressWarnings(as.numeric(gsub("%", "", v)))
        next
      }
      
      # datetime -> POSIXct (UTC)
      if (.looks_mdy_hms(v)) {
        df[[j]] <- .parse_mdy_hms(v)
        next
      }
      
      # else keep as character
      df[[j]] <- v
      next
    }
    
    # Other types: leave untouched
    df[[j]] <- v
  }
  
  df
}
