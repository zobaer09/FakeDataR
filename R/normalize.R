# R/normalize.R
# Internal helpers to lightly normalize input data before fakering.
# No exports; available inside the package namespace.

#' @keywords internal
#' @noRd
.normalize_input <- function(df) {
  stopifnot(is.data.frame(df))
  out <- df
  
  # Keep "no data"/"not applicable" as literal categories, not NA
  na_tokens <- c("", "NA", "N/A")
  
  for (nm in names(out)) {
    v <- out[[nm]]
    
    if (is.character(v)) {
      v <- trimws(v)
      v[v %in% na_tokens] <- NA_character_
      
      # Percent-like -> numeric
      if (.looks_like_percent(v)) v <- .percent_to_numeric(v)
      
      # Datetime-like -> POSIXct (US mm/dd/yyyy hh:mm[:ss]), parse only candidates
      dt <- .maybe_parse_datetime(v)
      if (!is.null(dt)) v <- dt
      
      # yes/no -> factor(no, yes)
      if (.looks_like_yesno(v)) v <- factor(tolower(v), levels = c("no","yes"))
    }
    
    out[[nm]] <- v
  }
  
  out
}

# --- helpers -------------------------------------------------------------------

#' @keywords internal
#' @noRd
.looks_like_percent <- function(x) {
  if (!is.character(x)) return(FALSE)
  s <- x[!is.na(x)]
  if (!length(s)) return(FALSE)
  rx <- "^\\s*\\d+(?:[\\.,]\\d+)?\\s*%\\s*$"
  mean(grepl(rx, s, perl = TRUE)) >= 0.6
}

#' @keywords internal
#' @noRd
.percent_to_numeric <- function(x) {
  # Prefer readr if present (handles locale/commas nicely)
  if (requireNamespace("readr", quietly = TRUE)) {
    return(readr::parse_number(x))
  }
  y <- gsub(",", ".", x, fixed = FALSE)
  y <- gsub("%", "", y, fixed = TRUE)
  suppressWarnings(as.numeric(y))
}

#' @keywords internal
#' @noRd
.looks_like_yesno <- function(x) {
  if (!is.character(x)) return(FALSE)
  s <- tolower(x[!is.na(x)])
  if (!length(s)) return(FALSE)
  u <- unique(s)
  all(u %in% c("yes","no")) && length(u) <= 2
}

#' @keywords internal
#' @noRd
.maybe_parse_datetime <- function(x) {
  # Parse only candidates; avoid touching long/messy strings
  if (!is.character(x)) return(NULL)
  
  rx <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}(?::\\d{2})?\\s*$"
  idx <- !is.na(x) & nchar(x) <= 40 & grepl(rx, x, perl = TRUE)
  if (!any(idx)) return(NULL)
  
  fmts <- c(
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S",
    "%m/%d/%y %H:%M"
  )
  
  # Prepare a full-length POSIXct result (all NA initially)
  res <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC")
  
  parsed_any <- FALSE
  for (fmt in fmts) {
    # Parse ONLY the candidate positions to avoid long-string errors
    parsed <- suppressWarnings(strptime(x[idx], format = fmt, tz = "UTC"))
    ok <- !is.na(parsed)
    if (any(ok)) {
      res[idx][ok] <- as.POSIXct(parsed[ok])
      parsed_any <- TRUE
      # keep looping to fill more with alternative formats
    }
  }
  
  if (!parsed_any) return(NULL)
  
  # Require that at least 60% of the candidates parsed to adopt this as datetime
  ok_rate <- mean(!is.na(res[idx]))
  if (is.na(ok_rate) || ok_rate < 0.6) return(NULL)
  
  res
}
