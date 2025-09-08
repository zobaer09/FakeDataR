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
    
    # Only touch character columns
    if (is.character(v)) {
      v <- trimws(v)
      v[v %in% na_tokens] <- NA_character_
      
      # Percent-like -> numeric (drops the % and parses)
      if (.looks_like_percent(v)) v <- .percent_to_numeric(v)
      
      # Datetime-like -> POSIXct (US mm/dd/yyyy hh:mm[:ss], 60%+ match)
      parsed_dt <- .maybe_parse_datetime(v)
      if (!is.null(parsed_dt)) v <- parsed_dt
      
      # Yes/No -> factor(no, yes) when values are only yes/no (case-insensitive)
      if (.looks_like_yesno(v)) v <- factor(tolower(v), levels = c("no", "yes"))
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
  mean(grepl(rx, s)) >= 0.6
}

#' @keywords internal
#' @noRd
.percent_to_numeric <- function(x) {
  # replace comma decimal to dot, strip %, then as.numeric
  y <- gsub(",", ".", x)
  y <- gsub("%", "", y, fixed = TRUE)
  suppressWarnings(as.numeric(y))
}

#' @keywords internal
#' @noRd
.looks_like_yesno <- function(x) {
  if (!is.character(x)) return(FALSE)
  s <- tolower(x[!is.na(x)])
  if (!length(s)) return(FALSE)
  uniq <- unique(s)
  all(uniq %in% c("yes", "no")) && length(uniq) <= 2
}

#' @keywords internal
#' @noRd
.maybe_parse_datetime <- function(x) {
  # Only try if strings look like mm/dd/yyyy hh:mm(:ss) for at least 60%
  if (!is.character(x)) return(NULL)
  s <- x[!is.na(x)]
  if (!length(s)) return(NULL)
  
  looks_dt <- grepl("^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}(?::\\d{2})?\\s*$",
                    s, perl = TRUE)
  if (mean(looks_dt) < 0.6) return(NULL)
  
  fmts <- c(
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %H:%M:%S",
    "%m/%d/%y %H:%M"
  )
  
  for (fmt in fmts) {
    suppressWarnings({
      tt <- strptime(x, format = fmt, tz = "UTC")
      out <- as.POSIXct(tt)
    })
    ok <- mean(!is.na(out))
    if (!is.na(ok) && ok >= 0.6) return(out)
  }
  
  NULL
}

