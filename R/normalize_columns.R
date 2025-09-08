#' @keywords internal
#' @noRd
.normalize_columns <- function(df) {
  if (!is.data.frame(df)) return(df)
  
  na_vals <- c("", "NA", "N/A", "na", "No data", "no data",
               "Not applicable", "not applicable")
  
  looks_datetime <- function(x) {
    if (!is.character(x)) return(FALSE)
    s <- na.omit(x)
    if (!length(s)) return(FALSE)
    s <- substr(s[seq_len(min(length(s), 2000))], 1, 40)  # hard guard
    rx <- "^\\s*\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}(:\\d{2})?\\s*(AM|PM|am|pm)?\\s*$"
    mean(grepl(rx, s, perl = TRUE)) >= 0.8
  }
  
  for (nm in names(df)) {
    x <- df[[nm]]
    
    # unify NA-ish tokens
    if (is.character(x)) {
      x[x %in% na_vals] <- NA_character_
    }
    
    # % -> numeric
    if (is.character(x)) {
      pc <- grepl("%", x)
      if (mean(pc, na.rm = TRUE) >= 0.8) {
        df[[nm]] <- suppressWarnings(readr::parse_number(x))
        next
      }
    }
    
    # datetime-like -> POSIXct
    if (looks_datetime(x)) {
      sx <- substr(x, 1, 40)
      out <- suppressWarnings(readr::parse_datetime(sx, format = "%m/%d/%Y %H:%M"))
      if (!all(is.na(out))) {
        attr(out, "tzone") <- "UTC"
        out[is.na(x)] <- NA
        df[[nm]] <- as.POSIXct(out, tz = "UTC")
        next
      }
    }
  }
  df
}
