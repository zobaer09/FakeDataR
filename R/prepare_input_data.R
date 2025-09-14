#' Prepare Input Data: Coerce to data.frame and (optionally) normalize values
#'
#' Converts common tabular objects to a base `data.frame`, and if `normalize = TRUE`
#' it applies light, conservative value normalization:
#' - Converts common date/time strings to POSIXct (best-effort across several formats)
#' - Converts percent-like character columns (e.g. "85%") to numeric (85)
#' - Maps a configurable set of "NA-like" strings to `NA`, while *keeping* common survey
#'   responses like "not applicable" or "prefer not to answer" as **real levels**
#' - Normalizes yes/no character columns to an ordered factor `c("no","yes")`
#'
#' @param data An object coercible to `data.frame` (data.frame/\pkg{tibble}/data.table/matrix/list, etc.)
#' @param normalize Logical, run value normalization step (default `TRUE`).
#' @param na_strings Character vector that should become `NA`
#'   (default: `c("", "NA", "N/A", "na", "No data", "no data")`).
#' @param keep_as_levels Character vector that should be **kept as values** (not `NA`),
#'   e.g., survey choices (default: `c("not applicable", "prefer not to answer", "unsure")`).
#'   Matching is case-insensitive.
#' @param percent_detect_threshold Proportion of non-missing values that must contain `%`
#'   before converting a character column to numeric (default `0.6`).
#' @param datetime_formats Candidate formats tried (in order) when parsing date-times strings.
#'   The best-fitting format (most successful parses) is used. Defaults cover
#'   `mm/dd/yyyy HH:MM(:SS)?`, ISO-8601, and date-only.
#' @return A base `data.frame`.
#' @export
prepare_input_data <- function(
    data,
    normalize = TRUE,
    na_strings = c("", "NA", "N/A", "na", "No data", "no data"),
    keep_as_levels = c("not applicable", "prefer not to answer", "unsure"),
    percent_detect_threshold = 0.6,
    datetime_formats = c(
      "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M",
      "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
      "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M",
      "%m/%d/%Y", "%Y-%m-%d"
    )
) {
  # ---------- class coercion (your original logic, slightly tidied) ----------
  if (is.null(data)) stop("Input is NULL. Please provide a dataset.")
  
  if (is.data.frame(data)) {
    df <- data
  } else if (inherits(data, c("tbl_df", "tibble", "data.table", "matrix", "table", "ftable"))) {
    df <- as.data.frame(data)
  } else if (inherits(data, c("ts", "zoo", "xts"))) {
    df <- as.data.frame(data)
  } else if (inherits(data, "grouped_df")) {
    message("Ungrouping grouped_df...")
    df <- as.data.frame(dplyr::ungroup(data))
  } else if (inherits(data, c("lm", "glm"))) {
    message("Extracting model data using model.frame()...")
    df <- as.data.frame(model.frame(data))
  } else if (is.list(data)) {
    try_df <- try(as.data.frame(data), silent = TRUE)
    if (inherits(try_df, "try-error")) {
      stop("Input is a list but not coercible to data.frame.")
    }
    df <- try_df
  } else {
    if (is.function(data) || is.language(data) || is.environment(data))
      stop("Unsupported object type: function, call, or environment.")
    if (inherits(data, "tbl_spark"))
      stop("Spark DataFrames are not supported directly. Use collect() to get a local data.frame.")
    if (inherits(data, "Table"))
      stop("Arrow Table not supported directly. Use as.data.frame() first.")
    if (inherits(data, "survey.design"))
      stop("survey.design objects not supported. Extract a data.frame manually.")
    stop("Unsupported input type. Please provide a data.frame, tibble, matrix, model, or compatible object.")
  }
  
  # ---------- value normalization (optional) ----------
  if (!normalize || nrow(df) == 0L) return(df)
  
  # helpers use closures over arguments
  lower <- function(x) tolower(trimws(x))
  
  # 1) map NA-ish strings BUT keep selected survey choices as literals
  na_set   <- unique(lower(na_strings))
  keep_set <- unique(lower(keep_as_levels))
  
  map_na_strings <- function(x) {
    if (!is.character(x)) return(x)
    lx <- lower(x)
    # keep_set wins over na_set
    ix_na <- lx %in% na_set & !(lx %in% keep_set)
    if (any(ix_na, na.rm = TRUE)) x[ix_na] <- NA_character_
    x
  }
  
  # 2) yes/no normalization
  normalize_yes_no <- function(x) {
    if (!is.character(x)) return(x)
    vals <- unique(lower(na.omit(x)))
    if (length(vals) > 0L && all(vals %in% c("yes","no")))
      return(factor(lower(x), levels = c("no","yes"), ordered = TRUE))
    x
  }
  
  # 3) percent detection & conversion
  is_percenty <- function(x) {
    if (!is.character(x)) return(FALSE)
    v <- x[!is.na(x)]
    if (!length(v)) return(FALSE)
    mean(grepl("%", v, fixed = TRUE)) >= percent_detect_threshold
  }
  to_numeric_percent <- function(x) {
    # strip % and non-digits/dots, then as.numeric
    if (!is.character(x)) return(x)
    y <- gsub("%", "", x, fixed = TRUE)
    y <- suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", y)))
    y
  }
  
  # 4) datetime detection & conversion: try formats and keep the best
  parse_dt_best <- function(x) {
    if (!is.character(x)) return(x)
    best <- NULL; best_ok <- -1L
    for (fmt in datetime_formats) {
      y <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
      ok <- sum(!is.na(y))
      if (ok > best_ok) { best_ok <- ok; best <- y }
    }
    if (best_ok > 0L) best else x
  }
  
  # apply in a careful order on character columns only
  chr_cols <- names(df)[vapply(df, is.character, logical(1))]
  
  # 1) map NA-ish strings
  df[chr_cols] <- lapply(df[chr_cols], map_na_strings)
  # 2) yes/no factors
  df[chr_cols] <- lapply(df[chr_cols], normalize_yes_no)
  
  # 3) percent → numeric (decide per-column)
  for (nm in chr_cols) {
    if (is_percenty(df[[nm]])) df[[nm]] <- to_numeric_percent(df[[nm]])
  }
  
  # 4) datetime → POSIXct (only if we actually parse many values)
  for (nm in chr_cols) {
    cand <- parse_dt_best(df[[nm]])
    if (inherits(cand, "POSIXct")) {
      # accept only if at least ~60% parsed successfully (avoid false positives)
      ok_ratio <- mean(!is.na(cand))
      if (is.finite(ok_ratio) && ok_ratio >= 0.6) df[[nm]] <- cand
    }
  }
  
  df
}
