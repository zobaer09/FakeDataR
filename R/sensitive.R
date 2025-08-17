# Sensitive-field detection & fakers
# (internal by default; not exported)

# Default regex rules for column-name detection
#' @keywords internal
#' @noRd
.default_sensitive_regex <- function() {
  list(
    id    = "(^id$)|(^.*_id$)|(^.*id$)|(^uid$)|(^user[_-]?id$)|(^employee[_-]?id$)",
    email = "(^e[-_ ]?mail$)|(^email$)|(^email_addr(ess)?$)|(^.*_email$)",
    phone = "(^phone$)|(^phone[-_ ]?number$)|(^tel$)|(^telephone$)|(^mobile$)|(^cell$)|(^.*_phone$)"
  )
}

# Return a named character vector: names = column names, values in {"id","email","phone"}
#' @keywords internal
#' @noRd
identify_sensitive_cols <- function(data, regex = .default_sensitive_regex()) {
  nms <- names(data)
  out <- character(0)
  for (nm in nms) {
    lbl <- NA_character_
    if (grepl(regex$email, nm, ignore.case = TRUE)) lbl <- "email"
    if (grepl(regex$phone, nm, ignore.case = TRUE)) lbl <- "phone"
    if (grepl(regex$id,    nm, ignore.case = TRUE)) lbl <- ifelse(is.na(lbl), "id", lbl)
    if (!is.na(lbl)) out[nm] <- lbl
  }
  out
}

# Replace each character of a template string with the same "kind":
# digits -> random digit; lowercase -> random lowercase; uppercase -> random uppercase; other chars kept.
#' @keywords internal
#' @noRd
.mask_like <- function(template) {
  chars <- strsplit(template, "", fixed = TRUE)[[1]]
  repl <- vapply(chars, function(ch) {
    if (grepl("[0-9]", ch)) {
      as.character(sample(0:9, 1))           # ensure character
    } else if (grepl("[A-Z]", ch)) {
      sample(LETTERS, 1)
    } else if (grepl("[a-z]", ch)) {
      sample(letters, 1)
    } else {
      ch
    }
  }, character(1))
  paste0(repl, collapse = "")
}

# Vectorized masker: sample a template from observed strings
#' @keywords internal
#' @noRd
.mask_from_examples <- function(x, n) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) {
    # simple phone-like pattern
    return(rep("XXX-0000", n))
  }
  tmpl <- sample(x, n, replace = TRUE)
  vapply(tmpl, .mask_like, character(1))
}


# Fake IDs (numeric or string) with approximate structure/length, aiming for uniqueness
#' @keywords internal
#' @noRd
.fake_id_like <- function(x, n) {
  if (is.numeric(x) || is.integer(x)) {
    xx <- suppressWarnings(as.numeric(x))
    xx <- xx[is.finite(xx)]
    if (length(xx) == 0) {
      # default 8-digit IDs
      pool_min <- 1e7; pool_max <- 1e8 - 1
    } else {
      # derive digit length from median
      digs <- floor(median(log10(abs(xx)), na.rm = TRUE)) + 1
      digs[!is.finite(digs)] <- 8
      digs <- max(4, min(12, digs))
      pool_min <- 10^(digs - 1)
      pool_max <- 10^digs - 1
    }
    range_size <- pool_max - pool_min + 1
    if (n <= range_size) {
      vals <- sample(seq.int(pool_min, pool_max), n, replace = FALSE)
    } else {
      vals <- sample(seq.int(pool_min, pool_max), n, replace = TRUE)
      # force uniqueness by adding tiny jitter then rounding
      dups <- duplicated(vals)
      if (any(dups)) {
        vals[dups] <- vals[dups] + sample(seq_len(sum(dups)))
      }
    }
    if (inherits(x, "integer")) vals <- as.integer(vals)
    return(vals)
  } else {
    # preserve formatting pattern
    out <- .mask_from_examples(x, n)
    # nudge toward uniqueness if needed
    if (any(duplicated(out))) {
      idx <- which(duplicated(out))
      out[idx] <- paste0(out[idx], "-", seq_along(idx))
    }
    out
  }
}

# RFC 2606 reserved domains so we never hit real addresses
#' @keywords internal
#' @noRd
.fake_email <- function(n) {
  doms <- c("example.com", "example.org", "example.net")
  local <- sprintf("user%06d", sample(1e6 - 1, n, replace = TRUE))
  paste0(local, "@", sample(doms, n, replace = TRUE))
}

# Keep punctuation/spacing, randomize digits; good for phone-like strings
#' @keywords internal
#' @noRd
.fake_phone_like <- function(x, n) {
  # Use observed pattern if possible
  if (length(x[!is.na(x) & nzchar(trimws(x))]) > 0) {
    return(.mask_from_examples(x, n))
  }
  # fallback: 10 digits grouped
  paste0(
    sprintf("%03d", sample(200:999, n, TRUE)), "-",
    sprintf("%03d", sample(200:999, n, TRUE)), "-",
    sprintf("%04d", sample(0:9999, n, TRUE))
  )
}

# Character label per name (or NA)
.detect_sensitive_label <- function(nms) {
  nmsl <- tolower(nms)
  lab <- rep(NA_character_, length(nmsl))
  
  is_id    <- grepl("\\b(id|user[_-]?id|customer[_-]?id|ssn|sin)\\b", nmsl)
  is_email <- grepl("mail", nmsl)
  is_phone <- grepl("phone|mobile|cell|tel", nmsl)
  
  lab[is_id]    <- "id"
  lab[is_email] <- "email"
  lab[is_phone] <- "phone"
  lab
}

# TRUE/FALSE sensitive flag
.detect_sensitive_logical <- function(nms) {
  !is.na(.detect_sensitive_label(nms))
}

