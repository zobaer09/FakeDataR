# Sensitive-field detection & fakers
# ---------------------------------
# This file exposes one public helper:
#   - detect_sensitive_columns()
# and keeps all fakers & older helpers internal for compatibility.

#' Detect sensitive columns by name
#'
#' Uses a broad, configurable regex library to match likely PII columns.
#' You can extend it with `extra_patterns` (they get ORed in) or replace
#' everything with a single `override_regex`.
#'
#' @param x_names Character vector of column names to check.
#' @param extra_patterns Character vector of additional regexes to OR in.
#'   Examples: c("MRN", "NHS", "Aadhaar", "passport")
#' @param override_regex Optional single regex string that fully replaces the
#'   defaults (case-insensitive). When supplied, `extra_patterns` is ignored.
#'
#' @return Character vector of names from `x_names` that matched.
#' @examples
#' detect_sensitive_columns(c("id","email","home_phone","zip","notes"))
#' detect_sensitive_columns(names(mtcars), extra_patterns = c("^vin$", "passport"))
#' @export
detect_sensitive_columns <- function(x_names,
                                     extra_patterns = NULL,
                                     override_regex = NULL) {
  stopifnot(is.character(x_names))
  rx <- if (!is.null(override_regex)) {
    override_regex
  } else {
    pats <- c(.default_sensitive_patterns(), extra_patterns %||% character(0))
    paste0("(?i)(", paste(pats, collapse = "|"), ")")
  }
  x_names[grepl(rx, x_names, perl = TRUE)]
}

# ---- Internal pattern libraries ----------------------------------------------

# A broad set of defaults covering many common PII-ish fields.
# NOTE: These are ORed together in detect_sensitive_columns().
#' @keywords internal
#' @noRd
.default_sensitive_patterns <- function() {
  c(
    # IDs / names
    "^id$", "employee[_-]?id", "user(name|[_-]?id)?$", "customer[_-]?id",
    "full[_-]?name", "first[_-]?name", "last[_-]?name", "middle[_-]?name",
    "nickname|alias",
    
    # Contact
    "email|e-mail", "phone|tel|mobile|cell|telephone|fax",
    
    # Address / geo
    "address|street|road|ave(nue)?|boulevard|blvd|apt|unit|suite",
    "zip|postal|post[_-]?code|postcode",
    "city|town|county|state|province|region|country",
    "lat(itude)?|lon(gitude)?|gps|geo",
    
    # Government / national IDs (international coverage)
    "RegId|ssn|sin|nin\\b|aadhaar|aadhar|bvn|curp|dni|cedul+a|cpf|\\bpan\\b|\\btin\\b|\\bein\\b|pesel",
    
    # Licenses & travel
    "passport|visa|license|licence|driver|\\bdl\\b|vin|plate",
    
    # Finance / payments
    "iban|swift|bic|routing|sort[_-]?code|account|acct|bank",
    "credit|debit|card|cvv|cvc|pan[_-]?number",
    
    # Auth / secrets / device
    "password|pass|pwd|pin|otp|secret|token|api[_-]?key|auth|bearer|session|cookie",
    "ip(_address)?|mac(_address)?|imei|imsi|serial|device|udid|android[_-]?id|idfa|gaid",
    
    # Medical / patient
    "MRN|NHS|medicare|medicaid|patient|diagnosis",
    
    # Birthdays / education
    "dob|date[_-]?of[_-]?birth|birthday|student[_-]?id"
  )
}

# A grouped library (for label mapping) kept internal. Each element is one group.
#' @keywords internal
#' @noRd
.default_sensitive_groups <- function() {
  list(
    id        = "(?i)(^id$|employee[_-]?id|customer[_-]?id|user[_-]?id$|nin\\b|MRN|NHS|ssn|sin|student[_-]?id)",
    email     = "(?i)(email|e-mail)",
    phone     = "(?i)(phone|tel|mobile|cell|telephone|fax)",
    address   = "(?i)(address|street|road|ave(nue)?|boulevard|blvd|apt|unit|suite|zip|postal|postcode|city|state|province|country)",
    geo       = "(?i)(lat(itude)?|lon(gitude)?|gps|geo)",
    gov_id    = "(?i)(passport|visa|license|licence|driver|\\bdl\\b|vin|plate|aadhaar|aadhar|bvn|curp|dni|cedul+a|cpf|\\bpan\\b|\\btin\\b|\\bein\\b|pesel)",
    finance   = "(?i)(iban|swift|bic|routing|sort[_-]?code|account|acct|bank|credit|debit|card|cvv|cvc|pan[_-]?number)",
    auth      = "(?i)(password|pass|pwd|pin|otp|secret|token|api[_-]?key|auth|bearer|session|cookie)",
    device    = "(?i)(ip(_address)?|mac(_address)?|imei|imsi|serial|device|udid|android[_-]?id|idfa|gaid)",
    medical   = "(?i)(MRN|NHS|medicare|medicaid|patient|diagnosis)",
    birthday  = "(?i)(dob|date[_-]?of[_-]?birth|birthday)",
    name      = "(?i)(full[_-]?name|first[_-]?name|last[_-]?name|middle[_-]?name|nickname|alias)"
  )
}

# ---- Back-compat wrappers (internal) ------------------------------------------

# Your original default list (id/email/phone only). Kept for compatibility.
#' @keywords internal
#' @noRd
.default_sensitive_regex <- function() {
  list(
    id    = "(?i)(^id$|(^.*_id$)|(^.*id$)|(^uid$)|(^user[_-]?id$)|(^employee[_-]?id$))",
    email = "(?i)(^e[-_ ]?mail$|^email$|^email_addr(ess)?$|^.*_email$)",
    phone = "(?i)(^phone$|^phone[-_ ]?number$|^tel$|^telephone$|^mobile$|^cell$|^.*_phone$)"
  )
}

# Return a named character vector: names = column names, values in {"id","email","phone"}
# (kept for back-compat; new code should prefer detect_sensitive_columns()).
#' @keywords internal
#' @noRd
identify_sensitive_cols <- function(data, regex = .default_sensitive_regex()) {
  nms <- names(data)
  out <- character(0)
  for (nm in nms) {
    lbl <- NA_character_
    if (grepl(regex$email, nm, perl = TRUE)) lbl <- "email"
    if (grepl(regex$phone, nm, perl = TRUE)) lbl <- "phone"
    if (grepl(regex$id,    nm, perl = TRUE)) lbl <- ifelse(is.na(lbl), "id", lbl)
    if (!is.na(lbl)) out[nm] <- lbl
  }
  out
}

# Optional: internal labeler across many groups (not exported)
#' @keywords internal
#' @noRd
.detect_sensitive_label <- function(nms) {
  groups <- .default_sensitive_groups()
  nmsl   <- tolower(nms)
  lab    <- rep(NA_character_, length(nmsl))
  for (g in names(groups)) {
    m <- grepl(groups[[g]], nmsl, perl = TRUE)
    lab[m] <- ifelse(is.na(lab[m]), g, lab[m])
  }
  lab
}

# TRUE/FALSE sensitive flag (internal)
#' @keywords internal
#' @noRd
.detect_sensitive_logical <- function(nms) {
  !is.na(.detect_sensitive_label(nms))
}

# ---- Masking and fakers (internal) -------------------------------------------

# Replace each character of a template string with the same kind (digit/lower/upper).
#' @keywords internal
#' @noRd
.mask_like <- function(template) {
  chars <- strsplit(template, "", fixed = TRUE)[[1]]
  repl <- vapply(chars, function(ch) {
    if (grepl("[0-9]", ch)) {
      as.character(sample(0:9, 1))
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

# Vectorized masker based on observed strings
#' @keywords internal
#' @noRd
.mask_from_examples <- function(x, n) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) return(rep("XXX-0000", n))
  tmpl <- sample(x, n, replace = TRUE)
  vapply(tmpl, .mask_like, character(1))
}

# Fake IDs (numeric or string) with approximate structure/length
#' @keywords internal
#' @noRd
.fake_id_like <- function(x, n) {
  if (is.numeric(x) || is.integer(x)) {
    xx <- suppressWarnings(as.numeric(x))
    xx <- xx[is.finite(xx)]
    if (length(xx) == 0) {
      pool_min <- 1e7; pool_max <- 1e8 - 1
    } else {
      digs <- floor(stats::median(log10(abs(xx)), na.rm = TRUE)) + 1
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
      dups <- duplicated(vals)
      if (any(dups)) vals[dups] <- vals[dups] + sample(seq_len(sum(dups)))
    }
    if (inherits(x, "integer")) vals <- as.integer(vals)
    vals
  } else {
    out <- .mask_from_examples(x, n)
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

# Keep punctuation/spacing, randomize digits; phone-like strings
#' @keywords internal
#' @noRd
.fake_phone_like <- function(x, n) {
  x_obs <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x_obs) > 0) return(.mask_from_examples(x_obs, n))
  paste0(
    sprintf("%03d", sample(200:999, n, TRUE)), "-",
    sprintf("%03d", sample(200:999, n, TRUE)), "-",
    sprintf("%04d", sample(0:9999, n, TRUE))
  )
}
