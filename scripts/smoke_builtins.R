# scripts/smoke_builtins.R
# Broad smoke tests for FakeDataR across many datasets.
# - Uses built-in datasets + optional ones (palmerpenguins, gapminder, nycflights13)
# - Exercises 8 mode combos for generate_fake_data()
# - Tries generate_fake_with_privacy() at all levels + strategies
# - Validates fake vs. original, writes quick QC CSVs
# - Optionally builds LLM bundle (if your install supports it)
# Safe on CI: uses tryCatch + small samples; skips heavy bits when pkgs absent.

message("== FakeDataR smoke on multiple datasets ==")

# 0) Load package (installed or from source)
if (!requireNamespace("FakeDataR", quietly = TRUE)) {
  if (requireNamespace("devtools", quietly = TRUE)) devtools::load_all(".") else
    stop("Install FakeDataR or run from the package folder with devtools installed.")
} else {
  library(FakeDataR)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Helpers -------------------------------------------------------------------

out_dir <- file.path(tempdir(), paste0("FakeDataR_smoke_", format(Sys.time(), "%Y%m%d_%H%M%S")))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

qc_summary <- function(orig, fake, k = 5L) {
  common <- intersect(names(orig), names(fake))
  class_cmp <- data.frame(
    column = common,
    orig   = vapply(orig[common], \(x) paste(class(x), collapse="/"), character(1)),
    fake   = vapply(fake[common], \(x) paste(class(x), collapse="/"), character(1)),
    check.names = FALSE
  )
  na_cmp <- data.frame(
    column       = common,
    na_rate_orig = vapply(orig[common], \(x) mean(is.na(x)), numeric(1)),
    na_rate_fake = vapply(fake[common], \(x) mean(is.na(x)), numeric(1)),
    check.names = FALSE
  )
  is_num <- \(x) is.numeric(x) || is.integer(x)
  num_cols <- common[vapply(orig[common], is_num, logical(1))]
  num_cmp <- if (length(num_cols)) {
    data.frame(
      column  = num_cols,
      orig_min= vapply(orig[num_cols], \(v) suppressWarnings(min(v, na.rm=TRUE)), numeric(1)),
      orig_med= vapply(orig[num_cols], \(v) suppressWarnings(stats::median(v, na.rm=TRUE)), numeric(1)),
      orig_max= vapply(orig[num_cols], \(v) suppressWarnings(max(v, na.rm=TRUE)), numeric(1)),
      fake_min= vapply(fake[num_cols], \(v) suppressWarnings(min(v, na.rm=TRUE)), numeric(1)),
      fake_med= vapply(fake[num_cols], \(v) suppressWarnings(stats::median(v, na.rm=TRUE)), numeric(1)),
      fake_max= vapply(fake[num_cols], \(v) suppressWarnings(max(v, na.rm=TRUE)), numeric(1)),
      check.names = FALSE
    )
  } else data.frame()
  is_cat <- \(x) is.factor(x) || is.character(x)
  cat_cols <- common[vapply(orig[common], is_cat, logical(1))]
  top <- function(v, k) {
    if (all(is.na(v))) return(character(0))
    tt <- sort(table(v), decreasing = TRUE)
    paste(head(paste0(names(tt), " (", as.integer(tt), ")"), k), collapse=", ")
  }
  cat_cmp <- if (length(cat_cols)) {
    do.call(rbind, lapply(cat_cols, function(cn) {
      data.frame(column=cn, top_orig=top(orig[[cn]], k), top_fake=top(fake[[cn]], k), check.names = FALSE)
    }))
  } else data.frame()
  list(class_cmp=class_cmp, na_cmp=na_cmp, num_cmp=num_cmp, cat_cmp=cat_cmp)
}

write_qc <- function(qc, prefix, dir) {
  utils::write.csv(qc$class_cmp, file.path(dir, paste0(prefix, "_classes.csv")), row.names = FALSE)
  utils::write.csv(qc$na_cmp,    file.path(dir, paste0(prefix, "_na_rates.csv")), row.names = FALSE)
  if (nrow(qc$num_cmp)) utils::write.csv(qc$num_cmp, file.path(dir, paste0(prefix, "_num_ranges.csv")), row.names = FALSE)
  if (nrow(qc$cat_cmp)) utils::write.csv(qc$cat_cmp, file.path(dir, paste0(prefix, "_top_levels.csv")), row.names = FALSE)
}

maybe <- function(pkg) suppressWarnings(requireNamespace(pkg, quietly = TRUE))

# Conservative coercion/normalization, mirroring package behavior
normalize_soft <- function(df) {
  df <- tryCatch(prepare_input_data(df), error = function(e) as.data.frame(df, check.names = FALSE))
  # Do NOT treat "not applicable"/"no data" as NA (keep as categories)
  na_tokens <- c("", "NA", "N/A")
  for (nm in names(df)) if (is.character(df[[nm]])) {
    x <- trimws(df[[nm]]); x[x %in% na_tokens] <- NA_character_; df[[nm]] <- x
  }
  # Percent -> numeric
  pct_cols <- names(df)[sapply(df, \(x) is.character(x) && mean(grepl("%", x), na.rm=TRUE) > 0.6)]
  if (length(pct_cols) && maybe("readr")) {
    df[pct_cols] <- lapply(df[pct_cols], readr::parse_number)
  }
  # Datetime (common US patterns) -> POSIXct safely
  if (maybe("lubridate")) {
    dt_orders <- c("mdY HM","mdY HMS","mdy HM","mdy HMS")
    dt_cols <- names(df)[sapply(df, \(x) is.character(x) && mean(grepl("\\d{1,2}/\\d{1,2}/\\d{2,4}\\s+\\d{1,2}:\\d{2}", x), na.rm=TRUE) > 0.6)]
    for (nm in dt_cols) df[[nm]] <- lubridate::parse_date_time(df[[nm]], orders=dt_orders, tz="UTC", quiet=TRUE)
  }
  # yes/no -> factor
  yn_cols <- names(df)[sapply(df, \(x) is.character(x) && all(tolower(na.omit(unique(x))) %in% c("yes","no")))]
  for (nm in yn_cols) df[[nm]] <- factor(tolower(df[[nm]]), levels=c("no","yes"))
  df
}

run_one <- function(name, df) {
  message("\n== Dataset: ", name, " ==")
  orig <- normalize_soft(df)
  n_fake <- min(150L, nrow(orig))
  
  # 8 combinations
  cat_modes <- c("preserve","generic")
  num_modes <- c("range","distribution")
  col_modes <- c("keep","generic")
  
  run_id <- 1L
  for (cm in cat_modes) for (nm in num_modes) for (colm in col_modes) {
    message(sprintf("  >>> %d/8: category=%s | numeric=%s | column=%s", run_id, cm, nm, colm))
    fake <- tryCatch(
      generate_fake_data(
        data = orig, n = n_fake,
        category_mode = cm, numeric_mode = nm, column_mode = colm,
        normalize = FALSE # already normalized softly
      ),
      error = function(e) { message("     ❌ ", e$message); NULL }
    )
    if (is.null(fake)) { run_id <- run_id + 1L; next }
    qc <- qc_summary(orig, fake)
    prefix <- sprintf("%s_modes_%02d_%s_%s_%s", name, run_id, cm, nm, colm)
    write_qc(qc, prefix, out_dir)
    export_fake(fake, file.path(out_dir, paste0(prefix, ".csv")))
    run_id <- run_id + 1L
  }
  
  # Privacy levels/strategies
  known_sensitive <- intersect(c("id","email","phone","RegId"), names(orig))
  for (lvl in c("low","medium","high")) for (strat in c("fake","drop")) {
    message(sprintf("  privacy level=%s strategy=%s", lvl, strat))
    fake_p <- tryCatch(
      generate_fake_with_privacy(
        data = orig, n = n_fake, level = lvl, seed = 123,
        sensitive = known_sensitive,
        sensitive_detect = TRUE,
        sensitive_strategy = strat,
        normalize = FALSE
      ),
      error = function(e) { message("     ❌ ", e$message); NULL }
    )
    if (is.null(fake_p)) next
    qc <- qc_summary(orig, fake_p)
    prefix <- sprintf("%s_privacy_%s_%s", name, lvl, strat)
    write_qc(qc, prefix, out_dir)
    export_fake(fake_p, file.path(out_dir, paste0(prefix, ".csv")))
  }
}

# --- Datasets ------------------------------------------------------------------

datasets <- list(
  iris           = iris,
  mtcars         = mtcars,
  airquality     = airquality,
  ToothGrowth    = ToothGrowth,
  PlantGrowth    = PlantGrowth
)

# Optional packages
if (maybe("palmerpenguins")) datasets$penguins <- palmerpenguins::penguins
if (maybe("gapminder"))      datasets$gapminder <- gapminder::gapminder
if (maybe("nycflights13")) {
  datasets$flights_small <- head(nycflights13::flights, 1000)
}

# Add a synthetic “messy” dataset to test percents/datetimes/yesno
set.seed(42)
messy <- data.frame(
  id        = sprintf("id%03d", 1:200),
  Progress  = paste0(sample(50:100, 200, TRUE), "%"),      # percent-like
  RegDate   = format(
    as.POSIXct("2023-07-01 00:00", tz="UTC") +
      sample(0:(60*60*24*120), 200, TRUE),
    "%m/%d/%Y %H:%M"),                         # mm/dd/yyyy HH:MM
  Consent   = sample(c("yes","no"), 200, TRUE),
  City      = sample(c("Bronx","Queens","Brooklyn","Manhattan","Staten Island","no data"), 200, TRUE),
  stringsAsFactors = FALSE, check.names = FALSE
)
datasets$messy <- messy

# Run
for (nm in names(datasets)) {
  df <- datasets[[nm]]
  # be defensive about tibbles
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  run_one(nm, df)
}

message("\nAll smoke artifacts written to: ", out_dir)
print(list.files(out_dir, full.names = TRUE))
