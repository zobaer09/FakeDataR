# util -------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- helpers ---------------------------------------------------------------

.sql_type_to_r <- function(db_type, colname = "") {
  dt <- tolower(as.character(db_type))
  nm <- tolower(as.character(colname))
  
  # Name-based hints
  nm_is_datetime <- grepl("(^|_)(at|time|timestamp|datetime)(s)?($|_)", nm)
  nm_is_date     <- grepl("(^|_)date($|_)", nm) || grepl("\\bdate\\b", nm)
  
  # 1) Start from DB-provided type
  if (grepl("bool|bit|tinyint\\s*\\(\\s*1\\s*\\)", dt) ||
      grepl("^(is|has|flag|active|enabled)", nm)) {
    out <- "logical"
  } else if (grepl("timestamp|timestamptz|datetime", dt)) {
    out <- "POSIXct"
  } else if (grepl("\\bdate\\b", dt)) {
    out <- "Date"
  } else if (grepl("int", dt)) {
    out <- "integer"
  } else if (grepl("float|real|double|decimal|numeric", dt)) {
    out <- "numeric"
  } else if (grepl("char|text|clob|uuid|json|xml|blob", dt) || dt == "" || is.na(dt)) {
    out <- "character"
  } else {
    out <- "character"
  }
  
  # 2) Override by column-name hints (handles SQLite DATETIME affinity weirdness)
  if (nm_is_datetime) {
    out <- "POSIXct"
  } else if (nm_is_date && out != "POSIXct") {
    out <- "Date"
  }
  
  # 3) Guard
  allowed <- c("integer","numeric","character","logical","Date","POSIXct")
  if (!out %in% allowed) out <- "character"
  out
}

# TRUE/FALSE sensitive flag by name
.detect_sensitive_logical <- function(names_vec) {
  nms <- tolower(as.character(names_vec))
  id_pat    <- "(^id$|_id$|\\bid\\b)"
  email_pat <- "email|e[-_ ]?mail"
  phone_pat <- "phone|tel|mobile|cell"
  grepl(paste(id_pat, email_pat, phone_pat, sep="|"), nms)
}

.detect_sensitive_name <- function(nm) {
  grepl("(^id$|email|e-mail|phone|tel|mobile|ssn|sin|passport|iban|account|card|^name$|address)",
        nm, ignore.case = TRUE)
}


# ---- main -----------------------------------------------------------------

#' Extract a table schema from a DB connection
#'
#' Returns a data frame describing the columns of a database table.
#'
#' @param conn A DBI connection.
#' @param table Character scalar: table name to introspect.
#' @param level Privacy preset to annotate in schema metadata:
#'   one of "low", "medium", "high". Default "medium".
#'
#' @return A data.frame with column metadata (e.g., name, type).
#'
#' @examples
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   on.exit(DBI::dbDisconnect(con), add = TRUE)
#'   DBI::dbWriteTable(con, "mtcars", mtcars[1:3, ])
#'   sc <- schema_from_db(con, "mtcars")
#'   head(sc)
#' }
#'
#' @export


schema_from_db <- function(conn, table, level = c("medium","low","high")) {
  level <- match.arg(level)
  
  sql <- paste0("SELECT * FROM ", DBI::dbQuoteIdentifier(conn, table), " LIMIT 0")
  res <- DBI::dbSendQuery(conn, sql)
  on.exit(DBI::dbClearResult(res), add = TRUE)
  info <- DBI::dbColumnInfo(res)  # name, type, ...
  
  types <- vapply(
    seq_len(nrow(info)),
    function(i) .sql_type_to_r(info$type[i], info$name[i]),
    character(1)
  )
  allowed <- c("integer","numeric","character","logical","Date","POSIXct")
  types[!types %in% allowed] <- "character"
  
  sch <- data.frame(
    name      = as.character(info$name),
    type      = types,
    nullable  = rep(TRUE, length(types)),
    sensitive = .detect_sensitive_logical(info$name),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  # exact column order the tests expect
  sch <- sch[, c("name","type","nullable","sensitive"), drop = FALSE]
  
  # harmless metadata (still a plain data.frame)
  attr(sch, "source")             <- "db_schema_only"
  attr(sch, "table")              <- as.character(table)
  attr(sch, "level")              <- level
  attr(sch, "sensitive_strategy") <- "fake"
  
  sch
}

# internal: df schema -> list schema for JSON writing
.df_schema_to_list_schema <- function(sch_df) {
  stopifnot(is.data.frame(sch_df))
  cols <- lapply(seq_len(nrow(sch_df)), function(i) {
    list(
      name      = sch_df$name[i],
      type      = sch_df$type[i],
      nullable  = isTRUE(sch_df$nullable[i]),
      sensitive = isTRUE(sch_df$sensitive[i])
    )
  })
  cols <- stats::setNames(cols, sch_df$name)
  
  lst <- list(
    version            = 1L,
    source             = attr(sch_df, "source") %||% "db_schema_only",
    table              = attr(sch_df, "table")  %||% NA_character_,
    level              = attr(sch_df, "level")  %||% "medium",
    sensitive_strategy = attr(sch_df, "sensitive_strategy") %||% "fake",
    columns            = cols
  )
  class(lst) <- c("FakeDataSchema", "list")
  lst
}

#' Generate fake data from a DB schema data.frame
#' @param sch_df data.frame from schema_from_db()
#' @param n rows
#' @param seed optional
#' @export
generate_fake_from_schema <- function(sch_df, n = 30, seed = NULL) {
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
  if (!is.data.frame(sch_df)) stop("sch_df must be a data.frame")
  required <- c("name","type","nullable","sensitive")
  if (!all(required %in% names(sch_df))) {
    stop("schema missing required columns: ", paste(setdiff(required, names(sch_df)), collapse = ", "))
  }
  # enforce order
  sch_df <- sch_df[, required, drop = FALSE]
  
  out <- vector("list", nrow(sch_df))
  names(out) <- sch_df$name
  
  for (i in seq_len(nrow(sch_df))) {
    tp <- sch_df$type[i]
    nullable <- isTRUE(sch_df$nullable[i])
    na_prop <- if (nullable) 0.1 else 0
    is_na <- rbinom(n, 1, na_prop) == 1
    
    v <- switch(tp,
                "integer" = {
                  x <- as.integer(sample.int(100000L, n, replace = TRUE))
                  x[is_na] <- NA_integer_; x
                },
                "numeric" = {
                  x <- runif(n)
                  x[is_na] <- NA_real_; x
                },
                "character" = {
                  x <- vapply(seq_len(n), function(j) paste0("x", paste(sample(letters, 5, TRUE), collapse="")), character(1))
                  x[is_na] <- NA_character_; x
                },
                "logical" = {
                  x <- sample(c(TRUE, FALSE), n, replace = TRUE)
                  x[is_na] <- NA; x
                },
                "Date" = {
                  pool <- seq(Sys.Date() - 365L, Sys.Date(), by = "day")
                  x <- as.Date(sample(pool, n, replace = TRUE))
                  x[is_na] <- as.Date(NA); x
                },
                "POSIXct" = {
                  now <- Sys.time()
                  x <- as.POSIXct(runif(n, as.numeric(now) - 7 * 24 * 3600, as.numeric(now)),
                                  origin = "1970-01-01", tz = "UTC")
                  x[is_na] <- as.POSIXct(NA, origin = "1970-01-01", tz = "UTC"); x
                },
                {
                  x <- rep(NA_character_, n); x
                }
    )
    
    out[[i]] <- v
  }
  
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}



#' Build an LLM bundle directly from a database table
#'
#' Reads just the schema from `table` on `conn`, synthesizes `n` fake rows,
#' writes a schema JSON, fake dataset(s), and a README prompt, and optionally
#' zips them into a single archive.
#'
#' @param conn A DBI connection.
#' @param table Character scalar: table name to read.
#' @inheritParams llm_bundle
#'
#' @return Invisibly, a list with useful paths:
#' \itemize{
#'   \item \code{schema_path} – schema JSON
#'   \item \code{files} – vector of written fake-data files
#'   \item \code{zip_path} – zip archive path (if \code{zip = TRUE})
#' }
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("DBI", quietly = TRUE) &&
#'     requireNamespace("RSQLite", quietly = TRUE)) {
#'   con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#'   on.exit(DBI::dbDisconnect(con), add = TRUE)
#'   DBI::dbWriteTable(con, "cars", head(cars, 20), overwrite = TRUE)
#'   out <- llm_bundle_from_db(
#'     con, "cars",
#'     n = 100, level = "medium",
#'     formats = c("csv","rds"),
#'     path = tempdir(), filename = "db_bundle",
#'     seed = 1, write_prompt = TRUE, zip = TRUE
#'   )
#' }
#' }
#'
#' @export


llm_bundle_from_db <- function(conn, table,
                               n = 30,
                               level = c("medium","low","high"),
                               formats = c("csv","rds"),
                               path = tempdir(),
                               filename = "fake_from_db",
                               seed = NULL,
                               write_prompt = TRUE,
                               zip = FALSE,
                               zip_filename = NULL,
                               sensitive_strategy = c("fake","drop")) {
  level <- match.arg(level)
  sensitive_strategy <- match.arg(sensitive_strategy)
  
  sch_df <- schema_from_db(conn, table, level = level)
  attr(sch_df, "sensitive_strategy") <- sensitive_strategy
  fake   <- generate_fake_from_schema(sch_df, n = n, seed = seed)
  
  # write JSON schema
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Writing schema requires the 'jsonlite' package. Please install it.")
  }
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  schema_path <- file.path(path, paste0(filename, "_schema.json"))
  jsonlite::write_json(
    .df_schema_to_list_schema(sch_df),
    schema_path,
    pretty = TRUE, auto_unbox = TRUE,
    na = "null", null = "null",
    POSIXt = "ISO8601", Date = "ISO8601"
  )
  
  # write data
  formats <- unique(tolower(formats))
  data_paths <- list()
  for (fmt in formats) {
    p <- file.path(path, paste0(filename, ".", fmt))
    export_fake(fake, p)
    data_paths[[fmt]] <- p
  }
  
  readme_path <- NULL
  if (isTRUE(write_prompt)) {
    readme_path <- generate_llm_prompt(
      fake_path   = data_paths[[1]],
      schema_path = schema_path,
      write_file  = TRUE,
      path        = path,
      filename    = "README_FOR_LLM.txt"
    )
  }
  
  zip_path <- NULL
  if (isTRUE(zip)) {
    if (is.null(zip_filename)) zip_filename <- paste0(filename, ".zip")
    zip_path <- file.path(path, zip_filename)
    files <- c(unlist(data_paths, use.names = FALSE), schema_path, readme_path)
    files <- files[!is.na(files)]
    zip_llm_bundle(files, zip_path)
  }
  
  list(
    data_paths  = data_paths,
    schema_path = schema_path,
    readme_path = readme_path,
    zip_path    = zip_path,
    fake        = fake,
    schema      = sch_df
  )
}
