# tools/inventory_package.R
inventory_package <- function(root = getwd(), size_warn_mb = 5) {
  stopifnot(dir.exists(root))
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  
  # Grab tracked/untracked from git if available
  has_git <- nchar(Sys.which("git")) > 0 && dir.exists(file.path(root, ".git"))
  tracked   <- if (has_git) system2("git", c("ls-files"), stdout = TRUE) else character(0)
  untracked <- if (has_git) system2("git", c("ls-files", "--others", "--exclude-standard"), stdout = TRUE) else character(0)
  
  files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  files <- files[!grepl("^\\.git(/|$)", files)]   # hide .git internals
  
  classify <- function(p) {
    if (grepl("^R/", p))                  return("R source")
    if (grepl("^man/.*\\.Rd$", p))        return("man Rd")
    if (grepl("^tests/", p))              return("tests")
    if (grepl("^vignettes/", p))          return("vignettes")
    if (grepl("^inst/", p))               return("inst (installed files)")
    if (grepl("^data/.*\\.(rda|RData)$", p, ignore.case=TRUE)) return("data (rda)")
    if (grepl("^data-raw/", p))           return("data-raw (not built)")
    if (grepl("^docs/", p))               return("pkgdown docs (site)")
    if (grepl("^pkgdown/", p))            return("pkgdown cfg/cache")
    if (grepl("^(DESCRIPTION|NAMESPACE)$", p)) return("package meta")
    if (grepl("^_pkgdown\\.yml$", p))     return("pkgdown config")
    if (grepl("^\\.Rbuildignore$", p))    return(".Rbuildignore")
    if (grepl("^\\.gitignore$", p))       return(".gitignore")
    if (grepl("\\.(Rproj|Rhistory|RData)$", p, ignore.case=TRUE)) return("R session/project")
    if (grepl("^\\.Rproj\\.user/", p))    return(".Rproj.user (IDE cache)")
    if (grepl("\\.(csv|xlsx|xls|parquet|rds|zip)$", p, ignore.case=TRUE)) return("artifacts/data")
    "other"
  }
  
  info <- file.info(files)
  df <- data.frame(
    path = files,
    type = vapply(files, classify, character(1)),
    size_kb = round(info$size / 1024, 1),
    modified = as.character(info$mtime),
    git_status = if (!has_git) "n/a" else ifelse(files %in% tracked, "tracked",
                                                 ifelse(files %in% untracked, "untracked", "ignored?")),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  
  # Flags
  df$flag_large <- ifelse(df$size_kb > size_warn_mb * 1024, "LARGE", "")
  df$flag_suspect <- ifelse(grepl("\\.(Rhistory|RData)$", df$path, ignore.case=TRUE) |
                              grepl("^\\.Rproj\\.user/", df$path) |
                              grepl("(^|/)node_modules(/|$)", df$path) |
                              grepl("(^|/)renv(/|$)", df$path),
                            "REVIEW", "")
  
  df <- df[order(df$type, df$path), ]
  
  # Write CSV + print summary
  out_csv <- file.path(root, "package_inventory.csv")
  utils::write.csv(df, out_csv, row.names = FALSE)
  message("Inventory written: ", out_csv)
  
  cat("\nTop-level summary:\n")
  print(as.data.frame(table(df$type))[order(-table(df$type)) ,], row.names = FALSE)
  
  cat("\nLarge files (> ", size_warn_mb, " MB):\n", sep = "")
  print(subset(df, flag_large == "LARGE", select = c(path, type, size_kb)), row.names = FALSE)
  
  cat("\nSuspect files (IDE caches / session files / vendor dirs):\n")
  print(subset(df, flag_suspect == "REVIEW", select = c(path, type)), row.names = FALSE)
  
  invisible(df)
}

# Run it
inventory_package()
