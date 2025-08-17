
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FakeDataR

<!-- badges: start -->

[![R-CMD-check](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://zobaer09.github.io/FakeDataR/)
<!-- badges: end -->

**FakeDataR** creates synthetic tabular data that mirrors the structure
and key patterns of a real dataset:

- preserves types
  (numeric/integer/Date/POSIXct/logical/factor/character)  
- keeps factor levels and NA / blank rates  
- optional numeric generation by **range** or **distribution**  
- privacy: detect `id`, `email`, `phone` columns and either **fake** or
  **drop** them  
- generate from a **database schema** (no data read)  
- export helpers (CSV/RDS/Parquet) and an **LLM bundle** for sharing

**Docs:** [Website](https://zobaer09.github.io/FakeDataR/) •
[Articles](https://zobaer09.github.io/FakeDataR/articles/) •
[Reference](https://zobaer09.github.io/FakeDataR/reference/)

## Installation

``` r
# install.packages("remotes")
remotes::install_github("zobaer09/FakeDataR")
```

Optional: - `DBI`, `RSQLite` – for the database demo below  
- `arrow` – to write Parquet via `export_fake()`

## Quick start

``` r
library(FakeDataR)

# Make fake data from a data frame
fake_co2 <- generate_fake_data(as.data.frame(CO2), n = 200, seed = 1)
head(fake_co2)
#>   Plant        Type  Treatment     conc    uptake
#> 1   Mn3 Mississippi nonchilled 270.3746 17.554606
#> 2   Qc1      Quebec nonchilled 627.2001 34.931717
#> 3   Mn1      Quebec nonchilled 775.1113 17.070400
#> 4   Qn1 Mississippi nonchilled 879.8511 16.579817
#> 5   Qn2 Mississippi    chilled 431.4751  8.494248
#> 6   Mn1      Quebec    chilled 817.9272 17.702660

# Validate key properties against the source
validate_fake(as.data.frame(CO2), fake_co2)
#>      column class_original class_fake class_match na_prop_original na_prop_fake
#> 1     Plant ordered/factor     factor       FALSE                0            0
#> 2      Type         factor     factor        TRUE                0            0
#> 3 Treatment         factor     factor        TRUE                0            0
#> 4      conc        numeric    numeric        TRUE                0            0
#> 5    uptake        numeric    numeric        TRUE                0            0
#>   na_match blank_prop_original blank_prop_fake blank_match
#> 1     TRUE                   0               0        TRUE
#> 2     TRUE                   0               0        TRUE
#> 3     TRUE                   0               0        TRUE
#> 4     TRUE                  NA              NA          NA
#> 5     TRUE                  NA              NA          NA
#>   range_within_original
#> 1                    NA
#> 2                    NA
#> 3                    NA
#> 4                  TRUE
#> 5                  TRUE
```

## Privacy: detect & handle sensitive columns

``` r
df <- data.frame(
  id    = 1:10,
  email = sprintf("user%02d@corp.com", 1:10),
  phone = sprintf("(415) 555-%04d", 1:10),
  spend = runif(10, 10, 100)
)

# Keep sensitive columns but fully synthetic content
fake_keep <- generate_fake_data(
  df, n = 20, seed = 2,
  sensitive_detect   = TRUE,
  sensitive_strategy = "fake"
)

# Or drop the sensitive columns entirely
fake_drop <- generate_fake_data(
  df, n = 20, seed = 2,
  sensitive_detect   = TRUE,
  sensitive_strategy = "drop"
)

names(fake_keep); names(fake_drop)
#> [1] "id"    "email" "phone" "spend"
#> [1] "spend"
```

## From a database schema (no rows read)

``` r
library(DBI); library(RSQLite)
#> Warning: package 'DBI' was built under R version 4.3.3

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbExecute(con, "
  CREATE TABLE employees (
    id INTEGER,
    email TEXT,
    phone TEXT,
    is_active BOOLEAN,
    hired_at TIMESTAMP,
    salary NUMERIC,
    dept TEXT
  )
")
#> [1] 0

sch      <- schema_from_db(con, "employees")
fake_db  <- generate_fake_from_schema(sch, n = 50, seed = 14)
str(fake_db$hired_at)   # POSIXct
#>  POSIXct[1:50], format: "2025-08-10 22:43:46" "2025-08-10 14:08:52" "2025-08-11 19:20:36" ...

DBI::dbDisconnect(con)
```

## Export (CSV/RDS/Parquet)

``` r
tmp <- tempfile(fileext = ".csv")
export_fake(fake_co2, tmp)

# Parquet (if arrow is available)
if (requireNamespace("arrow", quietly = TRUE)) {
  export_fake(fake_co2, sub("\\.csv$", ".parquet", tmp))
}
```

## Reproducibility

All generators accept `seed` for deterministic output:

``` r
a1 <- generate_fake_data(CO2, n = 123, seed = 42)
a2 <- generate_fake_data(CO2, n = 123, seed = 42)
identical(a1, a2)  # TRUE
```

## Learn more

- **Getting started** article: overview & real-data smoke tests  
- **Database workflow** article: schema-only generation + LLM bundle  
- Full function docs:
  [reference](https://zobaer09.github.io/FakeDataR/reference/)

------------------------------------------------------------------------

License: MIT. Contributions welcome via pull requests and issues.
