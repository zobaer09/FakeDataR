
<!-- README.md is generated from README.Rmd. Please edit that file -->

# FakeDataR

<p align="right">

<img src="man/figures/logo.png" alt="FakeDataR logo" width="140">
</p>

<!-- badges: start -->

<!-- CRAN status -->

[![CRAN
status](https://www.r-pkg.org/badges/version/FakeDataR)](https://CRAN.R-project.org/package=FakeDataR)
[![CRAN
checks](https://badges.cranchecks.info/worst/FakeDataR.svg)](https://cran.r-project.org/web/checks/check_results_FakeDataR.html)

<!-- Downloads -->

[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/FakeDataR?color=brightgreen)](https://cran.r-project.org/package=FakeDataR)

<!-- CI -->

[![R-CMD-check](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/zobaer09/FakeDataR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/zobaer09/FakeDataR/actions/workflows/pkgdown.yaml)

<!-- Site -->

[![Website](https://img.shields.io/badge/docs-pkgdown-blue)](https://zobaer09.github.io/FakeDataR/)

<!-- badges: end -->

**FakeDataR** makes safe, synthetic stand‑ins for real datasets.  
It mirrors **types**, **factor levels**, **NA/blank rates**, and even
handles **sensitive columns** (IDs, emails, phones) via *fake* or *drop*
strategies. You can also generate fake tables from a **database schema**
without reading any rows.

------------------------------------------------------------------------

## Installation

``` r
# install.packages("devtools")
devtools::install_github("zobaer09/FakeDataR")
```

## Quick start

Generate a fake dataset that matches the structure of a real one:

``` r
library(FakeDataR)

fake_mtc <- generate_fake_data(mtcars, n = 200, seed = 1)
head(fake_mtc)
#>        mpg      cyl     disp        hp     drat       wt     qsec        vs
#> 1 16.63945 5.070033 335.2440 282.43325 4.623352 3.588993 17.57882 0.2396067
#> 2 19.14491 4.874581 145.2945 314.84395 2.834732 4.191491 20.72770 0.6477649
#> 3 23.86205 6.067187 453.7102  93.73714 4.867064 3.012021 22.34145 0.9756708
#> 4 31.74288 5.075802 431.0475 264.19953 4.376889 5.247958 20.15496 0.3779988
#> 5 15.13953 4.724673 449.4281 328.11103 3.352964 1.975893 20.39140 0.4641441
#> 6 31.51216 6.074305 361.2276 327.86627 4.229320 1.665920 21.62005 0.8122963
#>           am     gear     carb
#> 1 0.13853856 3.123219 7.102635
#> 2 0.04752457 3.710643 7.770379
#> 3 0.03391887 4.154076 7.068414
#> 4 0.91608902 4.070063 4.064007
#> 5 0.84020039 4.208546 2.343565
#> 6 0.17887142 3.972298 1.576061
```

Validate that classes and basic patterns were preserved:

``` r
validate_fake(mtcars, fake_mtc)
#>    column class_original class_fake class_match na_prop_original na_prop_fake
#> 1     mpg        numeric    numeric        TRUE                0            0
#> 2     cyl        numeric    numeric        TRUE                0            0
#> 3    disp        numeric    numeric        TRUE                0            0
#> 4      hp        numeric    numeric        TRUE                0            0
#> 5    drat        numeric    numeric        TRUE                0            0
#> 6      wt        numeric    numeric        TRUE                0            0
#> 7    qsec        numeric    numeric        TRUE                0            0
#> 8      vs        numeric    numeric        TRUE                0            0
#> 9      am        numeric    numeric        TRUE                0            0
#> 10   gear        numeric    numeric        TRUE                0            0
#> 11   carb        numeric    numeric        TRUE                0            0
#>    na_match blank_prop_original blank_prop_fake blank_match
#> 1      TRUE                  NA              NA          NA
#> 2      TRUE                  NA              NA          NA
#> 3      TRUE                  NA              NA          NA
#> 4      TRUE                  NA              NA          NA
#> 5      TRUE                  NA              NA          NA
#> 6      TRUE                  NA              NA          NA
#> 7      TRUE                  NA              NA          NA
#> 8      TRUE                  NA              NA          NA
#> 9      TRUE                  NA              NA          NA
#> 10     TRUE                  NA              NA          NA
#> 11     TRUE                  NA              NA          NA
#>    range_within_original
#> 1                   TRUE
#> 2                   TRUE
#> 3                   TRUE
#> 4                   TRUE
#> 5                   TRUE
#> 6                   TRUE
#> 7                   TRUE
#> 8                   TRUE
#> 9                   TRUE
#> 10                  TRUE
#> 11                  TRUE
```

Preserve factor levels & numeric ranges (example with `palmerpenguins`):

``` r
if (requireNamespace("palmerpenguins", quietly = TRUE)) {
  peng <- na.omit(palmerpenguins::penguins[, c("species","island","bill_length_mm","sex")])
  fake_peng <- generate_fake_data(peng, n = 400, seed = 11, category_mode = "preserve")
  validate_fake(peng, fake_peng)
}
#>           column class_original class_fake class_match na_prop_original
#> 1        species         factor     factor        TRUE                0
#> 2         island         factor     factor        TRUE                0
#> 3 bill_length_mm        numeric    numeric        TRUE                0
#> 4            sex         factor     factor        TRUE                0
#>   na_prop_fake na_match blank_prop_original blank_prop_fake blank_match
#> 1            0     TRUE                   0               0        TRUE
#> 2            0     TRUE                   0               0        TRUE
#> 3            0     TRUE                  NA              NA          NA
#> 4            0     TRUE                   0               0        TRUE
#>   range_within_original
#> 1                    NA
#> 2                    NA
#> 3                  TRUE
#> 4                    NA
```

## Sensitive columns

Detect and handle PII by name. Use strategy `"fake"` (default) or
`"drop"`:

``` r
df <- data.frame(
  id    = 1:50,
  email = sprintf("user%03d@corp.com", 1:50),
  phone = sprintf("(415) 555-%04d", 1:50),
  spend = runif(50, 10, 500)
)

# Keep the columns but replace values with synthetic ones
fake_keep <- generate_fake_data(
  df, n = 80, seed = 12,
  sensitive_detect = TRUE, sensitive_strategy = "fake"
)
intersect(df$email, fake_keep$email)  # should be character(0)
#> character(0)

# Drop sensitive columns entirely
fake_drop <- generate_fake_data(
  df, n = 80, seed = 13,
  sensitive_detect = TRUE, sensitive_strategy = "drop"
)
names(fake_drop)  # no id/email/phone
#> [1] "spend"
```

## From a database schema (no data read)

Create fake rows using only the schema (types, nullability, etc.). This
works even when you cannot access the underlying data.

``` r
library(DBI); library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), ":memory:")
dbExecute(con, "
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

sch  <- schema_from_db(con, "employees")
fake <- generate_fake_from_schema(sch, n = 50, seed = 14)

str(fake$hired_at)  # POSIXct
dbDisconnect(con)
```

## Exporting

Write fake data to common formats (CSV, RDS, Parquet) and optionally
produce an **LLM bundle** containing a schema and README.

``` r
# CSV / RDS
export_fake(fake_mtc, file.path(tempdir(), "fake_mtc.csv"))
export_fake(fake_mtc, file.path(tempdir(), "fake_mtc.rds"))

# Parquet (requires the 'arrow' package)
# install.packages("arrow")
export_fake(fake_mtc, file.path(tempdir(), "fake_mtc.parquet"))

# End-to-end bundle
b <- llm_bundle(
  mtcars, n = 200, seed = 10, level = "high",
  formats = c("csv","rds"),
  path = tempdir(), filename = "mtcars_fake",
  write_prompt = TRUE, zip = TRUE
)
b$zip_path
```

## Reproducibility

All generators respect `seed`. Given the same inputs and a fixed package
version, results are reproducible:

``` r
a1 <- generate_fake_data(CO2, n = 123, seed = 42)
a2 <- generate_fake_data(CO2, n = 123, seed = 42)
identical(a1, a2)
#> [1] TRUE
```

## Learn more

- **Getting started** vignette:
  <https://zobaer09.github.io/FakeDataR/articles/getting-started.html>  
- **Database workflow** vignette:
  <https://zobaer09.github.io/FakeDataR/articles/database-workflow.html>  
- Full function **reference**:
  <https://zobaer09.github.io/FakeDataR/reference/>

------------------------------------------------------------------------

## Contributing

Issues and pull requests are welcome! If you find a bug or have a
feature request, please open an issue on GitHub.

## License

This package is distributed under the terms of the license included in
the repository (see `LICENSE`).
