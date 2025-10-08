
<!-- README.md is generated from README.Rmd. Please edit that file -->

if (!requireNamespace(“pkgdown”, quietly = TRUE) \|\|
!pkgdown::in_pkgdown()) { cat(’
<p align="right">

<img src="man/figures/logo.png" alt="FakeDataR logo" width="140">
</p>

’) }

# FakeDataR

[![CRAN
status](https://www.r-pkg.org/badges/version/FakeDataR)](https://CRAN.R-project.org/package=FakeDataR)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/FakeDataR?color=brightgreen)](https://cran.r-project.org/package=FakeDataR)
[![R-CMD-check](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zobaer09/FakeDataR/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/zobaer09/FakeDataR/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/zobaer09/FakeDataR/actions/workflows/pkgdown.yaml)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue)](https://zobaer09.github.io/FakeDataR/)

## Why FakeDataR?

- **Problem:** Many users can’t run large models locally. To get help
  from an LLM or share examples, they hand-edit real datasets, slow,
  brittle, and risky for privacy.
- **Solution:** **FakeDataR** runs entirely on your machine (R only). It
  reads your real data locally and generates a **synthetic copy** that
  mirrors structure (schema, types, factor levels, ranges, missingness).
  Sensitive fields can be masked or dropped. You can safely upload the
  **synthetic** dataset to an LLM or share with collaborators, without
  exposing the original data.

## What makes it different?

- **Local by default:** No cloud round-trips; your real data stays on
  your machine.
- **Schema-faithful:** Preserves column types, factor levels, NA
  patterns, and numeric ranges.
- **Privacy controls:** Heuristics for sensitive fields (e.g., IDs,
  emails, phones) with strategies (`fake`, `drop`).
- **LLM bundles:** Exports fake data + JSON schema + README prompt in
  one bundle.
- **DB schema support:** Build synthetic data from a database **schema**
  without pulling real rows.

> Related work exists (e.g., general fake-data generators, schema
> tools). FakeDataR focuses on **privacy-preserving, schema-faithful**
> synthesis aimed at LLM workflows and reproducible sharing.

## Install

``` r
install.packages("FakeDataR")  # CRAN
#> Installing package into 'C:/Users/Zobaer Ahmed/AppData/Local/Temp/RtmpeemZPG/temp_libpath28645e88556b'
#> (as 'lib' is unspecified)
#> installing the source package 'FakeDataR'
# devtools::install_github("zobaer09/FakeDataR")  # development (optional)
```

## Quick start

``` r
library(FakeDataR)

# 1) Start from a built-in data frame for reproducibility
df <- head(mtcars, 50)

# 2) Generate a synthetic copy
fake <- generate_fake_data(
  data = df, n = nrow(df),
  category_mode = "preserve",
  numeric_mode  = "range",
  column_mode   = "keep",
  normalize     = TRUE
)

# 3) Handle sensitive fields (auto-detect + fake)
fake_priv <- generate_fake_with_privacy(
  data = df, n = nrow(df),
  level = "low", sensitive_detect = TRUE,
  sensitive_strategy = "fake"
)

# 4) Export an LLM bundle (fake data + schema + README prompt)
bundle <- llm_bundle(
  data = df, n = min(100L, nrow(df)),
  level = "medium",
  formats = c("csv","rds"),
  path = tempdir(), filename = "demo_bundle",
  seed = 42, write_prompt = TRUE, zip = TRUE
)

bundle$zip_path
#> [1] "C:\\Users\\ZOBAER~1\\AppData\\Local\\Temp\\Rtmp69b5bE/demo_bundle.zip"
```

## When to use / not use

**Use when:**

- You need an LLM or collaborator to reproduce your data shape but not
  see the real values.
- You want quick, documented synthetic data for demos, tutorials, bug
  reports, or tests.

**Avoid when:**

- You need formal differential privacy guarantees (FakeDataR focuses on
  pragmatic masking and synthesis).
- You need exact statistical fidelity for modeling benchmarks. It is not
  a drop-in replacement for originals.

## Documentation

- 📚 **Site:** <https://zobaer09.github.io/FakeDataR/>
- 🐞 **Issues:** <https://github.com/zobaer09/FakeDataR/issues>

## Citation

If you use FakeDataR in your work, please cite the CRAN page.
