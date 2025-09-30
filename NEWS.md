# FakeDataR 0.2.2

- CRAN resubmission: 
  - Replaced arXiv URL with arXiv DOI (`doi:10.48550/arXiv.2108.07258`).
  - Ensured software/API names are in single quotes (e.g., 'SQL', 'LLM').
  - Expanded acronym in Title (“LLM = Large Language Model”).
- Documentation tidy-ups:
  - Added `\value{}` to `generate_fake_from_schema()` and clarified return classes.
  - Minor wording/typo fixes across vignettes and Rd files.
- CI/docs:
  - Fixed pkgdown workflow and GitHub Pages settings.

# FakeDataR 0.2.1

- CRAN fixes: expanded acronym, quoted software names, added references.
- Replaced `\dontrun{}` with `\donttest{}` in examples where feasible.
- Added `\value{}` sections; RNG avoids `.GlobalEnv` writes (uses `withr::with_seed()`).

# FakeDataR 0.2.0

- Initial public submission:
  - `generate_fake_data()` with category/numeric/column modes.
  - `generate_fake_with_privacy()` (sensitive detection + strategies).
  - `export_fake()` (CSV/RDS/Parquet), `llm_bundle()` & `llm_bundle_from_db()`.
  - `prepare_input_data()` normalization helpers; POSIXct/date handling.
  - Vignettes: getting started, DB workflow, privacy & validation.
