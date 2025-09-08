# tests/testthat/test-smoke-builtins.R
test_that("generate_fake_data works on small built-ins", {
  skip_on_cran()
  skip_if_not_installed("FakeDataR")
  
  smalls <- list(
    iris  = head(iris, 50),
    mtcars= head(mtcars, 50)
  )
  
  for (nm in names(smalls)) {
    orig <- FakeDataR::prepare_input_data(smalls[[nm]])
    f <- FakeDataR::generate_fake_data(
      orig, n = nrow(orig),
      category_mode = "preserve", numeric_mode = "range", column_mode = "keep",
      normalize = FALSE
    )
    expect_s3_class(f, "data.frame")
    expect_true(nrow(f) == nrow(orig))
    # factors should stay factors under preserve/keep when orig had factors
    common <- intersect(names(orig), names(f))
    for (cn in common) {
      if (is.factor(orig[[cn]])) expect_true(is.factor(f[[cn]]), info = paste(nm, cn))
    }
  }
})

test_that("privacy wrapper runs in simple cases", {
  skip_on_cran()
  df <- data.frame(
    id = sprintf("id%03d", 1:30),
    Progress = paste0(sample(80:100, 30, TRUE), "%"),
    email = paste0("a", 1:30, "@x.com"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  orig <- FakeDataR::prepare_input_data(df)
  # soft percent normalize so Progress becomes numeric
  if (requireNamespace("readr", quietly = TRUE)) {
    orig$Progress <- readr::parse_number(as.character(orig$Progress))
  }
  f <- FakeDataR::generate_fake_with_privacy(
    data = orig, n = 30, level = "low", seed = 1,
    sensitive = c("id","email"),
    sensitive_detect = TRUE, sensitive_strategy = "fake",
    normalize = FALSE
  )
  expect_s3_class(f, "data.frame")
  expect_true(nrow(f) == 30)
})


