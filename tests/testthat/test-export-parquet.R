testthat::skip_if_not_installed("arrow")

test_that("export parquet works when arrow is installed", {
  skip_if_not_installed("arrow")
  x <- generate_fake_data(ToothGrowth, n = 5, seed = 42)
  fp <- tempfile(fileext = ".parquet")
  export_fake(x, fp)
  expect_true(file.exists(fp))
})
