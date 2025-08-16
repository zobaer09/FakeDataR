test_that("export_fake writes csv and rds", {
  x <- generate_fake_data(ToothGrowth, n = 5, seed = 7)
  p_csv <- tempfile(fileext = ".csv")
  p_rds <- tempfile(fileext = ".rds")
  export_fake(x, p_csv)
  export_fake(x, p_rds)
  expect_true(file.exists(p_csv))
  expect_true(file.exists(p_rds))
})
