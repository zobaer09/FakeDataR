testthat::skip_if_not_installed("tibble")

test_that("prepare_input_data coerces common objects", {
  # tibble
  tb <- tibble::as_tibble(mtcars)
  out <- prepare_input_data(tb)
  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), ncol(mtcars))
  
  # matrix
  m <- as.matrix(iris[, 1:3])
  out <- prepare_input_data(m)
  expect_s3_class(out, "data.frame")
  expect_equal(ncol(out), 3)
  
  # base table (Titanic)
  out <- prepare_input_data(as.data.frame(Titanic))
  expect_s3_class(out, "data.frame")
  
  # ts
  out <- prepare_input_data(as.ts(AirPassengers))
  expect_s3_class(out, "data.frame")
  
  # grouped_df
  gf <- dplyr::group_by(mtcars, cyl)
  out <- prepare_input_data(gf)
  expect_s3_class(out, "data.frame")
  
  # models -> model.frame
  fit <- lm(mpg ~ wt + hp, data = mtcars)
  out <- prepare_input_data(fit)
  expect_true(all(c("mpg", "wt", "hp") %in% names(out)))
})


