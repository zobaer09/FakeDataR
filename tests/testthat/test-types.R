test_that("numeric mode range keeps types and shapes", {
  out <- generate_fake_data(mtcars, n = 7, numeric_mode = "range", seed = 1)
  expect_equal(nrow(out), 7)
  expect_equal(ncol(out), ncol(mtcars))
  expect_true(is.numeric(out$mpg))
})

test_that("integer stays integer", {
  df  <- data.frame(a = c(1L, 2L, NA, 4L))
  out <- generate_fake_data(df, n = 9, numeric_mode = "range", seed = 2)
  expect_true(is.integer(out$a))
})

test_that("factor/character work with modes", {
  out_p <- generate_fake_data(ToothGrowth, n = 6, category_mode = "preserve", seed = 3)
  expect_true(is.factor(out_p$supp))
  
  out_g <- generate_fake_data(ToothGrowth, n = 6, category_mode = "generic", seed = 3)
  expect_true(is.character(out_g$supp) || is.factor(out_g$supp))
})

test_that("Date range and class preserved", {
  d  <- seq(as.Date("2020-01-01"), by = "day", length.out = 10)
  df <- data.frame(dt = d)
  out <- generate_fake_data(df, n = 25, seed = 4)
  expect_s3_class(out$dt, "Date")
  expect_gte(min(out$dt, na.rm = TRUE), min(d))
  expect_lte(max(out$dt, na.rm = TRUE), max(d))
})

test_that("POSIXct range and class preserved", {
  dt <- seq.POSIXt(as.POSIXct("2023-01-01 00:00:00", tz = "UTC"), by = "hour", length.out = 24)
  df <- data.frame(ts = dt)
  out <- generate_fake_data(df, n = 12, seed = 5)
  expect_s3_class(out$ts, "POSIXct")
  expect_gte(min(out$ts, na.rm = TRUE), min(dt))
  expect_lte(max(out$ts, na.rm = TRUE), max(dt))
})

test_that("logical remains logical and only TRUE/FALSE/NA", {
  df  <- data.frame(flag = c(TRUE, FALSE, NA))
  out <- generate_fake_data(df, n = 9, seed = 6)
  expect_type(out$flag, "logical")
  expect_true(all(out$flag %in% c(TRUE, FALSE, NA)))
})
