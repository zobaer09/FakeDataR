x1 <- generate_fake_data(ToothGrowth, n = 6, column_mode = "generic", seed = 123)
x2 <- generate_fake_data(ToothGrowth, n = 6, column_mode = "custom",
                         custom_names = c("Length","Supplement","Dose"),
                         seed = 123)
x3 <- generate_fake_data(ToothGrowth, n = 8, category_mode = "custom",
                         custom_levels = list(supp = c("Dog","Cat")),
                         seed = 123)
x4 <- generate_fake_data(ToothGrowth, n = 8, column_mode = "custom",
                         custom_names = c("Length","Supplement","Dose"),
                         category_mode = "custom",
                         custom_levels = list(Supplement = c("Dog","Cat")),
                         seed = 123)

test_that("integer columns stay integer", {
  df <- data.frame(a = c(1L, 2L, NA, 4L))
  out <- generate_fake_data(df, n = 7, numeric_mode = "range", seed = 42)
  expect_true(is.integer(out$a))
})

test_that("Date range is preserved", {
  d <- seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = "day")
  df <- data.frame(dt = d)
  out <- generate_fake_data(df, n = 20, seed = 7)
  expect_gte(min(out$dt, na.rm = TRUE), min(d))
  expect_lte(max(out$dt, na.rm = TRUE), max(d))
  expect_s3_class(out$dt, "Date")
})

test_that("POSIXct range is preserved", {
  dt <- seq.POSIXt(as.POSIXct("2023-01-01 00:00:00", tz="UTC"), by="hour", length.out=24)
  df <- data.frame(ts = dt)
  out <- generate_fake_data(df, n = 10, seed = 99)
  expect_s3_class(out$ts, "POSIXct")
  expect_gte(min(out$ts, na.rm = TRUE), min(dt))
  expect_lte(max(out$ts, na.rm = TRUE), max(dt))
})

test_that("logical column remains logical", {
  df <- data.frame(flag = c(TRUE, FALSE, NA))
  out <- generate_fake_data(df, n = 9, seed = 1)
  expect_type(out$flag, "logical")
  expect_true(all(out$flag %in% c(TRUE, FALSE, NA)))
})
