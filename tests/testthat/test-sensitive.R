test_that("sensitive columns are faked and do not leak", {
  set.seed(42)
  df <- data.frame(
    id     = 1001:1020,
    email  = c("alice@test.com","bob@test.com", NA, rep("x@y.com", 17)),
    phone  = c("(415) 555-0101","+1-202-555-0123", NA, rep("212-555-9999", 17)),
    name   = c("Ann","Bob","Cara",rep("Don",17)),
    amount = c(10.5, 20.1, 15, rep(30,17))
  )
  
  fake <- generate_fake_data(df, n = 12, seed = 1)
  
  # No original sensitive values appear (ignore NAs)
  expect_length(intersect(na.omit(df$email), na.omit(fake$email)), 0)
  expect_length(intersect(na.omit(df$phone), na.omit(fake$phone)), 0)
  expect_length(intersect(df$id, fake$id), 0)
  
  # Types preserved for non-sensitive columns
  expect_type(fake$amount, "double")
  expect_type(fake$name, "character")
})

test_that("sensitive_strategy='drop' removes those columns", {
  df <- data.frame(
    id=1:5, email=sprintf("x%02d@test.com",1:5),
    phone=sprintf("555-%04d", 1:5), x=1:5
  )
  fake <- generate_fake_data(df, n = 10, sensitive_strategy = "drop", seed = 2)
  expect_false(any(c("id","email","phone") %in% names(fake)))
  expect_true("x" %in% names(fake))
})

test_that("NA rate roughly preserved for sensitive columns when faked", {
  set.seed(123)
  df <- data.frame(
    email = c("a@test.com", NA, "b@test.com", rep(NA, 7))
  )
  fake <- generate_fake_data(df, n = 200, seed = 123)
  # Original NA rate is 0.8
  expect_gt(mean(is.na(fake$email)), 0.7)
  expect_lt(mean(is.na(fake$email)), 0.9)
})
