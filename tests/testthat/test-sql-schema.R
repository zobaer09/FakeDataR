testthat::skip_if_not_installed("DBI")
testthat::skip_if_not_installed("RSQLite")

test_that("schema_from_db + generate_fake_from_schema work without reading rows", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(con, "
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
  
  sch <- schema_from_db(con, "employees")
  expect_s3_class(sch, "data.frame")
  expect_true(all(c("name","type","nullable","sensitive") %in% names(sch)))
  
  set.seed(123)
  fake <- generate_fake_from_schema(sch, n = 15, seed = 123)
  expect_equal(nrow(fake), 15L)
  expect_true(all(c("id","email","phone","is_active","hired_at","salary","dept") %in% names(fake)))
  expect_type(fake$id, "integer")
  expect_type(fake$email, "character")
  expect_type(fake$phone, "character")
  expect_type(fake$is_active, "logical")
  expect_s3_class(fake$hired_at, "POSIXct")
  expect_type(fake$salary, "double")
  expect_type(fake$dept, "character")
})

test_that("llm_bundle_from_db writes files and schema with sensitive flags", {
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("RSQLite")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbExecute(con, "
    CREATE TABLE customers (
      id INTEGER, email TEXT, phone TEXT, joined DATE, amount NUMERIC
    )
  ")
  
  b <- llm_bundle_from_db(con, "customers",
                          n = 10, level = "high", seed = 1,
                          formats = "rds", path = tempdir(),
                          filename = "cust", write_prompt = TRUE,
                          sensitive_strategy = "fake")
  
  expect_true(file.exists(b$schema_path))
  expect_true(file.exists(b$data_paths$rds))
  expect_true(file.exists(b$readme_path))
  
  sch <- jsonlite::fromJSON(b$schema_path)
  
  expect_equal(sch$source, "db_schema_only")
  expect_equal(sch$sensitive_strategy, "fake")
  
  # At least one column marked sensitive; columns may deserialize as df or list
  has_sensitive <- if (is.data.frame(sch$columns)) {
    any(sch$columns$sensitive)
  } else {
    any(vapply(sch$columns, function(c) isTRUE(c$sensitive), logical(1)))
  }
  expect_true(has_sensitive)
})






