test_that("llm_bundle creates files and a safe schema", {
  skip_if_not_installed("jsonlite")
  
  res <- llm_bundle(ToothGrowth, n = 12, level = "high", seed = 123,
                    formats = c("csv","rds"), path = tempdir(),
                    filename = "toothgrowth_fake_test")
  
  # files exist
  expect_true(file.exists(res$schema_path))
  expect_true(file.exists(res$data_paths$csv))
  expect_true(file.exists(res$data_paths$rds))
  
  # schema contents are JSON and safe
  sch <- jsonlite::fromJSON(res$schema_path, simplifyVector = FALSE)
  expect_equal(sch$package, "FakeDataR")
  expect_true(is.character(sch$version))
  expect_true(sch$privacy %in% c("low","medium","high"))
  expect_true(is.list(sch$columns))
  # no raw level names are leaked (we only store n_levels)
  has_levels <- vapply(sch$columns, function(x) !is.null(x$levels), logical(1))
  if (any(has_levels)) {
    lvl <- sch$columns[[which(has_levels)[1]]]$levels
    expect_true(all(names(lvl) %in% c("n_levels")))
  }
})

