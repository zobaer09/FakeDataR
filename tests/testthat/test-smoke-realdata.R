## 2b) As a quick automated test (`tests/testthat/test-smoke-realdata.R`)
## (keeps CI fast and won’t run on CRAN if you add CRAN later)

test_that("real-data smoke: gapminder", {
  skip_if_not_installed("gapminder")
  set.seed(21)
  gm <- gapminder::gapminder[sample.int(nrow(gapminder::gapminder), 1000), ]
  fake_gm <- generate_fake_data(
    gm, n = 500, seed = 21,
    numeric_mode = "distribution", category_mode = "preserve"
  )
  v <- validate_fake(gm, fake_gm)
  # Structure checks that shouldn’t be brittle:
  expect_true(all(v$class_match))
})
