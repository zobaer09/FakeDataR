test_that("detect_sensitive_columns: core matches", {
  nm <- c("id","email","home_phone","ZIP","address_line_1","DOB",
          "iban","api_key","device_id","lat","lon","notes")
  got <- FakeDataR::detect_sensitive_columns(nm)
  expect_true(all(c("id","email","home_phone","ZIP","address_line_1","DOB",
                    "iban","api_key","device_id","lat","lon") %in% got))
  expect_false("notes" %in% got)
})

test_that("detect_sensitive_columns: extra_patterns and override_regex", {
  nm <- c("mrn","passport_no","free_text")
  got_extra <- FakeDataR::detect_sensitive_columns(nm, extra_patterns = c("^mrn$", "passport"))
  expect_equal(sort(got_extra), sort(c("mrn","passport_no")))
  
  got_override <- FakeDataR::detect_sensitive_columns(nm, override_regex = "(?i)^free_text$")
  expect_equal(got_override, "free_text")
})
