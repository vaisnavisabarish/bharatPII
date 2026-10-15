test_that("Verhoeff matches the published worked example", {
  expect_true(verhoeff_valid_one("2363"))
  expect_equal(verhoeff_check_digit("236"), 3L)
  expect_false(verhoeff_valid_one("2364"))
})

test_that("Verhoeff round trip works for generated numbers", {
  for (b in c("23456789012", "98765432109", "50505050505")) {
    expect_true(verhoeff_valid_one(make_aadhaar(b)))
  }
})

test_that("Verhoeff rejects non-digit and NA input", {
  expect_false(verhoeff_valid_one("23A4"))
  expect_false(verhoeff_valid_one(NA_character_))
})

test_that("GSTIN check character round trip is stable", {
  g <- make_gstin("27AAPFU0939F1Z")
  expect_equal(nchar(g), 15L)
  expect_equal(gstin_check_char(substr(g, 1, 14)), substr(g, 15, 15))
})
