test_that("plain 10-digit mobile numbers are valid", {
  res <- detect_mobile(c("9876543210", "6123456789"))
  expect_true(all(res$valid))
  expect_equal(res$confidence, c(0.9, 0.9))
})

test_that("country-code and trunk prefixes are stripped", {
  res <- detect_mobile(c("+91 98765 43210", "+91-98765-43210",
                         "919876543210", "09876543210"))
  expect_true(all(res$valid))
})

test_that("numeric input works", {
  expect_true(detect_mobile(9876543210)$valid)
})

test_that("invalid numbers are rejected", {
  res <- detect_mobile(c("5876543210", "98765", "98765432101", "abcdefghij",
                         NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_mobile(character(0))), 0L)
  expect_error(detect_mobile(list(1)), "atomic vector")
})
