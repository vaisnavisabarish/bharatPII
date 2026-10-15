test_that("valid PIN codes are accepted", {
  res <- detect_pin(c("560001", "110001", "560 001", "400001"))
  expect_true(all(res$valid))
  expect_equal(res$confidence, rep(0.6, 4))
})

test_that("numeric input works", {
  expect_true(detect_pin(560001)$valid)
})

test_that("unknown postal-circle prefix has low confidence", {
  res <- detect_pin(c("100000", "299999"))
  expect_false(any(res$valid))
  expect_equal(res$confidence, c(0.1, 0.1))
})

test_that("leading zero, wrong length, NA and empty are rejected", {
  res <- detect_pin(c("000001", "5600", "5600011", NA, ""))
  expect_false(any(res$valid))
  expect_equal(res$confidence, rep(0, 5))
  expect_equal(nrow(detect_pin(character(0))), 0L)
  expect_error(detect_pin(list(1)), "atomic vector")
})
