test_that("known-bank IFSC codes are valid in any case", {
  res <- detect_ifsc(c("SBIN0001234", "sbin0001234", "HDFC0000123"))
  expect_true(all(res$valid))
  expect_equal(res$confidence, rep(0.95, 3))
})

test_that("unknown bank code has right shape but low confidence", {
  res <- detect_ifsc("ABCD0123456")
  expect_false(res$valid)
  expect_equal(res$confidence, 0.3)
})

test_that("fifth character must be zero", {
  expect_false(detect_ifsc("SBIN1001234")$valid)
  expect_equal(detect_ifsc("SBIN1001234")$confidence, 0)
})

test_that("bad length, NA and empty input are handled", {
  res <- detect_ifsc(c("SBIN000123", "SBIN00012345", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_ifsc(character(0))), 0L)
  expect_error(detect_ifsc(list("x")), "atomic vector")
})
