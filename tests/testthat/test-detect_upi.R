test_that("VPAs with known handles are valid", {
  res <- detect_upi(c("asha.k@okaxis", "ASHA_K@YBL", " ravi-1@oksbi "))
  expect_true(all(res$valid))
  expect_equal(res$confidence, rep(0.95, 3))
})

test_that("ordinary email addresses are not reported as valid", {
  res <- detect_upi(c("john@gmail.com", "a@b.co"))
  expect_false(any(res$valid))
  expect_equal(res$confidence, c(0, 0))
})

test_that("VPA-shaped value with unknown handle has low confidence", {
  res <- detect_upi("asha.k@unknownbank")
  expect_false(res$valid)
  expect_equal(res$confidence, 0.2)
})

test_that("bad structure, NA and empty input are handled", {
  res <- detect_upi(c("nothing", "@okaxis", "x@okaxis", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_upi(character(0))), 0L)
  expect_error(detect_upi(list("x")), "atomic vector")
})
