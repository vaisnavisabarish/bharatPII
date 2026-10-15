test_that("a valid synthetic GSTIN is accepted", {
  g <- make_gstin("27AAPFU0939F1Z")
  res <- detect_gstin(g)
  expect_true(res$valid)
  expect_equal(res$confidence, 0.98)
})

test_that("case, spaces and hyphens are tolerated", {
  g <- make_gstin("27AAPFU0939F1Z")
  expect_true(detect_gstin(tolower(g))$valid)
  expect_true(detect_gstin(paste0(" ", substr(g, 1, 2), "-", substr(g, 3, 15)))$valid)
})

test_that("bad checksum is rejected with low confidence", {
  g <- make_gstin("27AAPFU0939F1Z")
  bad <- paste0(substr(g, 1, 14), if (substr(g, 15, 15) == "A") "B" else "A")
  res <- detect_gstin(bad)
  expect_false(res$valid)
  expect_equal(res$confidence, 0.2)
})

test_that("invalid state code is rejected even with a valid checksum", {
  expect_false(detect_gstin(make_gstin("50AAPFU0939F1Z"))$valid)
  expect_false(detect_gstin(make_gstin("00AAPFU0939F1Z"))$valid)
})

test_that("invalid embedded PAN class is rejected", {
  # Position 6 is the 4th character of the embedded PAN (the holder class).
  expect_false(detect_gstin(make_gstin("27AAPXU0939F1Z"))$valid)
  expect_true(detect_gstin(make_gstin("27AAPFU0939F1Z"))$valid)
})

test_that("missing Z in position 14 is rejected", {
  expect_false(detect_gstin(make_gstin("27AAPFU0939F1A"))$valid)
})

test_that("wrong length, NA and empty input are handled", {
  res <- detect_gstin(c("27AAPFU0939F1", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_gstin(character(0))), 0L)
  expect_error(detect_gstin(list("x")), "atomic vector")
})
