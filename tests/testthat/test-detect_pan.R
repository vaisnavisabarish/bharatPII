test_that("valid PAN structures are accepted in any case", {
  res <- detect_pan(c("ABCPE1234F", "abcpe1234f", " ABCPE-1234F "))
  expect_true(all(res$valid))
  expect_equal(res$confidence, rep(0.95, 3))
})

test_that("each valid holder class is accepted", {
  for (cl in strsplit("PCHFATBLJG", "")[[1]]) {
    expect_true(detect_pan(paste0("ABC", cl, "E1234F"))$valid)
  }
})

test_that("invalid holder class has right shape but low confidence", {
  res <- detect_pan("ABCXE1234F")
  expect_false(res$valid)
  expect_equal(res$confidence, 0.1)
})

test_that("wrong shapes, NA and empty input are rejected", {
  res <- detect_pan(c("12345ABCDE", "ABCPE12345", "ABCPE1234", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_pan(character(0))), 0L)
})

test_that("non-vector input errors", {
  expect_error(detect_pan(list("ABCPE1234F")), "atomic vector")
})
