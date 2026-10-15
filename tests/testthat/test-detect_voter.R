test_that("valid EPIC numbers are accepted in any case", {
  res <- detect_voter(c("ABC1234567", "abc1234567", " ABC-1234567 "))
  expect_true(all(res$valid))
  expect_equal(res$confidence, rep(0.85, 3))
})

test_that("invalid shapes, NA and empty input are rejected", {
  res <- detect_voter(c("AB12345678", "ABC123456", "1234567ABC", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_voter(character(0))), 0L)
  expect_error(detect_voter(list("x")), "atomic vector")
})
