test_that("valid synthetic Aadhaar numbers are detected", {
  a <- make_aadhaar("23456789012")
  res <- detect_aadhaar(a)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("value", "valid", "confidence"))
  expect_true(res$valid)
  expect_equal(res$confidence, 0.98)
})

test_that("spaces and hyphens are tolerated", {
  a <- make_aadhaar("23456789012")
  spaced <- paste(substr(a, 1, 4), substr(a, 5, 8), substr(a, 9, 12))
  dashed <- paste(substr(a, 1, 4), substr(a, 5, 8), substr(a, 9, 12), sep = "-")
  expect_true(detect_aadhaar(spaced)$valid)
  expect_true(detect_aadhaar(dashed)$valid)
})

test_that("bad checksum has right format but low confidence", {
  a <- make_aadhaar("23456789012")
  last <- as.integer(substr(a, 12, 12))
  bad <- paste0(substr(a, 1, 11), (last + 1L) %% 10L)
  res <- detect_aadhaar(bad)
  expect_false(res$valid)
  expect_equal(res$confidence, 0.2)
})

test_that("first digit 0 or 1 is rejected even with a valid checksum", {
  res <- detect_aadhaar(c(make_aadhaar("12345678901"),
                          make_aadhaar("02345678901")))
  expect_false(any(res$valid))
  expect_equal(res$confidence, c(0, 0))
})

test_that("wrong length, text, NA and empty input are handled", {
  res <- detect_aadhaar(c("12345", "abcdefghijkl", NA, ""))
  expect_false(any(res$valid))
  expect_equal(nrow(detect_aadhaar(character(0))), 0L)
})

test_that("numeric and factor input work; lists are rejected", {
  expect_false(detect_aadhaar(123456789012)$valid)
  expect_s3_class(detect_aadhaar(factor("x")), "data.frame")
  expect_error(detect_aadhaar(list(1)), "atomic vector")
  expect_error(detect_aadhaar(data.frame(a = 1)), "atomic vector")
})
