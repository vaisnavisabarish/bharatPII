test_that("synthetic data has the expected shape", {
  set.seed(1)
  df <- synthetic_pii(20)
  expect_equal(nrow(df), 20L)
  expect_named(df, c("name", "aadhaar_no", "pan_no", "mobile", "upi_id",
                     "pincode", "dob", "gender", "salary"))
  expect_s3_class(df$dob, "Date")
  expect_true(all(df$dob >= as.Date("1960-01-01") &
                    df$dob <= as.Date("2004-12-31")))
})

test_that("every synthetic identifier passes its detector", {
  set.seed(2)
  df <- synthetic_pii(50)
  expect_true(all(detect_aadhaar(df$aadhaar_no)$valid))
  expect_true(all(detect_pan(df$pan_no)$valid))
  expect_true(all(detect_mobile(df$mobile)$valid))
  expect_true(all(detect_upi(df$upi_id)$valid))
  expect_true(all(detect_pin(df$pincode)$valid))
})

test_that("pii_scan finds the identifier columns", {
  set.seed(3)
  res <- pii_scan(synthetic_pii(30))
  expect_equal(
    res$columns$detected,
    c(NA, "aadhaar", "pan", "mobile", "upi", "pin", NA, NA, NA)
  )
})

test_that("invalid n is rejected", {
  expect_error(synthetic_pii(0), "whole number")
  expect_error(synthetic_pii(1.5), "whole number")
  expect_error(synthetic_pii("a"), "whole number")
  expect_error(synthetic_pii(c(1, 2)), "whole number")
  expect_equal(nrow(synthetic_pii(1)), 1L)
})
