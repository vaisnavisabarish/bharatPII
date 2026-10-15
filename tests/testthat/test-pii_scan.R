make_test_df <- function() {
  data.frame(
    aadhaar_no = c(make_aadhaar("23456789012"), make_aadhaar("34567890123"),
                   make_aadhaar("45678901234")),
    pan_no = c("ABCPE1234F", "BCDFE5678G", "XYZHA4321K"),
    mobile = c("9876543210", "8123456789", "7012345678"),
    gstin = c(make_gstin("27AAPFU0939F1Z"), make_gstin("29AABCT1332L1Z"),
              make_gstin("07AAPFU0939F1Z")),
    salary = c(50000, 62000, 58000),
    stringsAsFactors = FALSE
  )
}

test_that("pii_scan returns a pii_audit with one row per column", {
  res <- pii_scan(make_test_df())
  expect_s3_class(res, "pii_audit")
  expect_equal(nrow(res$columns), 5L)
  expect_equal(res$n_rows, 3L)
  expect_true(all(c("column", "detected", "label", "n_checked", "n_valid",
                    "hit_rate", "confidence", "name_match", "dpdp_category",
                    "risk_tier", "dpdp_basis") %in% names(res$columns)))
})

test_that("identifier columns are detected and salary is not", {
  res <- pii_scan(make_test_df())
  expect_equal(res$columns$detected,
               c("aadhaar", "pan", "mobile", "gstin", NA_character_))
  expect_equal(res$columns$hit_rate[1:4], rep(1, 4))
  expect_true(is.na(res$columns$confidence[5]))
})

test_that("threshold controls flagging", {
  df <- data.frame(pan_no = c("ABCPE1234F", "not a pan"))
  expect_equal(pii_scan(df)$columns$detected, "pan")
  expect_true(is.na(pii_scan(df, threshold = 0.6)$columns$detected))
})

test_that("PIN codes need a postal-looking column name", {
  df <- data.frame(pincode = c("560001", "110001"),
                   amount = c(560001, 110001))
  res <- pii_scan(df)
  expect_equal(res$columns$detected, c("pin", NA_character_))
})

test_that("missing, empty and list columns are handled", {
  df <- data.frame(x = c(NA_character_, NA_character_),
                   y = c("", "  "),
                   z = c("ABCPE1234F", "BCDFE5678G"))
  df$l <- list(1, 2)
  res <- pii_scan(df)
  expect_equal(res$columns$n_checked, c(0L, 0L, 2L, 0L))
  expect_equal(res$columns$detected,
               c(NA_character_, NA_character_, "pan", NA_character_))
})

test_that("masked values are no longer flagged", {
  df <- data.frame(aadhaar_no = c("XXXX-XXXX-9012", "XXXX-XXXX-0123"))
  res <- pii_scan(df)
  expect_true(is.na(res$columns$detected))
  expect_true(res$columns$name_match)
})

test_that("invalid arguments give informative errors", {
  expect_error(pii_scan("x"), "data frame")
  expect_error(pii_scan(data.frame()), "no columns")
  expect_error(pii_scan(data.frame(a = 1), threshold = 2), "threshold")
  expect_error(pii_scan(data.frame(a = 1), threshold = 0), "threshold")
  expect_error(pii_scan(data.frame(a = 1), threshold = "a"), "threshold")
})

test_that("print, summary and as.data.frame methods work", {
  res <- pii_scan(make_test_df())
  expect_output(print(res), "pii_audit")
  expect_output(print(res), "Aadhaar")
  expect_output(print(res), "RiskTier")
  s <- summary(res)
  expect_equal(s$risk_tier, c("High", "Medium", "Low", "None"))
  expect_equal(s$n_columns, c(2L, 2L, 0L, 1L))
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_equal(nrow(as.data.frame(res)), 5L)
})
