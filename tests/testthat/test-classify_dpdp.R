test_that("categories and risk tiers follow the rubric", {
  df <- data.frame(
    aadhaar_no = make_aadhaar("23456789012"),
    mobile = "9876543210",
    gstin = make_gstin("27AAPFU0939F1Z"),
    salary = 50000
  )
  cols <- pii_scan(df)$columns
  expect_equal(cols$risk_tier, c("High", "Medium", "Medium", "None"))
  expect_equal(cols$dpdp_category,
               c("Personal data", "Personal data",
                 "Personal data (context-dependent)",
                 "No identifier detected"))
  expect_match(cols$dpdp_basis[1], "Section 2\\(t\\)")
  expect_true(is.na(cols$dpdp_basis[4]))
})

test_that("classify_dpdp is idempotent", {
  res <- pii_scan(data.frame(pan_no = c("ABCPE1234F", "BCDFE5678G")))
  expect_identical(classify_dpdp(res), res)
})

test_that("classify_dpdp rejects other objects", {
  expect_error(classify_dpdp(data.frame()), "pii_audit")
  expect_error(classify_dpdp(NULL), "pii_audit")
})
