test_that("two high-risk identifiers give a weak score", {
  audit <- pii_scan(data.frame(
    aadhaar_no = make_aadhaar("23456789012"),
    pan_no = "ABCPE1234F"
  ))
  res <- dpdp_score(audit)
  expect_s3_class(res, "dpdp_score")
  expect_equal(res$breakdown$points[1], 0)
  expect_true(is.na(res$breakdown$points[2]))
  expect_equal(res$breakdown$points[3], 10)
  expect_equal(res$breakdown$status,
               c("HIGH", "NOT ASSESSED", "MEDIUM"))
  expect_equal(res$score, 14)
  expect_equal(res$rating, "Weak")
  expect_equal(res$exposure_status, "HIGH")
  expect_equal(res$n_flagged, 2L)
  expect_false(res$reid_assessed)
})

test_that("data without identifiers scores 100", {
  res <- dpdp_score(pii_scan(data.frame(salary = c(1, 2))))
  expect_equal(res$score, 100)
  expect_equal(res$rating, "Strong")
  expect_equal(res$exposure_status, "NONE")
})

test_that("re-identification risk is included when supplied", {
  df <- data.frame(g = rep(c("a", "b"), each = 10), salary = 1:20)
  safe <- dpdp_score(pii_scan(df), reid_risk(df, "g"))
  expect_equal(safe$score, 100)
  expect_true(safe$reid_assessed)

  df2 <- data.frame(id = 1:10)
  risky <- dpdp_score(pii_scan(df2), reid_risk(df2, "id"))
  expect_equal(risky$breakdown$points, c(50, 0, 20))
  expect_equal(risky$score, 70)
})

test_that("medium and low tiers deduct fewer points", {
  audit <- pii_scan(data.frame(mobile = "9876543210", pincode = "560001"))
  res <- dpdp_score(audit)
  expect_equal(res$breakdown$points[1], 50 - 10 - 3)
  expect_equal(res$exposure_status, "MEDIUM")
})

test_that("invalid input gives informative errors", {
  expect_error(dpdp_score(data.frame()), "pii_audit")
  audit <- pii_scan(data.frame(a = 1))
  expect_error(dpdp_score(audit, reid = "x"), "reid_risk")
})

test_that("print method works", {
  audit <- pii_scan(data.frame(
    aadhaar_no = make_aadhaar("23456789012"),
    pan_no = "ABCPE1234F"
  ))
  res <- dpdp_score(audit)
  expect_output(print(res), "Overall DPDP readiness: 14 / 100")
  expect_output(print(res), "Section 2\\(t\\) exposure: HIGH")
  expect_output(print(res), "not assessed")
})
