test_that("k is computed per row", {
  res <- reid_risk(data.frame(g = c("a", "a", "a", "b")), "g")
  expect_s3_class(res, "reid_risk")
  expect_equal(res$k, c(3L, 3L, 3L, 1L))
  expect_equal(res$n_classes, 2L)
  expect_equal(res$min_k, 1L)
  expect_equal(res$prop_k1, 0.25)
  expect_equal(res$prop_k_lt5, 1)
  expect_equal(res$risk_level, "HIGH")
})

test_that("risk levels follow the thresholds", {
  expect_equal(reid_risk(data.frame(g = rep("a", 10)), "g")$risk_level, "LOW")
  expect_equal(reid_risk(data.frame(g = rep("a", 10)), "g")$min_k, 10L)
  med <- reid_risk(data.frame(g = rep(letters[1:5], each = 4)), "g")
  expect_equal(med$risk_level, "MEDIUM")
  expect_equal(med$prop_k1, 0)
})

test_that("several variables and missing values are handled", {
  df <- data.frame(a = c(1, 1, 2, 2), b = c("x", "y", "x", "x"))
  expect_equal(reid_risk(df, c("a", "b"))$k, c(1L, 1L, 2L, 2L))
  na <- reid_risk(data.frame(a = c("a", NA, NA, "a")), "a")
  expect_equal(na$k, rep(2L, 4))
  expect_equal(na$n_classes, 2L)
})

test_that("date quasi-identifiers work", {
  df <- data.frame(d = as.Date(c("2000-01-01", "2000-01-01", "2001-05-05")))
  expect_equal(reid_risk(df, "d")$k, c(2L, 2L, 1L))
})

test_that("invalid input gives informative errors", {
  df <- data.frame(a = 1:3)
  expect_error(reid_risk("x", "a"), "data frame")
  expect_error(reid_risk(df[0, , drop = FALSE], "a"), "no rows")
  expect_error(reid_risk(df, 1), "character vector")
  expect_error(reid_risk(df, character(0)), "character vector")
  expect_error(reid_risk(df, "nope"), "not found")
})

test_that("print method works", {
  res <- reid_risk(data.frame(g = c("a", "a", "a", "b")), "g")
  expect_output(print(res), "reid_risk")
  expect_output(print(res), "Re-identification risk: HIGH")
  expect_output(print(res), "k = 1 for 25%")
})
