test_that("mask keeps the last characters and separators", {
  expect_equal(mask_values("2345 6789 0124", 4), "XXXX XXXX 0124")
  expect_equal(mask_values("ABC", 4), "XXC")
  expect_equal(mask_values(c("ABCDEFGH", NA), 4), c("XXXXEFGH", NA))
  res <- anonymize(data.frame(a = c("ABCDEFGH", NA)), c(a = "mask"))
  expect_equal(res$report$n_changed, 1L)
  expect_equal(res$report$n_non_missing, 1L)
})

test_that("hash is salted, deterministic and NA-safe", {
  df <- data.frame(a = c("abc", NA, "abc"))
  r1 <- anonymize(df, c(a = "hash"), salt = "s")
  r2 <- anonymize(df, c(a = "hash"), salt = "t")
  expect_equal(r1$data$a[1],
               digest::digest("s|abc", algo = "sha256", serialize = FALSE))
  expect_equal(r1$data$a[1], r1$data$a[3])
  expect_true(is.na(r1$data$a[2]))
  expect_false(identical(r1$data$a[1], r2$data$a[1]))
  r3 <- anonymize(df, c(a = "hash"))
  expect_equal(nchar(r3$data$a[1]), 64L)
  expect_equal(r3$data$a[1], r3$data$a[3])
})

test_that("dates become age bands relative to the reference date", {
  ref <- as.Date("2024-06-01")
  dob <- c("1994-06-02", "1994-06-01", "10/05/1990", "10-05-1990")
  res <- anonymize(data.frame(dob = dob), c(dob = "generalize"),
                   reference_date = ref)
  expect_equal(res$data$dob, c("20-29", "30-39", "30-39", "30-39"))
  d <- anonymize(data.frame(dob = as.Date("1994-06-01")),
                 c(dob = "generalize"), reference_date = ref)
  expect_equal(d$data$dob, "30-39")
  mixed <- anonymize(
    data.frame(dob = c("1990-05-10", "garbage", "1991-01-01")),
    c(dob = "generalize"), reference_date = ref
  )
  expect_equal(mixed$data$dob, c("30-39", NA, "30-39"))
})

test_that("PIN codes and numbers are generalised", {
  pin <- anonymize(data.frame(pincode = c("560001", "110001")),
                   c(pincode = "generalise"))
  expect_equal(pin$data$pincode, c("560XXX", "110XXX"))
  numpin <- anonymize(data.frame(pincode = c(560001, 110001)),
                      c(pincode = "generalize"))
  expect_equal(numpin$data$pincode, c("560XXX", "110XXX"))
  age <- anonymize(data.frame(age = c(25, 37, 41, NA)), c(age = "generalize"))
  expect_equal(age$data$age, c("20-29", "30-39", "40-49", NA))
})

test_that("columns that cannot be generalised give an error", {
  expect_error(
    anonymize(data.frame(a = c("foo", "bar")), c(a = "generalize")),
    "generalised"
  )
})

test_that("automatic mode masks flagged columns only", {
  df <- data.frame(pan_no = c("ABCPE1234F", "BCDFE5678G"), salary = c(1, 2),
                   stringsAsFactors = FALSE)
  res <- anonymize(df, "mask")
  expect_equal(res$report$column, "pan_no")
  expect_equal(res$data$salary, c(1, 2))
  expect_match(res$data$pan_no, "^X{6}[0-9]{3}[A-Z]$")
  expect_true(all(is.na(pii_scan(as.data.frame(res))$columns$detected)))
  expect_error(anonymize(df, "generalize"), "Automatic")
  expect_message(none <- anonymize(data.frame(a = 1:2), "mask"), "No identifier")
  expect_equal(nrow(none$report), 0L)
})

test_that("risk before and after is reported", {
  df <- data.frame(
    pincode = paste0("56000", 1:6),
    dob = c("1990-01-05", "1991-02-06", "1992-03-07",
            "1993-04-08", "1994-05-09", "1993-06-10"),
    gender = "F"
  )
  res <- anonymize(df, c(pincode = "generalize", dob = "generalize"),
                   quasi_vars = c("pincode", "dob", "gender"),
                   reference_date = as.Date("2024-06-01"))
  expect_equal(res$risk_before$risk_level, "HIGH")
  expect_equal(res$risk_after$risk_level, "LOW")
  expect_equal(res$risk_after$min_k, 6L)
  expect_output(print(res), "HIGH -> LOW")
  expect_s3_class(as.data.frame(res), "data.frame")
})

test_that("invalid arguments give informative errors", {
  df <- data.frame(a = "x")
  expect_error(anonymize("x", c(a = "mask")), "data frame")
  expect_error(anonymize(df[0, , drop = FALSE], c(a = "mask")), "no rows")
  expect_error(anonymize(df, 1), "character vector")
  expect_error(anonymize(df, c(a = "shred")), "Unknown")
  expect_error(anonymize(df, c(b = "mask")), "not found")
  expect_error(anonymize(df, c("mask", "hash")), "named")
  expect_error(anonymize(df, c(a = "mask", a = "hash")), "only once")
  expect_error(anonymize(df, c(a = "mask"), keep = -1), "keep")
  expect_error(anonymize(df, c(a = "hash"), salt = ""), "salt")
  expect_error(anonymize(df, c(a = "mask"), band_width = 0), "band_width")
  expect_error(anonymize(df, c(a = "mask"), reference_date = "nope"),
               "reference_date")
  expect_error(anonymize(df, c(a = "mask"), quasi_vars = "zzz"), "not found")
  l <- data.frame(a = 1:2)
  l$l <- list(1, 2)
  expect_error(anonymize(l, c(l = "mask")), "atomic")
})
