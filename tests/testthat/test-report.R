skip_if_no_render <- function() {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available(), "Pandoc is not available.")
}

make_report_audit <- function() {
  pii_scan(data.frame(
    mobile = c("9876543210", "8123456789", "7012345678"),
    group = c("A", "A", "B")
  ))
}

test_that("report creates an HTML report in the requested directory", {
  skip_if_no_render()

  audit <- make_report_audit()
  out_dir <- tempfile("bharatpii-report-")
  dir.create(out_dir)
  path <- report(audit, output_dir = out_dir)

  expect_true(file.exists(path))
  expect_match(basename(path), "^bharatpii-audit-.*\\.html$")
  expect_match(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    "bharatpii",
    ignore.case = TRUE
  )

  unlink(out_dir, recursive = TRUE)
})

test_that("report rejects invalid audit objects", {
  expect_error(report(list()), "pii_audit")
})

test_that("report rejects output filenames containing paths", {
  audit <- make_report_audit()
  expect_error(report(audit, output_file = "../outside.html"), "filename")
})

test_that("PDF reporting explains missing LaTeX", {
  skip_if(nzchar(Sys.which("pdflatex")))
  audit <- make_report_audit()
  expect_error(report(audit, format = "pdf"), "pdflatex")
})

test_that("report validates the optional result objects", {
  audit <- make_report_audit()
  expect_error(report(audit, reid = "x"), "reid_risk")
  expect_error(report(audit, score = "x"), "dpdp_score")
  expect_error(report(audit, anonymised = "x"), "pii_anonymized")
})

test_that("report validates output_dir and output_file", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  audit <- make_report_audit()

  expect_error(report(audit, output_dir = NA_character_), "output_dir")
  expect_error(report(audit, output_dir = c("a", "b")), "output_dir")
  expect_error(report(audit, output_dir = ""), "output_dir")

  expect_error(report(audit, output_file = ""), "filename")
  expect_error(report(audit, output_file = NA_character_), "filename")
  expect_error(report(audit, output_file = 1), "filename")
  expect_error(report(audit, output_file = ".."), "filename")
  expect_error(report(audit, output_file = "a\\b.html"), "filename")
  expect_error(report(audit, output_file = "audit.pdf"), "extension")
})

test_that("report renders every optional section without raw values", {
  skip_if_no_render()
  skip_on_cran()

  set.seed(42)
  df <- synthetic_pii(30)
  audit <- pii_scan(df)
  quasi <- c("pincode", "dob", "gender")
  risk <- reid_risk(df, quasi)
  score <- dpdp_score(audit, risk)
  anon <- anonymize(
    df,
    c(aadhaar_no = "mask", pincode = "generalize"),
    quasi_vars = quasi,
    reference_date = as.Date("2024-06-01")
  )

  out_dir <- file.path(tempfile("bharatpii-nested-"), "reports")
  path <- report(audit, output_file = "full-audit", output_dir = out_dir,
                 reid = risk, score = score, anonymised = anon)

  expect_true(file.exists(path))
  expect_equal(basename(path), "full-audit.html")

  html <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(html, "Re-identification", ignore.case = TRUE)
  expect_match(html, "Anonymisation", ignore.case = TRUE)
  expect_false(grepl(df$aadhaar_no[1], html, fixed = TRUE))
  expect_false(grepl(df$pan_no[1], html, fixed = TRUE))
  expect_false(grepl(df$mobile[1], html, fixed = TRUE))

  unlink(dirname(out_dir), recursive = TRUE)
})

test_that("report handles anonymisation results without a risk comparison", {
  skip_if_no_render()
  skip_on_cran()

  df <- data.frame(pan_no = c("ABCPE1234F", "BCDFE5678G"))
  anon <- anonymize(df, c(pan_no = "mask"))
  path <- report(pii_scan(df), output_file = "audit.html", anonymised = anon)

  expect_equal(basename(path), "audit.html")
  unlink(path)
})
