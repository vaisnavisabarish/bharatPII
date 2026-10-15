#' Generate a privacy audit report
#'
#' @description
#' Renders a privacy audit as HTML or PDF. The report contains column-level
#' detection results and optional re-identification, scoring and
#' anonymisation summaries. It does not include the original data values.
#'
#' @param audit A `pii_audit` object returned by [pii_scan()].
#' @param format Output format: `"html"` or `"pdf"`.
#' @param output_file Optional output filename. It must be a filename without
#'   directory components. The appropriate extension is added if omitted.
#' @param output_dir Directory for the generated report. Defaults to
#'   [tempdir()].
#' @param reid Optional `reid_risk` object returned by [reid_risk()].
#' @param score Optional `dpdp_score` object returned by [dpdp_score()].
#' @param anonymised Optional `pii_anonymized` object returned by
#'   [anonymize()].
#'
#' @return Invisibly, the normalised path to the generated report.
#'
#' @details
#' Requires the optional packages `rmarkdown` and `knitr`. PDF output also
#' requires a working `pdflatex` executable. Risk tiers and scores are
#' package-defined technical assessments, not legal advice or certification
#' of compliance with the Digital Personal Data Protection Act, 2023.
#'
#' @seealso [pii_scan()], [reid_risk()], [dpdp_score()], [anonymize()]
#' @export
#'
#' @examples
#' if (requireNamespace("rmarkdown", quietly = TRUE) &&
#'     requireNamespace("knitr", quietly = TRUE) &&
#'     rmarkdown::pandoc_available()) {
#'   audit <- pii_scan(data.frame(mobile = c("9876543210", "8123456789")))
#'   path <- report(audit)
#'   file.exists(path)
#' }
report <- function(audit, format = c("html", "pdf"),
                   output_file = NULL, output_dir = tempdir(),
                   reid = NULL, score = NULL, anonymised = NULL) {

  if (!inherits(audit, "pii_audit")) {
    cli::cli_abort("{.arg audit} must be a {.cls pii_audit} object.")
  }

  if (!is.null(reid) && !inherits(reid, "reid_risk")) {
    cli::cli_abort("{.arg reid} must be NULL or a {.cls reid_risk} object.")
  }
  if (!is.null(score) && !inherits(score, "dpdp_score")) {
    cli::cli_abort("{.arg score} must be NULL or a {.cls dpdp_score} object.")
  }
  if (!is.null(anonymised) && !inherits(anonymised, "pii_anonymized")) {
    cli::cli_abort(
      "{.arg anonymised} must be NULL or a {.cls pii_anonymized} object."
    )
  }

  format <- match.arg(format)

  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg rmarkdown} is required to generate reports.",
      "i" = "Install it with {.code install.packages(\"rmarkdown\")}."
    ))
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg knitr} is required to generate reports.",
      "i" = "Install it with {.code install.packages(\"knitr\")}."
    ))
  }

  if (format == "pdf" && !nzchar(Sys.which("pdflatex"))) {
    cli::cli_abort(c(
      "PDF output requires a working {.code pdflatex} executable.",
      "i" = "Install a LaTeX distribution or use {.code format = \"html\"}."
    ))
  }

  if (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir)) {
    cli::cli_abort("{.arg output_dir} must be a single non-empty path.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    cli::cli_abort("Cannot create output directory {.path {output_dir}}.")
  }
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)

  extension <- paste0(".", format)

  if (is.null(output_file)) {
    output_file <- basename(tempfile(
      pattern = "bharatpii-audit-",
      tmpdir = output_dir,
      fileext = extension
    ))
  } else {
    if (!is.character(output_file) || length(output_file) != 1L ||
        is.na(output_file) || !nzchar(output_file) ||
        output_file %in% c(".", "..") ||
        grepl("[/\\\\]", output_file)) {
      cli::cli_abort(
        "{.arg output_file} must be a filename without directory components."
      )
    }

    if (!grepl("\\.[A-Za-z0-9]+$", output_file)) {
      output_file <- paste0(output_file, extension)
    } else if (!endsWith(tolower(output_file), extension)) {
      cli::cli_abort(
        "{.arg output_file} must have the {.val {extension}} extension."
      )
    }
  }

  template <- system.file(
    "rmarkdown", "report-template.Rmd", package = "bharatpii"
  )
  if (!nzchar(template) || !file.exists(template)) {
    local_template <- file.path("inst", "rmarkdown", "report-template.Rmd")
    if (file.exists(local_template)) {
      template <- normalizePath(local_template, winslash = "/")
    }
  }
  if (!file.exists(template)) {
    cli::cli_abort(c(
      "The installed report template could not be found.",
      "i" = "Reinstall or reload the package after creating its template."
    ))
  }

  risk_summary <- function(x) {
    if (is.null(x)) return(NULL)
    data.frame(
      minimum_k = x$min_k,
      proportion_unique = x$prop_k1,
      proportion_k_below_5 = x$prop_k_lt5,
      risk_level = x$risk_level,
      stringsAsFactors = FALSE
    )
  }

  report_data <- list(
    generated_on = as.character(Sys.Date()),
    n_rows = audit$n_rows,
    threshold = audit$threshold,
    columns = audit$columns,
    reid = NULL,
    score_summary = NULL,
    score_breakdown = NULL,
    anonymisation = NULL
  )

  if (!is.null(reid)) {
    report_data$reid <- data.frame(
      quasi_identifiers = paste(reid$quasi_vars, collapse = ", "),
      rows = reid$n_rows,
      distinct_combinations = reid$n_classes,
      minimum_k = reid$min_k,
      proportion_unique = reid$prop_k1,
      proportion_k_below_5 = reid$prop_k_lt5,
      risk_level = reid$risk_level,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(score)) {
    report_data$score_summary <- data.frame(
      score = score$score,
      rating = score$rating,
      flagged_columns = score$n_flagged,
      reidentification_assessed = score$reid_assessed
    )
    report_data$score_breakdown <- score$breakdown
  }

  if (!is.null(anonymised)) {
    report_data$anonymisation <- list(
      changes = anonymised$report,
      risk_before = risk_summary(anonymised$risk_before),
      risk_after = risk_summary(anonymised$risk_after)
    )
  }

  output_format <- if (format == "html") {
    rmarkdown::html_document(toc = TRUE, self_contained = TRUE)
  } else {
    rmarkdown::pdf_document(toc = TRUE)
  }

  result <- rmarkdown::render(
    input = template,
    output_format = output_format,
    output_file = output_file,
    output_dir = output_dir,
    params = list(report_data = report_data),
    envir = new.env(parent = globalenv()),
    quiet = TRUE,
    clean = TRUE
  )

  invisible(normalizePath(result, winslash = "/", mustWork = TRUE))
}

