#' Methods for bharatpii result objects
#'
#' @description
#' Print the results of [reid_risk()], [dpdp_score()] and [anonymize()], or
#' extract the anonymised data frame from an [anonymize()] result.
#'
#' @param x A `reid_risk`, `dpdp_score` or `pii_anonymized` object.
#' @param row.names,optional,... Ignored; present for compatibility with the
#'   generic functions.
#'
#' @return The `print()` methods return `x` invisibly. `as.data.frame()`
#'   returns the anonymised data frame.
#'
#' @name bharatpii-methods
#'
#' @examples
#' df <- data.frame(g = c("a", "a", "a", "b"))
#' print(reid_risk(df, "g"))
#' res <- anonymize(data.frame(id = "ABCPE1234F"), c(id = "mask"))
#' print(res)
#' as.data.frame(res)
NULL

#' @rdname bharatpii-methods
#' @export
print.reid_risk <- function(x, ...) {
  cat(sprintf("<reid_risk>: quasi-identifiers: %s\n",
              paste(x$quasi_vars, collapse = ", ")))
  cat(sprintf("%d rows in %d groups; minimum k = %d\n",
              x$n_rows, x$n_classes, x$min_k))
  cat(sprintf("k = 1 for %.0f%% of rows; k < 5 for %.0f%% of rows\n",
              100 * x$prop_k1, 100 * x$prop_k_lt5))
  cat(sprintf("Re-identification risk: %s\n", x$risk_level))
  cat("Uniqueness within this data frame only; not a population-level estimate.\n")
  invisible(x)
}

#' @rdname bharatpii-methods
#' @export
print.dpdp_score <- function(x, ...) {
  cat("<dpdp_score>\n")
  cat(sprintf("Overall DPDP readiness: %d / 100 (%s)\n",
              as.integer(x$score), x$rating))
  cat(sprintf("Section 2(t) exposure: %s (%d flagged identifier column%s)\n",
              x$exposure_status, x$n_flagged,
              if (x$n_flagged == 1L) "" else "s"))
  b <- x$breakdown
  show <- data.frame(
    Section = b$section,
    Basis = b$basis,
    Points = ifelse(is.na(b$points), "not assessed",
                    sprintf("%.0f / %.0f", b$points, b$max_points)),
    Status = b$status,
    stringsAsFactors = FALSE
  )
  print(show, row.names = FALSE)
  if (!x$reid_assessed) {
    cat("Re-identification risk not assessed; supply `reid = reid_risk(...)`.\n")
    cat("The score covers the assessed sections only.\n")
  }
  cat("Technical assessment against a package-defined rubric; not legal advice.\n")
  invisible(x)
}

#' @rdname bharatpii-methods
#' @export
print.pii_anonymized <- function(x, ...) {
  n <- nrow(x$report)
  cat(sprintf("<pii_anonymized>: %d column%s transformed in %d rows\n",
              n, if (n == 1L) "" else "s", nrow(x$data)))
  print(x$report, row.names = FALSE)
  if (!is.null(x$risk_before)) {
    b <- x$risk_before
    a <- x$risk_after
    cat(sprintf("Re-identification risk (%s): %s -> %s\n",
                paste(b$quasi_vars, collapse = ", "),
                b$risk_level, a$risk_level))
    cat(sprintf("Rows with k = 1: %.0f%% -> %.0f%%\n",
                100 * b$prop_k1, 100 * a$prop_k1))
  }
  invisible(x)
}

#' @rdname bharatpii-methods
#' @export
as.data.frame.pii_anonymized <- function(x, row.names = NULL, optional = FALSE,
                                         ...) {
  x$data
}
