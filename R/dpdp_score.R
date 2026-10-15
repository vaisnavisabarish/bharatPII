#' Compute a DPDP-readiness score
#'
#' @description
#' Combines a [pii_scan()] result, and optionally a [reid_risk()] result, into
#' a single score from 0 to 100 with a section-wise breakdown. The rubric has
#' three sections:
#'
#' * **Direct identifier exposure** (50 points): 25 points are deducted for
#'   each flagged column in the `"High"` risk tier, 10 for `"Medium"` and 3 for
#'   `"Low"`. Basis: Section 2(t), which defines personal data.
#' * **Re-identification risk** (30 points): `30 * (1 - share of rows with
#'   k < 5)`. Only assessed when `reid` is supplied. Basis: Section 2(t),
#'   because data about an *identifiable* individual is personal data.
#' * **Identifier breadth** (20 points): 5 points are deducted for each
#'   distinct identifier type found. Basis: Section 8(5), reasonable security
#'   safeguards, since more identifier types increase the impact of a breach.
#'
#' If `reid` is not supplied, the score is calculated over the assessed
#' sections only and rescaled to 0-100. The rubric is a package-defined
#' technical assessment. The section references are indicative, and the score
#' is neither legal advice nor a certification of compliance.
#'
#' @param audit A `pii_audit` object returned by [pii_scan()].
#' @param reid Optional. A `reid_risk` object returned by [reid_risk()].
#'
#' @return An object of class `dpdp_score`: a list with elements `score`
#'   (0-100), `rating` (`"Strong"`, `"Moderate"` or `"Weak"`), `breakdown` (a
#'   data frame with the columns `section`, `basis`, `points`, `max_points`
#'   and `status`), `n_flagged`, `exposure_status` and `reid_assessed`.
#'
#' @seealso [pii_scan()], [reid_risk()]
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- synthetic_pii(20)
#' audit <- pii_scan(df)
#' risk <- reid_risk(df, c("pincode", "dob", "gender"))
#' dpdp_score(audit, risk)
dpdp_score <- function(audit, reid = NULL) {
  audit <- classify_dpdp(audit)
  if (!is.null(reid) && !inherits(reid, "reid_risk")) {
    cli::cli_abort(c(
      "{.arg reid} must be {.code NULL} or a {.cls reid_risk} object.",
      "i" = "Create one with {.code reid_risk()}.",
      "x" = "You supplied an object of class {.cls {class(reid)[1]}}."
    ))
  }

  cols <- audit$columns
  flagged <- cols[!is.na(cols$detected), , drop = FALSE]
  n_flagged <- nrow(flagged)

  penalty <- c(High = 25, Medium = 10, Low = 3)
  lost <- sum(unname(penalty[flagged$risk_tier]), na.rm = TRUE)
  exposure_pts <- max(0, 50 - lost)
  exposure_status <- if (any(flagged$risk_tier == "High")) {
    "HIGH"
  } else if (any(flagged$risk_tier == "Medium")) {
    "MEDIUM"
  } else if (any(flagged$risk_tier == "Low")) {
    "LOW"
  } else {
    "NONE"
  }

  n_types <- length(unique(flagged$detected))
  breadth_pts <- max(0, 20 - 5 * n_types)
  breadth_status <- if (n_types >= 3L) {
    "HIGH"
  } else if (n_types == 2L) {
    "MEDIUM"
  } else if (n_types == 1L) {
    "LOW"
  } else {
    "NONE"
  }

  reid_pts <- if (is.null(reid)) NA_real_ else 30 * (1 - reid$prop_k_lt5)
  reid_status <- if (is.null(reid)) "NOT ASSESSED" else reid$risk_level

  breakdown <- data.frame(
    section = c("Direct identifier exposure", "Re-identification risk",
                "Identifier breadth"),
    basis = c("Section 2(t)", "Section 2(t)", "Section 8(5)"),
    points = c(exposure_pts, reid_pts, breadth_pts),
    max_points = c(50, 30, 20),
    status = c(exposure_status, reid_status, breadth_status),
    stringsAsFactors = FALSE
  )

  assessed <- !is.na(breakdown$points)
  score <- round(100 * sum(breakdown$points[assessed]) /
                   sum(breakdown$max_points[assessed]))
  rating <- if (score >= 80) {
    "Strong"
  } else if (score >= 50) {
    "Moderate"
  } else {
    "Weak"
  }

  structure(
    list(
      score = score,
      rating = rating,
      breakdown = breakdown,
      n_flagged = n_flagged,
      exposure_status = exposure_status,
      reid_assessed = !is.null(reid)
    ),
    class = "dpdp_score"
  )
}
