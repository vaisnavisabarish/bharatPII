# Internal rubric. The DPDP Act, 2023 defines "personal data" (Section 2(t))
# but has no statutory "sensitive personal data" category. The risk tier below
# is a package convention used for technical prioritisation only.
dpdp_rubric <- function() {
  personal <- "Section 2(t): data about an identifiable individual"
  context <- paste("Section 2(t) applies when the value is linked to an",
                   "identifiable individual")
  data.frame(
    detected = c("aadhaar", "pan", "voter", "mobile", "upi",
                 "gstin", "ifsc", "pin"),
    dpdp_category = c(rep("Personal data", 5L),
                      rep("Personal data (context-dependent)", 3L)),
    risk_tier = c("High", "High", "High", "Medium", "Medium",
                  "Medium", "Low", "Low"),
    dpdp_basis = c(rep(personal, 5L), rep(context, 3L)),
    stringsAsFactors = FALSE
  )
}

#' Classify detected columns under the DPDP Act, 2023
#'
#' @description
#' Adds a DPDP category, a risk tier and the legal basis to every column of a
#' `pii_audit` object. The Digital Personal Data Protection Act, 2023 defines
#' "personal data" (Section 2(t)) but has no separate statutory category of
#' "sensitive personal data". The `risk_tier` (`"High"`, `"Medium"`, `"Low"`
#' or `"None"`) is therefore a package-defined convention for prioritising
#' technical work, and is not a legal classification or legal advice.
#'
#' @param audit A `pii_audit` object returned by [pii_scan()].
#'
#' @return The `pii_audit` object, with the columns `dpdp_category`,
#'   `risk_tier` and `dpdp_basis` set in its `columns` data frame. Calling the
#'   function again on a classified object gives the same result.
#'
#' @seealso [pii_scan()]
#' @export
#'
#' @examples
#' df <- data.frame(mobile = c("9876543210", "8123456789"))
#' classify_dpdp(pii_scan(df))
classify_dpdp <- function(audit) {
  if (!inherits(audit, "pii_audit")) {
    cli::cli_abort(c(
      "{.arg audit} must be a {.cls pii_audit} object.",
      "i" = "Create one with {.code pii_scan()}.",
      "x" = "You supplied an object of class {.cls {class(audit)[1]}}."
    ))
  }
  r <- dpdp_rubric()
  cols <- audit$columns
  i <- match(cols$detected, r$detected)
  found <- !is.na(i)
  cols$dpdp_category <- ifelse(found, r$dpdp_category[i],
                               "No identifier detected")
  cols$risk_tier <- ifelse(found, r$risk_tier[i], "None")
  cols$dpdp_basis <- ifelse(found, r$dpdp_basis[i], NA_character_)
  audit$columns <- cols
  audit
}
