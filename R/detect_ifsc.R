#' Detect and validate IFSC codes
#'
#' @description
#' An IFSC has 11 characters: four letters (the bank), the digit `0`, and six
#' alphanumeric characters (the branch). A code is reported as valid when the
#' structure is correct and the four-letter bank code is in the package's
#' internal, non-exhaustive list of bank codes. Spaces, hyphens and case are
#' ignored. A valid result does not prove that the branch exists.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.95 if the
#'   structure and bank code are valid, 0.3 if only the structure matches,
#'   0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_ifsc(c("SBIN0001234", "sbin0001234", "ABCD0123456", NA))
detect_ifsc <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  fmt <- !is.na(n) & grepl("^[A-Z]{4}0[A-Z0-9]{6}$", n)
  known <- fmt & substr(n, 1L, 4L) %in% ifsc_bank_codes
  confidence <- ifelse(known, 0.95, ifelse(fmt, 0.3, 0))
  detector_result(x, known, confidence)
}
