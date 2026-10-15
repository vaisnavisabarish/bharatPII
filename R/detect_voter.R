#' Detect and validate Voter ID (EPIC) numbers
#'
#' @description
#' An Elector Photo Identity Card (EPIC) number has three letters followed by
#' seven digits. Spaces, hyphens and case are ignored. Only the format is
#' checked, so a valid result does not prove that the number was issued.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.85 if the
#'   format is valid, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_voter(c("ABC1234567", "abc1234567", "AB12345678", NA))
detect_voter <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  ok <- !is.na(n) & grepl("^[A-Z]{3}[0-9]{7}$", n)
  confidence <- ifelse(ok, 0.85, 0)
  detector_result(x, ok, confidence)
}
