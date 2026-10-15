#' Detect and validate Indian PIN codes
#'
#' @description
#' A PIN code has six digits. The first digit is 1 to 9 and the first two
#' digits must belong to a postal circle. Spaces and hyphens are ignored. PIN
#' codes have no checksum and any six-digit number can look like one, so
#' confidence is deliberately modest.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.6 if the
#'   format and postal-circle prefix are valid, 0.1 if only the six-digit
#'   format matches, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_pin(c("560001", "560 001", "100000", "5600", NA))
detect_pin <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  fmt <- !is.na(n) & grepl("^[1-9][0-9]{5}$", n)
  ok <- fmt & substr(n, 1L, 2L) %in% pin_prefixes
  confidence <- ifelse(ok, 0.6, ifelse(fmt, 0.1, 0))
  detector_result(x, ok, confidence)
}
