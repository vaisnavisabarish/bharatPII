#' Detect and validate Aadhaar numbers
#'
#' @description
#' Checks whether each element looks like an Aadhaar number: 12 digits, first
#' digit between 2 and 9, and a valid Verhoeff check digit. Spaces and hyphens
#' are ignored. A valid result means the number is structurally valid; it does
#' not prove that the number was ever issued.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character, the original input), `valid` (logical) and `confidence`
#'   (numeric: 0.98 if the format and checksum pass, 0.2 if only the format
#'   passes, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_aadhaar(c("2345 6789 0124", "not an id", NA))
detect_aadhaar <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  fmt <- !is.na(n) & grepl("^[2-9][0-9]{11}$", n)
  ok <- vapply(
    seq_along(n),
    function(i) fmt[i] && verhoeff_valid_one(n[i]),
    logical(1)
  )
  confidence <- ifelse(ok, 0.98, ifelse(fmt, 0.2, 0))
  detector_result(x, ok, confidence)
}
