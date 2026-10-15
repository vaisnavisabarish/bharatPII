#' Detect and validate Indian mobile numbers
#'
#' @description
#' An Indian mobile number has 10 digits and starts with 6, 7, 8 or 9. An
#' optional `+91`, `91` or `0` prefix is removed first, and spaces and hyphens
#' are ignored. Mobile numbers have no checksum, so a valid result means only
#' that the format is plausible.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.9 if the
#'   format is valid, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_mobile(c("9876543210", "+91 98765 43210", "5876543210", NA))
detect_mobile <- function(x) {
  check_id_input(x)
  n <- sub("^\\+", "", normalise_id(x))
  len <- nchar(n)
  strip <- !is.na(n) &
    ((len == 12L & startsWith(n, "91")) | (len == 11L & startsWith(n, "0")))
  n[strip] <- ifelse(len[strip] == 12L,
                     substring(n[strip], 3L),
                     substring(n[strip], 2L))
  ok <- !is.na(n) & grepl("^[6-9][0-9]{9}$", n)
  confidence <- ifelse(ok, 0.9, 0)
  detector_result(x, ok, confidence)
}
