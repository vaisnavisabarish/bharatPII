#' Detect and validate PAN (Permanent Account Number) values
#'
#' @description
#' A PAN has 10 characters: five letters, four digits and one letter. The
#' fourth character gives the holder class and must be one of `P`, `C`, `H`,
#' `F`, `A`, `T`, `B`, `L`, `J` or `G`. Spaces and hyphens are ignored, and
#' case is ignored. PAN has no public checksum, so a valid result means only
#' that the structure is correct.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.95 if the
#'   structure and holder class are valid, 0.1 if only the 10-character
#'   pattern matches, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_pan(c("ABCPE1234F", "abcde1234f", "12345ABCDE", NA))
detect_pan <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  fmt <- !is.na(n) & grepl("^[A-Z]{5}[0-9]{4}[A-Z]$", n)
  cls <- fmt & grepl("^.{3}[PCHFATBLJG]", n)
  confidence <- ifelse(cls, 0.95, ifelse(fmt, 0.1, 0))
  detector_result(x, cls, confidence)
}
