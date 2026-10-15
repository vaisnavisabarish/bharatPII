#' Detect and validate UPI virtual payment addresses
#'
#' @description
#' A UPI VPA has the form `name@handle`. The name has 2 to 100 letters,
#' digits, dots, hyphens or underscores, and the handle is a payment service
#' provider such as `okaxis` or `ybl`. A VPA is reported as valid when the
#' structure is correct and the handle is in the package's internal,
#' non-exhaustive list of handles. Ordinary email addresses are therefore not
#' reported as valid. Case and surrounding whitespace are ignored.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.95 if the
#'   structure and handle are valid, 0.2 if only the structure matches,
#'   0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_upi(c("asha.k@okaxis", "john@gmail.com", "nothing", NA))
detect_upi <- function(x) {
  check_id_input(x)
  n <- tolower(trimws(as.character(x)))
  fmt <- !is.na(n) & grepl("^[a-z0-9._-]{2,100}@[a-z]{2,64}$", n)
  handle <- sub("^.*@", "", n)
  known <- fmt & handle %in% upi_handles
  confidence <- ifelse(known, 0.95, ifelse(fmt, 0.2, 0))
  detector_result(x, known, confidence)
}
