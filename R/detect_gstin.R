# Internal: state and union territory codes accepted in a GSTIN
gstin_state_codes <- c(sprintf("%02d", 1:38), "97", "99")

#' Detect and validate GSTIN values
#'
#' @description
#' A GSTIN has 15 characters: a two-digit state code, the holder's 10-character
#' PAN, a 13th entity-number character, the letter `Z`, and a final check
#' character. This function checks the structure, the state code (01 to 38,
#' 97 or 99), the PAN holder class and the base-36 checksum. Spaces and hyphens
#' are ignored, and case is ignored. A valid result does not prove that the
#' GSTIN is registered.
#'
#' @param x An atomic vector (character, numeric or factor) of candidate
#'   identifiers. `NA` values are returned as not valid.
#'
#' @return A data frame with one row per element of `x` and columns `value`
#'   (character), `valid` (logical) and `confidence` (numeric: 0.98 if every
#'   check passes, 0.2 if only the 15-character pattern matches, 0 otherwise).
#'
#' @family detectors
#' @export
#'
#' @examples
#' detect_gstin(c("27AAPFU0939F1ZV", "not a gstin", NA))
detect_gstin <- function(x) {
  check_id_input(x)
  n <- normalise_id(x)
  fmt <- !is.na(n) &
    grepl("^[0-9]{2}[A-Z]{5}[0-9]{4}[A-Z][1-9A-Z]Z[0-9A-Z]$", n)
  ok <- vapply(
    seq_along(n),
    function(i) {
      if (!fmt[i]) return(FALSE)
      substr(n[i], 1L, 2L) %in% gstin_state_codes &&
        grepl("^[PCHFATBLJG]$", substr(n[i], 6L, 6L)) &&
        substr(n[i], 15L, 15L) == gstin_check_char(substr(n[i], 1L, 14L))
    },
    logical(1)
  )
  confidence <- ifelse(ok, 0.98, ifelse(fmt, 0.2, 0))
  detector_result(x, ok, confidence)
}
