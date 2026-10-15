#' Normalise a character vector of identifiers
#'
#' Internal helper. Coerces to character, trims whitespace, removes spaces
#' and hyphens, and upper-cases.
#'
#' @param x A vector coercible to character.
#' @param upper Logical; upper-case the result.
#' @return A character vector the same length as `x`.
#' @keywords internal
#' @noRd
normalise_id <- function(x, upper = TRUE) {
  x <- as.character(x)
  x <- gsub("[[:space:]-]", "", x)
  if (upper) x <- toupper(x)
  x
}

#' Validate that the input is a vector of identifiers
#'
#' @param x Object to check.
#' @param arg Name of the argument, for error messages.
#' @return `x` invisibly; aborts with a cli error otherwise.
#' @keywords internal
#' @noRd
check_id_input <- function(x, arg = "x") {
  if (is.factor(x)) x <- as.character(x)
  if (!is.atomic(x) || is.null(x) || is.list(x)) {
    cli::cli_abort(c(
      "{.arg {arg}} must be an atomic vector of identifiers.",
      "x" = "You supplied an object of class {.cls {class(x)[1]}}."
    ))
  }
  invisible(x)
}

#' Build a standard detector result
#'
#' @param value Original input values.
#' @param valid Logical vector of validity.
#' @param confidence Numeric vector of confidence scores.
#' @return A data frame with columns `value`, `valid`, `confidence`.
#' @keywords internal
#' @noRd
detector_result <- function(value, valid, confidence) {
  data.frame(
    value = as.character(value),
    valid = valid,
    confidence = confidence,
    stringsAsFactors = FALSE
  )
}
