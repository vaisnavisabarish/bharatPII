# Verhoeff multiplication, permutation and inverse tables
verhoeff_d <- matrix(c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  1, 2, 3, 4, 0, 6, 7, 8, 9, 5,
  2, 3, 4, 0, 1, 7, 8, 9, 5, 6,
  3, 4, 0, 1, 2, 8, 9, 5, 6, 7,
  4, 0, 1, 2, 3, 9, 5, 6, 7, 8,
  5, 9, 8, 7, 6, 0, 4, 3, 2, 1,
  6, 5, 9, 8, 7, 1, 0, 4, 3, 2,
  7, 6, 5, 9, 8, 2, 1, 0, 4, 3,
  8, 7, 6, 5, 9, 3, 2, 1, 0, 4,
  9, 8, 7, 6, 5, 4, 3, 2, 1, 0
), nrow = 10, byrow = TRUE)

verhoeff_p <- matrix(c(
  0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
  1, 5, 7, 6, 2, 8, 3, 0, 9, 4,
  5, 8, 0, 3, 7, 9, 6, 1, 4, 2,
  8, 9, 1, 6, 0, 4, 3, 5, 2, 7,
  9, 4, 5, 3, 1, 2, 6, 8, 7, 0,
  4, 2, 8, 6, 5, 7, 3, 9, 0, 1,
  2, 7, 9, 3, 8, 0, 6, 4, 1, 5,
  7, 0, 4, 6, 9, 1, 3, 2, 5, 8
), nrow = 8, byrow = TRUE)

verhoeff_inv <- c(0, 4, 3, 2, 1, 5, 6, 7, 8, 9)

#' Check a digit string against the Verhoeff checksum
#'
#' @param s A single character string of digits.
#' @return Logical scalar.
#' @keywords internal
#' @noRd
verhoeff_valid_one <- function(s) {
  if (is.na(s) || !grepl("^[0-9]+$", s)) return(FALSE)
  digits <- rev(as.integer(strsplit(s, "")[[1]]))
  c <- 0L
  for (i in seq_along(digits)) {
    c <- verhoeff_d[c + 1L, verhoeff_p[((i - 1L) %% 8L) + 1L, digits[i] + 1L] + 1L]
  }
  c == 0L
}

#' Compute the Verhoeff check digit for a digit string
#'
#' @param s A single character string of digits (without check digit).
#' @return A single digit as an integer.
#' @keywords internal
#' @noRd
verhoeff_check_digit <- function(s) {
  digits <- rev(as.integer(strsplit(s, "")[[1]]))
  c <- 0L
  for (i in seq_along(digits)) {
    c <- verhoeff_d[c + 1L, verhoeff_p[(i %% 8L) + 1L, digits[i] + 1L] + 1L]
  }
  verhoeff_inv[c + 1L]
}

# GSTIN base-36 alphabet
gstin_chars <- c(0:9, LETTERS)

#' Compute the GSTIN check character
#'
#' @param first14 A single string of the first 14 GSTIN characters.
#' @return A single character (the expected 15th character).
#' @keywords internal
#' @noRd
gstin_check_char <- function(first14) {
  chars <- strsplit(first14, "")[[1]]
  vals <- match(chars, gstin_chars) - 1L
  total <- 0L
  for (i in seq_along(vals)) {
    f <- if (i %% 2L == 1L) 1L else 2L
    prod <- vals[i] * f
    total <- total + (prod %/% 36L) + (prod %% 36L)
  }
  gstin_chars[((36L - (total %% 36L)) %% 36L) + 1L]
}
