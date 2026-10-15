rand_chars <- function(n, len, pool) {
  vapply(
    seq_len(n),
    function(i) paste(sample(pool, len, replace = TRUE), collapse = ""),
    character(1)
  )
}

#' Generate synthetic Indian PII for examples and tests
#'
#' @description
#' Creates a data frame of fully synthetic employee records. The Aadhaar
#' numbers are built by running the Verhoeff checksum in reverse, so they pass
#' [detect_aadhaar()] but are not taken from any real person. PAN, mobile
#' number, UPI VPA and PIN code values follow the formats checked by the
#' package's detectors. No real identifiers are used. The function uses R's
#' random number generator, so call [set.seed()] first for reproducible
#' output.
#'
#' @param n A single whole number between 1 and 100000: the number of rows.
#'   Defaults to `100`.
#'
#' @return A data frame with `n` rows and the columns `name`, `aadhaar_no`,
#'   `pan_no`, `mobile`, `upi_id`, `pincode`, `dob` (a `Date`), `gender` and
#'   `salary`.
#'
#' @seealso [pii_scan()]
#' @export
#'
#' @examples
#' set.seed(2024)
#' df <- synthetic_pii(5)
#' df
#' pii_scan(df)
synthetic_pii <- function(n = 100) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1 ||
      n != floor(n) || n > 100000) {
    cli::cli_abort(c(
      "{.arg n} must be a single whole number between 1 and 100000.",
      "x" = "You supplied {.val {n}}."
    ))
  }
  n <- as.integer(n)
  digits <- as.character(0:9)

  base11 <- paste0(sample(2:9, n, replace = TRUE), rand_chars(n, 10L, digits))
  check <- vapply(base11, verhoeff_check_digit, numeric(1), USE.NAMES = FALSE)
  aadhaar <- paste0(base11, check)

  pan <- paste0(
    rand_chars(n, 3L, LETTERS),
    sample(c("P", "P", "P", "C", "F", "H"), n, replace = TRUE),
    rand_chars(n, 1L, LETTERS),
    rand_chars(n, 4L, digits),
    rand_chars(n, 1L, LETTERS)
  )

  mobile <- paste0(sample(6:9, n, replace = TRUE), rand_chars(n, 9L, digits))
  upi <- paste0(
    "user", sprintf("%04d", seq_len(n)), "@",
    sample(c("okaxis", "ybl", "oksbi", "paytm"), n, replace = TRUE)
  )
  pincode <- paste0(sample(pin_prefixes, n, replace = TRUE),
                    rand_chars(n, 4L, digits))

  start <- as.Date("1960-01-01")
  span <- as.integer(as.Date("2004-12-31") - start)
  dob <- start + sample.int(span + 1L, n, replace = TRUE) - 1L

  data.frame(
    name = sprintf("Employee_%04d", seq_len(n)),
    aadhaar_no = aadhaar,
    pan_no = pan,
    mobile = mobile,
    upi_id = upi,
    pincode = pincode,
    dob = dob,
    gender = sample(c("F", "M"), n, replace = TRUE),
    salary = sample(seq(25000, 150000, by = 500), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}
