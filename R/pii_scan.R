# Internal registry of detectors. Built inside a function so that it does not
# depend on the order in which package files are loaded.
pii_registry <- function() {
  list(
    aadhaar = list(fun = detect_aadhaar, label = "Aadhaar",
                   hint = "aadhaar|aadhar|adhar|uidai|^uid",
                   needs_hint = FALSE),
    pan = list(fun = detect_pan, label = "PAN",
               hint = "(^|[^a-z])pan([^a-z]|$)|pancard|permanent_?account",
               needs_hint = FALSE),
    gstin = list(fun = detect_gstin, label = "GSTIN",
                 hint = "gst", needs_hint = FALSE),
    ifsc = list(fun = detect_ifsc, label = "IFSC",
                hint = "ifsc", needs_hint = FALSE),
    upi = list(fun = detect_upi, label = "UPI VPA",
               hint = "(^|[^a-z])(upi|vpa)", needs_hint = FALSE),
    mobile = list(fun = detect_mobile, label = "Mobile number",
                  hint = "mobile|phone|cell|msisdn|contact_?(no|num)",
                  needs_hint = FALSE),
    pin = list(fun = detect_pin, label = "PIN code",
               hint = "(^|[^a-z])pin|postal|zip", needs_hint = TRUE),
    voter = list(fun = detect_voter, label = "Voter ID (EPIC)",
                 hint = "voter|(^|[^a-z])epic|elector", needs_hint = FALSE)
  )
}

# Internal: scan a single column and return a one-row data frame.
scan_one_column <- function(x, name, registry, threshold) {
  any_hint <- any(vapply(
    registry,
    function(r) grepl(r$hint, name, ignore.case = TRUE),
    logical(1)
  ))
  empty <- data.frame(
    column = name,
    detected = NA_character_,
    label = NA_character_,
    n_checked = 0L,
    n_valid = 0L,
    hit_rate = 0,
    confidence = NA_real_,
    name_match = any_hint,
    stringsAsFactors = FALSE
  )
  if (is.null(x) || !is.atomic(x)) return(empty)

  v <- as.character(x)
  v <- v[!is.na(v) & nzchar(trimws(v))]
  n <- length(v)
  if (n == 0L) return(empty)
  empty$n_checked <- n

  u <- unique(v)
  idx <- match(v, u)
  best <- NULL
  best_score <- -1

  for (key in names(registry)) {
    reg <- registry[[key]]
    res <- reg$fun(u)
    valid <- res$valid[idx]
    n_valid <- sum(valid)
    hit_rate <- n_valid / n
    name_match <- grepl(reg$hint, name, ignore.case = TRUE)
    if (n_valid == 0L || hit_rate < threshold) next
    if (reg$needs_hint && !name_match) next
    conf <- mean(res$confidence[idx][valid])
    score <- hit_rate * conf
    if (score > best_score) {
      best_score <- score
      best <- data.frame(
        column = name,
        detected = key,
        label = reg$label,
        n_checked = n,
        n_valid = as.integer(n_valid),
        hit_rate = hit_rate,
        confidence = conf,
        name_match = name_match,
        stringsAsFactors = FALSE
      )
    }
  }
  if (is.null(best)) empty else best
}

#' Scan a data frame for Indian personal identifiers
#'
#' @description
#' Runs the eight identifier detectors (Aadhaar, PAN, GSTIN, IFSC, UPI VPA,
#' mobile number, PIN code and Voter ID) over every column of a data frame.
#' A column is flagged when the share of its non-missing values that pass a
#' detector is at least `threshold`. Column names are used as a weak extra
#' signal, and PIN codes are only flagged when the column name looks like a
#' postal field, because any six-digit number resembles a PIN code. Each
#' flagged column is then classified with [classify_dpdp()].
#'
#' A valid result means the value is structurally valid. It does not prove
#' that the identifier was issued, and the risk tiers are a technical
#' convention, not legal advice.
#'
#' @param df A data frame (or tibble) to scan.
#' @param threshold A single number greater than 0 and at most 1: the minimum
#'   share of non-missing values in a column that must pass a detector for the
#'   column to be flagged. Defaults to `0.5`.
#'
#' @return An object of class `pii_audit`: a list with elements `columns`
#'   (a data frame with one row per column of `df` and the columns `column`,
#'   `detected`, `label`, `n_checked`, `n_valid`, `hit_rate`, `confidence`,
#'   `name_match`, `dpdp_category`, `risk_tier` and `dpdp_basis`), `n_rows`
#'   and `threshold`. For columns that are not flagged, `n_valid` and
#'   `hit_rate` are 0 and `confidence` is `NA`.
#'
#' @seealso [classify_dpdp()], [detect_aadhaar()]
#' @export
#'
#' @examples
#' df <- data.frame(
#'   name = c("Asha", "Ravi", "Meena"),
#'   pan_no = c("ABCPE1234F", "BCDFE5678G", "XYZHA4321K"),
#'   mobile = c("9876543210", "8123456789", "7012345678"),
#'   salary = c(50000, 62000, 58000)
#' )
#' audit <- pii_scan(df)
#' audit
pii_scan <- function(df, threshold = 0.5) {
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "{.arg df} must be a data frame.",
      "x" = "You supplied an object of class {.cls {class(df)[1]}}."
    ))
  }
  if (ncol(df) == 0L) {
    cli::cli_abort("{.arg df} has no columns to scan.")
  }
  if (!is.numeric(threshold) || length(threshold) != 1L ||
      is.na(threshold) || threshold <= 0 || threshold > 1) {
    cli::cli_abort(c(
      "{.arg threshold} must be a single number greater than 0 and at most 1.",
      "x" = "You supplied {.val {threshold}}."
    ))
  }

  registry <- pii_registry()
  rows <- lapply(
    seq_along(df),
    function(i) scan_one_column(df[[i]], names(df)[i], registry, threshold)
  )
  columns <- do.call(rbind, rows)
  rownames(columns) <- NULL

  audit <- structure(
    list(columns = columns, n_rows = nrow(df), threshold = threshold),
    class = "pii_audit"
  )
  classify_dpdp(audit)
}
