# ---- internal helpers -------------------------------------------------------

# Mask all but the last `keep` letters/digits, keeping separators. At most half
# of the characters are ever left visible, so short values are still masked.
mask_one <- function(s, keep) {
  ch <- strsplit(s, "", fixed = TRUE)[[1L]]
  # if (length(ch) == 0L) return(s)
  pos <- which(grepl("[[:alnum:]]", ch))
  n <- length(pos)
  keep_eff <- min(keep, n %/% 2L)
  ch[pos[seq_len(n - keep_eff)]] <- "X"
  paste(ch, collapse = "")
}

mask_values <- function(x, keep) {
  v <- as.character(x)
  ok <- !is.na(v)
  v[ok] <- vapply(v[ok], mask_one, character(1), keep = keep,
                  USE.NAMES = FALSE)
  v
}

hash_values <- function(x, salt) {
  v <- as.character(x)
  ok <- !is.na(v)
  v[ok] <- vapply(
    v[ok],
    function(s) {
      digest::digest(paste0(salt, "|", s), algo = "sha256", serialize = FALSE)
    },
    character(1),
    USE.NAMES = FALSE
  )
  v
}

# Parse dates in a few unambiguous shapes. The shape is checked first, because
# "%Y" would otherwise accept a two-digit day as a year.
parse_dates <- function(v) {
  s <- trimws(v)
  out <- rep(as.Date(NA), length(s))
  shapes <- list(
    c("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", "%Y-%m-%d"),
    c("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", "%d/%m/%Y"),
    c("^[0-9]{2}-[0-9]{2}-[0-9]{4}$", "%d-%m-%Y"),
    c("^[0-9]{4}/[0-9]{2}/[0-9]{2}$", "%Y/%m/%d")
  )
  for (sh in shapes) {
    todo <- is.na(out) & !is.na(s) & grepl(sh[1L], s)
    if (any(todo)) out[todo] <- as.Date(s[todo], format = sh[2L])
  }
  out
}

age_band <- function(dob, ref, width) {
  d <- as.POSIXlt(dob)
  r <- as.POSIXlt(ref)
  age <- r$year - d$year - ((r$mon < d$mon) | (r$mon == d$mon & r$mday < d$mday))
  lo <- (age %/% width) * width
  ifelse(is.na(age) | age < 0, NA_character_, paste0(lo, "-", lo + width - 1))
}

numeric_band <- function(x, width) {
  lo <- floor(x / width) * width
  lo_txt <- format(lo, scientific = FALSE, trim = TRUE)
  hi_txt <- format(lo + width - 1, scientific = FALSE, trim = TRUE)
  ifelse(is.na(x), NA_character_, paste0(lo_txt, "-", hi_txt))
}

generalize_values <- function(x, col, band_width, ref) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    return(age_band(as.Date(x), ref, band_width))
  }
  pin_hint <- pii_registry()$pin$hint
  if (is.numeric(x) && !grepl(pin_hint, col, ignore.case = TRUE)) {
    return(numeric_band(x, band_width))
  }
  if (is.numeric(x)) {
    v <- format(x, scientific = FALSE, trim = TRUE)
    v[is.na(x)] <- NA_character_
  } else {
    v <- as.character(x)
  }
  present <- !is.na(v) & nzchar(trimws(v))
  if (!any(present)) return(rep(NA_character_, length(v)))

  dates <- parse_dates(v)
  if (mean(!is.na(dates[present])) >= 0.5) {
    return(age_band(dates, ref, band_width))
  }
  p <- normalise_id(v)
  is_pin <- !is.na(p) & grepl("^[1-9][0-9]{5}$", p)
  if (mean(is_pin[present]) >= 0.5) {
    return(ifelse(is_pin, paste0(substr(p, 1L, 3L), "XXX"), NA_character_))
  }
  cli::cli_abort(c(
    "Column {.field {col}} cannot be generalised.",
    "i" = "Generalisation supports dates (to age bands), PIN codes and numbers."
  ))
}

# ---- exported function -----------------------------------------------------

#' Anonymise columns of a data frame
#'
#' @description
#' Masks, hashes or generalises the columns you name, and reports what was
#' changed. If `quasi_vars` is given, the re-identification risk from
#' [reid_risk()] is also computed before and after, so you can see the effect.
#'
#' The methods are:
#'
#' * `"mask"`: replaces every letter and digit with `X` except the last `keep`
#'   (never more than half of the characters), keeping separators such as
#'   spaces and hyphens.
#' * `"hash"`: replaces each value with its salted SHA-256 hash. Values that
#'   are equal get equal hashes, so joins on the column still work.
#' * `"generalize"` (or `"generalise"`): dates become age bands such as
#'   `"30-39"`, PIN codes keep their first three digits (for example
#'   `"560XXX"`, a postal region and not a district), and numbers become bands
#'   of width `band_width`. Values that cannot be generalised become `NA`.
#'
#' Masked and generalised data is easier to re-identify than fully removed
#' data, and hashing is pseudonymisation, not anonymisation. Check the result
#' with [reid_risk()] before sharing it.
#'
#' @param df A data frame (or tibble).
#' @param method Either a named character vector that maps column names to
#'   methods, such as `c(aadhaar_no = "mask", dob = "generalize")`, or a single
#'   unnamed `"mask"` or `"hash"`, which is applied to every column that
#'   [pii_scan()] flags.
#' @param salt A single non-empty string used for hashing, or `NULL`. When
#'   `NULL`, a random salt is generated and discarded, so hashes cannot be
#'   linked across calls. Supply a secret salt for consistent pseudonyms, and
#'   do not share it with the data.
#' @param quasi_vars Optional character vector of column names to use for the
#'   before/after [reid_risk()] comparison.
#' @param keep A single non-negative whole number: how many trailing letters
#'   and digits `"mask"` leaves visible. Defaults to `4`.
#' @param band_width A single positive number: the width of numeric bands and
#'   age bands. Defaults to `10`.
#' @param reference_date The date used to compute ages for date columns.
#'   Defaults to today.
#'
#' @return An object of class `pii_anonymized`: a list with elements `data`
#'   (the anonymised data frame), `report` (a data frame with the columns
#'   `column`, `method`, `n_changed` and `n_non_missing`), and `risk_before`
#'   and `risk_after` (`reid_risk` objects, or `NULL` when `quasi_vars` is not
#'   given). Use `as.data.frame()` to extract the data.
#'
#' @seealso [reid_risk()], [pii_scan()]
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- synthetic_pii(8)
#' res <- anonymize(
#'   df,
#'   method = c(aadhaar_no = "mask", pan_no = "hash", dob = "generalize"),
#'   salt = "example-salt",
#'   quasi_vars = c("pincode", "dob", "gender"),
#'   reference_date = as.Date("2024-06-01")
#' )
#' res
#' as.data.frame(res)[1:3, c("aadhaar_no", "dob")]
#'
#' # Mask every flagged column automatically
#' auto <- anonymize(df, "mask")
#' pii_scan(as.data.frame(auto))
anonymize <- function(df, method, salt = NULL, quasi_vars = NULL, keep = 4,
                      band_width = 10, reference_date = Sys.Date()) {
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "{.arg df} must be a data frame.",
      "x" = "You supplied an object of class {.cls {class(df)[1]}}."
    ))
  }
  if (nrow(df) == 0L) {
    cli::cli_abort("{.arg df} has no rows.")
  }
  if (!is.character(method) || length(method) == 0L || anyNA(method)) {
    cli::cli_abort(c(
      "{.arg method} must be a character vector of anonymisation methods.",
      "i" = "For example: {.code c(aadhaar_no = \"mask\", dob = \"generalize\")}."
    ))
  }
  if (!is.numeric(keep) || length(keep) != 1L || is.na(keep) || keep < 0 ||
      keep != floor(keep)) {
    cli::cli_abort("{.arg keep} must be a single non-negative whole number.")
  }
  if (!is.numeric(band_width) || length(band_width) != 1L ||
      is.na(band_width) || band_width <= 0) {
    cli::cli_abort("{.arg band_width} must be a single positive number.")
  }
  if (!is.null(salt) && (!is.character(salt) || length(salt) != 1L ||
                         is.na(salt) || !nzchar(salt))) {
    cli::cli_abort("{.arg salt} must be a single non-empty string or NULL.")
  }
  ref <- tryCatch(as.Date(reference_date), error = function(e) as.Date(NA))
  if (length(ref) != 1L || is.na(ref)) {
    cli::cli_abort("{.arg reference_date} must be a single valid date.")
  }

  method[method == "generalise"] <- "generalize"
  bad <- setdiff(unique(method), c("mask", "hash", "generalize"))
  if (length(bad) > 0L) {
    cli::cli_abort(c(
      "Unknown anonymisation method: {.val {bad}}.",
      "i" = "Use {.val mask}, {.val hash} or {.val generalize}."
    ))
  }

  risk_before <- if (is.null(quasi_vars)) NULL else reid_risk(df, quasi_vars)

  nm <- names(method)
  if (is.null(nm)) {
    if (length(method) != 1L) {
      cli::cli_abort(
        "An unnamed {.arg method} must be one method; otherwise name every method by column."
      )
    }
    if (method == "generalize") {
      cli::cli_abort(
        "Automatic column selection supports {.val mask} and {.val hash} only."
      )
    }
    audit <- pii_scan(df)
    cols <- audit$columns$column[!is.na(audit$columns$detected)]
    if (length(cols) == 0L) {
      message("No identifier columns detected; the data was returned unchanged.")
    }
    method <- rep(method, length(cols))
    names(method) <- cols
  } else if (any(is.na(nm) | !nzchar(nm))) {
    cli::cli_abort("Every element of {.arg method} must be named by a column.")
  }
  if (anyDuplicated(names(method)) > 0L) {
    cli::cli_abort("Each column may appear only once in {.arg method}.")
  }
  absent <- setdiff(names(method), names(df))
  if (length(absent) > 0L) {
    cli::cli_abort("Columns not found in {.arg df}: {.val {absent}}.")
  }

  if (any(method == "hash") && is.null(salt)) {
    salt <- digest::digest(list(Sys.time(), tempfile(), proc.time()[[3L]]),
                           algo = "sha256")
  }

  out <- df
  n_changed <- integer(length(method))
  n_present <- integer(length(method))
  for (i in seq_along(method)) {
    col <- names(method)[i]
    before <- df[[col]]
    if (!is.atomic(before)) {
      cli::cli_abort(
        "Column {.field {col}} must be an atomic vector to be anonymised."
      )
    }
    after <- switch(
      method[[i]],
      mask = mask_values(before, keep),
      hash = hash_values(before, salt),
      generalize = generalize_values(before, col, band_width, ref)
    )
    out[[col]] <- after
    present <- !is.na(before)
    changed <- present & (is.na(after) | as.character(before) != after)
    n_present[i] <- sum(present)
    n_changed[i] <- sum(changed)
  }

  report <- data.frame(
    column = names(method),
    method = unname(method),
    n_changed = n_changed,
    n_non_missing = n_present,
    stringsAsFactors = FALSE
  )
  risk_after <- if (is.null(quasi_vars)) NULL else reid_risk(out, quasi_vars)

  structure(
    list(data = out, report = report,
         risk_before = risk_before, risk_after = risk_after),
    class = "pii_anonymized"
  )
}
