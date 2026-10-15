#' Estimate re-identification risk with k-anonymity
#'
#' @description
#' Groups the rows of a data frame by a set of quasi-identifiers (for example
#' PIN code, date of birth and gender) and computes, for every row, `k`: the
#' number of rows that share the same combination of values. A row with
#' `k = 1` is unique on those columns and is the easiest to re-identify.
#' Missing values are treated as a value of their own.
#'
#' The risk level is a technical heuristic: `"HIGH"` when at least 20% of
#' rows are unique (`k = 1`), `"MEDIUM"` when at least 20% of rows have
#' `k < 5`, and `"LOW"` otherwise. It measures uniqueness inside the supplied
#' data frame only, not against the wider population.
#'
#' @param df A data frame (or tibble) with at least one row.
#' @param quasi_vars A character vector with the names of the columns of `df`
#'   to use as quasi-identifiers.
#'
#' @return An object of class `reid_risk`: a list with elements `quasi_vars`,
#'   `n_rows`, `k` (integer vector with one value per row), `n_classes`
#'   (number of distinct combinations), `min_k`, `prop_k1` (share of rows with
#'   `k = 1`), `prop_k_lt5` (share of rows with `k < 5`) and `risk_level`
#'   (`"HIGH"`, `"MEDIUM"` or `"LOW"`).
#'
#' @seealso [anonymize()], [dpdp_score()]
#' @export
#'
#' @examples
#' df <- data.frame(
#'   pincode = c("560001", "560001", "560001", "110001"),
#'   gender = c("F", "F", "F", "M")
#' )
#' reid_risk(df, quasi_vars = c("pincode", "gender"))
reid_risk <- function(df, quasi_vars) {
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "{.arg df} must be a data frame.",
      "x" = "You supplied an object of class {.cls {class(df)[1]}}."
    ))
  }
  if (nrow(df) == 0L) {
    cli::cli_abort("{.arg df} has no rows.")
  }
  if (!is.character(quasi_vars) || length(quasi_vars) == 0L ||
      anyNA(quasi_vars)) {
    cli::cli_abort(c(
      "{.arg quasi_vars} must be a character vector of column names.",
      "i" = "For example: {.code quasi_vars = c(\"pincode\", \"dob\")}."
    ))
  }
  absent <- setdiff(quasi_vars, names(df))
  if (length(absent) > 0L) {
    cli::cli_abort(c(
      "Quasi-identifiers not found in {.arg df}: {.val {absent}}.",
      "i" = "Check the spelling against {.code names(df)}."
    ))
  }

  parts <- lapply(unname(quasi_vars), function(v) {
    x <- as.character(df[[v]])
    x[is.na(x)] <- "<NA>"
    x
  })
  key <- do.call(paste, c(parts, list(sep = "\r")))
  idx <- match(key, unique(key))
  counts <- tabulate(idx)
  k <- counts[idx]

  prop_k1 <- mean(k == 1L)
  prop_k_lt5 <- mean(k < 5L)
  level <- if (prop_k1 >= 0.2) {
    "HIGH"
  } else if (prop_k_lt5 >= 0.2) {
    "MEDIUM"
  } else {
    "LOW"
  }

  structure(
    list(
      quasi_vars = quasi_vars,
      n_rows = length(k),
      k = k,
      n_classes = length(counts),
      min_k = min(k),
      prop_k1 = prop_k1,
      prop_k_lt5 = prop_k_lt5,
      risk_level = level
    ),
    class = "reid_risk"
  )
}
