#' Methods for pii_audit objects
#'
#' @description
#' Print, summarise or convert the result of [pii_scan()].
#'
#' @param x,object A `pii_audit` object returned by [pii_scan()].
#' @param row.names,optional,... Ignored; present for compatibility with the
#'   generic functions.
#'
#' @return `print()` returns `x` invisibly. `summary()` returns a data frame
#'   with one row per risk tier and the number of columns in that tier.
#'   `as.data.frame()` returns the per-column results as a data frame.
#'
#' @name pii_audit-methods
#'
#' @examples
#' audit <- pii_scan(data.frame(mobile = c("9876543210", "8123456789")))
#' print(audit)
#' summary(audit)
#' as.data.frame(audit)
NULL

#' @rdname pii_audit-methods
#' @export
print.pii_audit <- function(x, ...) {
  cols <- x$columns
  flagged <- !is.na(cols$detected)
  n_cols <- nrow(cols)
  cat(sprintf(
    "<pii_audit>: %d column%s scanned over %d row%s, %d flagged (threshold %.2f)\n",
    n_cols, if (n_cols == 1L) "" else "s",
    x$n_rows, if (x$n_rows == 1L) "" else "s",
    sum(flagged), x$threshold
  ))
  show <- data.frame(
    Column = cols$column,
    Detected = ifelse(flagged, cols$label, "(none)"),
    HitRate = sprintf("%.2f", cols$hit_rate),
    Confidence = ifelse(is.na(cols$confidence), "-",
                        sprintf("%.2f", cols$confidence)),
    stringsAsFactors = FALSE
  )
  if ("risk_tier" %in% names(cols)) {
    show$Category <- cols$dpdp_category
    show$RiskTier <- cols$risk_tier
  }
  print(show, row.names = FALSE)
  cat("Structural validity only; risk tiers are a package convention, not legal advice.\n")
  invisible(x)
}

#' @rdname pii_audit-methods
#' @export
summary.pii_audit <- function(object, ...) {
  tiers <- c("High", "Medium", "Low", "None")
  counts <- vapply(tiers, function(t) sum(object$columns$risk_tier == t),
                   integer(1))
  data.frame(risk_tier = tiers, n_columns = unname(counts),
             stringsAsFactors = FALSE)
}

#' @rdname pii_audit-methods
#' @export
as.data.frame.pii_audit <- function(x, row.names = NULL, optional = FALSE, ...) {
  x$columns
}
