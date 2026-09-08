#' Detect Aadhaar-shaped candidates
#'
#' Finds 12 ASCII digits, either contiguous or in three groups of four
#' separated by one or more spaces or tabs. Candidates may occur within text,
#' but cannot be immediately adjacent to another ASCII digit. Formatting is
#' removed only from matched candidates; unrelated numbers are not joined.
#'
#' @param x A character vector. Missing values are allowed. Numeric inputs are
#'   rejected to avoid loss of formatting or precision.
#' @return A data frame with one row per candidate, in input and occurrence
#'   order. Inputs without candidates receive one row as well. Columns are
#'   `input_index` (integer), `candidate` (matched text), `normalized` (digits),
#'   `start` and `end` (one-based inclusive character positions), `valid`
#'   (logical checksum result), and `status` (`"valid"`, `"invalid_checksum"`,
#'   `"no_candidate"`, or `"missing"`). For unmatched or missing inputs,
#'   candidate fields and `valid` are `NA`. Empty input returns zero rows
#'   with the same column types.
#' @details Valid means only that a 12-digit candidate passes the Verhoeff
#'   checksum. This does not verify issuance, identity, or ownership, and no
#'   external service is contacted. No additional allocation rules are applied.
#' @examples
#' detect_aadhaar(c("No identifier here", NA_character_))
#' @export
detect_aadhaar <- function(x) {
  if (!is.character(x) || !is.null(dim(x))) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  empty <- data.frame(
    input_index = integer(), candidate = character(), normalized = character(),
    start = integer(), end = integer(), valid = logical(), status = character(),
    stringsAsFactors = FALSE
  )
  if (!length(x)) return(empty)

  pattern <- "(?<![0-9])(?:[0-9]{12}|[0-9]{4}[ \\t]+[0-9]{4}[ \\t]+[0-9]{4})(?![0-9])"
  rows <- lapply(seq_along(x), function(i) {
    positions <- if (is.na(x[i])) -1L else gregexpr(pattern, x[i], perl = TRUE)[[1L]]
    if (positions[1L] == -1L) {
      return(data.frame(
        input_index = i, candidate = NA_character_, normalized = NA_character_,
        start = NA_integer_, end = NA_integer_, valid = NA,
        status = if (is.na(x[i])) "missing" else "no_candidate",
        stringsAsFactors = FALSE
      ))
    }
    starts <- as.integer(positions)
    ends <- starts + attr(positions, "match.length") - 1L
    candidates <- substring(x[i], starts, ends)
    normalized <- gsub("[ \\t]", "", candidates, perl = TRUE)
    valid <- vapply(normalized, .verhoeff_valid, logical(1), USE.NAMES = FALSE)
    data.frame(
      input_index = rep.int(i, length(starts)), candidate = candidates,
      normalized = normalized, start = starts, end = ends, valid = valid,
      status = ifelse(valid, "valid", "invalid_checksum"),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Verhoeff's dihedral multiplication and position permutation tables.
.verhoeff_valid <- function(x) {
  d <- rbind(
    c(0,1,2,3,4,5,6,7,8,9), c(1,2,3,4,0,6,7,8,9,5),
    c(2,3,4,0,1,7,8,9,5,6), c(3,4,0,1,2,8,9,5,6,7),
    c(4,0,1,2,3,9,5,6,7,8), c(5,9,8,7,6,0,4,3,2,1),
    c(6,5,9,8,7,1,0,4,3,2), c(7,6,5,9,8,2,1,0,4,3),
    c(8,7,6,5,9,3,2,1,0,4), c(9,8,7,6,5,4,3,2,1,0)
  )
  p <- rbind(
    c(0,1,2,3,4,5,6,7,8,9), c(1,5,7,6,2,8,3,0,9,4),
    c(5,8,0,3,7,9,6,1,4,2), c(8,9,1,6,0,4,3,5,2,7),
    c(9,4,5,3,1,2,6,8,7,0), c(4,2,8,6,5,7,3,9,0,1),
    c(2,7,9,3,8,0,6,4,1,5), c(7,0,4,6,9,1,3,2,5,8)
  )
  digits <- rev(utf8ToInt(x) - utf8ToInt("0"))
  checksum <- 0L
  for (i in seq_along(digits)) {
    checksum <- d[checksum + 1L, p[(i - 1L) %% 8L + 1L, digits[i] + 1L] + 1L]
  }
  checksum == 0L
}
