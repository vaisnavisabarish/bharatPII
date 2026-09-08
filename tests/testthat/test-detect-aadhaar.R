# Generate synthetic check digits independently, using dihedral arithmetic
# and repeated permutation rather than the implementation's lookup tables.
synthetic_candidate <- function(prefix) {
  permutation <- c(1L, 5L, 7L, 6L, 2L, 8L, 3L, 0L, 9L, 4L)
  digits <- rev(utf8ToInt(prefix) - 48L)
  state <- 0L
  for (i in seq_along(digits)) {
    digit <- digits[i]
    for (j in seq_len(i %% 8L)) digit <- permutation[digit + 1L]
    rotation <- (state %% 5L + if (state < 5L) digit %% 5L else -(digit %% 5L)) %% 5L
    reflection <- xor(state >= 5L, digit >= 5L)
    state <- rotation + 5L * reflection
  }
  inverse <- if (state < 5L) (-state) %% 5L else state
  paste0(prefix, inverse)
}

test_that("synthetic candidates pass and all single digit changes fail", {
  prefixes <- c(paste(rep(2L, 11L), collapse = ""),
                paste(rep(7L, 11L), collapse = ""),
                paste(2:9, collapse = ""))
  prefixes[3] <- paste0(prefixes[3], "246")
  values <- vapply(prefixes, synthetic_candidate, character(1), USE.NAMES = FALSE)
  result <- detect_aadhaar(values)
  expect_true(all(result$valid))
  expect_identical(result$status, rep("valid", 3))
  expect_identical(result$input_index, 1:3)
  for (value in values) {
    changed <- unlist(lapply(seq_len(12), function(i) {
      alternatives <- setdiff(as.character(0:9), substr(value, i, i))
      vapply(alternatives, function(digit) {
        altered <- value
        substr(altered, i, i) <- digit
        altered
      }, character(1))
    }), use.names = FALSE)
    expect_true(all(!detect_aadhaar(changed)$valid))
    expect_true(all(detect_aadhaar(changed)$status == "invalid_checksum"))
  }
})

test_that("formatting, multiple matches and character offsets are preserved", {
  value <- synthetic_candidate(paste(rep(3L, 11L), collapse = ""))
  formatted <- paste(substr(value, 1, 4), substr(value, 5, 8),
                     substr(value, 9, 12), sep = " \t")
  input <- paste0("\u0915: ", formatted, "; ", value, ".")
  result <- detect_aadhaar(c(input, value))
  expect_identical(result$input_index, c(1L, 1L, 2L))
  expect_identical(result$normalized, rep(value, 3))
  expect_identical(result$candidate, c(formatted, value, value))
  expect_identical(result$start, c(4L, nchar(formatted) + 6L, 1L))
  expect_identical(substring(input, result$start[1:2], result$end[1:2]),
                   c(formatted, value))
  expect_true(all(result$valid))
})

test_that("missing, malformed and empty inputs have a stable schema", {
  value <- synthetic_candidate(paste(rep(4L, 11L), collapse = ""))
  inputs <- c(NA_character_, "", "nothing", substr(value, 1, 11),
              paste0(value, "0"), paste0("0", value),
              paste(strsplit(value, "")[[1]], collapse = " "),
              paste(substr(value, 1, 4), substr(value, 5, 8),
                    substr(value, 9, 12), sep = "-"))
  result <- detect_aadhaar(inputs)
  expect_identical(result$status, c("missing", rep("no_candidate", 7)))
  expect_true(all(is.na(result$valid)))
  expect_true(all(is.na(result$candidate)))
  expect_true(all(is.na(result$normalized)))
  expect_true(all(is.na(result$start)))
  expect_true(all(is.na(result$end)))
  empty <- detect_aadhaar(character())
  expect_equal(nrow(empty), 0)
  expect_identical(vapply(empty, typeof, character(1)),
                   vapply(result, typeof, character(1)))
  expect_error(detect_aadhaar(123), "character vector")
  expect_error(detect_aadhaar(list(value)), "character vector")
  expect_error(detect_aadhaar(factor(value)), "character vector")
  expect_error(detect_aadhaar(matrix(value)), "character vector")
})
