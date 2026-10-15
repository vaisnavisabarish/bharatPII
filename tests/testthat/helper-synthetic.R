# Synthetic identifier builders: run the checksums in reverse so that no real
# identifiers are ever used.
make_aadhaar <- function(base11) {
  paste0(base11, verhoeff_check_digit(base11))
}

make_gstin <- function(first14) {
  paste0(first14, gstin_check_char(first14))
}
