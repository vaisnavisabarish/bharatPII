# Internal lookup tables. These are non-exhaustive snapshots used only to
# raise or lower detector confidence. Verify against the RBI and NPCI
# published lists before relying on them for legal purposes.

# First four characters of IFSC codes (bank identifiers)
ifsc_bank_codes <- c(
  "SBIN", "HDFC", "ICIC", "UTIB", "PUNB", "BARB", "CNRB", "UBIN", "BKID",
  "IDIB", "CBIN", "IOBA", "MAHB", "UCBA", "PSIB", "INDB", "KKBK", "YESB",
  "FDRL", "SIBL", "KARB", "KVBL", "RATN", "IDFB", "AUBL", "BDBL", "CIUB",
  "DLXB", "JAKA", "TMBL", "DCBL", "SCBL", "HSBC", "CITI", "DBSS", "ESFB",
  "UJVN", "SRCB", "ABHY", "KCCB", "IBKL", "LAVB", "DEUT", "BARC", "CSBK",
  "EQUI", "UTKS", "SURY", "IPOS", "PYTM", "AIRP", "FINO", "JIOP", "ANDB",
  "ALLA", "CORP", "SYNB", "VIJB", "ORBC", "UTBI"
)

# UPI payment service provider handles (the part after the @)
upi_handles <- c(
  "okaxis", "oksbi", "okhdfcbank", "okicici", "ybl", "ibl", "axl", "paytm",
  "ptyes", "ptsbi", "pthdfc", "ptaxis", "upi", "apl", "sbi", "hdfcbank",
  "icici", "axisbank", "pnb", "boi", "cnrb", "unionbank", "idfcbank",
  "indus", "kotak", "yesbank", "fbl", "federal", "aubank", "rbl",
  "freecharge", "fam", "jupiter", "ikwik", "postbank", "airtel", "jio",
  "waicici", "wahdfcbank", "waaxis", "wasbi", "abfspay", "barodampay",
  "cboi", "csbpay", "dbs", "equitas", "hsbc", "idbi", "iob", "jkb", "kvb",
  "lvb", "pingpay", "sliceaxis", "tmb", "uboi", "ubi", "yapl", "okbizaxis"
)

# Two-digit PIN prefixes that correspond to Indian postal circles
pin_prefixes <- sprintf("%02d", c(11:28, 30:34, 36:53, 56:85, 90:99))
