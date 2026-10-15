## Test environments

* Local: Windows 11 x64, R 4.6.1
* GitHub Actions (r-lib/actions check-standard): Windows, macOS and Ubuntu
* win-builder: R-devel (2026-10-09 r90655 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes locally and on GitHub Actions.

win-builder (R-devel) reported 1 NOTE:

* This is a new submission.
* The words flagged as possibly misspelled in DESCRIPTION are proper
  nouns and acronyms, not typos: Aadhaar (India's national identity
  number), DPDP (Digital Personal Data Protection Act), GSTIN (Goods and
  Services Tax Identification Number), IFSC (Indian Financial System
  Code) and Verhoeff (the checksum algorithm used by Aadhaar).
* The version number was changed from 0.0.0.9000 to 0.1.0 after that
  check, to address the note about large version components.

## Notes

* All examples, tests and vignettes use synthetic data only.
* The package does not write files outside `tempdir()` unless the user
  supplies an output location.
* Identifier checks are structural (format and checksum) and do not
  prove that an identifier was issued. The DPDP-informed classifications
  and scores are technical conventions, not legal advice.
