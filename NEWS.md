# bharatpii 0.1.0

* Initial development version.
* Added detectors: `detect_aadhaar()`, `detect_pan()`, `detect_gstin()`,
  `detect_ifsc()`, `detect_upi()`, `detect_mobile()`, `detect_pin()` and
  `detect_voter()`.
* Added `pii_scan()`, `classify_dpdp()` and methods for `pii_audit` objects.
* Added `reid_risk()` (k-anonymity), `dpdp_score()` (technical readiness
  score) and `anonymize()` (mask, salted hash, generalise).
* Added `synthetic_pii()` for synthetic test and example data.
* Added `report()` for HTML technical audit reports.
* Added the vignettes 'Getting Started' and 'DPDP Compliance Workflow'.
