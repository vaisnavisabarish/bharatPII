# bharatPII

### Indian PII Detection & DPDP Act 2023 Compliance Auditor

**bharatPII** is an R package designed to detect Indian Personally Identifiable Information (PII) inside datasets and provide a practical privacy-auditing workflow directly within the R/data-science ecosystem.

The package focuses on Indian identifiers such as **Aadhaar, PAN, GSTIN, IFSC, UPI VPA, mobile numbers, and PIN codes**. Instead of relying only on loose regular-expression matching, bharatPII combines pattern detection with structural and checksum-based validation where applicable.

---

## Why bharatPII?

Data used in academic projects, research, analytics, and machine-learning workflows can easily contain personal information.

Existing privacy and data-quality tools are mostly designed around generic identifiers or non-Indian contexts. This creates a gap for Indian datasets containing identifiers such as Aadhaar, PAN, GSTIN, and UPI IDs.

bharatPII aims to provide a simple R-native workflow:

```text
Dataset
   ↓
PII Detection
   ↓
Identifier Validation
   ↓
DPDP Classification
   ↓
Risk Assessment
   ↓
Anonymization
   ↓
Compliance Report
```

The project was specifically designed around the Indian context rather than simply combining existing generic data-quality packages. The CRAN overlap analysis found no existing package reaching the project's 60% conceptual-overlap threshold.

---

## Key Features

### 1. Indian PII Detection

bharatPII can scan dataframe columns for several Indian identifier formats:

* Aadhaar
* PAN
* GSTIN
* IFSC
* UPI VPA
* Indian mobile numbers
* Indian PIN codes

The current package architecture assigns a dedicated detector to each identifier type.

### 2. Validation Beyond Regex

The package attempts to distinguish genuine identifier patterns from random strings.

For example, Aadhaar detection incorporates the **Verhoeff checksum algorithm**, while PAN and GSTIN use structural validation rules.

This reduces the number of false positives compared with simple pattern matching.

### 3. DPDP Classification

Detected fields can be classified into privacy-related categories such as:

* Personal Data
* Sensitive Personal Data
* Quasi-identifiers
* Non-personal data

The classification layer is implemented through `classify_dpdp()`.

> **Note:** The compliance functionality is intended as a technical auditing aid and does not constitute legal certification or legal advice.

### 4. DPDP Readiness Score

`dpdp_score()` produces a simple **0–100 score** based on detected categories.

This gives users a quick way to understand how much potentially sensitive information exists in a dataset and which columns contribute to the score.

### 5. Re-identification Risk

Not all privacy risks come from obvious identifiers.

A combination such as:

```text
PIN Code + Date of Birth + Gender
```

can potentially make an individual easier to identify.

bharatPII uses a **k-anonymity-based approach** through `reid_risk()` to estimate this type of risk.

### 6. Data Anonymization

Flagged columns can be transformed using:

* Masking
* Salted SHA-256 hashing
* Generalization

For example:

```text
Date of Birth → Age Band
PIN Code      → Postal Region
Identifier    → Masked/Hashed Value
```

The `anonymize()` function provides these transformations as part of the workflow.

### 7. Audit Reporting

The package is designed to generate an audit report containing detected PII, classification results, risk information, and compliance-related findings.

The reporting layer is designed around **R Markdown**, allowing the results to be exported into shareable HTML/PDF reports.

---

## Example Workflow

```r
library(bharatPII)

# Scan a dataframe
audit <- pii_scan(employee_df)

# Classify detected information
audit <- classify_dpdp(audit)

# View the audit
print(audit)

# Calculate DPDP readiness score
score <- dpdp_score(audit)

score$score

# Estimate re-identification risk
risk <- reid_risk(
  employee_df,
  quasi_vars = c("pincode", "dob", "gender")
)

risk$min_k

# Anonymize selected columns
clean_df <- anonymize(
  employee_df,
  method = c(
    aadhaar_no = "mask",
    dob = "generalize"
  )
)
```

The overall workflow follows the package design described in the project build guide.

---

## Example Output

A typical audit can identify columns such as:

```text
Column        Detected Type       Category
------------------------------------------------
aadhaar_no    Aadhaar             Sensitive Personal Data
pan_no        PAN                 Personal Data
salary        None                Non-personal
```

The project proposal demonstrates this type of output together with a compliance score and k-anonymity-based risk assessment.

---

## Package Architecture

```text
bharatPII/
│
├── DESCRIPTION
├── NAMESPACE
├── LICENSE
├── README.md
├── README.Rmd
│
├── R/
│   ├── detect_aadhaar.R
│   ├── detect_pan.R
│   ├── detect_gstin.R
│   ├── detect_ifsc.R
│   ├── detect_upi.R
│   ├── detect_mobile.R
│   ├── detect_pincode.R
│   ├── pii_scan.R
│   ├── classify_dpdp.R
│   ├── dpdp_score.R
│   ├── reid_risk.R
│   ├── anonymize.R
│   ├── report.R
│   ├── print.pii_audit.R
│   └── utils.R
│
├── tests/
│   └── testthat/
│
├── vignettes/
│   ├── getting-started.Rmd
│   └── dpdp-compliance-workflow.Rmd
│
└── man/
```

The package structure follows the standard R package development workflow using `usethis`, `roxygen2`, and `testthat`.

---

## Main Functions

| Function           | Purpose                                            |
| ------------------ | -------------------------------------------------- |
| `pii_scan()`       | Scans a dataframe for Indian PII                   |
| `detect_aadhaar()` | Detects and validates Aadhaar patterns             |
| `detect_pan()`     | Detects structurally valid PAN formats             |
| `detect_gstin()`   | Detects GSTIN patterns                             |
| `detect_ifsc()`    | Detects IFSC codes                                 |
| `detect_upi()`     | Detects UPI VPAs                                   |
| `detect_mobile()`  | Detects Indian mobile numbers                      |
| `detect_pincode()` | Detects Indian PIN codes                           |
| `classify_dpdp()`  | Classifies detected information                    |
| `dpdp_score()`     | Calculates a 0–100 privacy-readiness score         |
| `reid_risk()`      | Estimates re-identification risk using k-anonymity |
| `anonymize()`      | Masks, hashes, or generalizes sensitive data       |
| `report()`         | Generates an audit report                          |

---

## Installation

### Development Version

Clone the repository and open the project in RStudio.

Then install the development dependencies:

```r
install.packages(c(
  "devtools",
  "usethis",
  "roxygen2",
  "testthat",
  "digest",
  "stringr",
  "knitr",
  "rmarkdown"
))
```

Load the package during development:

```r
devtools::load_all()
```

The build guide specifies **R ≥ 4.2** and the above development toolchain for building the package.

---

## Testing

The project uses **testthat** for automated unit testing.

Run:

```r
devtools::test()
```

The tests cover:

* Valid identifier detection
* Invalid identifier rejection
* Boundary cases
* PII dataframe scanning
* Classification behaviour
* Anonymization functions

For example:

```r
test_that("PAN structural validation works", {
  expect_true(
    detect_pan("AAAPL1234C")$is_match
  )

  expect_false(
    detect_pan("1234567890")$is_match
  )
})
```

Synthetic data is used for testing rather than real Aadhaar or PAN information.

---

## Privacy & Ethical Considerations

Privacy is an important part of the project itself.

**Real Aadhaar, PAN, or other sensitive identifiers should never be used as test data.**

The project uses synthetically generated identifiers and intentionally invalid examples for testing. Valid synthetic Aadhaar values can be generated using the same checksum logic implemented by the package.

This allows the detection system to be tested without exposing real personal information.

---

## CRAN Readiness

The project is being developed with CRAN-style package standards in mind.

The intended workflow includes:

```r
devtools::document()
devtools::load_all()
devtools::test()
devtools::check()
devtools::check(cran = TRUE)
```

The project targets:

* Complete roxygen2 documentation
* Unit-test coverage
* Working vignettes
* Clean `R CMD check`
* DESCRIPTION and NAMESPACE files
* MIT licensing
* Reproducible package structure

The proposed evaluation target is **0 errors, 0 warnings, and 0 notes** before CRAN submission.

---

## Development Roadmap

### Phase 1 — Detection Engine

* [x] Package architecture
* [ ] Aadhaar detector
* [ ] PAN detector
* [ ] GSTIN detector
* [ ] IFSC detector
* [ ] UPI detector
* [ ] Mobile number detector
* [ ] PIN code detector

### Phase 2 — Privacy Analysis

* [ ] DPDP classification
* [ ] DPDP readiness scoring
* [ ] Re-identification risk analysis
* [ ] Risk visualization

### Phase 3 — Anonymization

* [ ] Masking
* [ ] Hashing
* [ ] Generalization
* [ ] Before/after risk comparison

### Phase 4 — Reporting & Documentation

* [ ] Audit report generation
* [ ] Getting Started vignette
* [ ] DPDP Compliance Workflow vignette
* [ ] Complete API documentation

### Phase 5 — CRAN Preparation

* [ ] Full testthat suite
* [ ] `R CMD check`
* [ ] `devtools::check(cran = TRUE)`
* [ ] Documentation cleanup
* [ ] CRAN submission preparation

---

## Project Timeline

The original project plan is structured around an approximately six-week development cycle:

```text
Week 1–2
Design + identifier research + DPDP mapping

Week 3
Core detection + risk/anonymization modules

Week 4
Integration + S3 classes + documentation

Week 5
Testing + synthetic datasets + vignettes

Week 6
R CMD check + polishing + CRAN preparation
```

This division was designed for a two-member team working on detection and compliance components in parallel.

---

## Scope

### In Scope

* Indian PII detection
* Pattern and structural validation
* Aadhaar checksum validation
* DPDP-oriented classification
* Privacy/readiness scoring
* k-anonymity-based risk estimation
* Data masking and anonymization
* Audit reporting
* R package development
* Testing and documentation

### Currently Out of Scope

* Real-time API deployment
* Web/GUI interface
* Generic international PII detection
* Full NLP-based named entity recognition
* Legal certification
* Production-scale enterprise compliance systems

The scope is intentionally limited so that the core package can be completed and evaluated properly within the academic project timeline.

---

## What Makes bharatPII Different?

The main idea behind bharatPII is **not simply detecting PII**.

The package connects multiple stages of the privacy workflow:

```text
Detect
  ↓
Validate
  ↓
Classify
  ↓
Measure Risk
  ↓
Anonymize
  ↓
Report
```

The combination of **Indian identifier validation + DPDP-oriented classification + quantitative re-identification risk + anonymization** is the central novelty of the project.


---

## Academic Project

**Course:** Programming for Data Science
**Institution:** VIT Chennai
**Project:** bharatPII — Indian PII Detection & DPDP Act 2023 Compliance Auditor

---

## Disclaimer

bharatPII is an academic and technical privacy-auditing project.

Its compliance classifications and scores are intended to assist data practitioners in identifying potential privacy issues. They should **not be treated as legal advice, legal certification, or a substitute for professional compliance review**.

---

## Future Scope

Possible future extensions include:

* Additional Indian identifier types
* Improved checksum and validation algorithms
* More sophisticated NLP-based PII detection
* Column-level and row-level privacy visualizations
* Advanced re-identification metrics
* More configurable DPDP rule sets
* Interactive Shiny interface
* Additional report formats
* Expanded CRAN documentation and examples

---

## License

This project is released under the **MIT License**.

---

> **bharatPII — Bringing Indian-context privacy detection and auditing directly into the R data-science workflow.**
