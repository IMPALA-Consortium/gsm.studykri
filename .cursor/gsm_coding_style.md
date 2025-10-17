# GSM Coding Style Guide

Based on analysis of the [gsm.core](https://github.com/Gilead-BioStats/gsm.core) package framework.

## 1. Function Naming Conventions

### ALL Package Functions Use PascalCase_With_Underscores

**ALL functions in GSM packages use PascalCase with underscores between words:**

- **Pipeline Functions**: `Input_Rate()`, `Transform_Rate()`, `Analyze_Rate()`
- **Workflow Functions**: `MakeWorkflowList()`, `RunWorkflows()`, `CombineSpecs()`
- **Data Functions**: `Ingest()`, `Transform()`, `Analyze()`, `Flag()`, `Summarize()`
- **Study Functions**: `Generate_Study_Sample()`, `Generate_Study_Portfolio()`
- **Calculation Functions**: `Calculate_Study_KRI()`, `Calculate_Site_KRI()`, `Calculate_Portfolio_KRI()`
- **Comparison Functions**: `Compare_Study_KRI()`, `Compare_Study_Against_Portfolio()`
- **Plotting Functions**: `Plot_Study_KRI()`, `Plot_KRI_Comparison()`, `Plot_Portfolio_Comparison()`
- **Helper Functions**: `Apply_Sampling_Bias()`, `Filter_Study_Domains()`, `Bootstrap_KRI_CI()`

### Naming Pattern Rules

1. **Verb_Noun** pattern: `Transform_Rate()`, `Calculate_Study_KRI()`, `Generate_Study_Sample()`
2. **Verb_Noun_Detail** pattern: `Compare_Study_Against_Portfolio()`, `Apply_Sampling_Bias()`
3. **Action_Target** pattern: `Plot_Study_KRI()`, `Filter_Study_Domains()`
4. Each word starts with a capital letter
5. Words are separated by underscores
6. Acronyms like KRI are kept in uppercase: `Calculate_Study_KRI()` not `Calculate_Study_Kri()`

### Examples from gsm.studykri

```r
# Correct - PascalCase with underscores
Generate_Study_Sample()
Calculate_Study_KRI()
Plot_KRI_Comparison()
Apply_Sampling_Bias()
Filter_Study_Domains()
Bootstrap_KRI_CI()

# Incorrect - snake_case
generate_study_sample()
calculate_study_kri()
plot_kri_comparison()
```

## 2. Parameter Naming Conventions

### Prefixes by Data Type

GSM uses Hungarian notation prefixes to indicate parameter types:

- **`l`** - Lists: `lData`, `lWorkflows`, `lPortfolio`, `lIngest`, `lMapped`
- **`df`** - Data frames: `dfSiteKRI`, `dfStudyKRI`, `dfComparison`, `dfPatientMap`
- **`str`** - Strings/Characters: `strStudyID`, `strKRI`, `strBias`, `strMethod`, `strTitle`, `strPath`, `strPackage`
- **`n`** - Integers/Counts: `nPatients`, `nSites`, `nBootstrap`, `nMaxStudies`
- **`d`** - Numeric/Decimals: `dBiasStrength`, `dConfLevel`, `dThreshold`
- **`b`** - Booleans: `bShowCI`, `bShowFlags`
- **`v`** - Vectors: `vPatients`, `vSampledPatients`, `vBiasedPatients`

### Standard Parameter Names

Common parameters across functions:

- `lData` - Input list of data domains
- `strStudyID` - Study identifier
- `seed` - Random seed for reproducibility
- `...` - Additional arguments passed to methods

## 3. Column Naming Conventions

### Standard Output Columns

Use **UPPERCASE** for standard data model columns:

**Study/Site Identifiers:**
- `STUDY`, `SITE_NUMBER`, `SITEID`, `SUBJID`

**Temporal:**
- `YYYYMM` - Year-month format for monthly aggregations

**Metrics:**
- `NAME` - KRI/metric name
- `VALUE` - Primary metric value
- `VALUE_CUM` - Cumulative numerator
- `N_RECORDS_CUM` - Cumulative denominator
- `RATIO_CUM` - Cumulative ratio

**Confidence Intervals:**
- `LOWER_CI`, `UPPER_CI` - Confidence interval bounds

**Comparison Metrics:**
- `PORTFOLIO_LOWER`, `PORTFOLIO_UPPER`, `PORTFOLIO_MEDIAN`
- `ONGOING_VALUE`, `ONGOING_LOWER`, `ONGOING_UPPER`

**Flags:**
- `FLAG` - Boolean flag for out-of-range values
- `DEVIATION` - Categorical deviation status

### Internal/Temporary Columns

Use **lowercase** for internal processing columns:

- `original_id`, `new_id`, `site_number`
- `ae_count`, `pd_count`, `visit_count`

## 4. File and Directory Structure

### R Package Structure

```
package/
├── R/                          # Function definitions
│   ├── calculate_*.R          # Calculation functions
│   ├── plot_*.R              # Plotting functions
│   ├── generate_*.R          # Data generation functions
│   └── utils-*.R             # Utility functions
├── inst/
│   └── workflows/
│       ├── 01_mappings/      # Data mapping YAML files
│       ├── 02_metrics/       # KRI calculation YAML files
│       ├── 03_reporting/     # Reporting YAML files
│       └── 04_modules/       # Module YAML files
├── man/                       # Roxygen documentation
├── tests/
│   └── testthat/             # Unit tests
├── vignettes/                 # Long-form documentation
└── data-raw/                  # Data generation scripts
```

### File Naming

- **R scripts**: `action_target.R` (e.g., `calculate_study_kri.R`, `plot_kri_comparison.R`)
- **Test files**: `test-functionality.R` (e.g., `test-calculate_study_kri.R`)
- **YAML configs**: `DOMAIN.yaml` or `metric_name.yaml` (UPPERCASE for domains)
- **Vignettes**: `descriptive-name.Rmd` (lowercase with hyphens)

## 5. Code Formatting

### Indentation and Spacing

```r
# Use 2 spaces for indentation (not tabs)
function_name <- function(param1, param2) {
  if (condition) {
    result <- calculate_something(
      data = param1,
      method = param2
    )
  }
  
  return(result)
}
```

### Line Length

- Maximum 80-100 characters per line
- Break long function calls across multiple lines
- Align parameters vertically when breaking

```r
# Good
result <- calculate_study_kri(
  dfSiteKRI = site_kri,
  nBootstrap = 1000,
  dConfLevel = 0.95,
  seed = 123
)

# Avoid
result <- calculate_study_kri(dfSiteKRI = site_kri, nBootstrap = 1000, dConfLevel = 0.95, seed = 123)
```

### Spacing Rules

```r
# Spaces around operators
x <- a + b

# Space after commas
c(1, 2, 3, 4)

# No space around namespace operators
dplyr::filter()
package::function()

# No space inside parentheses
mean(x)  # not mean( x )
```

### Pipe Operator

```r
# Use %>% from dplyr
# Each step on new line, indented
result <- data %>%
  dplyr::filter(condition) %>%
  dplyr::mutate(new_col = value) %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(metric = mean(value))
```

## 6. Documentation Standards

### Roxygen2 Function Documentation

Every exported function must include:

```r
#' Short Title (One Line)
#'
#' @description
#' Longer description explaining what the function does,
#' its purpose, and key behavior. Can span multiple lines.
#'
#' @param lData List of clinical trial data domains
#' @param strStudyID Character string for study identifier
#' @param nBootstrap Integer number of bootstrap iterations (default: 1000)
#' @param seed Integer random seed for reproducibility (default: NULL)
#'
#' @return Data frame with columns:
#'   \item{STUDY}{Study identifier}
#'   \item{VALUE}{Metric value}
#'   \item{LOWER_CI}{Lower confidence bound}
#'
#' @details
#' Additional details about methodology, algorithms, or
#' important considerations.
#'
#' @examples
#' \dontrun{
#' result <- calculate_study_kri(
#'   dfSiteKRI = site_kri,
#'   nBootstrap = 1000,
#'   seed = 123
#' )
#' }
#'
#' @export
```

### Documentation Sections

Required sections:
- **Title**: One-line description (first line)
- **@description**: Detailed explanation
- **@param**: All parameters with types and defaults
- **@return**: Return value structure
- **@examples**: Working examples (use `\dontrun{}` for examples requiring external data)
- **@export**: For user-facing functions

Optional sections:
- **@details**: Extended methodology or algorithm details
- **@keywords internal**: For internal functions
- **@importFrom**: For imported functions

### Internal Functions

```r
#' Internal Helper Function
#'
#' @description
#' Brief description of internal function purpose.
#'
#' @param x Input parameter
#'
#' @return Return value
#'
#' @keywords internal
internal_helper <- function(x) {
  # Implementation
}
```

## 7. Error Handling and Validation

### Input Validation

Always validate inputs at function entry:

```r
function_name <- function(dfData, strID, nIterations = 100) {
  # Validate required parameters
  if (!is.data.frame(dfData)) {
    stop("dfData must be a data frame")
  }
  
  if (!is.character(strID) || length(strID) != 1) {
    stop("strID must be a single character string")
  }
  
  # Validate column requirements
  required_cols <- c("STUDY", "SITE_NUMBER", "VALUE")
  if (!all(required_cols %in% names(dfData))) {
    stop(sprintf(
      "dfData must contain columns: %s",
      paste(required_cols, collapse = ", ")
    ))
  }
  
  # Validate numeric ranges
  if (nIterations < 1) {
    stop("nIterations must be positive")
  }
  
  # Proceed with function logic
}
```

### Error Messages

```r
# Good - specific and informative
stop("dfSiteKRI must contain columns: STUDY, YYYYMM, VALUE")

# Good - use sprintf for dynamic messages
stop(sprintf(
  "Expected %d sites but found %d",
  expected_n, actual_n
))

# Avoid - too vague
stop("Invalid input")
```

### Warnings

```r
# Use warnings for non-fatal issues
if (length(vBiasedPatients) == 0) {
  warning("No biased patients identified, using all patients")
}

# Use messages for informational output
message(sprintf(
  "Plotting first %d of %d studies",
  nMaxStudies, total_studies
))
```

## 8. Testing Standards

### Test File Organization

```r
# tests/testthat/test-calculate_study_kri.R

test_that("calculate_study_kri returns correct structure", {
  # Setup
  dfSiteKRI <- create_test_data()
  
  # Execute
  result <- calculate_study_kri(dfSiteKRI, nBootstrap = 100)
  
  # Assert
  expect_s3_class(result, "data.frame")
  expect_true(all(c("STUDY", "VALUE", "LOWER_CI") %in% names(result)))
})

test_that("calculate_study_kri handles edge cases", {
  # Test with single site
  dfSingleSite <- data.frame(
    STUDY = "TEST",
    SITE_NUMBER = "Site_001",
    VALUE_CUM = 10,
    N_RECORDS_CUM = 100
  )
  
  result <- calculate_study_kri(dfSingleSite)
  expect_equal(result$LOWER_CI, result$UPPER_CI)
})

test_that("calculate_study_kri validates inputs", {
  expect_error(
    calculate_study_kri(list()),
    "dfSiteKRI must contain columns"
  )
})
```

### Test Coverage

- **Happy path**: Normal expected usage
- **Edge cases**: Empty data, single row, missing values
- **Error cases**: Invalid inputs, type mismatches
- **Output validation**: Correct structure, expected values

## 9. Class and Object Conventions

### S3 Classes

Define custom classes for structured outputs:

```r
# Constructor function returns structured object
generate_study_sample <- function(...) {
  result <- list(
    lData = filtered_data,
    dfPatientMap = patient_map,
    metadata = metadata_list
  )
  
  structure(result, class = "StudySample")
}

# Print method for custom class
#' @export
print.StudySample <- function(x, ...) {
  cat("Synthetic Study Sample\n")
  cat("======================\n")
  cat(sprintf("Study ID: %s\n", x$metadata$study_id))
  cat(sprintf("Patients: %d\n", x$metadata$n_patients))
}
```

### Class Names

- Use **PascalCase** for class names: `StudySample`, `StudyKRI`, `StudyPortfolio`
- Match constructor function pattern: `generate_study_sample()` returns `StudySample` class

## 10. Workflow and Configuration

### YAML Configuration Files

```yaml
# Standard workflow YAML structure
meta:
  Name: kri_name
  Type: rate
  
steps:
  - name: InputData
    output: dfInput
    params:
      strGroup: Site
      
  - name: Transform
    output: dfTransformed
    
  - name: Analyze
    output: dfAnalyzed
```

### YAML Naming

- **Keys**: PascalCase for workflow keys (`Name`, `Type`, `Group`)
- **Values**: Match function parameter names
- **File names**: UPPERCASE for domain mappings (e.g., `AE.yaml`, `DM.yaml`)

## 11. Dependencies and Imports

### Package Dependencies

```r
# DESCRIPTION file imports
Imports:
    dplyr (>= 1.0.0),
    ggplot2 (>= 3.3.0),
    tidyr (>= 1.0.0),
    stats,
    utils

# Always use explicit namespacing in package code
result <- dplyr::filter(data, condition)
plot <- ggplot2::ggplot(data, ggplot2::aes(x, y))

# Exception: pipe operator can be re-exported
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`
```

### Global Variables

Declare all non-standard evaluation variables:

```r
# R/utils-globals.R
utils::globalVariables(c(
  # Column names used in dplyr/tidyr
  "STUDY", "SITE_NUMBER", "VALUE",
  "LOWER_CI", "UPPER_CI",
  
  # Temporary variables
  "temp_var", "calculated_value"
))
```

## 12. Version Control and Releases

### Commit Messages

Follow conventional commits:

```
feat: add bootstrap confidence interval calculation
fix: correct handling of single-site studies
docs: update calculate_study_kri examples
test: add edge case tests for empty data
refactor: simplify patient sampling logic
```

### Semantic Versioning

- **MAJOR**: Breaking changes to API
- **MINOR**: New features, backward compatible
- **PATCH**: Bug fixes

### Code Review Requirements

Before merging:
- All tests pass
- Code coverage maintained
- Documentation updated
- Styler formatting applied
- No linter errors

## 13. Code Quality Tools

### Styler

Format code before commits:

```r
# Format entire package
styler::style_pkg()

# Format specific file
styler::style_file("R/calculate_study_kri.R")
```

### Lintr

Check code style:

```r
# Lint package
lintr::lint_package()

# Lint specific file
lintr::lint("R/calculate_study_kri.R")
```

## 14. Vignette Standards

### Vignette Structure

```rmd
---
title: "Title of Vignette"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Title of Vignette}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

# Introduction

Brief overview of topic.

# Setup

```{r setup}
library(gsm.studykri)
```

# Examples

Practical, executable examples.

# Conclusion

Summary and next steps.
```

### Vignette Guidelines

- Keep examples **short and precise**
- **Never** set `eval = FALSE` on chunks
- **Never** set `error = TRUE`
- **Never** hallucinate code output
- Use real data or package-included sample data
- Focus on practical usage, not theory

## 15. Summary: Key Style Points

### Critical Conventions

1. **Function names**: PascalCase for pipeline/workflow, snake_case for utilities
2. **Parameters**: Hungarian notation (`l`, `df`, `str`, `n`, `d`, `b`, `v`)
3. **Columns**: UPPERCASE for data model, lowercase for internal
4. **Documentation**: Complete Roxygen2 for all exported functions
5. **Validation**: Always validate inputs at function entry
6. **Testing**: Comprehensive tests for happy path, edge cases, and errors
7. **Formatting**: 2-space indentation, 80-100 char lines, consistent spacing
8. **Dependencies**: Explicit namespacing (except re-exported pipe)
9. **Workflows**: YAML-driven configuration for data pipelines
10. **Quality**: Use styler and lintr before commits

### Package Philosophy

- **GCP-compliant**: Suitable for regulatory environments
- **Workflow-driven**: YAML configurations for flexibility
- **Well-documented**: Roxygen2, vignettes, and examples
- **Well-tested**: Unit tests and workflow tests
- **Reproducible**: Seed parameters and version locking
- **Modular**: Separate packages for core, mapping, KRI, reporting

---

**References:**
- [gsm.core GitHub Repository](https://github.com/Gilead-BioStats/gsm.core)
- [gsm.core Documentation](https://gilead-biostats.github.io/gsm.core)

