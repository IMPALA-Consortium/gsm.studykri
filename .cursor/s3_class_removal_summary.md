# S3 Class Removal Summary

## Overview

Removed all S3 class structures from the package to align with GSM framework design principles. All functions now return plain lists instead of S3 objects.

## Rationale

The GSM framework design does not use S3 classes for data structures. Functions return simple lists that can be easily manipulated and passed between functions without class-specific methods.

## Changes Made

### 1. Generate_Study_Sample (R/generate_study_sample.R)

**Before:**
```r
structure(
  list(lData = ..., dfPatientMap = ..., metadata = ...),
  class = "StudySample"
)
```

**After:**
```r
list(
  lData = lFilteredData,
  dfPatientMap = dfPatientMap,
  metadata = lMetadata
)
```

**Removed:**
- `print.StudySample()` method (deleted entirely)
- S3 class assignment

**Updated Documentation:**
- Changed `@return A \`StudySample\` S3 object` → `@return A list`

---

### 2. Generate_Study_Portfolio (R/generate_study_portfolio.R)

**Before:**
```r
structure(lPortfolio, class = "StudyPortfolio")
```

**After:**
```r
lPortfolio  # Just return the list
```

**Removed:**
- `print.StudyPortfolio()` method (deleted entirely)
- S3 class assignment

**Updated Documentation:**
- Changed `@return A \`StudyPortfolio\` S3 object` → `@return A named list of study samples`

---

### 3. Compare_Study_Against_Portfolio (R/calculate_portfolio_kri.R)

**Before:**
```r
structure(
  list(...),
  class = "StudyKRIComparison"
)
```

**After:**
```r
list(
  ongoing_kri = dfOngoingKRI,
  ongoing_ci = dfOngoingCI,
  portfolio_ci = dfPortfolioCI,
  comparison = dfComparison,
  metadata = list(...)
)
```

**Removed:**
- `print.StudyKRIComparison()` method (deleted entirely)
- `plot.StudyKRIComparison()` method (deleted entirely)
- S3 class assignment

**Updated Documentation:**
- Changed `@return List with class 'StudyKRIComparison'` → `@return List containing:`
- Removed `plot(result)` from examples

---

## Test File Updates

### test-generate_study_sample.R

**Changed:**
```r
# Before
expect_s3_class(result, "StudySample")

# After
expect_type(result, "list")
```

**Removed:**
- Test for `print.StudySample` output
- Replaced with test for list structure

---

### test-generate_study_portfolio.R

**Changed:**
```r
# Before
expect_s3_class(lPortfolio, "StudyPortfolio")
expect_s3_class(lPortfolio[[i]], "StudySample")

# After
expect_type(lPortfolio, "list")
expect_type(lPortfolio[[i]], "list")
expect_named(lPortfolio[[i]], c("lData", "dfPatientMap", "metadata"))
```

**Removed:**
- Test for `print.StudyPortfolio` output
- Replaced with test for list structure and named elements

---

## Impact Summary

### Functions Affected
1. `Generate_Study_Sample()` - returns plain list
2. `Generate_Study_Portfolio()` - returns plain named list
3. `Compare_Study_Against_Portfolio()` - returns plain list

### Methods Removed
1. `print.StudySample()` - 15 lines removed
2. `print.StudyPortfolio()` - 19 lines removed
3. `print.StudyKRIComparison()` - 23 lines removed
4. `plot.StudyKRIComparison()` - 47 lines removed

**Total: 104 lines of S3 method code removed**

### Benefits

1. **Simplicity**: Functions return simple, predictable list structures
2. **Compatibility**: No special handling needed for class-specific methods
3. **Framework Alignment**: Matches gsm.core design philosophy
4. **Flexibility**: Lists can be easily manipulated and combined
5. **Maintainability**: Less code to maintain, no method dispatch complexity

### Migration Notes

All existing code that called these functions will continue to work because:
- List access patterns remain unchanged (`result$lData`, `result$metadata`, etc.)
- No code was relying on S3 method dispatch
- Structure of returned lists is identical, just without class attribute

### What Users Should Know

**Before (with S3 classes):**
```r
result <- Generate_Study_Sample(...)
print(result)  # Would use print.StudySample method
# Output: Formatted S3 print output
```

**After (plain lists):**
```r
result <- Generate_Study_Sample(...)  
print(result)  # Uses default list print
# Output: Standard R list output
```

Users who want formatted output can access the list elements directly:
```r
result <- Generate_Study_Sample(...)
cat(sprintf("Study: %s\n", result$metadata$study_id))
cat(sprintf("Patients: %d\n", result$metadata$n_patients))
```

---

## Files Modified

1. `/R/generate_study_sample.R` - Removed StudySample class
2. `/R/generate_study_portfolio.R` - Removed StudyPortfolio class
3. `/R/calculate_portfolio_kri.R` - Removed StudyKRIComparison class
4. `/tests/testthat/test-generate_study_sample.R` - Updated tests
5. `/tests/testthat/test-generate_study_portfolio.R` - Updated tests

---

**Completed**: 2025-10-16  
**Design Principle**: GSM framework uses plain lists, not S3 classes

