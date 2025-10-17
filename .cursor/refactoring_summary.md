# Refactoring Summary: generate_study_sample and generate_study_portfolio

## Overview

Refactored both functions to align with the GSM coding style guide based on the gsm.core framework.

## Files Modified

1. `/R/generate_study_sample.R`
2. `/R/generate_study_portfolio.R`

## Key Changes Applied

### 1. Documentation Improvements

**Before:**
- Basic Roxygen2 documentation
- Minimal details section
- Simple examples

**After:**
- Enhanced `@description` with multi-line formatting
- Added comprehensive `@details` section explaining:
  - Different bias types and their purpose
  - How bias strength affects sampling
  - Custom configuration options
- Improved `@return` documentation with explicit S3 class naming
- Added more complete examples including custom configurations
- Added `@description` to print methods
- Added `@return` to print methods (invisible return)

### 2. Input Validation Enhancements

**generate_study_sample:**
```r
# Added validation for:
- nPatients must be positive numeric
- nSites must be positive numeric  
- Demographics must have 'subjid' column
- More specific error messages with sprintf()
- Variable to store valid bias types: vValidBiasTypes
```

**generate_study_portfolio:**
```r
# Added validation for:
- vBiasTypes must be non-empty character vector
- Validate all bias types are valid
- Custom config validation when lStudyConfig provided
- Check all required fields in custom configs
- Better error messages with field names
```

### 3. Code Formatting Consistency

**Spacing:**
- Consistent blank lines between sections
- Removed extra blank lines at function start
- Aligned multi-line function calls vertically

**Named Arguments:**
```r
# Before:
sample(vPatients, nPatients, replace = TRUE)
stats::runif(1, 0.5, 0.9)

# After:
sample(vPatients, size = nPatients, replace = TRUE)
stats::runif(1, min = 0.5, max = 0.9)
```

**Variable Extraction:**
```r
# Before:
sprintf("Site_%03d", 1:nSites)

# After:
vSiteIDs <- sprintf("Site_%03d", seq_len(nSites))
dfPatientMap$site_number <- sample(vSiteIDs, ...)
```

### 4. Improved Comments

**Before:**
```r
# Set seed if provided
# Validate inputs
# Apply bias to patient selection
```

**After:**
```r
# Set seed if provided for reproducibility
# Validate input types
# Validate bias type
# Validate required domain
# Calculate biased vs unbiased sample sizes
# Sample from both pools
```

### 5. Error Message Improvements

**Before:**
```r
stop("strBias must be one of: 'none', 'high_ae', 'high_pd', 'mixed'")
stop("nStudies must be at least 1")
```

**After:**
```r
stop(sprintf(
  "strBias must be one of: %s",
  paste(vValidBiasTypes, collapse = ", ")
))
stop(sprintf(
  "Invalid bias types: %s. Must be one of: %s",
  paste(vInvalidTypes, collapse = ", "),
  paste(vValidBiasTypes, collapse = ", ")
))
```

### 6. Return Statement Refinements

**Before:**
```r
return(structure(
  list(...),
  class = "StudySample"
))
```

**After:**
```r
# Return structured S3 object
structure(
  list(
    lData = lFilteredData,
    dfPatientMap = dfPatientMap,
    metadata = lMetadata
  ),
  class = "StudySample"
)
```

### 7. Print Method Improvements

**Added to both print methods:**
- `@description` section
- `@return` documentation
- `invisible(x)` return for proper R S3 method convention
- Enhanced print output (portfolio shows bias strength)

### 8. Variable Naming Consistency

All variables already followed Hungarian notation correctly:
- `l` prefix for lists (lData, lStudyConfig, lPortfolio)
- `v` prefix for vectors (vPatients, vBiasTypes, vSiteIDs)
- `n` prefix for integers (nPatients, nSites, nStudySeed)
- `df` prefix for data frames (dfPatientMap, dfDM)
- `str` prefix for strings (strStudyID, strBias)
- `d` prefix for decimals (dBiasStrength)

### 9. Function Logic Improvements

**generate_study_portfolio:**
```r
# Before:
names(lPortfolio) <- sapply(lStudyConfig, function(x) x$study_id)

# After:
names(lPortfolio) <- vapply(
  lStudyConfig,
  function(x) x$study_id,
  character(1)
)
```
*Uses `vapply` instead of `sapply` for type safety*

## Style Compliance Checklist

- [x] Hungarian notation for all parameters
- [x] Comprehensive Roxygen2 documentation
- [x] Input validation with informative error messages
- [x] Consistent 2-space indentation
- [x] Line length within 80-100 characters
- [x] Named arguments in function calls
- [x] Descriptive comments
- [x] S3 class structure properly documented
- [x] Print methods return invisible(x)
- [x] No linting errors
- [x] Explicit namespace usage where applicable (stats::)

## Testing Impact

No changes to function behavior or output structure, only:
- Enhanced validation (more specific error messages)
- Improved documentation
- Better code readability

All existing tests should continue to pass without modification.

## Next Steps

Consider applying the same refactoring patterns to:
1. `/R/apply_sampling_bias.R`
2. `/R/filter_study_domains.R`
3. `/R/calculate_study_kri.R`
4. `/R/calculate_site_kri.R`
5. `/R/plot_study_kri.R`
6. `/R/calculate_portfolio_kri.R`

---

**Refactoring Completed:** 2025-10-16
**Style Guide Reference:** `.cursor/gsm_coding_style.md`

