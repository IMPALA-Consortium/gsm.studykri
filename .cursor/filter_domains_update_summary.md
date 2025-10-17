# Filter_Study_Domains Update Summary

## Overview

Enhanced `Filter_Study_Domains()` to gracefully handle missing subject ID columns and support multiple subject ID column name variations from the clindata package.

## Changes Made

### 1. Filter_Study_Domains (R/filter_study_domains.R)

**New Parameter:**
```r
vSubjIdCol = c(
  "subjid",           # Most common (SDTM standard)
  "subjectid",        # Alternative format
  "subjid_nsv",       # Non-standard version
  "subject_nsv",      # Subject non-standard version
  "subjinit_nsv",     # Subject initials non-standard version
  "subjectname",      # EDC systems
  "subjectenrollmentnumber"  # CTMS systems
)
```

**New Behavior:**

**Before:**
- Skipped domains without `subjid` column (hard-coded)
- Lost data silently

**After:**
- Searches for matching column from `vSubjIdCol` vector
- If no match found: returns domain **unfiltered** with **warning**
- If multiple matches: uses first, issues message
- Dynamically uses the found column name in join operations

**Key Implementation:**
```r
# Find matching subject ID column
vMatchingCols <- intersect(vSubjIdCol, names(dfDomain))

if (length(vMatchingCols) == 0) {
  warning(sprintf("Domain '%s' has no matching subject ID column..."))
  lFilteredData[[domain_name]] <- dfDomain  # Return unfiltered
  next
}

# Use first matching column with dynamic column name
strSubjCol <- vMatchingCols[1]
dfFiltered <- dfDomain %>%
  dplyr::inner_join(
    dfPatientMap,
    by = stats::setNames("original_id", strSubjCol),
    relationship = "many-to-many"
  ) %>%
  dplyr::mutate(
    !!strSubjCol := new_id,  # Dynamic column assignment
    siteid = site_number
  )
```

### 2. Generate_Study_Sample (R/generate_study_sample.R)

**New Parameter:**
- Added `vSubjIdCol` parameter with same default values
- Passes through to `Filter_Study_Domains()`
- User can now customize subject ID column search

**Example Usage:**
```r
# Use default subject ID columns
study <- Generate_Study_Sample(
  lData = lData,
  strStudyID = "STUDY001"
)

# Specify custom subject ID columns
study <- Generate_Study_Sample(
  lData = lData,
  strStudyID = "STUDY002",
  vSubjIdCol = c("patientid", "subject_id", "subjid")
)
```

### 3. Updated Documentation

**Filter_Study_Domains:**
- Added `@param vSubjIdCol` documentation
- Added `@details` section explaining column search order
- Added example showing custom column usage
- Updated description to mention unfiltered return behavior

**Generate_Study_Sample:**
- Added `@param vSubjIdCol` documentation
- Added example showing custom column specification

### 4. Global Variables (R/utils-globals.R)

Added all subject ID column variations to prevent R CMD check notes:
```r
"subjectid", "subjid_nsv", "subject_nsv", "subjinit_nsv",
"subjectname", "subjectenrollmentnumber"
```

## Subject ID Columns from clindata

Identified from all clindata package datasets:

| Domain | Subject ID Columns |
|--------|-------------------|
| ctms_protdev | subjectenrollmentnumber |
| ctms_study | num_plan_subj, num_enrolled_subj_m |
| edc_data_pages | subjectname |
| edc_data_points | subjectname |
| edc_queries | subjectname |
| rawplus_ae | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |
| rawplus_consent | subjid |
| rawplus_dm | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |
| rawplus_enroll | subjectid, subjid |
| rawplus_ex | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |
| rawplus_ie | subjid |
| rawplus_ixrsrand | subjid, subjinit_nsv |
| rawplus_lb | subjid |
| rawplus_sdrgcomp | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |
| rawplus_studcomp | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |
| rawplus_visdt | subjid, subjectid, subjid_nsv, subjinit_nsv, subject_nsv |

## Benefits

1. **Robustness**: Handles diverse data sources with different column naming conventions
2. **Transparency**: Warns when domains can't be filtered (instead of silent skipping)
3. **Flexibility**: Users can specify custom subject ID columns if needed
4. **Data Preservation**: Unmatched domains returned unfiltered rather than lost
5. **Informative**: Messages when multiple matches found, indicating which column was used

## Technical Details

### Dynamic Column Assignment

Uses rlang's bang-bang (`!!`) operator for dynamic column assignment:
```r
dplyr::mutate(!!strSubjCol := new_id)
```

This allows the function to work with any subject ID column name, not just hard-coded "subjid".

### Named Vector for Join

Uses `stats::setNames()` to create dynamic join specification:
```r
by = stats::setNames("original_id", strSubjCol)
# Equivalent to: by = c("subjid" = "original_id") 
# but column name is dynamic
```

## Backward Compatibility

✅ Fully backward compatible
- Default `vSubjIdCol` includes "subjid" as first element
- Existing code without the parameter will work unchanged
- Only behavior change: unmatched domains returned (instead of skipped)

## Error Handling

- **Warning**: Issued when no subject ID column matched
- **Message**: Issued when multiple subject ID columns found
- **Error**: Validation on `vSubjIdCol` (must be non-empty character vector)

---

**Updated**: 2025-10-16  
**Affects**: `Filter_Study_Domains()`, `Generate_Study_Sample()`  
**Backward Compatible**: Yes

