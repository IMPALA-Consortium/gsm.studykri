# Function Renaming Summary

## Overview

All functions renamed to follow GSM PascalCase_With_Underscores convention where each word starts with a capital letter and words are separated by underscores.

## Complete Renaming List

### Data Generation Functions
| Old Name | New Name |
|----------|----------|
| `generate_study_sample` | `Generate_Study_Sample` |
| `generate_study_portfolio` | `Generate_Study_Portfolio` |
| `apply_sampling_bias` | `Apply_Sampling_Bias` |
| `filter_study_domains` | `Filter_Study_Domains` |

### Calculation Functions
| Old Name | New Name |
|----------|----------|
| `calculate_study_kri` | `Calculate_Study_KRI` |
| `calculate_site_kri` | `Calculate_Site_KRI` |
| `calculate_portfolio_kri` | `Calculate_Portfolio_KRI` |
| `calculate_ae_rate` | `Calculate_AE_Rate` |
| `bootstrap_kri_ci` | `Bootstrap_KRI_CI` |

### Comparison Functions
| Old Name | New Name |
|----------|----------|
| `compare_study_kri` | `Compare_Study_KRI` |
| `compare_study_against_portfolio` | `Compare_Study_Against_Portfolio` |

### Plotting Functions
| Old Name | New Name |
|----------|----------|
| `plot_study_kri` | `Plot_Study_KRI` |
| `plot_kri_comparison` | `Plot_KRI_Comparison` |
| `plot_portfolio_comparison` | `Plot_Portfolio_Comparison` |

## Files Modified

1. `/R/generate_study_sample.R`
   - Renamed: `generate_study_sample` → `Generate_Study_Sample`
   - Updated calls to: `Apply_Sampling_Bias()`, `Filter_Study_Domains()`

2. `/R/generate_study_portfolio.R`
   - Renamed: `generate_study_portfolio` → `Generate_Study_Portfolio`
   - Updated call to: `Generate_Study_Sample()`

3. `/R/apply_sampling_bias.R`
   - Renamed: `apply_sampling_bias` → `Apply_Sampling_Bias`
   - Updated documentation references

4. `/R/filter_study_domains.R`
   - Renamed: `filter_study_domains` → `Filter_Study_Domains`

5. `/R/calculate_study_kri.R`
   - Renamed: `calculate_study_kri` → `Calculate_Study_KRI`
   - Renamed: `bootstrap_kri_ci` → `Bootstrap_KRI_CI`
   - Renamed: `compare_study_kri` → `Compare_Study_KRI`
   - Updated internal call to: `Bootstrap_KRI_CI()`

6. `/R/calculate_site_kri.R`
   - Renamed: `calculate_site_kri` → `Calculate_Site_KRI`
   - Renamed: `calculate_ae_rate` → `Calculate_AE_Rate`
   - Updated internal call to: `Calculate_AE_Rate()`

7. `/R/plot_study_kri.R`
   - Renamed: `plot_study_kri` → `Plot_Study_KRI`
   - Renamed: `plot_kri_comparison` → `Plot_KRI_Comparison`
   - Renamed: `plot_portfolio_comparison` → `Plot_Portfolio_Comparison`

8. `/R/calculate_portfolio_kri.R`
   - Renamed: `calculate_portfolio_kri` → `Calculate_Portfolio_KRI`
   - Renamed: `compare_study_against_portfolio` → `Compare_Study_Against_Portfolio`
   - Updated calls to: `Calculate_Study_KRI()`, `Calculate_Portfolio_KRI()`

## Documentation Updates

All Roxygen2 documentation updated:
- Function definitions
- `@examples` sections
- Cross-references in `@param` and `@description`
- Internal function call references

## Naming Convention Applied

**Pattern**: Each word starts with capital letter, separated by underscores

Examples:
- Single word: `Transform()`
- Two words: `Transform_Rate()`
- Three words: `Calculate_Study_KRI()`
- Four+ words: `Compare_Study_Against_Portfolio()`

**Acronym Handling**: 
- KRI remains uppercase: `Calculate_Study_KRI()` 
- AE remains uppercase: `Calculate_AE_Rate()`

## Breaking Changes

⚠️ **This is a breaking change** - all existing code calling these functions must be updated.

### Migration Example

```r
# Old code (will break)
study <- generate_study_sample(
  lData = data,
  strStudyID = "TEST001"
)

site_kri <- calculate_site_kri(
  lData = study$lData,
  strKRI = "ae_rate"
)

# New code (correct)
study <- Generate_Study_Sample(
  lData = data,
  strStudyID = "TEST001"
)

site_kri <- Calculate_Site_KRI(
  lData = study$lData,
  strKRI = "ae_rate"
)
```

## Testing Requirements

All existing tests must be updated to use new function names:
- Update all function calls in test files
- Test file names can remain as `test-functionname.R` (snake_case)
- Update any string-based function name references

## Vignette Updates Required

All vignettes and examples need updating:
- Code chunks with function calls
- Inline code references  
- Example workflows

## Benefits

1. **Consistency**: Aligns with gsm.core framework conventions
2. **Readability**: Clear word boundaries with capitals
3. **Recognition**: Immediately identifies package functions
4. **Standards**: Follows established GSM package patterns

---

**Completed**: 2025-10-16  
**Compliance**: GSM Coding Style Guide v1.0

