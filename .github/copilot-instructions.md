# GitHub Copilot Instructions for gsm.studykri

## Project Overview
You are working on `gsm.studykri`, an R package for analyzing study-level Key Risk Indicators (KRIs) in clinical trials. The package compares KRIs from ongoing studies against reference studies using bootstrap confidence intervals and the GSM (Good Statistical Monitoring) framework.

## Core Architecture & Data Flow

```r
# Standard Pipeline Flow
lRaw → SimulatePortfolio() → lPortfolio
lPortfolio → Ingest() → lIngest
lIngest → RunWorkflows(mapping_wf) → lMapped
lMapped → RunWorkflows(metrics_wf) → lAnalyzed
lAnalyzed → RunWorkflows(reporting_wf) → lReport
lReport → RunWorkflows(module_wf) → lModule
```

## Key Functions & Workflow
- `Input_CumCountSitebyMonth()` - Aggregate site-level counts by month
- `Transform_CumCount()` - Transform to study-level cumulative KRIs  
- `Analyze_StudyKRI()` - Bootstrap confidence intervals for individual studies
- `Analyze_StudyKRI_PredictBounds()` - Calculate CI bounds for study
- `Analyze_StudyKRI_PredictBoundsGroup()` - Calculate CI bounds for reference group
- `Visualize_StudyKRI()` - Plot study vs reference group comparison

## Coding Standards

### Function Naming - CRITICAL
**ALL functions use PascalCase_With_Underscores:**
```r
# Correct
Input_CountSiteByMonth()
Transform_CumCount()
Analyze_StudyKRI()
Visualize_StudyKRI()

# NEVER use snake_case
input_count_site_by_month()  # WRONG
```

### Parameter Naming - Hungarian Notation
- `l` - Lists: `lData`, `lWorkflows`, `lPortfolio`
- `df` - Data frames: `dfSiteKRI`, `dfStudyKRI`, `dfComparison`
- `str` - Strings: `strStudyID`, `strKRI`, `strMethod`
- `n` - Integers: `nPatients`, `nSites`, `nBootstrap`
- `d` - Decimals: `dBiasStrength`, `dConfLevel`
- `b` - Booleans: `bShowCI`, `bShowFlags`
- `v` - Vectors: `vPatients`, `vBiasedPatients`

### Column Naming
- **Raw/mapped domains**: lowercase_with_underscores (`studyid`, `site_num`, `aeser`)
- **Derived/aggregate**: PascalCase (`GroupID`, `ParticipantCount`, `Numerator`)

### dplyr NSE Handling
```r
# Use .data and .env for NSE
filter(.data[[filter_col]] == .env$filter_value)
summarise("{sum_col}" := sum(.data$wt))

# Use tidy-select for select/rename (NOT .data/.env)
select(any_of(c("am", "wt")))
```

## Critical Rules

### Security & Privacy
- **NEVER include study, site, or patient identifiers** in responses
- Sanitize any accidentally provided identifiers

### Code Quality
- **Always set `eval = TRUE`** in Rmd/qmd chunks - never `eval = FALSE`
- **Never set `error = TRUE`** in chunks
- **Always process all files** unless specifically instructed otherwise
- Keep functions **≤ 50 lines** unless explicitly requested otherwise
- Use **dbplyr-compatible** functions only for metric calculations

### Testing
- Never edit tests prefixed with "hand written test -"
- Make all roxygen examples **executable** - no `\dontrun{}`

### Documentation
- Keep roxygen descriptions **≤ 2 sentences** per function
- Avoid repeating gsm.core/gsm.kri help topics
- Use concise, declarative sentences

## Domain Mappings
Available clinical data domains:
```
ctms_protdev, ctms_site, ctms_study
edc_data_pages, edc_data_points, edc_queries  
rawplus_ae, rawplus_consent, rawplus_dm, rawplus_enroll
rawplus_ex, rawplus_ie, rawplus_ixrsrand, rawplus_lb
rawplus_sdrgcomp, rawplus_studcomp, rawplus_visdt
```

## KRI Metrics Supported
- Adverse Event Reporting Rate
- Serious Adverse Event Reporting Rate  
- Protocol Deviation Rates (Non-important & Important)
- Grade 3+ Lab Abnormality Rate
- Study/Treatment Discontinuation Rates
- Query Rate & Outstanding Query Rate
- Outstanding Data Entry Rate & Data Change Rate
- Screen Failure Rate

## File Structure
```
R/                     # PascalCase_Function.R
inst/workflow/
  1_mappings/          # DOMAIN.yaml (UPPERCASE)
  2_metrics/           # kri_name.yaml
  3_reporting/         # Report templates
  4_modules/           # Module configs
man/                   # Auto-generated docs
tests/testthat/        # test-functionality.R
```

## Workflow Implementation Steps
1. **Mappings** - YAML configs in `inst/workflow/1_mappings/`
2. **Sample Data** - Use `SimulatePortfolio()` for test data
3. **KRI Functions** - Pipeline functions following GSM pattern
4. **Report Templates** - HTML reports with study comparisons
5. **Integration** - End-to-end workflow testing

## Key Dependencies
- **gsm.core** - Core framework and workflow engine
- **gsm.mapping** - Data ingestion and mapping
- **dplyr/tidyr** - Data manipulation (dbplyr-compatible only)
- **ggplot2** - Visualization
- **usethis** - Package development utilities

## Development Workflow
- Use **COMPACT mode** by default (minimal comments, small composable functions)
- Switch to **EXPLAIN mode** only when explicitly requested
- Break multi-step implementations into **small, verifiable substeps**
- Wait for confirmation after each substep before continuing
- Self-review for redundancy and remove unnecessary code/text
- Use `styler::style_pkg()` and `lintr::lint_package()` before commits

## Bootstrap Methodology
The package uses site-level resampling with replacement to:
1. Calculate study-level KRI confidence intervals
2. Create reference group CIs from multiple studies  
3. Compare individual study performance against reference groups
4. Ensure equal weighting when combining multiple reference studies

Remember: **Non-overlapping confidence intervals indicate statistically significant differences** between study and reference group KRIs.
