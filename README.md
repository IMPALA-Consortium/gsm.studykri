
# gsm.studykri

<!-- badges: start -->
<!-- badges: end -->

## Overview

{gsm.studykri} provides a new approach for calculating KRI lower and
upper limits on study-level for quality monitoring in clinical trials.
The method uses bootstrapping to calculate confidence intervals for a
given study over-time. The confidence intervals can then be used to
compare the study-level KRI against a fixed expectation or against the
confidence intervals and KRI values over time of one or more reference
studies. The bootstrapping method resamples a new set of sites with
replacement from the original study data set.

## Installation

``` r
install.packages("pak")
pak::pak("Gilead-BioStats/clindata")
pak::pak("Gilead-BioStats/gsm.core")
pak::pak("Gilead-BioStats/gsm.mapping")
pak::pak("Gilead-BioStats/gsm.kri")
pak::pak("Gilead-BioStats/gsm.reporting")
pak::pak("IMPALA-Consortium/gsm.studykri")
```

## Example: Days on Study

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(gsm.core)
library(gsm.kri)
library(gsm.studykri)

# Load raw data from clindata package
lRaw <- list(
  Raw_AE = clindata::rawplus_ae,
  Raw_SUBJ = clindata::rawplus_dm
)

# Simulate portfolio with 4 studies, AA-4 oversampled for low AE counts
lPortfolio <- SimulatePortfolio(
  lRaw = lRaw,
  nStudies = 4,
  dfConfig = tibble(
    studyid = c("AA-1", "AA-2", "AA-3", "AA-4"),
    nSubjects = c(500, 750, 150, 200),
    strOversampleDomain = rep("Raw_AE", 4),
    vOversamplQuantileRange_min = c(0, 0, 0, 0),
    vOversamplQuantileRange_max = c(1, 1, 1, 0.75)
  )
)
#> Filtered to 1016 subjects with Raw_AE records in 0.00-1.00 quantile range (1-31 records)
#> Filtered to 1016 subjects with Raw_AE records in 0.00-1.00 quantile range (1-31 records)
#> Filtered to 1016 subjects with Raw_AE records in 0.00-1.00 quantile range (1-31 records)
#> Filtered to 773 subjects with Raw_AE records in 0.00-0.75 quantile range (1-6 records)

# Calculate site-level counts by month using days on study as denominator
# Pass subjects as denominator with start and end dates to calculate person-days
dfInputDays <- Input_CountSiteByMonth(
  dfSubjects = lPortfolio$Raw_SUBJ,
  dfNumerator = lPortfolio$Raw_AE,
  dfDenominator = lPortfolio$Raw_SUBJ,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "firstparticipantdate",
  strDenominatorEndDateCol = "lastparticipantdate"
) %>% tibble()

# Aggregate to study-level cumulative KRI by month
dfTransformed <- Transform_CumCount(
  dfInputDays,
  nMinDenominator = 25,
  vBy = c("StudyID")
) %>% tibble()

# Generate bootstrap resamples at site-level (1000 iterations)
df_Analyzed_Bootstrap_Site <- dfInputDays %>%
  Analyze_StudyKRI(nBootstrapReps = 1000) %>%
  tibble()

# Aggregate each bootstrap iteration to study-level
df_Analyzed_Bootstrap_Study <- df_Analyzed_Bootstrap_Site %>%
  Transform_CumCount(
    nMinDenominator = 25,
    vBy = c("StudyID", "BootstrapRep")
  ) %>%
  tibble()

# Calculate 95% confidence intervals from bootstrap distribution
df_Bounds <- Analyze_StudyKRI_PredictBounds(
  df_Analyzed_Bootstrap_Study,
  vBy = c("StudyID"),
  nConfLevel = 0.95
) %>% tibble()

# Calculate reference portfolio bounds from studies AA-1, AA-2, AA-3
# Reference bounds equalize site counts across studies for fair comparison
df_Bounds_Ref <- dfInputDays %>%
  Analyze_StudyKRI_PredictBoundsRefSet(
    vStudyFilter = c("AA-1", "AA-2", "AA-3"),
    nBootstrapReps = 1000,
    nConfLevel = 0.95,
    nMinDenominator = 25
  ) %>%
  tibble()
#> Resampling with minimum group count: 66

# Plot AA-4 KRI vs reference portfolio confidence intervals
Visualize_StudyKRI(
  dfStudyKRI = dfTransformed,
  dfBoundsRef = df_Bounds_Ref,
  dfBounds = df_Bounds,
  strStudyID = "AA-4"
)
```

<img src="man/figures/README-example-1.png" width="100%" />

## AI Disclaimer

This package and documentation were developed with assistance from AI
tools, including Cursor and Claude Sonnet 4.5. All AI-generated content
has been reviewed.

## License

MIT © 2025 IMPALA Consortium

## Sample Report

- [Sample Report
  1](https://impala-consortium.github.io/gsm.studykri/report_studykri_AA-1.html)
- [Sample Report 2 (under-reporting
  study)](https://impala-consortium.github.io/gsm.studykri/report_studykri_AA-2.html)

## Quality Control

This project was developed until MVP
a2d4e9d8d588e87c3708168261505ac3da52a912

Since {gsm} is designed for use in a
[GCP](https://en.wikipedia.org/wiki/Good_clinical_practice) framework,
we have since extensive quality control as part of our development
process. In particular, we do the following during early development:

- **Unit Tests** - Unit tests are written for all core functions, 100%
  coverage required.
- **Workflow Tests** - Additional unit tests confirm that core workflows
  behave as expected.
- **Function Documentation** - Detailed documentation for each exported
  function with examples is maintained with Roxygen.
- **Package Checks** - Standard package checks are run using GitHub
  Actions and must be passing before PRs are merged.
- **Continuous Integration** - Continuous integration is provided via
  GitHub Actions.
- **Code Formatting** - Code is formatted with {styler} before each
  release.
- **Contributor Guidelines** - Detailed contributor guidelines including
  step-by-step processes for code development and releases are provided
  as a vignette.
- **Code Demonstration** -
  [Cookbook](https://impala-consortium.github.io/gsm.studykri/articles/Cookbook.html)
  Vignette provides demos and explanations for code usage.

### Parking

As development progresses, we will also conduct the following quality
control steps:

- **Qualification Workflow** - All assessments have been Qualified as
  described in the Qualification Workflow Vignette. A Qualification
  Report Vignette is generated and attached to each release.
- **Code Review** - Code review is conducted using GitHub Pull Requests
  (PRs), and a log of all PRs is included in the Qualification Report
  Vignette.
- **Data Specifications** - Machine-readable data specifications are
  maintained for all KRIs. Specifications are automatically added to
  relevant function documentation.
- **Regression Testing** - Extensive QC and testing is done before each
  release.
