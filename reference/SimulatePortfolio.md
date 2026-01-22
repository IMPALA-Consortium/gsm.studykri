# Simulate a Portfolio of Studies

Creates a portfolio of multiple synthetic studies by resampling from
source data. Each study is generated using ResampleStudy with
configurable parameters.

## Usage

``` r
SimulatePortfolio(
  lRaw,
  dfConfig = NULL,
  nStudies = 5,
  seed = NULL,
  vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname",
    "subjectid"),
  vSiteIDs = c("invid", "siteid", "site_num")
)
```

## Arguments

- lRaw:

  list. Named list of raw data domains (must include Raw_SUBJ).

- dfConfig:

  data.frame. Optional configuration for each study. Must contain
  columns 'studyid' and 'nSubjects'. See Details.

- nStudies:

  numeric. Number of studies to generate if dfConfig is NULL. Default:
  5.

- seed:

  numeric. Random seed for reproducibility. Default: NULL.

- vSubjectIDs:

  Character vector defining hierarchical order for subject ID column
  lookup. Default: c("subjid", "subjectenrollmentnumber", "subject_nsv")

- vSiteIDs:

  Character vector defining hierarchical order for site ID column
  lookup. Default: c("invid", "siteid", "site_num")

## Value

list. Combined portfolio with same structure as lRaw, containing data
from all simulated studies row-bound together.

## Details

If dfConfig is provided, it should contain:

- studyid: Unique study identifier

- nSubjects: Number of subjects per study

- TargetSiteCount: (Optional) Target number of sites

- strOversampleDomain: (Optional) Domain for stratified sampling

- vOversamplQuantileRange_min/max: (Optional) Quantile range for
  stratification

- replacement: (Optional) Sample with replacement (default TRUE)

If dfConfig is NULL, generates default configuration with random
parameters.

Domains are automatically processed based on columns present:

- Subject-level domains (have vSubjectIDs columns) are filtered to
  sampled subjects

- Site-level domains (have vSiteIDs columns but no subject columns) are
  filtered to used sites

- Other domains have studyid updated only

## Examples

``` r
# Simple portfolio with default parameters
lRaw <- list(
  Raw_SUBJ = clindata::rawplus_dm,
  Raw_AE = clindata::rawplus_ae,
  Raw_SITE = clindata::ctms_site
)

# Add required columns to Raw_SUBJ
lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjid

lPortfolio <- SimulatePortfolio(lRaw, nStudies = 3, seed = 123)

# Custom configuration
dfConfig <- data.frame(
  studyid = c("TRIAL001", "TRIAL002"),
  nSubjects = c(50, 75),
  TargetSiteCount = c(15, 20)
)
lPortfolio_custom <- SimulatePortfolio(lRaw, dfConfig = dfConfig, seed = 456)
```
