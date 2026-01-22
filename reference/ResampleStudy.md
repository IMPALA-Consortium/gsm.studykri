# Resample Study Data

Creates a resampled version of a study with new subject and study IDs.
Supports stratified sampling based on domain activity (e.g., oversample
patients with high protocol deviations) and randomizes site assignments.

## Usage

``` r
ResampleStudy(
  lRaw,
  strNewStudyID,
  nSubjects = NULL,
  TargetSiteCount = NULL,
  replacement = TRUE,
  strOversampleDomain = NULL,
  vOversamplQuantileRange = c(0, 1),
  seed = NULL,
  vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname",
    "subjectid"),
  vSiteIDs = c("invid", "siteid", "site_num")
)
```

## Arguments

- lRaw:

  Named list of raw data domains (e.g., Raw_SUBJ, Raw_AE, etc.)

- strNewStudyID:

  Character string for the new study ID

- nSubjects:

  Integer number of subjects to sample. NULL (default) samples same
  number as enrolled subjects in original data

- TargetSiteCount:

  Numeric. Approximate target number of sites in the resampled study. If
  NULL (default), uses sites from sampled subjects naturally. If
  specified, generates approximately N sites with weighted patient
  distributions. Note: Final site count may vary as sites with zero
  patients are excluded. Must be a positive integer.

- replacement:

  Logical indicating whether to sample with replacement (default: TRUE)

- strOversampleDomain:

  Character string naming a domain to use for stratified sampling. NULL
  (default) samples from all enrolled subjects

- vOversamplQuantileRange:

  Numeric vector of length 2 with quantile range (0-1) for oversampling.
  Default c(0, 1) includes all subjects

- seed:

  Integer seed for reproducibility. NULL (default) uses current random
  state

- vSubjectIDs:

  Character vector defining hierarchical order for subject ID column
  lookup. Default: c("subjid", "subjectenrollmentnumber", "subject_nsv",
  "subjectname", "subjectid"). **Important:** All columns in vSubjectIDs
  must exist in Raw_SUBJ for proper mapping. If a column is missing, add
  it to Raw_SUBJ before calling this function. For example:
  `lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv`

- vSiteIDs:

  Character vector defining hierarchical order for site ID column
  lookup. Default: c("invid", "siteid", "site_num")

## Value

Named list of resampled data domains with updated IDs

## Details

This function performs the following steps:

1.  Optionally filters subjects by their activity level in a specified
    domain

2.  Samples subjects with or without replacement

3.  Randomizes site assignments by shuffling invid values (or generates
    new sites if TargetSiteCount specified)

4.  Updates all subject, study, and site IDs across all domains

5.  Maintains referential integrity across domains

Domains are automatically categorized by columns present:

- **Subject-level**: Has column in vSubjectIDs → filter to sampled
  subjects

- **Site-level**: Has column in vSiteIDs but no subject column → filter
  to used sites

- **Other**: Neither → update studyid field only

When `TargetSiteCount` is specified:

- Generates TargetSiteCount site IDs with metadata sampled from original
  sites

- Samples patient counts per site from the distribution observed in
  sampled subjects

- Creates weighted site assignment: subjects are assigned to sites
  proportionally to sampled patient counts

- Final site count may be less than target if some sites receive no
  patients through sampling

The function handles multiple subject ID formats:

- subjid: Simple ID (e.g., "0496")

- subjectid: Composite ID (e.g., "X1670496-113XXX")

- subject_nsv: NSV format (e.g., "0496-113XXX")

## Examples

``` r
# Load test data
lRaw <- list(
  Raw_SUBJ = clindata::rawplus_dm,
  Raw_AE = clindata::rawplus_ae,
  Raw_SITE = clindata::ctms_site,
  Raw_STUDY = clindata::ctms_study
)

# Add required columns to Raw_SUBJ (required for vSubjectIDs)
lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjid

# Standard resampling
lStudy1 <- ResampleStudy(lRaw, "STUDY001", seed = 123)

# Oversample from high-AE patients (top 25%)
lStudy2 <- ResampleStudy(
  lRaw,
  "STUDY002",
  nSubjects = 50,
  strOversampleDomain = "Raw_AE",
  vOversamplQuantileRange = c(0.75, 1.0),
  seed = 456
)
#> Filtered to 309 subjects with Raw_AE records in 0.75-1.00 quantile range (6-31 records)

# Generate study with target of ~30 sites
lStudy3 <- ResampleStudy(
  lRaw,
  "STUDY003",
  nSubjects = 200,
  TargetSiteCount = 30,
  seed = 789
)
```
