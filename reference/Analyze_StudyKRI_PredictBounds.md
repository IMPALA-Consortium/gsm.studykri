# Calculate Bootstrap-Based Confidence Bounds for Study-Level KRI

Calculates confidence bounds for study-level KRI metrics using bootstrap
resampling. This is a convenience function that orchestrates the
complete workflow:

1.  Filters input data to target studies (specified in dfStudyRef or all
    studies if NULL)

2.  Generates bootstrap resamples via `BootstrapStudyKRI()`

3.  Aggregates to study level via
    [`Transform_CumCount()`](https://impala-consortium.github.io/gsm.studykri/reference/Transform_CumCount.md)

4.  Calculates confidence intervals via `CalculateStudyBounds()`

## Usage

``` r
Analyze_StudyKRI_PredictBounds(
  dfInput,
  dfStudyRef = NULL,
  nBootstrapReps = 1000,
  nConfLevel = 0.95,
  nMinDenominator = 25,
  seed = NULL,
  tblBootstrapReps = NULL,
  tblMonthSequence = NULL,
  strStudyCol = "StudyID",
  strGroupCol = "GroupID",
  strStudyMonthCol = "StudyMonth"
)
```

## Arguments

- dfInput:

  data.frame or tbl_lazy. Group-level data from
  `Input_CumCountSiteByMonth`. Expected columns: GroupID, one or more
  Numerator columns, Denominator, StudyID, MonthYYYYMM.

- dfStudyRef:

  data.frame or NULL. Optional study reference mapping. If provided, the
  unique values in the first column identify the target studies for
  which bounds will be calculated. If NULL (default), bounds are
  calculated for all studies found in dfInput.

- nBootstrapReps:

  integer. Number of bootstrap replicates to generate. Default: 1000.

- nConfLevel:

  numeric. Confidence level between 0 and 1. Default: 0.95 (95%
  confidence interval).

- nMinDenominator:

  numeric. Minimum cumulative denominator threshold for
  Transform_CumCount filtering. Default: 25.

- seed:

  integer or NULL. Random seed for reproducibility. Default: NULL.

- tblBootstrapReps:

  tbl_lazy, data.frame, or NULL. For lazy table inputs: Optional
  pre-generated bootstrap replicate indices. Default: NULL.

- tblMonthSequence:

  tbl_lazy, data.frame, or NULL. For lazy table inputs: Optional
  pre-generated complete month sequences. Default: NULL.

- strStudyCol:

  character. Column name for study identifier in dfInput. Default:
  "StudyID".

- strGroupCol:

  character. Column name for group identifier. Default: "GroupID".

- strStudyMonthCol:

  character. Name of study month column. Default: "StudyMonth".

## Value

A tibble (or tbl_lazy if input was lazy) with confidence intervals:

- `StudyID`: Target study identifier

- `StudyMonth`: Sequential month number

- `Median_*`, `Lower_*`, `Upper_*`: Bounds for each Metric column

- `BootstrapCount`: Number of bootstrap samples used

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: Calculate bounds for all studies in dfInput
Bounds_Wide <- purrr::map(
  lJoined, 
  ~ Analyze_StudyKRI_PredictBounds(.)
)

# Example 2: Calculate bounds for specific studies using reference mapping
dfStudyRef <- data.frame(
  study = c("AA-1", "AA-1", "AA-2", "AA-2"),
  studyref = c("AA-3", "AA-4", "AA-3", "AA-5")
)

# Calculate bounds for target studies (AA-1 and AA-2) specified in first column
Bounds_Wide <- purrr::map(
  lJoined, 
  ~ Analyze_StudyKRI_PredictBounds(., dfStudyRef = dfStudyRef)
)
} # }
```
