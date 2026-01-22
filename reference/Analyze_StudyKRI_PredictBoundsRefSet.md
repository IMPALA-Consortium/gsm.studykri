# Predict Bounds for Combined Group of Studies

Calculates percentile-based confidence intervals (bounds) for a combined
portfolio of comparison studies. This function creates a "comparison
envelope" by:

1.  Finding the minimum group (site) count across selected studies

2.  Resampling each study with that minimum to ensure fair comparison

3.  Combining data across all studies

4.  Calculating confidence intervals for the pooled distribution

This enables comparing one study's KRI against expected variation from a
portfolio of similar studies.

## Usage

``` r
Analyze_StudyKRI_PredictBoundsRefSet(
  dfInput,
  vStudyFilter = NULL,
  nBootstrapReps = 1000,
  nConfLevel = 0.95,
  strStudyCol = "StudyID",
  strGroupCol = "GroupID",
  strStudyMonthCol = "StudyMonth",
  seed = NULL,
  tblBootstrapReps = NULL,
  tblMonthSequence = NULL
)
```

## Arguments

- dfInput:

  data.frame or tbl_lazy. Site-level data from
  `Input_CumCountSiteByMonth`. Must contain columns: `StudyID`,
  `GroupID`, one or more `Numerator_*` columns, `Denominator`,
  `MonthYYYYMM`.

- vStudyFilter:

  character or NULL. Study IDs to include in comparison group. If NULL
  (default), uses all studies found in dfInput. Example:
  `c("STUDY1", "STUDY2", "STUDY3")`.

- nBootstrapReps:

  integer. Number of bootstrap replicates (default: 1000).

- nConfLevel:

  numeric. Confidence level for the bounds, between 0 and 1 (default:
  0.95 for 95% CI).

- strStudyCol:

  character. Column name for study identifier (default: "StudyID").

- strGroupCol:

  character. Column name for group identifier (default: "GroupID").

- strStudyMonthCol:

  character. Column name for sequential study month (default:
  "StudyMonth").

- seed:

  integer or NULL. Random seed for reproducibility (default: NULL).

- tblBootstrapReps:

  tbl_lazy, data.frame, or NULL. For lazy table inputs: Optional
  pre-generated bootstrap replicate indices with a `BootstrapRep` column
  containing values 1 to N (where N is the desired number of
  replicates). If provided, overrides the `nBootstrapReps` parameter. If
  NULL, attempts to create temp table (requires write privileges).

- tblMonthSequence:

  tbl_lazy, data.frame, or NULL. For lazy table inputs: Optional
  pre-generated month sequence with only a `MonthYYYYMM` column (output
  of
  [`GenerateMonthSeq()`](https://impala-consortium.github.io/gsm.studykri/reference/GenerateMonthSeq.md)).
  If NULL, attempts to create temp table (requires write privileges).

## Value

A tibble (or tbl_lazy if input was lazy) with confidence intervals.
Output columns: `StudyMonth`, `Median_*`, `Lower_*`, `Upper_*` for each
Metric column, `BootstrapCount`, `GroupCount`, `StudyCount`. Note: No
`StudyID` column as studies are intentionally combined.

## Examples

``` r
# Create example site-level data for multiple studies
dfSiteLevel <- data.frame(
  StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 60),
  GroupID = rep(paste0("Site", 1:10), each = 6, times = 3),
  Numerator = sample(0:5, 180, replace = TRUE),
  Denominator = sample(10:20, 180, replace = TRUE),
  MonthYYYYMM = rep(rep(202301:202306, each = 10), times = 3),
  Metric = runif(180, 0.1, 0.5),
  GroupLevel = "Site",
  stringsAsFactors = FALSE
)

# Calculate comparison envelope from 3 studies
dfGroupBounds <- Analyze_StudyKRI_PredictBoundsRefSet(
  dfInput = dfSiteLevel,
  vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
  nBootstrapReps = 100, # Use small number for example
  nConfLevel = 0.95,
  seed = 42
)
#> Resampling with minimum group count: 10

print(head(dfGroupBounds))
#> # A tibble: 6 × 7
#>   StudyMonth Median Lower Upper BootstrapCount GroupCount StudyCount
#>        <int>  <dbl> <dbl> <dbl>          <int>      <int>      <int>
#> 1          1  0.178 0.138 0.211            100         10          3
#> 2          2  0.156 0.122 0.194            100         10          3
#> 3          3  0.166 0.137 0.197            100         10          3
#> 4          4  0.168 0.142 0.196            100         10          3
#> 5          5  0.171 0.147 0.195            100         10          3
#> 6          6  0.170 0.150 0.192            100         10          3
```
