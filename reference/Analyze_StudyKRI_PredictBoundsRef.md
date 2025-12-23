# Predict Bounds for Multiple Studies Using Reference Study Mappings

Wrapper for `Analyze_StudyKRI_PredictBoundsRefSet` that applies
study-specific reference groups. For each study in `dfStudyRef`,
calculates bounds using its mapped reference studies.

## Usage

``` r
Analyze_StudyKRI_PredictBoundsRef(
  dfInput,
  dfStudyRef,
  nBootstrapReps = 1000,
  nConfLevel = 0.95,
  strGroupCol = "GroupID",
  strStudyMonthCol = "StudyMonth",
  nMinDenominator = 25,
  seed = NULL,
  tblBootstrapReps = NULL,
  tblMonthSequence = NULL
)
```

## Arguments

- dfInput:

  data.frame or tbl_lazy. Site-level data from
  `Input_CumCountSiteByMonth`. Must contain one or more `Numerator_*`
  columns, `Denominator`, `MonthYYYYMM`.

- dfStudyRef:

  data.frame or tbl_lazy. Study-to-reference mappings with at least two
  columns: first column = target studies, second column = reference
  studies. Can have multiple rows per target study (one row per
  target-reference pair).

- nBootstrapReps:

  integer. Number of bootstrap replicates (default: 1000).

- nConfLevel:

  numeric. Confidence level for the bounds (default: 0.95).

- strGroupCol:

  character. Column name for group identifier (default: "GroupID").

- strStudyMonthCol:

  character. Column name for study month (default: "StudyMonth").

- nMinDenominator:

  numeric. Minimum denominator (default: 25).

- seed:

  integer or NULL. Random seed (default: NULL).

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

A tibble (or tbl_lazy if input was lazy) with columns: `StudyID`,
`StudyRefID`, `StudyMonth`, `Median_*`, `Lower_*`, `Upper_*` for each
Metric column, `BootstrapCount`, `GroupCount`, `StudyCount`.

## Examples

``` r
# Create study reference mapping
dfStudyRef <- data.frame(
  study = c(rep("STUDY1", 2), rep("STUDY2", 2)),
  studyref = c("REF1", "REF2", "REF2", "REF3")
)

# Create site-level data
dfSiteLevel <- data.frame(
  StudyID = rep(c("STUDY1", "STUDY2", "REF1", "REF2", "REF3"), each = 60),
  GroupID = rep(paste0("Site", 1:10), each = 6, times = 5),
  Numerator = sample(0:5, 300, replace = TRUE),
  Denominator = sample(10:20, 300, replace = TRUE),
  MonthYYYYMM = rep(rep(202301:202306, each = 10), times = 5),
  Metric = runif(300, 0.1, 0.5),
  GroupLevel = "Site"
)

# Calculate study-specific reference bounds
dfBounds <- Analyze_StudyKRI_PredictBoundsRef(
  dfInput = dfSiteLevel,
  dfStudyRef = dfStudyRef,
  nBootstrapReps = 100,
  seed = 42
)
#> Resampling with minimum group count: 10
#> Resampling with minimum group count: 10

print(head(dfBounds))
#> # A tibble: 6 × 9
#>   StudyMonth Median  Lower Upper BootstrapCount GroupCount StudyCount StudyID
#>        <int>  <dbl>  <dbl> <dbl>          <int>      <int>      <int> <chr>  
#> 1          1  0.146 0.0820 0.176            100         10          2 STUDY1 
#> 2          2  0.146 0.123  0.184            100         10          2 STUDY1 
#> 3          3  0.150 0.128  0.170            100         10          2 STUDY1 
#> 4          4  0.149 0.131  0.166            100         10          2 STUDY1 
#> 5          5  0.150 0.126  0.169            100         10          2 STUDY1 
#> 6          6  0.153 0.138  0.170             98         10          2 STUDY1 
#> # ℹ 1 more variable: StudyRefID <chr>
```
