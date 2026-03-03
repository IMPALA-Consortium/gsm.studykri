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
  strMinGroupsCol = "MinGroups",
  bMixStudies = FALSE,
  tblBootstrapReps = NULL,
  tblMonthSequence = NULL,
  vDbIntRandomRange = NULL,
  funCompute = NULL
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
  target-reference pair). Optional column (name specified by
  strMinGroupsCol): minimum group count for performance optimization.

- nBootstrapReps:

  integer. Number of bootstrap replicates (default: 1000).

- nConfLevel:

  numeric. Confidence level for the bounds (default: 0.95).

- strGroupCol:

  character. Column name for group identifier (default: "GroupID").

- strStudyMonthCol:

  character. Column name for study month (default: "StudyMonth").

- strMinGroupsCol:

  character. Column name for minimum groups in dfStudyRef (default:
  "MinGroups"). If this column exists in dfStudyRef, its value will be
  used instead of calculating minimum group counts, improving
  performance with database backends.

- bMixStudies:

  logical. If TRUE, aggregates studies at the StudyMonth level before
  calculating cumulative counts (faster SQL performance, but disables
  extrapolation). If FALSE (default), keeps study-level granularity
  through extrapolation step, allowing studies with different timeline
  lengths to be properly aligned before aggregation. Use TRUE for
  database backends when SQL performance is critical and all studies
  have similar timeline lengths. Default: FALSE.

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

- vDbIntRandomRange:

  Numeric vector of length 2 or NULL. When using database backends that
  return large integers instead of 0-1 decimals for random numbers,
  specify the min/max range as c(min, max). Accepts both numeric and
  character vectors (character values are automatically converted to
  numeric, useful when reading from YAML files). Common values:

  - Snowflake: c(-9223372036854775808, 9223372036854775807) (signed
    64-bit)

  - Other backends: c(0, 18446744073709551615) (unsigned 64-bit)
    Default: NULL (no normalization, assumes 0-1 decimal random values).

- funCompute:

  function or NULL. Optional function to apply to intermediate database
  results for performance optimization. Typically used to
  cache/materialize intermediate results using
  [`dplyr::compute()`](https://dplyr.tidyverse.org/reference/compute.html).
  Only applied when dfInput is a database table (tbl_dbi). Requires
  database write permissions to create temporary tables. Example:
  `funCompute = dplyr::compute`. Default: NULL.

## Value

A tibble (or tbl_lazy if input was lazy) with columns: `StudyID`,
`StudyRefID`, `StudyMonth`, `Median_*`, `Lower_*`, `Upper_*` for each
Metric column, `BootstrapCount`, `GroupCount`, `StudyCount`.

## Details

For optimal performance with database backends:

- Include a MinGroups column in dfStudyRef (or specify custom name via
  strMinGroupsCol) to avoid collecting the full dataset to count groups

- Calculate as: min(n_distinct(GroupID)) across all reference studies
  per target study

- This avoids materializing the entire Analysis_Input table just to
  count groups

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
  nBootstrapReps = 100
)
#> Calculated minimum group count: 10
#> Calculated minimum group count: 10

# Example with Snowflake backend
dfBounds_Snowflake <- Analyze_StudyKRI_PredictBoundsRef(
  dfInput = dfSiteLevel,
  dfStudyRef = dfStudyRef,
  nBootstrapReps = 100,
  vDbIntRandomRange = c(-9223372036854775808, 9223372036854775807)
)
#> Calculated minimum group count: 10
#> Calculated minimum group count: 10

# Example with database backend using funCompute to cache results
# (Assumes dfSiteLevel is a tbl_dbi database table)
dfBounds_Cached <- Analyze_StudyKRI_PredictBoundsRef(
  dfInput = dfSiteLevel,
  dfStudyRef = dfStudyRef,
  nBootstrapReps = 100,
  funCompute = dplyr::compute  # Cache intermediate results in database
)
#> Calculated minimum group count: 10
#> Calculated minimum group count: 10

print(head(dfBounds))
#> # A tibble: 6 × 9
#>   StudyMonth Median Lower Upper BootstrapCount GroupCount StudyCount StudyID
#>        <int>  <dbl> <dbl> <dbl>          <int>      <int>      <int> <chr>  
#> 1          1  0.144 0.103 0.194            100         10          2 STUDY1 
#> 2          2  0.156 0.112 0.178            100         10          2 STUDY1 
#> 3          3  0.150 0.104 0.175            100         10          2 STUDY1 
#> 4          4  0.142 0.113 0.166            100         10          2 STUDY1 
#> 5          5  0.146 0.121 0.167            100         10          2 STUDY1 
#> 6          6  0.148 0.122 0.168            100         10          2 STUDY1 
#> # ℹ 1 more variable: StudyRefID <chr>
```
