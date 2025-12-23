# Transform Cumulative Counts to Study Level

Transforms site-level cumulative counts into study-level metrics by
aggregating across groups (sites/countries) by calendar month. Creates
sequential study months, applies minimum denominator filtering, and
calculates cumulative metrics. Supports both in-memory data frames and
dbplyr lazy tables. Handles multiple Numerator columns (e.g.,
Numerator_kri0001, Numerator_kri0003).

## Usage

``` r
Transform_CumCount(dfInput, vBy, nMinDenominator = 25, tblMonthSequence = NULL)
```

## Arguments

- dfInput:

  data.frame or tbl. Output from Input_CountSiteByMonth with columns:
  GroupID, GroupLevel, Numerator (or Numerator\_\*), Denominator,
  StudyID, MonthYYYYMM. Note: Input should contain monthly (not
  cumulative) counts. This function calculates cumulative sums at the
  study level.

- vBy:

  character. Vector of column names for grouping (e.g., "StudyID" or
  c("StudyID", "BootstrapRep")).

- nMinDenominator:

  numeric. Minimum cumulative denominator threshold for filtering
  early/sparse data (default: 25).

- tblMonthSequence:

  tbl_lazy, data.frame, or NULL. For lazy table inputs: Optional
  pre-generated month sequence with only a `MonthYYYYMM` column (output
  of
  [`GenerateMonthSeq()`](https://impala-consortium.github.io/gsm.studykri/reference/GenerateMonthSeq.md)).
  Must contain consecutive months with NO gaps. If NULL, attempts to
  create temp table (requires write privileges). If data.frame provided,
  will be written to temp table.

## Value

A data.frame with study-level aggregated cumulative counts and ratios.
Output columns: `vBy` columns, `MonthYYYYMM`, `StudyMonth`, Numerator
columns, `Denominator`, Metric columns, `GroupCount`.

## Examples

``` r
# Generate input data
dfSubjects <- clindata::rawplus_dm
dfNumerator <- clindata::rawplus_ae
dfDenominator <- clindata::rawplus_visdt

dfInput <- Input_CountSiteByMonth(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "visit_dt"
)

# Transform to study level
dfTransformed <- Transform_CumCount(
  dfInput = dfInput,
  vBy = "StudyID",
  nMinDenominator = 25
)

print(dfTransformed)
#> # A tibble: 190 × 7
#>    StudyID        MonthYYYYMM StudyMonth Numerator Denominator Metric GroupCount
#>    <chr>                <dbl>      <int>     <int>       <int>  <dbl>      <int>
#>  1 AA-AA-000-0000      200401          1         4          33  0.121         12
#>  2 AA-AA-000-0000      200402          2         5          50  0.1           14
#>  3 AA-AA-000-0000      200403          3        11          78  0.141         19
#>  4 AA-AA-000-0000      200404          4        23         114  0.202         23
#>  5 AA-AA-000-0000      200405          5        34         161  0.211         29
#>  6 AA-AA-000-0000      200406          6        39         213  0.183         35
#>  7 AA-AA-000-0000      200407          7        55         278  0.198         41
#>  8 AA-AA-000-0000      200408          8        67         363  0.185         49
#>  9 AA-AA-000-0000      200409          9        88         447  0.197         51
#> 10 AA-AA-000-0000      200410         10       105         538  0.195         56
#> # ℹ 180 more rows
```
