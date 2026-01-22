# Transform Cumulative Counts to Study Level

Transforms site-level counts into study-level metrics by aggregating
across groups (sites/countries) by calendar month and calculating
cumulative sums. Supports both in-memory data frames and dbplyr lazy
tables. Handles multiple Numerator columns (e.g., Numerator_kri0001,
Numerator_kri0003).

Note: StudyMonth is calculated by this function based on MonthYYYYMM
order. Date normalization should be done at the input level
(Input_CountSiteByMonth).

## Usage

``` r
Transform_CumCount(dfInput, vBy, tblMonthSequence = NULL)
```

## Arguments

- dfInput:

  data.frame or tbl. Output from Input_CountSiteByMonth with columns:
  GroupID, Numerator (or Numerator\_\*), Denominator, StudyID,
  MonthYYYYMM. Note: Input should contain monthly (not cumulative)
  counts. This function calculates cumulative sums at the study level
  and adds StudyMonth.

- vBy:

  character. Vector of column names for grouping (e.g., "StudyID" or
  c("StudyID", "BootstrapRep")).

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

# Transform to study level and calculate StudyMonth
dfTransformed <- Transform_CumCount(
  dfInput = dfInput,
  vBy = "StudyID"
)

print(dfTransformed)
#> # A tibble: 194 × 7
#>    StudyID        MonthYYYYMM StudyMonth Numerator Denominator Metric GroupCount
#>    <chr>                <dbl>      <int>     <int>       <int>  <dbl>      <int>
#>  1 AA-AA-000-0000      200309          1         1           2  0.5            1
#>  2 AA-AA-000-0000      200310          2         1           5  0.2            3
#>  3 AA-AA-000-0000      200311          3         1          10  0.1            5
#>  4 AA-AA-000-0000      200312          4         3          16  0.188          7
#>  5 AA-AA-000-0000      200401          5         4          33  0.121         12
#>  6 AA-AA-000-0000      200402          6         5          50  0.1           14
#>  7 AA-AA-000-0000      200403          7        11          78  0.141         19
#>  8 AA-AA-000-0000      200404          8        23         114  0.202         23
#>  9 AA-AA-000-0000      200405          9        34         161  0.211         29
#> 10 AA-AA-000-0000      200406         10        39         213  0.183         35
#> # ℹ 184 more rows
```
