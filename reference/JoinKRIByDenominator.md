# Join KRI Analysis_Input by Denominator Type

Groups bound Analysis_Input data by denominator type and pivots
numerators into separate columns for shared bootstrap processing.

This function validates that all KRIs sharing the same DenominatorType
have identical AccrualThreshold values. This is critical because
different AccrualThreshold values cause different Denominator
calculations due to date adjustment logic in Input_CountSiteByMonth,
which would lead to incorrect results when pivoting the data.

## Usage

``` r
JoinKRIByDenominator(dfInput, dfMetrics, bSkipValidation = FALSE)
```

## Arguments

- dfInput:

  data.frame. Bound Analysis_Input from BindResults. Must contain
  columns: MetricID, GroupID, GroupLevel, Numerator, Denominator,
  StudyID, MonthYYYYMM, and DenominatorType.

- dfMetrics:

  data.frame. Metrics metadata from MakeMetric. Must contain columns:
  MetricID and AccrualThreshold.

- bSkipValidation:

  logical. If TRUE, skips validation checks that require collect()
  operations (AccrualThreshold consistency, GroupLevel consistency, and
  Denominator value matching). Use this to improve performance with
  large database tables when you are confident your workflow
  configuration is correct. Default: FALSE.

## Value

Named list with one tibble per denominator type. Each tibble has common
columns plus Numerator columns renamed by MetricID.

## Examples

``` r
dfInput <- data.frame(
  MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
  GroupID = c("Site1", "Site2", "Site1", "Site2"),
  GroupLevel = "Site",
  Numerator = c(5, 3, 2, 1),
  Denominator = c(100, 80, 100, 80),
  StudyID = "AA-1",
  MonthYYYYMM = 202301,
  DenominatorType = "Visit"
)

dfMetrics <- data.frame(
  MetricID = c("kri0001", "kri0003"),
  AccrualThreshold = c(180, 180)
)

lJoined <- JoinKRIByDenominator(dfInput, dfMetrics)

names(lJoined)
#> [1] "Visit"

# Example of error condition (different AccrualThreshold for same DenominatorType)
if (FALSE) { # \dontrun{
dfMetrics_mismatched <- data.frame(
  MetricID = c("kri0001", "kri0003"),
  AccrualThreshold = c(180, 25) # Different values!
)

# This will throw an error:
lJoined <- JoinKRIByDenominator(dfInput, dfMetrics_mismatched)
} # }

# Example 3: Skip validation for performance with large database tables
if (FALSE) { # \dontrun{
# When working with lazy tables and confident about data quality:
lJoined <- JoinKRIByDenominator(
  dfInput = tbl_lazy_input,
  dfMetrics = dfMetrics,
  bSkipValidation = TRUE
)
} # }
```
