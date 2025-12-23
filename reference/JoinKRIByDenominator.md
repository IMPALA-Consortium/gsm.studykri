# Join KRI Analysis_Input by Denominator Type

Groups bound Analysis_Input data by denominator type and pivots
numerators into separate columns for shared bootstrap processing.

## Usage

``` r
JoinKRIByDenominator(dfInput, dfMetrics)
```

## Arguments

- dfInput:

  data.frame. Bound Analysis_Input from BindResults.

- dfMetrics:

  data.frame. Metrics metadata from MakeMetric.

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

lJoined <- JoinKRIByDenominator(dfInput)

names(lJoined)
#> [1] "Visit"
```
