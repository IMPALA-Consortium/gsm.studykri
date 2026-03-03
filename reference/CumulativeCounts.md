# Calculate Cumulative Counts and Metrics

Calculates cumulative sums of numerators and denominators, then computes
metrics as cumulative numerator divided by cumulative denominator.

## Usage

``` r
CumulativeCounts(dfAggregated, vBy, vNumeratorCols)
```

## Arguments

- dfAggregated:

  data.frame or tbl from AggrStudyMonth

- vBy:

  character vector of grouping columns

- vNumeratorCols:

  character vector of numerator column names

## Value

data.frame/tbl with cumulative counts and metrics
