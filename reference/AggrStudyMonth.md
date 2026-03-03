# Aggregate to Study Level by Month with Complete Timeline

Aggregates site-level data to study level by calendar month, fills
missing months with zeros, and calculates StudyMonth ranking.

## Usage

``` r
AggrStudyMonth(dfInput, vBy, vNumeratorCols, tblMonthSequence = NULL)
```

## Arguments

- dfInput:

  data.frame or tbl with site-level monthly counts

- vBy:

  character vector of grouping columns

- vNumeratorCols:

  character vector of numerator column names

- tblMonthSequence:

  tbl_lazy, data.frame, or NULL for month sequence

## Value

data.frame/tbl with study-level monthly data and StudyMonth
