# Calculate Days Per Month from Date Range

Helper function that calculates days contributed to each month from
start/end date pairs. Handles date ranges spanning multiple months with
dbplyr compatibility.

## Usage

``` r
CalculateDaysByMonth(dfData, strStartDateCol, strEndDateCol, vGroupCols)
```

## Arguments

- dfData:

  data.frame or tbl with date range records

- strStartDateCol:

  character. Start date column name

- strEndDateCol:

  character. End date column name

- vGroupCols:

  character. Grouping columns to preserve

## Value

data.frame or tbl with MonthYYYYMM and Days columns
