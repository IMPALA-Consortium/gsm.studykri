# Transform Wide Format Results to Long Format

Converts wide-format dataframes from `_Wide` workflow steps back into
long format with standard columns (`MetricID`, `Numerator`, `Metric`,
etc.). Auto-detects column patterns and extracts MetricID from suffixes.

## Usage

``` r
Transform_Long(lWide, strDenominatorCol = "DenominatorType")
```

## Arguments

- lWide:

  list. Named list of wide-format dataframes keyed by denominator type
  (e.g., "Visits", "Days"). Each dataframe contains columns with
  MetricID suffixes (e.g., `Median_kri0001`, `Lower_kri0001`).

- strDenominatorCol:

  character. Column name for denominator type in output (default:
  "DenominatorType").

## Value

A tibble in long format with columns:

- `MetricID`: Extracted from column suffixes (e.g., "kri0001")

- `DenominatorType`: From list keys (e.g., "Visits", "Days")

- Original id columns preserved

- Pivoted value columns (e.g., `Median`, `Lower`, `Upper`)

## Examples

``` r
# Create example wide-format data
dfWide <- data.frame(
  StudyID = c("AA-1", "AA-2"),
  StudyMonth = c(1, 1),
  Median_kri0001 = c(0.5, 0.6),
  Lower_kri0001 = c(0.3, 0.4),
  Upper_kri0001 = c(0.7, 0.8),
  Median_kri0003 = c(0.2, 0.25),
  Lower_kri0003 = c(0.1, 0.15),
  Upper_kri0003 = c(0.3, 0.35),
  BootstrapCount = c(1000, 1000)
)

lWide <- list(Visits = dfWide)
dfLong <- Transform_Long(lWide)
print(dfLong)
#> # A tibble: 4 × 8
#>   MetricID DenominatorType StudyID StudyMonth BootstrapCount Median Lower Upper
#>   <chr>    <chr>           <chr>        <dbl>          <dbl>  <dbl> <dbl> <dbl>
#> 1 kri0001  Visits          AA-1             1           1000   0.5   0.3   0.7 
#> 2 kri0001  Visits          AA-2             1           1000   0.6   0.4   0.8 
#> 3 kri0003  Visits          AA-1             1           1000   0.2   0.1   0.3 
#> 4 kri0003  Visits          AA-2             1           1000   0.25  0.15  0.35
```
