# Apply Date Adjustment for Minimum Denominator Threshold

Internal utility function that adjusts dates for the first N denominator
events to create a consistent study start date across studies and
bootstrap replicates. Also adjusts corresponding numerator dates to
maintain temporal relationships.

## Usage

``` r
ApplyMinDenominatorDateAdjustment(
  dfNumerator_processed,
  dfDenominator,
  strSubjectCol,
  strStudyCol,
  strGroupCol,
  strNumeratorDateCol,
  strDenominatorDateCol,
  strDenominatorEndDateCol,
  nMinDenominator
)
```

## Arguments

- dfNumerator_processed:

  Data frame with processed numerator data (with MonthYYYYMM)

- dfDenominator:

  Data frame with original denominator data

- strSubjectCol:

  Name of subject ID column

- strStudyCol:

  Name of study ID column

- strGroupCol:

  Name of group ID column (e.g., site)

- strNumeratorDateCol:

  Name of numerator date column

- strDenominatorDateCol:

  Name of denominator date column

- strDenominatorEndDateCol:

  Optional name of denominator end date column

- nMinDenominator:

  Minimum denominator threshold for date adjustment

## Value

List with two elements:

- dfNumerator: Adjusted numerator data with updated MonthYYYYMM

- dfDenominator: Adjusted denominator data with updated date columns
