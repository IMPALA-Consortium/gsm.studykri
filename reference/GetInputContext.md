# Extract KRI context string from dfInput for warning messages

Builds a descriptive suffix from Numerator column names (KRI IDs) and
DenominatorType (if present). DenominatorType is only collected when
this function is called, keeping it lazy.

## Usage

``` r
GetInputContext(dfInput)
```

## Arguments

- dfInput:

  data.frame or tbl

## Value

character scalar, e.g. " KRI: Analysis_kri0004. DenominatorType: Days on
Study."
