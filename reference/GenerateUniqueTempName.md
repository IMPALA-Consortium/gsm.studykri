# Generate Unique Temporary Table Name

Creates a unique table name by appending process ID and random integer.
This prevents conflicts when multiple function calls create temp tables
in the same database session (e.g., when processing multiple KRIs via
purrr::map).

## Usage

``` r
GenerateUniqueTempName(base_name)
```

## Arguments

- base_name:

  character. Base name for the table (e.g., "bootstrap_reps")

## Value

character. Unique table name (e.g., "bootstrap_reps_12345_678901")
