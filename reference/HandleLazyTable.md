# Handle Lazy Table with Fallback Strategy

Handles lazy tables with consistent fallback:

1.  For in-memory data frames: return dfMem directly

2.  Use user-supplied table if provided

3.  Attempt to write temp table to database

4.  Throw expressive error if write fails

## Usage

``` r
HandleLazyTable(
  tblInput,
  tblUser = NULL,
  dfMem,
  strTempTableName,
  strTableType
)
```

## Arguments

- tblInput:

  tbl_lazy or data.frame. The input table context (for extracting
  connection if lazy)

- tblUser:

  tbl_lazy, data.frame, or NULL. User-supplied table

- dfMem:

  data.frame. In-memory data for auto-generation

- strTempTableName:

  character. Name for temp table

- strTableType:

  character. Description for error messages

## Value

tbl_lazy or data.frame table that can be joined
