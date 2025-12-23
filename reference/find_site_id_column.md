# Find site ID column

Find site ID column

## Usage

``` r
find_site_id_column(df, vSiteIDs)
```

## Arguments

- df:

  Data frame to search

- vSiteIDs:

  Character vector of column names to search in hierarchical order

## Value

List with 'column' (column name found) and 'ids' (vector of site ID
values), or NULL if none found
