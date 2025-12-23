# Find subject ID column and map to subjid

Find subject ID column and map to subjid

## Usage

``` r
find_subject_id_column(df, raw_subj, vSubjectIDs)
```

## Arguments

- df:

  Data frame to search

- raw_subj:

  Raw_SUBJ data frame for mapping composite IDs

- vSubjectIDs:

  Character vector of column names to search in hierarchical order

## Value

List with 'column' (column name found) and 'ids' (vector of subjid
values)
