# Generate sites with weighted patient distribution

Generate sites with weighted patient distribution

## Usage

``` r
generate_sites(
  raw_site_df,
  sampled_subj,
  new_study_id,
  target_site_count,
  vSiteIDs
)
```

## Arguments

- raw_site_df:

  Raw site data frame

- sampled_subj:

  Data frame of sampled subjects

- new_study_id:

  New study ID string

- target_site_count:

  Target number of sites to generate

- vSiteIDs:

  Site ID column names in hierarchical order

## Value

List with generated_sites and updated_sampled_subj
