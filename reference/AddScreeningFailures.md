# Add Screening Failures to Enrolled Subjects

Augments a dataframe of enrolled subjects (enrollyn == "Y") with
screening failures (enrollyn == "N") sampled from a source dataframe.
For each study-site combination in the enrolled data, the function
randomly selects an enrollment ratio from the source data and samples
the corresponding number of screening failures to achieve that ratio.

## Usage

``` r
AddScreeningFailures(
  dfEnrolled,
  dfSource,
  strSiteCol = "siteid",
  strStudyCol = "studyid",
  seed = NULL
)
```

## Arguments

- dfEnrolled:

  Dataframe containing only enrolled patients (enrollyn == "Y"), modeled
  after `clindata::rawplus_enroll`. Must contain columns for study
  identifier, site identifier, and `enrollyn`. Can contain data for
  multiple studies.

- dfSource:

  Dataframe containing both enrolled (Y) and screening failure (N)
  patients. Serves as a patient pool and source of enrollment ratios.
  The study identifier in `dfSource` is ignored - ratios are calculated
  per site only and used as examples for sampling. Must contain the same
  columns as `dfEnrolled`.

- strSiteCol:

  Character string specifying the column name for site identifier.
  Default: "siteid"

- strStudyCol:

  Character string specifying the column name for study identifier.
  Default: "studyid"

- seed:

  Integer seed for reproducibility. Default: NULL (uses current random
  state)

## Value

The `dfEnrolled` dataframe augmented with sampled screening failure
records. The returned dataframe contains both enrolled (Y) and screening
failure (N) patients.

## Details

The function performs the following steps:

1.  **Calculate enrollment ratios** per site in `dfSource`:

    - Groups by site and counts Y and N patients (study identifier is
      ignored)

    - **Excludes sites with zero enrolled patients** to avoid division
      by zero

    - Computes ratio: n_enrolled / (n_enrolled + n_screening_failures)

    - Creates a pool of ratios from all sites with at least one enrolled
      patient

2.  **Process each study-site combination** in `dfEnrolled`:

    - Identifies unique study-site combinations

    - Counts enrolled patients at each specific study-site

    - Randomly selects one ratio from the pool (independent per
      study-site)

    - Calculates number of screening failures needed: n_enrolled \* (1 -
      ratio) / ratio

3.  **Sample screening failures**:

    - Filters `dfSource` to only enrollyn == "N" patients

    - Samples calculated number **with replacement** from entire pool
      (ignoring sites)

    - Updates both study and site identifiers to match the target
      study-site in `dfEnrolled`

4.  **Combine and return**:

    - Row-binds sampled screening failures to `dfEnrolled`

    - Returns combined dataframe

**Example ratio calculation:** If a study-site has 100 enrolled patients
and the randomly selected ratio is 0.8 (80% enrolled, 20% screen
failures), the function adds: `100 * (1 - 0.8) / 0.8 = 25` screening
failures

**Multi-study handling:** When `dfEnrolled` contains multiple studies,
each study-site combination is processed independently. The same site
name can appear in multiple studies (e.g., "Site1" in both "Study A" and
"Study B"), and each will receive its own sampled screening failures.

**Note:** Sampling is done with replacement, so the same screening
failure record from `dfSource` can appear multiple times in the output.

## Examples

``` r
# Single study example
dfEnrolled <- clindata::rawplus_enroll |>
  dplyr::filter(enrollyn == "Y")

# Source data with both Y and N
dfSource <- clindata::rawplus_enroll

# Add screening failures
dfAugmented <- AddScreeningFailures(
  dfEnrolled = dfEnrolled,
  dfSource = dfSource,
  strSiteCol = "siteid",
  strStudyCol = "studyid",
  seed = 123
)

# Check results
table(dfAugmented$enrollyn) # Should show both Y and N
#> 
#>    N    Y 
#> 1822 1301 
nrow(dfAugmented) > nrow(dfEnrolled) # Should be TRUE
#> [1] TRUE

# Multi-study example
dfEnrolled_multi <- data.frame(
  studyid = c(rep("Study1", 50), rep("Study2", 30)),
  siteid = c(rep(c("Site1", "Site2"), each = 25), rep(c("Site1", "Site3"), each = 15)),
  enrollyn = "Y"
)

dfAugmented_multi <- AddScreeningFailures(
  dfEnrolled = dfEnrolled_multi,
  dfSource = dfSource,
  strSiteCol = "siteid",
  strStudyCol = "studyid",
  seed = 123
)

# Each study-site gets independent screening failures
table(dfAugmented_multi$studyid, dfAugmented_multi$siteid)
#>         
#>          Site1 Site2 Site3
#>   Study1    38    31     0
#>   Study2    30     0    30
```
