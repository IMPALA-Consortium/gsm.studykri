# Calculate Monthly Count by Site and Month

Calculates monthly event counts and ratios by site and month for KRI
analysis. Joins subject, numerator, and denominator data to create
monthly metrics. When strDenominatorEndDateCol is provided, calculates
days between start/end dates per month instead of counting records.
Note: This function returns monthly counts, not cumulative. Cumulative
aggregation is performed at the study level in Transform_CumCount.
Supports both in-memory data frames and dbplyr lazy tables.

## Usage

``` r
Input_CountSiteByMonth(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorDateCol,
  strDenominatorDateCol,
  strDenominatorEndDateCol = NULL,
  strDenominatorType = NULL
)
```

## Arguments

- dfSubjects:

  data.frame or tbl. Subject-level data with enrolled subjects.

- dfNumerator:

  data.frame or tbl. Event data for numerator (e.g., adverse events).

- dfDenominator:

  data.frame or tbl. Event data for denominator (e.g., visits).

- strStudyCol:

  character. Column name for study identifier (default: "studyid").

- strGroupCol:

  character. Column name for site identifier (default: "invid").

- strGroupLevel:

  character. Grouping level name (default: "Site").

- strSubjectCol:

  character. Column name for subject identifier (default: "subjid").

- strNumeratorDateCol:

  character. Date column name in numerator data.

- strDenominatorDateCol:

  character. Date column name in denominator data (start date).

- strDenominatorEndDateCol:

  character. End date column in denominator data (default: NULL). When
  provided, calculates sum of days between start/end dates per month
  instead of record counts.

- strDenominatorType:

  character. Optional denominator type label (e.g., "Visits", "Days on
  Study"). When provided, adds a DenominatorType column to the output.

## Value

data.frame with columns: GroupID, GroupLevel, Numerator, Denominator,
Metric, StudyID, MonthYYYYMM. If strDenominatorType is provided, also
includes DenominatorType column.

## Examples

``` r
dfSubjects <- clindata::rawplus_dm
dfNumerator <- clindata::rawplus_ae
dfDenominator <- clindata::rawplus_visdt

# Count-based calculation
result <- Input_CountSiteByMonth(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfDenominator,
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "visit_dt"
)

# Days-based calculation with end dates
result_days <- Input_CountSiteByMonth(
  dfSubjects = dfSubjects,
  dfNumerator = dfNumerator,
  dfDenominator = dfSubjects,
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "firstparticipantdate",
  strDenominatorEndDateCol = "lastparticipantdate"
)
```
