# Create Study KRI Comparison Charts

Generates ggplot2 charts comparing study-level KRIs against comparison
groups. Similar to gsm.kri::MakeCharts but uses Visualize_StudyKRI for
study comparisons.

## Usage

``` r
MakeCharts_StudyKRI(
  dfResults,
  dfBounds,
  dfBoundsRef,
  dfMetrics,
  nMaxMonth = NULL
)
```

## Arguments

- dfResults:

  data.frame. Stacked study-level results (from BindResults).

- dfBounds:

  data.frame. Stacked study-specific bounds (from BindResults).

- dfBoundsRef:

  data.frame. Stacked reference/comparison group bounds (from
  BindResults).

- dfMetrics:

  data.frame. Metric metadata (from MakeMetric).

- nMaxMonth:

  integer. Maximum study month to display (NULL = all).

## Value

list. Named list of ggplot objects, one per study-metric combination.
Names are in format "StudyID_MetricID" (e.g., "STUDY001_kri0001").
