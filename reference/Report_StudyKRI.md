# Generate StudyKRI reports per study (Report_KRI-compatible)

Accepts the same inputs as `gsm.reporting::Report_KRI`, but iterates
over studies inferred from `lCharts` (names formatted as
`StudyID_MetricID`) and renders one report per study. Charts are
expected to be created with
[`gsm.studykri::MakeCharts_StudyKRI`](https://impala-consortium.github.io/gsm.studykri/reference/MakeCharts_StudyKRI.md),
which already filters to studies with available reference bounds.

## Usage

``` r
Report_StudyKRI(
  lCharts,
  dfResults,
  dfGroups,
  dfMetrics,
  strOutputFile,
  strInputPath,
  ...
)
```

## Arguments

- lCharts:

  list. Output of `MakeCharts_StudyKRI` for the portfolio.

- dfResults:

  data.frame. Bound results (e.g.,
  `BindResults(..., 'Analysis_Transformed')`).

- dfGroups:

  data.frame. Group metadata to annotate the report.

- dfMetrics:

  data.frame. Metric metadata (e.g., `MakeMetric(workflows)`).

- strOutputFile:

  character. Base output HTML path. The study ID is appended before the
  extension (e.g., `report.html` -\> `report_<StudyID>.html`).

- strInputPath:

  character. Path to report template Rmd.

- ...:

  Additional parameters forwarded to
  [`gsm.kri::Report_KRI`](https://gilead-biostats.github.io/gsm.kri/reference/Report_KRI.html).

## Value

Named character vector of output file paths (names are StudyIDs).
