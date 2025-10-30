#' Generate StudyKRI reports per study (Report_KRI-compatible)
#'
#' @description
#' Accepts the same inputs as `gsm.reporting::Report_KRI`, but iterates over
#' studies inferred from `lCharts` (names formatted as `StudyID_MetricID`) and
#' renders one report per study. Charts are expected to be created with
#' `gsm.studykri::MakeCharts_StudyKRI`, which already filters to studies with
#' available reference bounds.
#'
#' @param lCharts list. Output of `MakeCharts_StudyKRI` for the portfolio.
#' @param dfResults data.frame. Bound results (e.g., `BindResults(..., 'Analysis_Transformed')`).
#' @param dfGroups data.frame. Group metadata to annotate the report.
#' @param dfMetrics data.frame. Metric metadata (e.g., `MakeMetric(workflows)`).
#' @param strOutputFile character. Base output HTML path. The study ID is
#'   appended before the extension (e.g., `report.html` -> `report_<StudyID>.html`).
#' @param strInputPath character. Path to report template Rmd.
#' @param ... Additional parameters forwarded to `gsm.kri::Report_KRI`.
#'
#' @return Named character vector of output file paths (names are StudyIDs).
#'
#' @export
Report_KRI_StudyKRI <- function(
  lCharts,
  dfResults,
  dfGroups,
  dfMetrics,
  strOutputFile,
  strInputPath,
  ...
) {
  if (!is.list(lCharts) || length(lCharts) == 0) return(character(0))

  # Extract StudyIDs from chart names formatted as "<StudyID>_<MetricID>"
  vChartNames <- names(lCharts)
  vStudyIds <- unique(sub("_.*$", "", vChartNames))

  vOutFiles <- stats::setNames(rep("", length(vStudyIds)), vStudyIds)

  # Helper to build per-study output filename
  add_suffix <- function(path, study) {
    ext_pos <- regexpr("\\.[^.]*$", path)
    if (ext_pos[1] > 0) {
      paste0(substr(path, 1, ext_pos[1] - 1), "_", study, substr(path, ext_pos[1], nchar(path)))
    } else {
      paste0(path, "_", study, ".html")
    }
  }

  for (study in vStudyIds) {
    # Subset charts for this study
    study_prefix <- paste0(study, "_")
    lCharts_study <- lCharts[startsWith(names(lCharts), study_prefix)]
    if (length(lCharts_study) == 0) next

    # Subset results for this study if column exists
    if ("StudyID" %in% names(dfResults)) {
      dfResults_study <- dfResults[dfResults$StudyID == study, , drop = FALSE]
    } else {
      dfResults_study <- dfResults
    }

    # Compose per-study output file
    out_file <- add_suffix(strOutputFile, study)

    # Delegate to gsm.kri::Report_KRI
    gsm.kri::Report_KRI(
      lCharts = lCharts_study,
      dfResults = dfResults_study,
      dfGroups = dfGroups,
      dfMetrics = dfMetrics,
      strOutputFile = out_file,
      strInputPath = strInputPath,
      ...
    )

    vOutFiles[[study]] <- out_file
  }

  vOutFiles[nzchar(vOutFiles)]
}


