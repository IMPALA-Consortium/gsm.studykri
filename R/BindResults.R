#' Helper function to bind results from multiple workflows
#'
#' @description
#'
#' Used to stack results (e.g. `dfSummary`) from a list of analysis pipeline
#' output formatted like the result of `RunWorkflows()`. Also adds study level
#' metadata when provided.
#'
#' @param lAnalysis Named List of analysis results in the format returned by
#'   [gsm.core::RunWorkflows()].
#' @param strName Name of the object to stack. Pulled from `lAnalysis` (or from
#'   `lAnalysis$lData` when `bUselData` is `TRUE`).
#' @param dSnapshotDate Date of the snapshot. Default is [Sys.Date()].
#' @param strStudyID Study ID.
#' @param bUselData Should the function bind results from an `lData` object (or
#'   look directly in the root elements of `lAnalysis`)? Default is `FALSE.`
#'
#' @return A data frame.
#'
#' @export

BindResults <- function(
    lAnalysis,
    strName,
    dSnapshotDate = Sys.Date(),
    strStudyID = NULL,
    bUselData = FALSE) {
  lResults <- lAnalysis %>%
    purrr::imap(
      function(result, metric) {
        if (bUselData) {
          subResult <- result$lData[[strName]]
        } else {
          subResult <- result[[strName]]
        }
        return(subResult %>% dplyr::mutate(MetricID = metric))
      }
    )
  dfResults <- Reduce(dplyr::union_all, lResults)

  dfResults <- dplyr::mutate(dfResults, SnapshotDate = as.Date(dSnapshotDate))

  if (!is.null(strStudyID)) {
    dfResults <- dplyr::mutate(dfResults, StudyID = strStudyID)
  }

  return(dfResults)
}
