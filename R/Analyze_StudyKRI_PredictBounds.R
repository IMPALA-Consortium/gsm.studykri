#' Calculate Confidence Intervals from Bootstrap Distribution
#'
#' @description
#' Calculates percentile-based confidence intervals from bootstrap distributions
#' of study-level KRI metrics. Returns median estimates with upper and lower
#' confidence bounds for each time point. Supports both single-study and
#' multi-study comparison scenarios with full dbplyr compatibility.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' @importFrom stats quantile median
#'
#' @param dfInput data.frame or tbl. Bootstrapped study-level data from
#'   `Transform_CumCount` with `BootstrapRep` column. Must contain columns:
#'   `vBy` columns, `StudyMonth`, `Metric`, `BootstrapRep`.
#' @param vBy character. Vector of column names for grouping CI calculation.
#'   Use `"StudyID"` for single study analysis, `character(0)` for combined
#'   multi-study analysis. Default: `"StudyID"`.
#' @param nConfLevel numeric. Confidence level between 0 and 1. Default: 0.95
#'   (95% confidence interval).
#' @param strMetricCol character. Name of metric column. Default: `"Metric"`.
#' @param strStudyMonthCol character. Name of study month column.
#'   Default: `"StudyMonth"`.
#'
#' @return A data.frame (or tbl_lazy if input was lazy) with columns:
#'   - `vBy` columns (if `length(vBy) > 0`)
#'   - `StudyMonth`: Sequential month number
#'   - `MedianMetric`: Median metric value across bootstrap samples
#'   - `LowerBound`: Lower confidence bound
#'   - `UpperBound`: Upper confidence bound
#'   - `BootstrapCount`: Number of bootstrap samples used
#'
#' @examples
#' \dontrun{
#' # Single study analysis
#' dfBootstrap <- Analyze_StudyKRI(
#'   dfInput = dfInput,
#'   nBootstrapReps = 1000,
#'   seed = 42
#' )
#'
#' dfBootstrapStudy <- Transform_CumCount(
#'   dfInput = dfBootstrap,
#'   vBy = c("StudyID", "BootstrapRep"),
#'   nMinDenominator = 25
#' )
#'
#' dfBounds <- Analyze_StudyKRI_PredictBounds(
#'   dfInput = dfBootstrapStudy,
#'   vBy = "StudyID",
#'   nConfLevel = 0.95
#' )
#'
#' # Multi-study comparison (combined)
#' dfComparison <- dfBootstrapStudy %>%
#'   dplyr::filter(StudyID %in% c("STUDY1", "STUDY2", "STUDY3"))
#'
#' dfGroupBounds <- Analyze_StudyKRI_PredictBounds(
#'   dfInput = dfComparison,
#'   vBy = character(0),  # Combine all studies
#'   nConfLevel = 0.95
#' )
#' }
#'
#' @export
Analyze_StudyKRI_PredictBounds <- function(
  dfInput,
  vBy = "StudyID",
  nConfLevel = 0.95,
  strMetricCol = "Metric",
  strStudyMonthCol = "StudyMonth"
) {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data.frame or tbl object")
  }
  
  # Check required columns (use colnames for lazy table compatibility)
  required_cols <- c("BootstrapRep", strMetricCol, strStudyMonthCol)
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # Check vBy columns if specified
  if (length(vBy) > 0) {
    missing_by <- setdiff(vBy, colnames(dfInput))
    if (length(missing_by) > 0) {
      stop(sprintf(
        "vBy columns not found in dfInput: %s",
        paste(missing_by, collapse = ", ")
      ))
    }
  }
  
  # Validate confidence level
  if (!is.numeric(nConfLevel) || length(nConfLevel) != 1 || 
      nConfLevel <= 0 || nConfLevel >= 1) {
    stop("nConfLevel must be a single numeric value between 0 and 1")
  }
  
  # Calculate lower and upper percentiles based on confidence level
  lower_percentile <- (1 - nConfLevel) / 2
  upper_percentile <- 1 - lower_percentile
  
  # Build grouping columns (vBy + StudyMonth)
  group_cols <- if (length(vBy) > 0) {
    c(vBy, strStudyMonthCol)
  } else {
    strStudyMonthCol
  }
  
  # Calculate summary statistics across bootstrap replicates
  dfBounds <- dfInput %>%
    dplyr::summarise(
      MedianMetric = median(.data[[strMetricCol]], na.rm = TRUE),
      LowerBound = quantile(.data[[strMetricCol]], .env$lower_percentile, na.rm = TRUE),
      UpperBound = quantile(.data[[strMetricCol]], .env$upper_percentile, na.rm = TRUE),
      BootstrapCount = dplyr::n(),
      .by = dplyr::all_of(.env$group_cols)
    )
  
  # Arrange by grouping columns and study month
  dfResult <- dfBounds %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(.env$group_cols)))
  
  # Return lazy table if input was lazy, data.frame otherwise
  if (inherits(dfInput, "tbl_lazy")) {
    return(dfResult)
  } else {
    return(as.data.frame(dfResult))
  }
}

