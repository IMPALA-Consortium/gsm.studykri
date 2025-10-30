#' Create Study KRI Comparison Charts
#'
#' @description
#' Generates ggplot2 charts comparing study-level KRIs against comparison groups.
#' Similar to gsm.kri::MakeCharts but uses Visualize_StudyKRI for study comparisons.
#'
#' @param dfResults data.frame. Stacked study-level results (from BindResults).
#' @param dfBounds data.frame. Stacked study-specific bounds (from BindResults).
#' @param dfBoundsRef data.frame. Stacked reference/comparison group bounds (from BindResults).
#' @param dfMetrics data.frame. Metric metadata (from MakeMetric).
#' @param nMaxMonth integer. Maximum study month to display (NULL = all).
#'
#' @return list. Named list of ggplot objects, one per study-metric combination.
#'   Names are in format "StudyID_MetricID" (e.g., "STUDY001_kri0001").
#'
#' @importFrom rlang .data
#'
#' @export
MakeCharts_StudyKRI <- function(
  dfResults,
  dfBounds,
  dfBoundsRef,
  dfMetrics,
  nMaxMonth = NULL
) {
  # Validate inputs
  if (!is.data.frame(dfResults) || nrow(dfResults) == 0) {
    stop("dfResults must be a non-empty data frame")
  }
  
  required_cols <- c("MetricID", "StudyID", "StudyMonth", "Metric")
  missing_cols <- setdiff(required_cols, names(dfResults))
  if (length(missing_cols) > 0) {
    stop(sprintf("dfResults missing columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  # Get unique study-metric combinations
  dfCombinations <- dfResults %>%
    dplyr::distinct(.data$StudyID, .data$MetricID)
  
  # Initialize list for charts
  lCharts <- list()
  
  # Generate chart for each study-metric combination
  for (i in seq_len(nrow(dfCombinations))) {
    study_id <- dfCombinations$StudyID[i]
    metric_id <- dfCombinations$MetricID[i]
    
    # Filter data for this combination
    df_study_kri <- dfResults %>%
      dplyr::filter(.data$StudyID == study_id, .data$MetricID == metric_id)
    
    df_study_bounds <- dfBounds %>%
      dplyr::filter(.data$StudyID == study_id, .data$MetricID == metric_id)
    
    df_boundsref <- dfBoundsRef %>%
      dplyr::filter(.data$StudyID == study_id, .data$MetricID == metric_id)
    
    # If no reference bounds for this study, skip (e.g., reference-only studies)
    if (nrow(df_boundsref) == 0) {
      next  # Skip to next study
    }
    
    # Get metric name for y-axis label
    metric_row <- dfMetrics[dfMetrics$MetricID == metric_id, ]
    metric_name <- if (nrow(metric_row) > 0 && "Metric" %in% names(metric_row)) {
      metric_row$Metric[1]
    } else {
      metric_id
    }
    
    # Generate plot
    chart_name <- paste(study_id, metric_id, sep = "_")
    
    tryCatch({
      lCharts[[chart_name]] <- Visualize_StudyKRI(
        dfStudyKRI = df_study_kri,
        dfBoundsRef = df_boundsref,
        dfBounds = df_study_bounds,
        strStudyID = study_id,
        strYlab = metric_name,
        nMaxMonth = nMaxMonth
      )
    }, error = function(e) {
      warning(sprintf("Failed to create chart for %s: %s", chart_name, e$message))
      lCharts[[chart_name]] <<- NULL
    })
  }
  
  return(lCharts)
}

