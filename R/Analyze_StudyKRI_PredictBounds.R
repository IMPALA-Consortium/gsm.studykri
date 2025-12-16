#' Calculate Confidence Intervals from Bootstrap Distribution (Internal)
#'
#' @description
#' Internal helper function that calculates percentile-based confidence intervals 
#' from bootstrap distributions of study-level KRI metrics. Returns median estimates 
#' with upper and lower confidence bounds for each time point. Supports both 
#' single-study and multi-study comparison scenarios with full dbplyr compatibility.
#' Auto-detects multiple Metric columns (e.g., `Metric_Analysis_kri0001`) and
#' calculates bounds for each.
#'
#' This function is intended for internal use. For the main workflow, use
#' `Analyze_StudyKRI_PredictBounds()` which calls this function internally.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' @importFrom stats quantile median
#'
#' @param dfInput data.frame or tbl. Bootstrapped study-level data from
#'   `Transform_CumCount` with `BootstrapRep` column. Must contain columns:
#'   `vBy` columns, `StudyMonth`, one or more `Metric_*` columns, `BootstrapRep`.
#' @param vBy character. Vector of column names for grouping CI calculation.
#'   Use `"StudyID"` for single study analysis, `character(0)` for combined
#'   multi-study analysis. Default: `"StudyID"`.
#' @param nConfLevel numeric. Confidence level between 0 and 1. Default: 0.95
#'   (95% confidence interval).
#' @param strStudyMonthCol character. Name of study month column.
#'   Default: `"StudyMonth"`.
#'
#' @return A tibble (or tbl_lazy if input was lazy) with columns:
#'   - `vBy` columns (if `length(vBy) > 0`)
#'   - `StudyMonth`: Sequential month number
#'   - `Median_*`, `Lower_*`, `Upper_*`: Bounds for each Metric column
#'     (e.g., `Median_Analysis_kri0001`, `Lower_Analysis_kri0001`, `Upper_Analysis_kri0001`)
#'   - `BootstrapCount`: Number of bootstrap samples used
#'
#' @keywords internal
#' @noRd
CalculateStudyBounds <- function(
    dfInput,
    vBy = "StudyID",
    nConfLevel = 0.95,
    strStudyMonthCol = "StudyMonth") {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data.frame or tbl object")
  }

  # Auto-detect Metric columns (matches both "Metric" and "Metric_*")
  vMetricCols <- grep("^Metric", colnames(dfInput), value = TRUE)
  if (length(vMetricCols) == 0) {
    stop("dfInput must have at least one Metric column")
  }

  # Check required columns (use colnames for lazy table compatibility)
  required_cols <- c("BootstrapRep", strStudyMonthCol)
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

  # Calculate summary statistics across bootstrap replicates for all Metric columns
  dfBounds <- dfInput %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::all_of(.env$vMetricCols),
        .fns = list(
          Median = ~ median(.x, na.rm = TRUE),
          Lower = ~ quantile(.x, .env$lower_percentile, na.rm = TRUE),
          Upper = ~ quantile(.x, .env$upper_percentile, na.rm = TRUE)
        ),
        .names = "{.fn}_{gsub('^Metric_', '', .col)}"
      ),
      BootstrapCount = dplyr::n(),
      .by = dplyr::all_of(.env$group_cols)
    ) %>%
    # remove trailing metric from colnames which is the case for single metric
    dplyr::rename_with(~ stringr::str_replace(., "_Metric", ""))

  # Arrange by grouping columns and study month
  dfResult <- dfBounds %>%
    SortDf(dplyr::across(dplyr::all_of(.env$group_cols)))

  return(dfResult)
}

#' Calculate Bootstrap-Based Confidence Bounds for Study-Level KRI
#'
#' @description
#' Calculates confidence bounds for study-level KRI metrics using bootstrap resampling.
#' This is a convenience function that orchestrates the complete workflow:
#' \enumerate{
#'   \item Filters input data to target studies (specified in dfStudyRef or all studies if NULL)
#'   \item Generates bootstrap resamples via `BootstrapStudyKRI()`
#'   \item Aggregates to study level via `Transform_CumCount()`
#'   \item Calculates confidence intervals via `CalculateStudyBounds()`
#' }
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#'
#' @param dfInput data.frame or tbl_lazy. Group-level data from `Input_CumCountSiteByMonth`.
#'   Expected columns: GroupID, one or more Numerator columns, Denominator, 
#'   StudyID, MonthYYYYMM.
#' @param dfStudyRef data.frame or NULL. Optional study reference mapping. If provided,
#'   the unique values in the first column identify the target studies for which
#'   bounds will be calculated. If NULL (default), bounds are calculated for all
#'   studies found in dfInput.
#' @param nBootstrapReps integer. Number of bootstrap replicates to generate.
#'   Default: 1000.
#' @param nConfLevel numeric. Confidence level between 0 and 1. Default: 0.95
#'   (95% confidence interval).
#' @param nMinDenominator numeric. Minimum cumulative denominator threshold for
#'   Transform_CumCount filtering. Default: 25.
#' @param seed integer or NULL. Random seed for reproducibility. Default: NULL.
#' @param tblBootstrapReps tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated bootstrap replicate indices. Default: NULL.
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated complete month sequences. Default: NULL.
#' @param strStudyCol character. Column name for study identifier in dfInput. 
#'   Default: "StudyID".
#' @param strGroupCol character. Column name for group identifier. Default: "GroupID".
#' @param strStudyMonthCol character. Name of study month column. Default: "StudyMonth".
#'
#' @return A tibble (or tbl_lazy if input was lazy) with confidence intervals:
#'   - `StudyID`: Target study identifier
#'   - `StudyMonth`: Sequential month number
#'   - `Median_*`, `Lower_*`, `Upper_*`: Bounds for each Metric column
#'   - `BootstrapCount`: Number of bootstrap samples used
#'
#' @examples
#' \dontrun{
#' # Example 1: Calculate bounds for all studies in dfInput
#' Bounds_Wide <- purrr::map(
#'   lJoined, 
#'   ~ Analyze_StudyKRI_PredictBounds(.)
#' )
#' 
#' # Example 2: Calculate bounds for specific studies using reference mapping
#' dfStudyRef <- data.frame(
#'   study = c("AA-1", "AA-1", "AA-2", "AA-2"),
#'   studyref = c("AA-3", "AA-4", "AA-3", "AA-5")
#' )
#'
#' # Calculate bounds for target studies (AA-1 and AA-2) specified in first column
#' Bounds_Wide <- purrr::map(
#'   lJoined, 
#'   ~ Analyze_StudyKRI_PredictBounds(., dfStudyRef = dfStudyRef)
#' )
#' }
#'
#' @export
Analyze_StudyKRI_PredictBounds <- function(
    dfInput,
    dfStudyRef = NULL,
    nBootstrapReps = 1000,
    nConfLevel = 0.95,
    nMinDenominator = 25,
    seed = NULL,
    tblBootstrapReps = NULL,
    tblMonthSequence = NULL,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    strStudyMonthCol = "StudyMonth") {
  
  # Handle dfStudyRef - extract target studies
  if (is.null(dfStudyRef)) {
    # Extract all unique study IDs from input
    vTargetStudies <- dfInput %>%
      dplyr::select(dplyr::all_of(.env$strStudyCol)) %>%
      dplyr::distinct() %>%
      dplyr::pull(.env$strStudyCol)
    
    if (length(vTargetStudies) == 0) {
      stop("No studies found in dfInput")
    }
    
    message(sprintf(
      "Using all %d studies found in dfInput",
      length(vTargetStudies)
    ))
  } else {
    # Input validation for dfStudyRef
    if (!is.data.frame(dfStudyRef)) {
      stop("dfStudyRef must be a data.frame or NULL")
    }
    
    if (ncol(dfStudyRef) == 0) {
      stop("dfStudyRef must have at least one column")
    }
    
    # Extract target studies from first column of dfStudyRef
    strStudyRefCol <- colnames(dfStudyRef)[1]
    vTargetStudies <- unique(dfStudyRef[[strStudyRefCol]])
    
    if (length(vTargetStudies) == 0) {
      stop("No target studies found in dfStudyRef")
    }
  }
  
  # Filter input data to target studies only
  dfFiltered <- dfInput %>%
    dplyr::filter(.data[[strStudyCol]] %in% .env$vTargetStudies)
  
  # Step 1: Generate bootstrap resamples
  dfBootstrapped <- BootstrapStudyKRI(
    dfInput = dfFiltered,
    nBootstrapReps = nBootstrapReps,
    nGroups = NULL,
    strStudyCol = strStudyCol,
    strGroupCol = strGroupCol,
    seed = seed,
    tblBootstrapReps = tblBootstrapReps
  )
  
  # Step 2: Aggregate to study level by bootstrap replicate and study
  dfStudyLevel <- Transform_CumCount(
    dfInput = dfBootstrapped,
    vBy = c("BootstrapRep", strStudyCol),
    nMinDenominator = nMinDenominator,
    tblMonthSequence = tblMonthSequence
  )
  
  # Step 3: Calculate confidence intervals
  dfBounds <- CalculateStudyBounds(
    dfInput = dfStudyLevel,
    vBy = strStudyCol,
    nConfLevel = nConfLevel,
    strStudyMonthCol = strStudyMonthCol
  )
  
  return(dfBounds)
}
