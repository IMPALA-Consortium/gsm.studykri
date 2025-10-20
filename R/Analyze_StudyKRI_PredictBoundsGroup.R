#' Predict Bounds for Combined Group of Studies
#'
#' @description
#' Calculates percentile-based confidence intervals (bounds) for a combined portfolio
#' of comparison studies. This function creates a "comparison envelope" by:
#' \enumerate{
#'   \item Finding the minimum group (site) count across selected studies
#'   \item Resampling each study with that minimum to ensure fair comparison
#'   \item Combining data across all studies
#'   \item Calculating confidence intervals for the pooled distribution
#' }
#'
#' This enables comparing one study's KRI against expected variation from a portfolio
#' of similar studies.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#'
#' @param dfInput data.frame or tbl_lazy. Site-level data from `Input_CumCountSiteByMonth`.
#'   Must contain columns: `StudyID`, `GroupID`, `Numerator`, `Denominator`, `MonthYYYYMM`.
#' @param vStudyFilter character. Study IDs to include in comparison group
#'   (e.g., `c("STUDY1", "STUDY2", "STUDY3")`).
#' @param nBootstrapReps integer. Number of bootstrap replicates (default: 1000).
#' @param nConfLevel numeric. Confidence level for the bounds, between 0 and 1
#'   (default: 0.95 for 95% CI).
#' @param strStudyCol character. Column name for study identifier (default: "StudyID").
#' @param strGroupCol character. Column name for group identifier (default: "GroupID").
#' @param strStudyMonthCol character. Column name for sequential study month
#'   (default: "StudyMonth").
#' @param strMetricCol character. Column name for metric (default: "Metric").
#' @param nMinDenominator numeric. Minimum denominator for Transform_CumCount
#'   (default: 25).
#' @param seed integer or NULL. Random seed for reproducibility (default: NULL).
#'
#' @return A data.frame (or tbl_lazy if input was lazy) with confidence intervals.
#'   Output columns: `StudyMonth`, `MedianMetric`, `LowerBound`, `UpperBound`,
#'   `BootstrapCount`, `GroupCount`, `StudyCount`.
#'   Note: No `StudyID` column as studies are intentionally combined.
#'
#' @examples
#' # Create example site-level data for multiple studies
#' dfSiteLevel <- data.frame(
#'   StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 60),
#'   GroupID = rep(paste0("Site", 1:10), each = 6, times = 3),
#'   Numerator = sample(0:5, 180, replace = TRUE),
#'   Denominator = sample(10:20, 180, replace = TRUE),
#'   MonthYYYYMM = rep(rep(202301:202306, each = 10), times = 3),
#'   Metric = runif(180, 0.1, 0.5),
#'   GroupLevel = "Site",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate comparison envelope from 3 studies
#' dfGroupBounds <- Analyze_StudyKRI_PredictBoundsGroup(
#'   dfInput = dfSiteLevel,
#'   vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
#'   nBootstrapReps = 100,  # Use small number for example
#'   nConfLevel = 0.95,
#'   seed = 42
#' )
#'
#' print(head(dfGroupBounds))
#'
#' @export
Analyze_StudyKRI_PredictBoundsGroup <- function(
  dfInput,
  vStudyFilter,
  nBootstrapReps = 1000,
  nConfLevel = 0.95,
  strStudyCol = "StudyID",
  strGroupCol = "GroupID",
  strStudyMonthCol = "StudyMonth",
  strMetricCol = "Metric",
  nMinDenominator = 25,
  seed = NULL
) {
  # Input Validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data.frame or tbl object")
  }

  # Check required columns
  required_cols <- c(strStudyCol, strGroupCol, "Numerator", "Denominator", "MonthYYYYMM")
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Validate vStudyFilter
  if (!is.character(vStudyFilter) || length(vStudyFilter) == 0) {
    stop("vStudyFilter must be a non-empty character vector")
  }

  # Validate parameters
  if (!is.numeric(nBootstrapReps) || length(nBootstrapReps) != 1 || nBootstrapReps < 1) {
    stop("nBootstrapReps must be a positive integer")
  }
  nBootstrapReps <- as.integer(nBootstrapReps)

  if (!is.numeric(nConfLevel) || length(nConfLevel) != 1 || nConfLevel <= 0 || nConfLevel >= 1) {
    stop("nConfLevel must be between 0 and 1")
  }

  if (!is.numeric(nMinDenominator) || length(nMinDenominator) != 1 || nMinDenominator < 0) {
    stop("nMinDenominator must be a single non-negative numeric value")
  }

  # Helper to detect lazy tables
  is_lazy_table <- function(x) {
    inherits(x, "tbl_lazy")
  }

  # Filter to specified studies
  dfFiltered <- dfInput %>%
    dplyr::filter(.data[[strStudyCol]] %in% .env$vStudyFilter)

  # Verify studies exist (only for in-memory data frames)
  if (is.data.frame(dfFiltered) && nrow(dfFiltered) == 0) {
    stop("No data found for specified studies in vStudyFilter")
  }

  # Count unique groups per study
  dfGroupCounts <- dfFiltered %>%
    dplyr::summarise(
      GroupCount = dplyr::n_distinct(.data[[strGroupCol]]),
      .by = dplyr::all_of(.env$strStudyCol)
    )

  # Find minimum across studies
  # For lazy tables, collect just the counts
  if (is_lazy_table(dfGroupCounts)) {
    dfGroupCounts_mem <- dplyr::collect(dfGroupCounts)
    nMinGroups <- min(dfGroupCounts_mem$GroupCount)
  } else {
    nMinGroups <- min(dfGroupCounts$GroupCount)
  }

  # Inform user
  message(sprintf("Resampling with minimum group count: %d", nMinGroups))

  # Resample each study independently with nGroups = nMinGroups
  dfBootstrapped <- Analyze_StudyKRI(
    dfInput = dfFiltered,
    nBootstrapReps = nBootstrapReps,
    nGroups = nMinGroups,  # Key: use minimum to ensure fair comparison
    strStudyCol = strStudyCol,
    strGroupCol = strGroupCol,
    seed = seed
  )

  # Transform to study-level, but group by BootstrapRep only (not StudyID)
  # This combines all studies into a single distribution per bootstrap replicate
  dfStudyLevel <- Transform_CumCount(
    dfInput = dfBootstrapped,
    vBy = "BootstrapRep",  # Critical: only group by BootstrapRep, not StudyID
    nMinDenominator = nMinDenominator
  )

  # Calculate CI for the combined distribution
  dfBounds <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfStudyLevel,
    vBy = character(0),  # No additional grouping
    nConfLevel = nConfLevel,
    strMetricCol = strMetricCol,
    strStudyMonthCol = strStudyMonthCol
  )

  # Add metadata
  dfResult <- dfBounds %>%
    dplyr::mutate(
      GroupCount = .env$nMinGroups,
      StudyCount = length(.env$vStudyFilter)
    )

  # Return lazy table if input was lazy, data.frame otherwise
  if (is_lazy_table(dfInput)) {
    return(dfResult)
  } else {
    return(as.data.frame(dfResult))
  }
}

