#' Transform Cumulative Counts to Study Level
#'
#' @description
#' Transforms site-level cumulative counts into study-level metrics by aggregating
#' across groups (sites/countries) by calendar month. Creates sequential study months,
#' applies minimum denominator filtering, and calculates cumulative metrics.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#'
#' @param dfInput data.frame. Output from Input_CumCountSiteByMonth with columns:
#'   GroupID, GroupLevel, Numerator, Denominator, Metric, StudyID, MonthYYYYMM.
#' @param vBy character. Vector of column names for grouping (e.g., "StudyID" or
#'   c("StudyID", "BootstrapRep")).
#' @param nMinDenominator numeric. Minimum cumulative denominator threshold for
#'   filtering early/sparse data (default: 25).
#'
#' @return A data.frame with study-level aggregated cumulative counts and ratios.
#'   Output columns: `vBy` columns, `MonthYYYYMM`, `StudyMonth`, `Numerator`,
#'   `Denominator`, `Metric`, `GroupCount`.
#'   Note: `Numerator` and `Denominator` are cumulative sums. `Metric` is the ratio.
#'
#' @examples
#' # Generate input data
#' dfSubjects <- clindata::rawplus_dm
#' dfNumerator <- clindata::rawplus_ae
#' dfDenominator <- clindata::rawplus_visdt
#'
#' dfInput <- Input_CumCountSiteByMonth(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "visit_dt"
#' )
#'
#' # Transform to study level
#' dfTransformed <- Transform_CumCount(
#'   dfInput = dfInput,
#'   vBy = "StudyID",
#'   nMinDenominator = 25
#' )
#'
#' print(dfTransformed)
#'
#' @export
Transform_CumCount <- function(
  dfInput,
  vBy,
  nMinDenominator = 25
) {
  # Input validation
  if (!is.data.frame(dfInput)) {
    stop("dfInput must be a data frame")
  }
  
  required_cols <- c("GroupID", "Numerator", "Denominator", "MonthYYYYMM")
  missing_cols <- setdiff(required_cols, names(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  if (!is.character(vBy) || length(vBy) == 0) {
    stop("vBy must be a non-empty character vector")
  }
  
  missing_by <- setdiff(vBy, names(dfInput))
  if (length(missing_by) > 0) {
    stop(sprintf(
      "vBy columns not found in dfInput: %s",
      paste(missing_by, collapse = ", ")
    ))
  }
  
  if (!is.numeric(nMinDenominator) || length(nMinDenominator) != 1 || nMinDenominator < 0) {
    stop("nMinDenominator must be a single non-negative numeric value")
  }
  
  if (nrow(dfInput) == 0) {
    stop("dfInput has no rows")
  }
  
  # Filter out rows with NA MonthYYYYMM before processing
  dfFiltered_input <- dfInput %>%
    dplyr::filter(!is.na(.data$MonthYYYYMM))
  
  if (nrow(dfFiltered_input) == 0) {
    warning("All rows have NA MonthYYYYMM, returning empty data frame")
    return(data.frame(
      stringsAsFactors = FALSE
    )[0, c(vBy, "MonthYYYYMM", "StudyMonth", "Numerator", 
           "Denominator", "Metric", "GroupCount")]
    )
  }
  
  # Create initial StudyMonth by ranking calendar months within groups
  dfWithStudyMonth <- dfFiltered_input %>%
    dplyr::mutate(
      StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
      .by = dplyr::all_of(.env$vBy)
    )
  
  # Aggregate to study level by grouping columns and StudyMonth
  dfAggregated <- dfWithStudyMonth %>%
    dplyr::summarise(
      MonthYYYYMM = min(.data$MonthYYYYMM),  # Keep the calendar month
      Numerator = sum(.data$Numerator, na.rm = TRUE),
      Denominator = sum(.data$Denominator, na.rm = TRUE),
      GroupCount = dplyr::n_distinct(.data$GroupID),
      .by = c(dplyr::all_of(.env$vBy), "StudyMonth")
    )
  
  # Apply minimum denominator filter
  dfFiltered <- dfAggregated %>%
    dplyr::filter(.data$Denominator > .env$nMinDenominator)
  
  # Re-rank StudyMonth after filtering to ensure sequential numbering
  dfReranked <- dfFiltered %>%
    dplyr::mutate(
      StudyMonth = dplyr::dense_rank(.data$StudyMonth),
      .by = dplyr::all_of(.env$vBy)
    )
  
  # Calculate metric
  dfResult <- dfReranked %>%
    dplyr::mutate(
      Metric = dplyr::if_else(
        .data$Denominator > 0,
        .data$Numerator / .data$Denominator,
        NA_real_
      )
    ) %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth)
  
  # Select final columns in desired order
  final_cols <- c(vBy, "MonthYYYYMM", "StudyMonth", "Numerator", 
                  "Denominator", "Metric", "GroupCount")
  
  dfFinal <- dfResult %>%
    dplyr::select(dplyr::all_of(.env$final_cols))
  
  return(as.data.frame(dfFinal))
}

