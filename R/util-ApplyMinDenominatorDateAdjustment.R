#' Apply Date Adjustment for Minimum Denominator Threshold
#'
#' @description
#' Internal utility function that adjusts dates for the first N denominator events
#' to create a consistent study start date across studies and bootstrap replicates.
#' Also adjusts corresponding numerator dates to maintain temporal relationships.
#'
#' @param dfNumerator_processed Data frame with processed numerator data (with MonthYYYYMM)
#' @param dfDenominator Data frame with original denominator data
#' @param dfSubjects Data frame with subject-level data for joining
#' @param strSubjectCol Name of subject ID column
#' @param strStudyCol Name of study ID column
#' @param strGroupCol Name of group ID column (e.g., site)
#' @param strNumeratorDateCol Name of numerator date column
#' @param strDenominatorDateCol Name of denominator date column
#' @param strDenominatorEndDateCol Optional name of denominator end date column
#' @param nMinDenominator Minimum denominator threshold for date adjustment
#'
#' @return List with two elements:
#'   - dfNumerator: Adjusted numerator data with updated MonthYYYYMM
#'   - dfDenominator: Adjusted denominator data with updated date columns
#'
#' @keywords internal
ApplyMinDenominatorDateAdjustment <- function(
    dfNumerator_processed,
    dfDenominator,
    dfSubjects,
    strSubjectCol,
    strStudyCol,
    strGroupCol,
    strNumeratorDateCol,
    strDenominatorDateCol,
    strDenominatorEndDateCol,
    nMinDenominator) {
  
  # Step 1: Join denominator with subjects to get study information
  dfDenom_with_study <- dfDenominator %>%
    dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
    dplyr::inner_join(
      dfSubjects %>% dplyr::select(dplyr::all_of(c(strSubjectCol, strStudyCol, strGroupCol))),
      by = stats::setNames(strSubjectCol, strSubjectCol)
    ) %>%
    dplyr::filter(!is.na(.data[[strDenominatorDateCol]]))
  
  # Step 2: Rank denominator events by start date within each study
  dfDenom_ranked <- dfDenom_with_study %>%
    SortDf(.data[[strStudyCol]], .data[[strDenominatorDateCol]]) %>%
    dplyr::mutate(
      denom_rank = dplyr::row_number(),
      .by = dplyr::all_of(.env$strStudyCol)
    )
  
  # Step 3: Find threshold date (MAX date of first N events) per study
  dfThresholdDates <- dfDenom_ranked %>%
    dplyr::filter(.data$denom_rank <= .env$nMinDenominator) %>%
    dplyr::summarise(
      threshold_date = max(.data[[strDenominatorDateCol]], na.rm = TRUE),
      .by = dplyr::all_of(.env$strStudyCol)
    )
  
  # Step 4: Calculate offset and adjust start dates
  dfDenom_adjusted <- dfDenom_ranked %>%
    dplyr::left_join(dfThresholdDates, by = strStudyCol) %>%
    dplyr::mutate(
      # Calculate offset: days to shift forward
      date_offset_days = dplyr::if_else(
        .data[[strDenominatorDateCol]] <= .data$threshold_date,
        as.numeric(.data$threshold_date - .data[[strDenominatorDateCol]]),
        0
      ),
      # Apply offset to start date
      adjusted_start_date = .data[[strDenominatorDateCol]] + as.integer(.data$date_offset_days)
    )
  
  # Step 5: If end date exists, apply same offset to preserve duration
  if (!is.null(strDenominatorEndDateCol)) {
    dfDenom_adjusted <- dfDenom_adjusted %>%
      dplyr::mutate(
        # Apply same offset to end date
        adjusted_end_date = .data[[strDenominatorEndDateCol]] + as.integer(.data$date_offset_days)
      )
  }
  
  # Step 6: Apply adjusted dates back to original denominator data
  # Replace original denominator with adjusted version for date calculations
  dfDenominator <- dfDenom_adjusted %>%
    dplyr::mutate(
      !!strDenominatorDateCol := .data$adjusted_start_date
    )
  
  if (!is.null(strDenominatorEndDateCol)) {
    dfDenominator <- dfDenominator %>%
      dplyr::mutate(
        !!strDenominatorEndDateCol := .data$adjusted_end_date
      )
  }
  
  # Clean up temporary columns
  dfDenominator <- dfDenominator %>%
    dplyr::select(-dplyr::any_of(c("denom_rank", "threshold_date", "date_offset_days", 
                                    "adjusted_start_date", "adjusted_end_date")))
  
  # Step 7: Adjust numerator dates to align with denominator
  # Use study-level threshold date instead of joining all denominator events
  dfNum_processed <- dfNumerator_processed %>%
    dplyr::left_join(
      dfThresholdDates,
      by = strStudyCol
    ) %>%
    dplyr::mutate(
      # Calculate offset for numerator events that fall on or before threshold
      offset_to_apply = dplyr::if_else(
        !is.na(.data$threshold_date) & 
          .data[[strNumeratorDateCol]] <= .data$threshold_date,
        as.numeric(.data$threshold_date - .data[[strNumeratorDateCol]]),
        0
      ),
      # Apply offset
      adjusted_num_date = .data[[strNumeratorDateCol]] + as.integer(.data$offset_to_apply),
      # Recalculate MonthYYYYMM from adjusted date
      year = lubridate::year(.data$adjusted_num_date),
      month = lubridate::month(.data$adjusted_num_date),
      MonthYYYYMM = .data$year * 100 + .data$month
    ) %>%
    dplyr::select(-dplyr::any_of(c("threshold_date", "offset_to_apply", 
                                    "adjusted_num_date", "year", "month")))
  
  return(list(
    dfNumerator = dfNum_processed,
    dfDenominator = dfDenominator
  ))
}
