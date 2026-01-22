#' Join KRI Analysis_Input by Denominator Type
#'
#' @description
#' Groups bound Analysis_Input data by denominator type and pivots numerators
#' into separate columns for shared bootstrap processing.
#'
#' This function validates that all KRIs sharing the same DenominatorType have
#' identical AccrualThreshold values. This is critical because different
#' AccrualThreshold values cause different Denominator calculations due to date
#' adjustment logic in Input_CountSiteByMonth, which would lead to incorrect
#' results when pivoting the data.
#'
#' @param dfInput data.frame. Bound Analysis_Input from BindResults.
#'   Must contain columns: MetricID, GroupID, GroupLevel, Numerator, Denominator,
#'   StudyID, MonthYYYYMM, and DenominatorType.
#' @param dfMetrics data.frame. Metrics metadata from MakeMetric.
#'   Must contain columns: MetricID and AccrualThreshold.
#'
#' @return Named list with one tibble per denominator type. Each tibble
#'   has common columns plus Numerator columns renamed by MetricID.
#'
#' @examples
#' dfInput <- data.frame(
#'   MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
#'   GroupID = c("Site1", "Site2", "Site1", "Site2"),
#'   GroupLevel = "Site",
#'   Numerator = c(5, 3, 2, 1),
#'   Denominator = c(100, 80, 100, 80),
#'   StudyID = "AA-1",
#'   MonthYYYYMM = 202301,
#'   DenominatorType = "Visit"
#' )
#'
#' dfMetrics <- data.frame(
#'   MetricID = c("kri0001", "kri0003"),
#'   AccrualThreshold = c(180, 180)
#' )
#' 
#' lJoined <- JoinKRIByDenominator(dfInput, dfMetrics)
#' 
#' names(lJoined)
#'
#' # Example of error condition (different AccrualThreshold for same DenominatorType)
#' \dontrun{
#' dfMetrics_mismatched <- data.frame(
#'   MetricID = c("kri0001", "kri0003"),
#'   AccrualThreshold = c(180, 25)  # Different values!
#' )
#' 
#' # This will throw an error:
#' lJoined <- JoinKRIByDenominator(dfInput, dfMetrics_mismatched)
#' }
#'
#' @export
JoinKRIByDenominator <- function(dfInput, dfMetrics) {
  # Validate DenominatorType column exists in dfInput
  if (!"DenominatorType" %in% colnames(dfInput)) {
    stop("dfInput must contain a 'DenominatorType' column. Use strDenominatorType parameter in Input_CountSiteByMonth.")
  }

  # Get unique denominator types
  vDenomTypes <- dfInput %>%
    dplyr::distinct(.data$DenominatorType) %>%
    dplyr::collect() %>%
    dplyr::pull(.data$DenominatorType)

  # Validate that all metrics within each DenominatorType have the same AccrualThreshold
  for (strDenomType in vDenomTypes) {
    # Get MetricIDs for this denominator type
    vMetricIDs <- dfInput %>%
      dplyr::filter(.data$DenominatorType == .env$strDenomType) %>%
      dplyr::distinct(.data$MetricID) %>%
      dplyr::collect() %>%
      dplyr::pull(.data$MetricID)
    
    # Get AccrualThreshold values from dfMetrics
    vThresholds <- dfMetrics %>%
      dplyr::filter(.data$MetricID %in% .env$vMetricIDs) %>%
      dplyr::pull(.data$AccrualThreshold)
    
    # Check if all thresholds are identical
    if (length(unique(vThresholds)) > 1) {
      vMetricDetails <- dfMetrics %>%
        dplyr::filter(.data$MetricID %in% .env$vMetricIDs) %>%
        dplyr::select(.data$MetricID, .data$AccrualThreshold) %>%
        dplyr::arrange(.data$AccrualThreshold)
      
      stop(sprintf(
        "Cannot join KRIs with DenominatorType '%s' because they have different AccrualThreshold values:\n%s\n\nKRIs sharing the same DenominatorType must have identical AccrualThreshold (nMinDenominator) values because this parameter affects the Denominator calculation through date adjustment.",
        strDenomType,
        paste(capture.output(print(vMetricDetails)), collapse = "\n")
      ))
    }
  }

  # Validate that all metrics within each DenominatorType have the same GroupLevel
  for (strDenomType in vDenomTypes) {
    # Get unique GroupLevel values for this denominator type
    vGroupLevels <- dfInput %>%
      dplyr::filter(.data$DenominatorType == .env$strDenomType) %>%
      dplyr::distinct(.data$GroupLevel) %>%
      dplyr::collect() %>%
      dplyr::pull(.data$GroupLevel)
    
    # Check for NA or empty GroupLevel FIRST (before checking for differences)
    if (any(is.na(vGroupLevels)) || any(vGroupLevels == "")) {
      vMetricIDs <- dfInput %>%
        dplyr::filter(.data$DenominatorType == .env$strDenomType) %>%
        dplyr::filter(is.na(.data$GroupLevel) | .data$GroupLevel == "") %>%
        dplyr::distinct(.data$MetricID) %>%
        dplyr::collect() %>%
        dplyr::pull(.data$MetricID)
      
      stop(sprintf(
        "KRIs with DenominatorType '%s' have missing or empty GroupLevel values: %s\n\nEnsure all KRI YAML files have 'GroupLevel' defined in the meta section (e.g., 'GroupLevel: Site').",
        strDenomType,
        paste(vMetricIDs, collapse = ", ")
      ))
    }
    
    # Check if all GroupLevel values are identical
    if (length(unique(vGroupLevels)) > 1) {
      # Get MetricIDs for this denominator type to show in error
      vMetricIDs <- dfInput %>%
        dplyr::filter(.data$DenominatorType == .env$strDenomType) %>%
        dplyr::distinct(.data$MetricID, .data$GroupLevel) %>%
        dplyr::collect()
      
      stop(sprintf(
        "Cannot join KRIs with DenominatorType '%s' because they have different GroupLevel values:\n%s\n\nKRIs sharing the same DenominatorType must have identical GroupLevel values for proper data alignment during pivot operations.",
        strDenomType,
        paste(capture.output(print(vMetricIDs)), collapse = "\n")
      ))
    }
  }

  # Build output list
 lResult <- lapply(vDenomTypes, function(strDenomType) {
    dfFiltered <- dfInput %>%
      dplyr::filter(.data$DenominatorType == .env$strDenomType)

    # Validate that all metrics have identical Denominator values for matching id_cols
    # This ensures the pivot_wider operation works correctly
    dfDenomCheck <- dfFiltered %>%
      dplyr::group_by(.data$StudyID, .data$GroupID, .data$MonthYYYYMM) %>%
      dplyr::summarise(
        n_unique_denoms = dplyr::n_distinct(.data$Denominator),
        n_metrics = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(.data$n_unique_denoms > 1) %>%
      dplyr::collect()
    
    if (nrow(dfDenomCheck) > 0) {
      # Get details about the mismatched denominators for error message
      dfDenomDetails <- dfFiltered %>%
        dplyr::semi_join(
          dfDenomCheck,
          by = c("StudyID", "GroupID", "MonthYYYYMM")
        ) %>%
        dplyr::select(.data$MetricID, .data$StudyID, .data$GroupID, 
                      .data$MonthYYYYMM, .data$Denominator) %>%
        dplyr::arrange(.data$StudyID, .data$GroupID, .data$MonthYYYYMM, .data$MetricID) %>%
        dplyr::collect()
      
      stop(sprintf(
        "Cannot join KRIs with DenominatorType '%s' because they have different Denominator values for the same (StudyID, GroupID, MonthYYYYMM) combinations:\n%s\n\nThis indicates the KRIs are using different denominator calculation methods (e.g., one with strDenominatorEndDateCol, one without) or have mismatched data. Ensure all KRIs with the same DenominatorType use identical Input_CountSiteByMonth parameters.",
        strDenomType,
        paste(capture.output(print(head(dfDenomDetails, 20))), collapse = "\n")
      ))
    }

    # Pivot wider: Numerator becomes Numerator_<MetricID>
    dfWide <- dfFiltered %>%
      tidyr::pivot_wider(
        id_cols = c("GroupID", "GroupLevel", "Denominator", "StudyID", "MonthYYYYMM"),
        names_from = "MetricID",
        values_from = "Numerator",
        names_prefix = "Numerator_"
      )

    # Return dfWide directly to preserve lazy evaluation when input is lazy
    dfWide
  })

  names(lResult) <- vDenomTypes
  lResult
}




