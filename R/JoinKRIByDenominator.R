#' Join KRI Analysis_Input by Denominator Type
#'
#' @description
#' Groups bound Analysis_Input data by denominator type and pivots numerators
#' into separate columns for shared bootstrap processing.
#'
#' @param dfInput data.frame. Bound Analysis_Input from BindResults.
#' @param dfMetrics data.frame. Metrics metadata from MakeMetric.
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
#' lJoined <- JoinKRIByDenominator(dfInput)
#' 
#' names(lJoined)
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

  # Build output list
 lResult <- lapply(vDenomTypes, function(strDenomType) {
    dfFiltered <- dfInput %>%
      dplyr::filter(.data$DenominatorType == .env$strDenomType)

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




