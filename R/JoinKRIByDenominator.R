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
#'   MonthYYYYMM = 202301
#' )
#' dfMetrics <- data.frame(
#'   MetricID = c("kri0001", "kri0003"),
#'   Denominator = c("Visits", "Visits")
#' )
#' lJoined <- JoinKRIByDenominator(dfInput, dfMetrics)
#' names(lJoined)
#'
#' @export
JoinKRIByDenominator <- function(dfInput, dfMetrics) {
  # Join to get denominator type
 dfJoined <- dfInput %>%
    dplyr::left_join(
      dfMetrics %>% dplyr::select("MetricID", DenominatorType = "Denominator"),
      by = "MetricID"
    )

  # Get unique denominator types
  # Collect if lazy table before extracting unique values
  if (inherits(dfJoined, "tbl_lazy")) {
    vDenomTypes <- dfJoined %>%
      dplyr::distinct(.data$DenominatorType) %>%
      dplyr::collect() %>%
      dplyr::pull(.data$DenominatorType)
  } else {
    vDenomTypes <- dfJoined %>%
      dplyr::pull(.data$DenominatorType) %>%
      unique()
  }

  # Build output list
 lResult <- lapply(vDenomTypes, function(strDenomType) {
    dfFiltered <- dfJoined %>%
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




