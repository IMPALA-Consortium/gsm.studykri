#' Transform Cumulative Counts to Study Level
#'
#' @description
#' Transforms site-level cumulative counts into study-level metrics by aggregating
#' across groups (sites/countries) by calendar month. Creates sequential study months,
#' applies minimum denominator filtering, and calculates cumulative metrics.
#' Supports both in-memory data frames and dbplyr lazy tables.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' @importFrom tidyr unnest
#'
#' @param dfInput data.frame or tbl. Output from Input_CountSiteByMonth with columns:
#'   GroupID, GroupLevel, Numerator, Denominator, Metric, StudyID, MonthYYYYMM.
#'   Note: Input should contain monthly (not cumulative) counts. This function
#'   calculates cumulative sums at the study level.
#' @param vBy character. Vector of column names for grouping (e.g., "StudyID" or
#'   c("StudyID", "BootstrapRep")).
#' @param nMinDenominator numeric. Minimum cumulative denominator threshold for
#'   filtering early/sparse data (default: 25).
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated complete month sequences. Must contain columns matching
#'   vBy and MonthYYYYMM with NO gaps. If NULL, attempts to create temp table
#'   (requires write privileges). If data.frame provided, will be written to temp table.
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
#' dfInput <- Input_CountSiteByMonth(
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
    nMinDenominator = 25,
    tblMonthSequence = NULL) {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data frame or tbl object")
  }

  required_cols <- c("GroupID", "Numerator", "Denominator", "MonthYYYYMM")
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  if (!is.character(vBy) || length(vBy) == 0) {
    stop("vBy must be a non-empty character vector")
  }

  missing_by <- setdiff(vBy, colnames(dfInput))
  if (length(missing_by) > 0) {
    stop(sprintf(
      "vBy columns not found in dfInput: %s",
      paste(missing_by, collapse = ", ")
    ))
  }

  if (!is.numeric(nMinDenominator) || length(nMinDenominator) != 1 || nMinDenominator < 0) {
    stop("nMinDenominator must be a single non-negative numeric value")
  }

  # Filter out rows with NA MonthYYYYMM before processing
  dfInput <- dfInput %>%
    dplyr::filter(!is.na(.data$MonthYYYYMM))

  # Create initial StudyMonth by ranking calendar months within groups
  dfWithStudyMonth <- dfInput %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
    dplyr::mutate(
      StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM)
    ) %>%
    dplyr::ungroup()

  # Aggregate to study level by grouping columns and StudyMonth
  dfAggregated <- dfWithStudyMonth %>%
    dplyr::summarise(
      MonthYYYYMM = min(.data$MonthYYYYMM), # Keep the calendar month
      Numerator = sum(.data$Numerator, na.rm = TRUE),
      Denominator = sum(.data$Denominator, na.rm = TRUE),
      GroupCount = dplyr::n_distinct(.data$GroupID),
      .by = c(dplyr::all_of(.env$vBy), "StudyMonth")
    )

  # Fill gaps in calendar months with zeros to maintain timeline continuity
  # Get group-specific month ranges (collect works for both types)
  dfMonthRanges <- dfAggregated %>%
    dplyr::summarise(
      min_month = min(.data$MonthYYYYMM),
      max_month = max(.data$MonthYYYYMM),
      .by = dplyr::all_of(.env$vBy)
    ) %>%
    dplyr::collect()
  
  # Get global min/max to generate single month sequence
  global_min <- min(dfMonthRanges$min_month)
  global_max <- max(dfMonthRanges$max_month)
  
  # Generate complete month sequence once (reused across all groups)
  dfAllMonths <- generate_month_seq(global_min, global_max)
  
  # Cross join all groups with all months
  dfCompleteMonths_mem <- dfMonthRanges %>%
    dplyr::select(dplyr::all_of(.env$vBy)) %>%
    dplyr::cross_join(dfAllMonths)
  
  # Filter to group-specific month ranges
  dfCompleteMonths_mem <- dfCompleteMonths_mem %>%
    dplyr::left_join(
      dfMonthRanges %>% dplyr::select(dplyr::all_of(.env$vBy), "min_month", "max_month"),
      by = vBy
    ) %>%
    dplyr::filter(
      .data$MonthYYYYMM >= .data$min_month &
      .data$MonthYYYYMM <= .data$max_month
    ) %>%
    dplyr::select(-"min_month", -"max_month")
  
  # Convert to lazy table if needed (single inline if allowed)
  if (inherits(dfAggregated, "tbl_lazy")) {
    dfCompleteMonths <- expand_lazy_table(
      tblInput = dfAggregated,
      tblExpansion = tblMonthSequence,
      dfExpansion_mem = dfCompleteMonths_mem,
      strTempTableName = "month_sequences",
      strExpansionType = "complete month sequences"
    )
  } else {
    dfCompleteMonths <- dfCompleteMonths_mem
  }
  
  # Left join to fill gaps and recalculate StudyMonth
  dfAggregated <- dfCompleteMonths %>%
    dplyr::left_join(
      dfAggregated %>% dplyr::select(-"StudyMonth"),
      by = c(vBy, "MonthYYYYMM")
    ) %>%
    dplyr::mutate(
      Numerator = dplyr::coalesce(.data$Numerator, 0L),
      Denominator = dplyr::coalesce(.data$Denominator, 0L),
      GroupCount = dplyr::coalesce(.data$GroupCount, 0L),
      StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
      .by = dplyr::all_of(.env$vBy)
    )

  # Calculate cumulative sums at study level
  # This ensures that when sites drop out, their cumulative contributions persist
  dfCumulative <- dfAggregated %>%
    SortDf(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
    dplyr::mutate(
      Numerator = cumsum(.data$Numerator),
      Denominator = cumsum(.data$Denominator)
    ) %>%
    dplyr::ungroup()

  # Apply minimum denominator filter
  dfFiltered <- dfCumulative %>%
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
    SortDf(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth)

  # Select final columns in desired order
  final_cols <- c(
    vBy, "MonthYYYYMM", "StudyMonth", "Numerator",
    "Denominator", "Metric", "GroupCount"
  )

  dfFinal <- dfResult %>%
    dplyr::select(dplyr::all_of(.env$final_cols))

  return(dfFinal)
}
