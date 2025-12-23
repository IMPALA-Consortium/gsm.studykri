#' Transform Cumulative Counts to Study Level
#'
#' @description
#' Transforms site-level cumulative counts into study-level metrics by aggregating
#' across groups (sites/countries) by calendar month. Creates sequential study months,
#' applies minimum denominator filtering, and calculates cumulative metrics.
#' Supports both in-memory data frames and dbplyr lazy tables.
#' Handles multiple Numerator columns (e.g., Numerator_kri0001, Numerator_kri0003).
#'
#' @param dfInput data.frame or tbl. Output from Input_CountSiteByMonth with columns:
#'   GroupID, GroupLevel, Numerator (or Numerator_*), Denominator, StudyID, MonthYYYYMM.
#'   Note: Input should contain monthly (not cumulative) counts. This function
#'   calculates cumulative sums at the study level.
#' @param vBy character. Vector of column names for grouping (e.g., "StudyID" or
#'   c("StudyID", "BootstrapRep")).
#' @param nMinDenominator numeric. Minimum cumulative denominator threshold for
#'   filtering early/sparse data (default: 25).
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated month sequence with only a `MonthYYYYMM` column
#'   (output of `GenerateMonthSeq()`). Must contain consecutive months with NO gaps.
#'   If NULL, attempts to create temp table (requires write privileges).
#'   If data.frame provided, will be written to temp table.
#'
#' @return A data.frame with study-level aggregated cumulative counts and ratios.
#'   Output columns: `vBy` columns, `MonthYYYYMM`, `StudyMonth`, Numerator columns,
#'   `Denominator`, Metric columns, `GroupCount`.
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

  # Auto-detect numerator columns (matches both "Numerator" and "Numerator_*")
  vNumeratorCols <- grep("^Numerator", colnames(dfInput), value = TRUE)

  required_cols <- c("GroupID", "Denominator", "MonthYYYYMM")
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  if (length(vNumeratorCols) == 0) {
    stop("dfInput must have at least one Numerator column")
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

  # Aggregate to study level by grouping columns and MonthYYYYMM first

  # (aggregate on full data using hash-based grouping - no sorting needed)
  dfAggregated <- dfInput %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::all_of(.env$vNumeratorCols),
        .fns = ~ sum(.x, na.rm = TRUE)
      ),
      Denominator = sum(.data$Denominator, na.rm = TRUE),
      GroupCount = dplyr::n_distinct(.data$GroupID),
      .by = c(dplyr::all_of(.env$vBy), "MonthYYYYMM")
    ) %>%
    # Now add StudyMonth via dense_rank on the AGGREGATED data (much fewer rows)
    dplyr::mutate(
      StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
      .by = dplyr::all_of(.env$vBy)
    )

  # Fill gaps in calendar months with zeros to maintain timeline continuity
  dfMonthRanges <- dfAggregated %>%
    dplyr::summarise(
      min_month = min(.data$MonthYYYYMM, na.rm = TRUE),
      max_month = max(.data$MonthYYYYMM, na.rm = TRUE),
      .by = dplyr::all_of(.env$vBy)
    )

  global_min <- dfMonthRanges %>%
    dplyr::summarise(min_month = min(.data$min_month, na.rm = TRUE)) %>%
    dplyr::pull(.data$min_month)

  global_max <- dfMonthRanges %>%
    dplyr::summarise(max_month = max(.data$max_month, na.rm = TRUE)) %>%
    dplyr::pull(.data$max_month)

  dfAllMonths_mem <- GenerateMonthSeq(global_min, global_max)

  # HandleLazyTable handles both lazy and in-memory cases:
  # - For lazy: writes to temp table or uses provided tblUser
  # - For in-memory: returns dfMem directly
  dfAllMonths <- HandleLazyTable(
    tblInput = dfAggregated,
    tblUser = tblMonthSequence,
    dfMem = dfAllMonths_mem,
    strTempTableName = "month_sequences",
    strTableType = "month sequence"
  )

  # Cross-join and filter (unified for both lazy and in-memory)
  dfCompleteMonths <- dfMonthRanges %>%
    dplyr::select(dplyr::all_of(.env$vBy)) %>%
    dplyr::cross_join(dfAllMonths) %>%
    dplyr::left_join(
      dfMonthRanges %>% dplyr::select(dplyr::all_of(.env$vBy), "min_month", "max_month"),
      by = vBy
    ) %>%
    dplyr::filter(
      .data$MonthYYYYMM >= .data$min_month &
        .data$MonthYYYYMM <= .data$max_month
    ) %>%
    dplyr::select(-"min_month", -"max_month")

  # Left join to fill gaps and recalculate StudyMonth
  dfAggregated <- dfCompleteMonths %>%
    dplyr::left_join(
      dfAggregated %>% dplyr::select(-"StudyMonth"),
      by = c(vBy, "MonthYYYYMM")
    ) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(.env$vNumeratorCols),
        .fns = ~ dplyr::coalesce(.x, 0L)
      ),
      Denominator = dplyr::coalesce(.data$Denominator, 0L),
      GroupCount = dplyr::coalesce(.data$GroupCount, 0L),
      StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
      .by = dplyr::all_of(.env$vBy)
    )

  # Calculate cumulative sums at study level
  dfCumulative <- dfAggregated %>%
    SortDf(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(.env$vNumeratorCols),
        .fns = cumsum
      ),
      Denominator = cumsum(.data$Denominator)
    ) %>%
    dplyr::ungroup()

  # Apply minimum denominator filter
  dfFiltered <- dfCumulative %>%
    dplyr::filter(.data$Denominator > .env$nMinDenominator)

  # Re-rank StudyMonth after filtering
  dfReranked <- dfFiltered %>%
    dplyr::mutate(
      StudyMonth = dplyr::dense_rank(.data$StudyMonth),
      .by = dplyr::all_of(.env$vBy)
    )

  # Generate Metric columns using across with .names
  vMetricCols <- gsub("^Numerator", "Metric", vNumeratorCols)
  dfResult <- dfReranked %>%
    SortDf(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth) %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::all_of(.env$vNumeratorCols),
        # Note: .data pronoun intentionally removed from lambda function for dbplyr compatibility
        # dbplyr cannot properly translate .data inside across() lambda functions
        .fns = ~ dplyr::if_else(
          Denominator > 0,
          .x / Denominator,
          NA_real_
        ),
        .names = "{gsub('^Numerator', 'Metric', .col)}"
      )
    )

  # Select final columns in desired order
  final_cols <- c(
    vBy, "MonthYYYYMM", "StudyMonth", vNumeratorCols,
    "Denominator", vMetricCols, "GroupCount"
  )

  dfFinal <- dfResult %>%
    dplyr::select(dplyr::all_of(.env$final_cols))

  return(dfFinal)
}
