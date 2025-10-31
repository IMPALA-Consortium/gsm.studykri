#' Calculate Days Per Month from Date Range
#'
#' @description
#' Helper function that calculates days contributed to each month from start/end date pairs.
#' Handles date ranges spanning multiple months with dbplyr compatibility.
#'
#' @param dfData data.frame or tbl with date range records
#' @param strStartDateCol character. Start date column name
#' @param strEndDateCol character. End date column name
#' @param vGroupCols character. Grouping columns to preserve
#'
#' @return data.frame or tbl with MonthYYYYMM and Days columns
#' @keywords internal
calculate_days_by_month <- function(dfData, strStartDateCol, strEndDateCol, vGroupCols) {
  is_lazy <- inherits(dfData, "tbl_lazy")
  
  # Create month expansion data
  dfData_expanded <- dfData %>%
    dplyr::filter(
      !is.na(.data[[strStartDateCol]]) & !is.na(.data[[strEndDateCol]])
    ) %>%
    dplyr::mutate(
      start_year = lubridate::year(.data[[strStartDateCol]]),
      start_month = lubridate::month(.data[[strStartDateCol]]),
      start_day = lubridate::day(.data[[strStartDateCol]]),
      end_year = lubridate::year(.data[[strEndDateCol]]),
      end_month = lubridate::month(.data[[strEndDateCol]]),
      end_day = lubridate::day(.data[[strEndDateCol]]),
      start_yyyymm = .data$start_year * 100 + .data$start_month,
      end_yyyymm = .data$end_year * 100 + .data$end_month
    )
  
  # Generate month sequence
  if (is_lazy) {
    # For lazy tables, collect min/max to generate sequence
    date_range <- dfData_expanded %>%
      dplyr::summarise(
        min_yyyymm = min(.data$start_yyyymm, na.rm = TRUE),
        max_yyyymm = max(.data$end_yyyymm, na.rm = TRUE)
      ) %>%
      dplyr::collect()
    
    min_ym <- date_range$min_yyyymm
    max_ym <- date_range$max_yyyymm
  } else {
    min_ym <- min(dfData_expanded$start_yyyymm, na.rm = TRUE)
    max_ym <- max(dfData_expanded$end_yyyymm, na.rm = TRUE)
  }
  
  # Generate complete month sequence
  min_year <- floor(min_ym / 100)
  min_month <- min_ym %% 100
  max_year <- floor(max_ym / 100)
  max_month <- max_ym %% 100
  
  date_seq <- seq(
    as.Date(paste0(min_year, "-", sprintf("%02d", min_month), "-01")),
    as.Date(paste0(max_year, "-", sprintf("%02d", max_month), "-01")),
    by = "month"
  )
  
  dfMonths <- data.frame(
    MonthYYYYMM = as.numeric(format(date_seq, "%Y%m"))
  )
  
  # Create month lookup table in database if lazy
  if (is_lazy) {
    tblMonths <- expand_lazy_table(
      tblInput = dfData_expanded,
      tblExpansion = NULL,
      dfExpansion_mem = dfMonths,
      strTempTableName = "temp_month_sequence",
      strExpansionType = "month sequence"
    )
  } else {
    tblMonths <- dfMonths
  }
  
  # Cross join to expand records to all months, then filter to relevant range
  dfExpanded <- dfData_expanded %>%
    dplyr::cross_join(tblMonths) %>%
    dplyr::filter(
      .data$MonthYYYYMM >= .data$start_yyyymm &
      .data$MonthYYYYMM <= .data$end_yyyymm
    )
  
  # Calculate days for each month
  dfDays <- dfExpanded %>%
    dplyr::mutate(
      month_year = floor(.data$MonthYYYYMM / 100),
      month_num = .data$MonthYYYYMM %% 100,
      # Days in month calculation (dbplyr compatible)
      days_in_month = dplyr::case_when(
        .data$month_num %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31L,
        .data$month_num %in% c(4, 6, 9, 11) ~ 30L,
        # Leap year calculation
        .data$month_num == 2 & (.data$month_year %% 4 == 0 & (.data$month_year %% 100 != 0 | .data$month_year %% 400 == 0)) ~ 29L,
        .data$month_num == 2 ~ 28L,
        TRUE ~ 30L
      ),
      # Calculate days contributed to this month
      Days = dplyr::case_when(
        # Same month for start and end
        .data$start_yyyymm == .data$end_yyyymm ~ .data$end_day - .data$start_day + 1L,
        # First month of range
        .data$MonthYYYYMM == .data$start_yyyymm ~ .data$days_in_month - .data$start_day + 1L,
        # Last month of range
        .data$MonthYYYYMM == .data$end_yyyymm ~ .data$end_day,
        # Middle month
        TRUE ~ .data$days_in_month
      )
    ) %>%
    dplyr::select(dplyr::all_of(c(vGroupCols, "MonthYYYYMM", "Days")))
  
  return(dfDays)
}

#' Calculate Monthly Count by Site and Month
#'
#' @description
#' Calculates monthly event counts and ratios by site and month for KRI analysis.
#' Joins subject, numerator, and denominator data to create monthly metrics.
#' When strDenominatorEndDateCol is provided, calculates days between start/end dates
#' per month instead of counting records.
#' Note: This function returns monthly counts, not cumulative. Cumulative aggregation
#' is performed at the study level in Transform_CumCount.
#' Supports both in-memory data frames and dbplyr lazy tables.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' @importFrom lubridate year month
#' 
#' @param dfSubjects data.frame or tbl. Subject-level data with enrolled subjects.
#' @param dfNumerator data.frame or tbl. Event data for numerator (e.g., adverse events).
#' @param dfDenominator data.frame or tbl. Event data for denominator (e.g., visits).
#' @param strStudyCol character. Column name for study identifier (default: "studyid").
#' @param strGroupCol character. Column name for site identifier (default: "invid").
#' @param strGroupLevel character. Grouping level name (default: "Site").
#' @param strSubjectCol character. Column name for subject identifier (default: "subjid").
#' @param strNumeratorDateCol character. Date column name in numerator data.
#' @param strDenominatorDateCol character. Date column name in denominator data (start date).
#' @param strDenominatorEndDateCol character. End date column in denominator data (default: NULL). When provided, calculates sum of days between start/end dates per month instead of record counts.
#'
#' @return data.frame with columns: GroupID, GroupLevel, Numerator, Denominator, Metric, StudyID, MonthYYYYMM
#'
#' @examples
#' dfSubjects <- clindata::rawplus_dm
#' dfNumerator <- clindata::rawplus_ae
#' dfDenominator <- clindata::rawplus_visdt
#'
#' # Count-based calculation
#' result <- Input_CountSiteByMonth(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "visit_dt"
#' )
#'
#' # Days-based calculation with end dates
#' result_days <- Input_CountSiteByMonth(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfSubjects,
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "firstparticipantdate",
#'   strDenominatorEndDateCol = "lastparticipantdate"
#' )
#'
#' @export
Input_CountSiteByMonth <- function(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorDateCol,
  strDenominatorDateCol,
  strDenominatorEndDateCol = NULL
) {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfSubjects, c("data.frame", "tbl"))) {
    stop("dfSubjects must be a data.frame or tbl object")
  }
  
  if (!inherits(dfNumerator, c("data.frame", "tbl"))) {
    stop("dfNumerator must be a data.frame or tbl object")
  }
  
  if (!inherits(dfDenominator, c("data.frame", "tbl"))) {
    stop("dfDenominator must be a data.frame or tbl object")
  }
  
  # Validate required columns in dfSubjects (use colnames for lazy table compatibility)
  required_subj_cols <- c(strStudyCol, strGroupCol, strSubjectCol)
  missing_subj_cols <- setdiff(required_subj_cols, colnames(dfSubjects))
  if (length(missing_subj_cols) > 0) {
    stop(sprintf(
      "dfSubjects missing required columns: %s",
      paste(missing_subj_cols, collapse = ", ")
    ))
  }
  
  # Validate required columns in dfNumerator (use colnames for lazy table compatibility)
  required_num_cols <- c(strSubjectCol, strNumeratorDateCol)
  missing_num_cols <- setdiff(required_num_cols, colnames(dfNumerator))
  if (length(missing_num_cols) > 0) {
    stop(sprintf(
      "dfNumerator missing required columns: %s",
      paste(missing_num_cols, collapse = ", ")
    ))
  }
  
  # Validate required columns in dfDenominator (use colnames for lazy table compatibility)
  required_denom_cols <- c(strSubjectCol, strDenominatorDateCol)
  if (!is.null(strDenominatorEndDateCol)) {
    required_denom_cols <- c(required_denom_cols, strDenominatorEndDateCol)
  }
  missing_denom_cols <- setdiff(required_denom_cols, colnames(dfDenominator))
  if (length(missing_denom_cols) > 0) {
    stop(sprintf(
      "dfDenominator missing required columns: %s",
      paste(missing_denom_cols, collapse = ", ")
    ))
  }
  
  # Helper to detect lazy tables
  is_lazy_table <- function(x) {
    inherits(x, "tbl_lazy")
  }
  
  # Filter to enrolled subjects only (use colnames for lazy table compatibility)
  if ("enrollyn" %in% colnames(dfSubjects)) {
    dfSubjects <- dfSubjects %>%
      dplyr::filter(.data$enrollyn == "Y")
  }
  
  # For in-memory data frames, check for empty results
  # For lazy tables, skip this check as it would force evaluation
  if (is.data.frame(dfSubjects) && nrow(dfSubjects) == 0) {
    stop("No enrolled subjects found in dfSubjects")
  }
  
  # Process numerator data
  dfNum_processed <- dfNumerator %>%
    dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
    dplyr::inner_join(
      dfSubjects %>% dplyr::select(dplyr::all_of(c(strSubjectCol, strStudyCol, strGroupCol))),
      by = stats::setNames(strSubjectCol, strSubjectCol)
    ) %>%
    dplyr::filter(!is.na(.data[[strNumeratorDateCol]])) %>%
    # Conditional date handling for dbplyr compatibility
    {
      if (is_lazy_table(.)) {
        dplyr::mutate(.,
          year = lubridate::year(.data[[strNumeratorDateCol]]),
          month = lubridate::month(.data[[strNumeratorDateCol]]),
          MonthYYYYMM = .data$year * 100 + .data$month
        ) %>%
          dplyr::select(-"year", -"month")
      } else {
        dplyr::mutate(.,
          MonthYYYYMM = as.numeric(format(as.Date(.data[[strNumeratorDateCol]]), "%Y%m"))
        )
      }
    }
  
  # Count numerator events by site-month
  dfNum_counts <- dfNum_processed %>%
    dplyr::group_by(
      .data[[strStudyCol]],
      .data[[strGroupCol]],
      .data$MonthYYYYMM
    ) %>%
    dplyr::summarise(
      Numerator = dplyr::n(),
      .groups = "drop"
    )
  
  # Process denominator data - two modes: count or days
  if (is.null(strDenominatorEndDateCol)) {
    # Mode 1: Count denominator records by month (existing logic)
    dfDenom_processed <- dfDenominator %>%
      dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
      dplyr::inner_join(
        dfSubjects %>% dplyr::select(dplyr::all_of(c(strSubjectCol, strStudyCol, strGroupCol))),
        by = stats::setNames(strSubjectCol, strSubjectCol)
      ) %>%
      dplyr::filter(!is.na(.data[[strDenominatorDateCol]])) %>%
      # Conditional date handling for dbplyr compatibility
      {
        if (is_lazy_table(.)) {
          dplyr::mutate(.,
            year = lubridate::year(.data[[strDenominatorDateCol]]),
            month = lubridate::month(.data[[strDenominatorDateCol]]),
            MonthYYYYMM = .data$year * 100 + .data$month
          ) %>%
            dplyr::select(-"year", -"month")
        } else {
          dplyr::mutate(.,
            MonthYYYYMM = as.numeric(format(as.Date(.data[[strDenominatorDateCol]]), "%Y%m"))
          )
        }
      }
    
    # Count denominator events by site-month
    dfDenom_counts <- dfDenom_processed %>%
      dplyr::group_by(
        .data[[strStudyCol]],
        .data[[strGroupCol]],
        .data$MonthYYYYMM
      ) %>%
      dplyr::summarise(
        Denominator = dplyr::n(),
        .groups = "drop"
      )
    
  } else {
    # Mode 2: Calculate days between start/end dates by month
    dfDenom_with_subjects <- dfDenominator %>%
      dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
      dplyr::inner_join(
        dfSubjects %>% dplyr::select(dplyr::all_of(c(strSubjectCol, strStudyCol, strGroupCol))),
        by = stats::setNames(strSubjectCol, strSubjectCol)
      )
    
    # Call helper to calculate days per month
    dfDays <- calculate_days_by_month(
      dfData = dfDenom_with_subjects,
      strStartDateCol = strDenominatorDateCol,
      strEndDateCol = strDenominatorEndDateCol,
      vGroupCols = c(strStudyCol, strGroupCol)
    )
    
    # Sum days by site-month
    dfDenom_counts <- dfDays %>%
      dplyr::group_by(
        .data[[strStudyCol]],
        .data[[strGroupCol]],
        .data$MonthYYYYMM
      ) %>%
      dplyr::summarise(
        Denominator = sum(.data$Days, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # Combine numerator and denominator
  dfCombined <- dfNum_counts %>%
    dplyr::full_join(
      dfDenom_counts,
      by = c(strStudyCol, strGroupCol, "MonthYYYYMM")
    ) %>%
    dplyr::mutate(
      Numerator = dplyr::coalesce(.data$Numerator, 0L),
      Denominator = dplyr::coalesce(.data$Denominator, 0L)
    )
  
  # Return monthly counts (NOT cumulative)
  # Cumulative sums will be calculated at study level in Transform_CumCount
  dfResult <- dfCombined %>%
    dplyr::mutate(
      Metric = dplyr::if_else(
        .data$Denominator > 0,
        .data$Numerator / .data$Denominator,
        NA_real_
      ),
      GroupLevel = .env$strGroupLevel
    )
  
  # Select and rename columns
  dfFinal <- dfResult %>%
    dplyr::select(
      GroupID = dplyr::all_of(.env$strGroupCol),
      "GroupLevel",
      "Numerator",
      "Denominator",
      "Metric",
      StudyID = dplyr::all_of(.env$strStudyCol),
      "MonthYYYYMM"
    )
  
  # Return lazy table if input was lazy, data.frame otherwise
  if (inherits(dfNumerator, "tbl_lazy") || inherits(dfDenominator, "tbl_lazy") || inherits(dfSubjects, "tbl_lazy")) {
    return(dfFinal)
  } else {
    return(as.data.frame(dfFinal))
  }
}
