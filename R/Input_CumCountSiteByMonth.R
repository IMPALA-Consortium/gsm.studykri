#' Calculate Cumulative Count by Site and Month
#'
#' @description
#' Calculates cumulative event counts and ratios by site and month for KRI analysis.
#' Joins subject, numerator, and denominator data to create monthly cumulative metrics.
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
#' @param strNumeratorCol character. Column name for numerator flag (default: NULL, counts all rows).
#' @param strDenominatorCol character. Column name for denominator flag (default: NULL, counts all rows).
#' @param strNumeratorDateCol character. Date column name in numerator data.
#' @param strDenominatorDateCol character. Date column name in denominator data.
#'
#' @return data.frame with columns: GroupID, GroupLevel, Numerator, Denominator, Metric, StudyID, MonthYYYYMM
#'
#' @examples
#' dfSubjects <- clindata::rawplus_dm
#' dfNumerator <- clindata::rawplus_ae
#' dfDenominator <- clindata::rawplus_visdt
#'
#' result <- Input_CumCountSiteByMonth(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "visit_dt"
#' )
#'
#' @export
Input_CumCountSiteByMonth <- function(
  dfSubjects,
  dfNumerator,
  dfDenominator,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorCol = NULL,
  strDenominatorCol = NULL,
  strNumeratorDateCol,
  strDenominatorDateCol
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
  
  # Apply numerator filter if specified
  if (!is.null(strNumeratorCol)) {
    if (!strNumeratorCol %in% colnames(dfNum_processed)) {
      stop(sprintf("strNumeratorCol '%s' not found in dfNumerator", strNumeratorCol))
    }
    dfNum_processed <- dfNum_processed %>%
      dplyr::filter(.data[[strNumeratorCol]] == 1 | .data[[strNumeratorCol]] == "Y")
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
  
  # Process denominator data
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
  
  # Apply denominator filter if specified
  if (!is.null(strDenominatorCol)) {
    if (!strDenominatorCol %in% colnames(dfDenom_processed)) {
      stop(sprintf("strDenominatorCol '%s' not found in dfDenominator", strDenominatorCol))
    }
    dfDenom_processed <- dfDenom_processed %>%
      dplyr::filter(.data[[strDenominatorCol]] == 1 | .data[[strDenominatorCol]] == "Y")
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
  
  # Calculate cumulative counts by site with conditional window_order
  if (is_lazy_table(dfCombined)) {
    dfResult <- dfCombined %>%
      dplyr::group_by(.data[[strStudyCol]], .data[[strGroupCol]]) %>%
      dbplyr::window_order(.data$MonthYYYYMM) %>%
      dplyr::mutate(
        Numerator = cumsum(.data$Numerator),
        Denominator = cumsum(.data$Denominator),
        Metric = dplyr::if_else(
          .data$Denominator > 0,
          .data$Numerator / .data$Denominator,
          NA_real_
        ),
        GroupLevel = .env$strGroupLevel
      ) %>%
      dplyr::ungroup()
  } else {
    dfResult <- dfCombined %>%
      dplyr::arrange(.data[[strStudyCol]], .data[[strGroupCol]], .data$MonthYYYYMM) %>%
      dplyr::group_by(.data[[strStudyCol]], .data[[strGroupCol]]) %>%
      dplyr::mutate(
        Numerator = cumsum(.data$Numerator),
        Denominator = cumsum(.data$Denominator),
        Metric = dplyr::if_else(
          .data$Denominator > 0,
          .data$Numerator / .data$Denominator,
          NA_real_
        ),
        GroupLevel = .env$strGroupLevel
      ) %>%
      dplyr::ungroup()
  }
  
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
