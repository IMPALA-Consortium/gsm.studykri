#' Calculate Cumulative Count by Site and Month
#'
#' @description
#' Calculates cumulative event counts and ratios by site and month for KRI analysis.
#' Joins subject, numerator, and denominator data to create monthly cumulative metrics.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' 
#' @param dfSubjects data.frame. Subject-level data with enrolled subjects.
#' @param dfNumerator data.frame. Event data for numerator (e.g., adverse events).
#' @param dfDenominator data.frame. Event data for denominator (e.g., visits).
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
  # Input validation
  if (!is.data.frame(dfSubjects)) {
    stop("dfSubjects must be a data.frame")
  }
  
  if (!is.data.frame(dfNumerator)) {
    stop("dfNumerator must be a data.frame")
  }
  
  if (!is.data.frame(dfDenominator)) {
    stop("dfDenominator must be a data.frame")
  }
  
  # Validate required columns in dfSubjects
  required_subj_cols <- c(strStudyCol, strGroupCol, strSubjectCol)
  missing_subj_cols <- setdiff(required_subj_cols, names(dfSubjects))
  if (length(missing_subj_cols) > 0) {
    stop(sprintf(
      "dfSubjects missing required columns: %s",
      paste(missing_subj_cols, collapse = ", ")
    ))
  }
  
  # Validate required columns in dfNumerator
  required_num_cols <- c(strSubjectCol, strNumeratorDateCol)
  missing_num_cols <- setdiff(required_num_cols, names(dfNumerator))
  if (length(missing_num_cols) > 0) {
    stop(sprintf(
      "dfNumerator missing required columns: %s",
      paste(missing_num_cols, collapse = ", ")
    ))
  }
  
  # Validate required columns in dfDenominator
  required_denom_cols <- c(strSubjectCol, strDenominatorDateCol)
  missing_denom_cols <- setdiff(required_denom_cols, names(dfDenominator))
  if (length(missing_denom_cols) > 0) {
    stop(sprintf(
      "dfDenominator missing required columns: %s",
      paste(missing_denom_cols, collapse = ", ")
    ))
  }
  
  # Filter to enrolled subjects only
  if ("enrollyn" %in% names(dfSubjects)) {
    dfSubjects <- dfSubjects[dfSubjects$enrollyn == "Y", ]
  }
  
  if (nrow(dfSubjects) == 0) {
    stop("No enrolled subjects found in dfSubjects")
  }
  
  # Process numerator data
  dfNum_processed <- dfNumerator %>%
    dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
    dplyr::inner_join(
      dfSubjects[, c(strSubjectCol, strStudyCol, strGroupCol)],
      by = stats::setNames(strSubjectCol, strSubjectCol)
    ) %>%
    dplyr::filter(!is.na(.data[[strNumeratorDateCol]])) %>%
    dplyr::mutate(
      Month = as.numeric(format(as.Date(.data[[strNumeratorDateCol]]), "%Y%m"))
    )
  
  # Apply numerator filter if specified
  if (!is.null(strNumeratorCol)) {
    if (!strNumeratorCol %in% names(dfNum_processed)) {
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
      .data$Month
    ) %>%
    dplyr::summarise(
      Numerator = dplyr::n(),
      .groups = "drop"
    )
  
  # Process denominator data
  dfDenom_processed <- dfDenominator %>%
    dplyr::select(-dplyr::any_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
    dplyr::inner_join(
      dfSubjects[, c(strSubjectCol, strStudyCol, strGroupCol)],
      by = stats::setNames(strSubjectCol, strSubjectCol)
    ) %>%
    dplyr::filter(!is.na(.data[[strDenominatorDateCol]])) %>%
    dplyr::mutate(
      Month = as.numeric(format(as.Date(.data[[strDenominatorDateCol]]), "%Y%m"))
    )
  
  # Apply denominator filter if specified
  if (!is.null(strDenominatorCol)) {
    if (!strDenominatorCol %in% names(dfDenom_processed)) {
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
      .data$Month
    ) %>%
    dplyr::summarise(
      Denominator = dplyr::n(),
      .groups = "drop"
    )
  
  # Combine numerator and denominator
  dfCombined <- dfNum_counts %>%
    dplyr::full_join(
      dfDenom_counts,
      by = c(strStudyCol, strGroupCol, "Month")
    ) %>%
    dplyr::mutate(
      Numerator = dplyr::coalesce(.data$Numerator, 0L),
      Denominator = dplyr::coalesce(.data$Denominator, 0L)
    ) %>%
    dplyr::arrange(
      .data[[strStudyCol]],
      .data[[strGroupCol]],
      .data$Month
    )
  
  # Calculate cumulative counts by site
  dfResult <- dfCombined %>%
    dplyr::group_by(
      .data[[strStudyCol]],
      .data[[strGroupCol]]
    ) %>%
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
    dplyr::ungroup() %>%
    dplyr::select(
      GroupID = dplyr::all_of(.env$strGroupCol),
      "GroupLevel",
      "Numerator",
      "Denominator",
      "Metric",
      StudyID = dplyr::all_of(.env$strStudyCol),
      MonthYYYYMM = "Month"
    )
  
  return(as.data.frame(dfResult))
}
