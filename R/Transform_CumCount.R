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
  tblMonthSequence = NULL
) {
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
  
  # Helper to detect lazy tables
  is_lazy_table <- function(x) {
    inherits(x, "tbl_lazy")
  }
  
  # For in-memory data frames, check for empty results
  # For lazy tables, skip this check as it would force evaluation
  if (is.data.frame(dfInput) && nrow(dfInput) == 0) {
    stop("dfInput has no rows")
  }
  
  # Filter out rows with NA MonthYYYYMM before processing
  dfFiltered_input <- dfInput %>%
    dplyr::filter(!is.na(.data$MonthYYYYMM))
  
  # For in-memory data frames, check if filtered data is empty
  # For lazy tables, skip this check as it would force evaluation
  if (is.data.frame(dfFiltered_input) && nrow(dfFiltered_input) == 0) {
    warning("All rows have NA MonthYYYYMM, returning empty data frame")
    return(data.frame(
      stringsAsFactors = FALSE
    )[0, c(vBy, "MonthYYYYMM", "StudyMonth", "Numerator", 
           "Denominator", "Metric", "GroupCount")]
    )
  }
  
  # Create initial StudyMonth by ranking calendar months within groups
  if (is_lazy_table(dfFiltered_input)) {
    dfWithStudyMonth <- dfFiltered_input %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
      dbplyr::window_order(.data$MonthYYYYMM) %>%
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM)
      ) %>%
      dplyr::ungroup()
  } else {
    dfWithStudyMonth <- dfFiltered_input %>%
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
        .by = dplyr::all_of(.env$vBy)
      )
  }
  
  # Aggregate to study level by grouping columns and StudyMonth
  dfAggregated <- dfWithStudyMonth %>%
    dplyr::summarise(
      MonthYYYYMM = min(.data$MonthYYYYMM),  # Keep the calendar month
      Numerator = sum(.data$Numerator, na.rm = TRUE),
      Denominator = sum(.data$Denominator, na.rm = TRUE),
      GroupCount = dplyr::n_distinct(.data$GroupID),
      .by = c(dplyr::all_of(.env$vBy), "StudyMonth")
    )
  
  # Fill gaps in calendar months with zeros to maintain timeline continuity
  # This ensures StudyMonth represents actual elapsed time, not compressed time
  if (!is_lazy_table(dfAggregated)) {
    # For in-memory data frames, fill gaps in MonthYYYYMM
    # Generate complete month sequences for each group
    dfMonthRanges <- dfAggregated %>%
      dplyr::summarise(
        min_month = min(.data$MonthYYYYMM),
        max_month = max(.data$MonthYYYYMM),
        .by = dplyr::all_of(.env$vBy)
      )
    
    # Helper function to generate complete YYYYMM sequence
    generate_month_seq <- function(start_yyyymm, end_yyyymm) {
      start_year <- floor(start_yyyymm / 100)
      start_month <- start_yyyymm %% 100
      end_year <- floor(end_yyyymm / 100)
      end_month <- end_yyyymm %% 100
      
      dates <- seq(
        as.Date(paste0(start_year, "-", sprintf("%02d", start_month), "-01")),
        as.Date(paste0(end_year, "-", sprintf("%02d", end_month), "-01")),
        by = "month"
      )
      
      as.numeric(format(dates, "%Y%m"))
    }
    
    # Create complete month grid for each group
    dfCompleteMonths <- dfMonthRanges %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        MonthYYYYMM = list(generate_month_seq(.data$min_month, .data$max_month))
      ) %>%
      dplyr::select(-"min_month", -"max_month") %>%
      tidyr::unnest("MonthYYYYMM")
    
    # Left join to fill gaps with zeros
    dfAggregated <- dfCompleteMonths %>%
      dplyr::left_join(
        dfAggregated %>% dplyr::select(-"StudyMonth"),
        by = c(vBy, "MonthYYYYMM")
      ) %>%
      dplyr::mutate(
        Numerator = dplyr::coalesce(.data$Numerator, 0L),
        Denominator = dplyr::coalesce(.data$Denominator, 0L),
        GroupCount = dplyr::coalesce(.data$GroupCount, 0L)
      ) %>%
      # Recalculate StudyMonth as sequential numbering
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM),
        .by = dplyr::all_of(.env$vBy)
      )
  } else {
    # Lazy table: Use helper function for gap filling
    
    # Get actual date ranges (collect small summary)
    dfMonthRanges <- dfAggregated %>% 
      dplyr::summarise(
        min_month = min(.data$MonthYYYYMM),
        max_month = max(.data$MonthYYYYMM),
        .by = dplyr::all_of(.env$vBy)
      ) %>%
      dplyr::collect()
    
    if (!is.null(tblMonthSequence)) {
      # User provided - validate first
      validate_month_sequence(tblMonthSequence, vBy)
      
      # Filter to actual study date ranges
      # For each study, filter month sequence to its actual range
      dfCompleteMonths <- tblMonthSequence
      
      # Build a filter for each study's date range
      for (i in seq_len(nrow(dfMonthRanges))) {
        study_row <- dfMonthRanges[i, , drop = FALSE]
        min_m <- study_row$min_month
        max_m <- study_row$max_month
        
        # Build filter condition for this study
        study_filter <- TRUE
        for (col in vBy) {
          if (i == 1) {
            # First iteration - start fresh
            dfCompleteMonths <- tblMonthSequence %>%
              dplyr::filter(
                .data[[col]] %in% dfMonthRanges[[col]] &
                .data$MonthYYYYMM >= min_m & 
                .data$MonthYYYYMM <= max_m
              )
            break
          }
        }
      }
    } else {
      # Auto-generate month sequences
      # Helper function to generate complete YYYYMM sequence
      generate_month_seq <- function(start_yyyymm, end_yyyymm) {
        start_year <- floor(start_yyyymm / 100)
        start_month <- start_yyyymm %% 100
        end_year <- floor(end_yyyymm / 100)
        end_month <- end_yyyymm %% 100
        
        dates <- seq(
          as.Date(paste0(start_year, "-", sprintf("%02d", start_month), "-01")),
          as.Date(paste0(end_year, "-", sprintf("%02d", end_month), "-01")),
          by = "month"
        )
        
        as.numeric(format(dates, "%Y%m"))
      }
      
      dfCompleteMonths_mem <- dfMonthRanges %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          MonthYYYYMM = list(generate_month_seq(.data$min_month, .data$max_month))
        ) %>%
        dplyr::select(-"min_month", -"max_month") %>%
        tidyr::unnest("MonthYYYYMM")
      
      # Use helper to write or error
      dfCompleteMonths <- expand_lazy_table(
        tblInput = dfAggregated,
        tblExpansion = NULL,
        dfExpansion_mem = dfCompleteMonths_mem,
        strTempTableName = "month_sequences",
        strExpansionType = "complete month sequences"
      )
    }
    
    # Left join to fill gaps (same as in-memory logic)
    dfAggregated <- dfCompleteMonths %>%
      dplyr::left_join(
        dfAggregated %>% dplyr::select(-"StudyMonth"),
        by = c(vBy, "MonthYYYYMM")
      ) %>%
      dplyr::mutate(
        Numerator = dplyr::coalesce(.data$Numerator, 0L),
        Denominator = dplyr::coalesce(.data$Denominator, 0L),
        GroupCount = dplyr::coalesce(.data$GroupCount, 0L)
      ) %>%
      # Recalculate StudyMonth as sequential numbering
      dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
      dbplyr::window_order(.data$MonthYYYYMM) %>%
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$MonthYYYYMM)
      ) %>%
      dplyr::ungroup()
  }
  
  # Calculate cumulative sums at study level
  # This ensures that when sites drop out, their cumulative contributions persist
  if (is_lazy_table(dfAggregated)) {
    dfCumulative <- dfAggregated %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
      dbplyr::window_order(.data$StudyMonth) %>%
      dplyr::mutate(
        Numerator = cumsum(.data$Numerator),
        Denominator = cumsum(.data$Denominator)
      ) %>%
      dplyr::ungroup()
  } else {
    dfCumulative <- dfAggregated %>%
      dplyr::arrange(dplyr::across(dplyr::all_of(.env$vBy)), .data$StudyMonth) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
      dplyr::mutate(
        Numerator = cumsum(.data$Numerator),
        Denominator = cumsum(.data$Denominator)
      ) %>%
      dplyr::ungroup()
  }
  
  # Apply minimum denominator filter
  dfFiltered <- dfCumulative %>%
    dplyr::filter(.data$Denominator > .env$nMinDenominator)
  
  # Re-rank StudyMonth after filtering to ensure sequential numbering
  if (is_lazy_table(dfFiltered)) {
    dfReranked <- dfFiltered %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(.env$vBy))) %>%
      dbplyr::window_order(.data$StudyMonth) %>%
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$StudyMonth)
      ) %>%
      dplyr::ungroup()
  } else {
    dfReranked <- dfFiltered %>%
      dplyr::mutate(
        StudyMonth = dplyr::dense_rank(.data$StudyMonth),
        .by = dplyr::all_of(.env$vBy)
      )
  }
  
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
  
  # Return lazy table if input was lazy, data.frame otherwise
  if (inherits(dfInput, "tbl_lazy")) {
    return(dfFinal)
  } else {
    return(as.data.frame(dfFinal))
  }
}

