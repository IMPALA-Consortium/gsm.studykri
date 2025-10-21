#' Expand Lazy Table with Fallback Strategy
#'
#' @description
#' Handles expansion of lazy tables with consistent fallback:
#' 1. Use user-supplied expansion table if provided
#' 2. Attempt to write temp table to database
#' 3. Throw expressive error if write fails
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#'
#' @param tblInput tbl_lazy. The lazy table context (for extracting connection)
#' @param tblExpansion tbl_lazy, data.frame, or NULL. User-supplied expansion
#' @param dfExpansion_mem data.frame. In-memory expansion data for auto-generation
#' @param strTempTableName character. Name for temp table
#' @param strExpansionType character. Description for error messages
#'
#' @return tbl_lazy expansion table that can be joined
#' @keywords internal
expand_lazy_table <- function(tblInput, 
                              tblExpansion = NULL,
                              dfExpansion_mem,
                              strTempTableName,
                              strExpansionType) {
  
  # Option 1: User provided expansion table
  if (!is.null(tblExpansion)) {
    if (inherits(tblExpansion, "tbl_lazy")) {
      # Verify same connection
      con_input <- dbplyr::remote_con(tblInput)
      con_expansion <- dbplyr::remote_con(tblExpansion)
      
      if (!identical(con_input, con_expansion)) {
        stop(sprintf(
          "User-supplied %s table must be on the same database connection as input data",
          strExpansionType
        ))
      }
      return(tblExpansion)
    } else if (is.data.frame(tblExpansion)) {
      # User provided data.frame - use it for writing
      dfExpansion_mem <- tblExpansion
    } else {
      stop(sprintf(
        "%s must be a data.frame or tbl_lazy, got: %s",
        strExpansionType,
        class(tblExpansion)[1]
      ))
    }
  }
  
  # Option 2: Attempt to write temp table
  tryCatch({
    con <- dbplyr::remote_con(tblInput)
    
    if (is.null(con)) {
      stop("Cannot extract database connection from lazy table")
    }
    
    # Attempt to write temp table
    tblResult <- dplyr::copy_to(
      con, 
      dfExpansion_mem, 
      name = strTempTableName, 
      temporary = TRUE, 
      overwrite = TRUE
    )
    
    return(tblResult)
    
  }, error = function(e) {
    stop(sprintf(
      paste0(
        "Failed to create %s for lazy table.\n\n",
        "Possible solutions:\n",
        "1. Supply pre-generated expansion via the appropriate parameter\n",
        "2. Call collect() to bring data to memory before processing\n",
        "3. Ensure database connection has CREATE TEMP TABLE privileges\n\n",
        "Original error: %s"
      ),
      strExpansionType,
      conditionMessage(e)
    ), call. = FALSE)
  })
}

#' Validate Month Sequence Has No Gaps
#'
#' @param tblMonthSeq tbl_lazy or data.frame. Month sequence to validate
#' @param vGroupCols character. Grouping columns (e.g., "StudyID")
#' @keywords internal
validate_month_sequence <- function(tblMonthSeq, vGroupCols) {
  # Collect to check (small sample if large)
  sample <- tblMonthSeq %>% 
    dplyr::collect()
  
  if (nrow(sample) == 0) {
    stop("Month sequence table is empty")
  }
  
  # Check required columns BEFORE trying to sort
  if (!"MonthYYYYMM" %in% names(sample)) {
    stop("Month sequence must have 'MonthYYYYMM' column")
  }
  
  for (col in vGroupCols) {
    if (!col %in% names(sample)) {
      stop(sprintf("Month sequence must have '%s' column", col))
    }
  }
  
  # Now we can safely sort
  sample <- sample %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(vGroupCols)), .data$MonthYYYYMM)
  
  # Generate expected sequence helper
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
  
  # For each group, verify no gaps
  for (grp_val in unique(sample[[vGroupCols[1]]])) {
    grp_data <- sample %>% dplyr::filter(.data[[vGroupCols[1]]] == grp_val)
    months <- sort(grp_data$MonthYYYYMM)
    
    # Generate expected complete sequence
    expected <- generate_month_seq(min(months), max(months))
    
    if (!all(expected %in% months)) {
      missing <- setdiff(expected, months)
      stop(sprintf(
        paste0(
          "Month sequence for %s='%s' has gaps. Missing months: %s\n\n",
          "User-supplied month sequences must be complete with no gaps."
        ),
        vGroupCols[1], grp_val, paste(missing, collapse=", ")
      ), call. = FALSE)
    }
  }
  
  invisible(TRUE)
}

