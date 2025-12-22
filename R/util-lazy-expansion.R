#' Expand Lazy Table with Fallback Strategy
#'
#' @description
#' Handles expansion of lazy tables with consistent fallback:
#' 1. Use user-supplied expansion table if provided
#' 2. Attempt to write temp table to database
#' 3. Throw expressive error if write fails
#'
#' @param tblInput tbl_lazy. The lazy table context (for extracting connection)
#' @param tblExpansion tbl_lazy, data.frame, or NULL. User-supplied expansion
#' @param dfExpansion_mem data.frame. In-memory expansion data for auto-generation
#' @param strTempTableName character. Name for temp table
#' @param strExpansionType character. Description for error messages
#'
#' @return tbl_lazy expansion table that can be joined
#' @keywords internal
ExpandLazyTable <- function(tblInput,
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
  tryCatch(
    {
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
    },
    error = function(e) {
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
    }
  )
}

#' Sort Data Frame or Lazy Table
#'
#' @param data data.frame or tbl_lazy
#' @param ... Columns to sort by
#' @return Sorted data
#' @keywords internal
SortDf <- function(data, ...) {
  if (inherits(data, "data.frame")) {
    data %>% dplyr::arrange(...)
  } else if (inherits(data, "tbl_lazy")) {
    data %>% dbplyr::window_order(...)
  }
}

#' Generate Complete Month Sequence
#'
#' @param start_yyyymm numeric. Start month in YYYYMM format
#' @param end_yyyymm numeric. End month in YYYYMM format
#' @return data.frame with MonthYYYYMM column
#' @keywords internal
GenerateMonthSeq <- function(start_yyyymm, end_yyyymm) {
  start_year <- floor(start_yyyymm / 100)
  start_month <- start_yyyymm %% 100
  end_year <- floor(end_yyyymm / 100)
  end_month <- end_yyyymm %% 100

  dates <- seq(
    as.Date(paste0(start_year, "-", sprintf("%02d", start_month), "-01")),
    as.Date(paste0(end_year, "-", sprintf("%02d", end_month), "-01")),
    by = "month"
  )

  tibble::tibble(MonthYYYYMM = as.numeric(format(dates, "%Y%m")))
}
