#' Generate Unique Temporary Table Name
#'
#' Creates a unique table name by appending process ID and random integer.
#' This prevents conflicts when multiple function calls create temp tables
#' in the same database session (e.g., when processing multiple KRIs via purrr::map).
#'
#' @param base_name character. Base name for the table (e.g., "bootstrap_reps")
#' @return character. Unique table name (e.g., "bootstrap_reps_12345_678901")
#' @keywords internal
GenerateUniqueTempName <- function(base_name) {
  suffix <- paste0(Sys.getpid(), "_", sample.int(1e6, 1))
  paste0(base_name, "_", suffix)
}

#' Handle Lazy Table with Fallback Strategy
#'
#' @description
#' Handles lazy tables with consistent fallback:
#' 1. For in-memory data frames: return dfMem directly
#' 2. Use user-supplied table if provided
#' 3. Attempt to write temp table to database
#' 4. Throw expressive error if write fails
#'
#' @param tblInput tbl_lazy or data.frame. The input table context (for extracting connection if lazy)
#' @param tblUser tbl_lazy, data.frame, or NULL. User-supplied table
#' @param dfMem data.frame. In-memory data for auto-generation
#' @param strTempTableName character. Name for temp table
#' @param strTableType character. Description for error messages
#'
#' @return tbl_lazy or data.frame table that can be joined
#' @keywords internal
HandleLazyTable <- function(tblInput,
                            tblUser = NULL,
                            dfMem,
                            strTempTableName,
                            strTableType) {
  # For in-memory data frames, just return the in-memory data directly
  if (!inherits(tblInput, "tbl_lazy")) {
    return(dfMem)
  }

  # Option 1: User provided table (for lazy inputs)
  if (!is.null(tblUser)) {
    if (inherits(tblUser, "tbl_lazy")) {
      # Verify same connection
      con_input <- dbplyr::remote_con(tblInput)
      con_user <- dbplyr::remote_con(tblUser)

      if (!identical(con_input, con_user)) {
        stop(sprintf(
          "User-supplied %s table must be on the same database connection as input data",
          strTableType
        ))
      }
      return(tblUser)
    } else if (is.data.frame(tblUser)) {
      # User provided data.frame - use it for writing
      dfMem <- tblUser
    } else {
      stop(sprintf(
        "%s must be a data.frame or tbl_lazy, got: %s",
        strTableType,
        class(tblUser)[1]
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

      # Generate unique name to prevent conflicts in parallel/nested execution
      unique_name <- GenerateUniqueTempName(strTempTableName)

      # Attempt to write temp table
      tblResult <- dplyr::copy_to(
        con,
        dfMem,
        name = unique_name,
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
          "1. Supply pre-generated table via the appropriate parameter\n",
          "2. Call collect() to bring data to memory before processing\n",
          "3. Ensure database connection has CREATE TEMP TABLE privileges\n\n",
          "Original error: %s"
        ),
        strTableType,
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
#' @param start_yyyymm numeric or character. Start month in YYYYMM format.
#'   Character values (e.g., from Snowflake) are automatically coerced to numeric.
#' @param end_yyyymm numeric or character. End month in YYYYMM format.
#'   Character values (e.g., from Snowflake) are automatically coerced to numeric.
#' @return data.frame with MonthYYYYMM column
#' @keywords internal
GenerateMonthSeq <- function(start_yyyymm, end_yyyymm) {
  # Coerce to numeric to handle database backends (e.g., Snowflake) that return character
  start_yyyymm <- as.numeric(start_yyyymm)
  end_yyyymm <- as.numeric(end_yyyymm)

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
