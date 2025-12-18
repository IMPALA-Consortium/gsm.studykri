#' Transform Wide Format Results to Long Format
#'
#' @description
#' Converts wide-format dataframes from `_Wide` workflow steps back into long
#' format with standard columns (`MetricID`, `Numerator`, `Metric`, etc.).
#' Auto-detects column patterns and extracts MetricID from suffixes.
#'
#' @param lWide list. Named list of wide-format dataframes keyed by denominator
#'   type (e.g., "Visits", "Days"). Each dataframe contains columns with
#'   MetricID suffixes (e.g., `Median_kri0001`, `Lower_kri0001`).
#' @param strDenominatorCol character. Column name for denominator type in
#'   output (default: "DenominatorType").
#'
#' @return A tibble in long format with columns:
#'   - `MetricID`: Extracted from column suffixes (e.g., "kri0001")
#'   - `DenominatorType`: From list keys (e.g., "Visits", "Days")
#'   - Original id columns preserved
#'   - Pivoted value columns (e.g., `Median`, `Lower`, `Upper`)
#'
#' @examples
#' # Create example wide-format data
#' dfWide <- data.frame(
#'   StudyID = c("AA-1", "AA-2"),
#'   StudyMonth = c(1, 1),
#'   Median_kri0001 = c(0.5, 0.6),
#'   Lower_kri0001 = c(0.3, 0.4),
#'   Upper_kri0001 = c(0.7, 0.8),
#'   Median_kri0003 = c(0.2, 0.25),
#'   Lower_kri0003 = c(0.1, 0.15),
#'   Upper_kri0003 = c(0.3, 0.35),
#'   BootstrapCount = c(1000, 1000)
#' )
#'
#' lWide <- list(Visits = dfWide)
#' dfLong <- Transform_Long(lWide)
#' print(dfLong)
#'
#' @export
Transform_Long <- function(lWide, strDenominatorCol = "DenominatorType") {
  if (!is.list(lWide) || length(lWide) == 0) {
    stop("lWide must be a non-empty named list")
  }

  if (is.null(names(lWide)) || any(names(lWide) == "")) {
    stop("lWide must have named elements (denominator types)")
  }

  lLong <- lapply(names(lWide), function(strDenomType) {
    dfWide <- lWide[[strDenomType]]

    if (!inherits(dfWide, c("data.frame", "tbl"))) {
      stop(sprintf("Element '%s' must be a data.frame or tbl", strDenomType))
    }

    vCols <- colnames(dfWide)

    # Detect wide columns with MetricID suffixes
    vWidePatterns <- c("^Numerator_", "^Metric_", "^Median_", "^Lower_", "^Upper_")
    vWideCols <- vCols[grepl(paste(vWidePatterns, collapse = "|"), vCols)]

    if (length(vWideCols) == 0) {
      # Use dplyr::mutate for lazy table compatibility
      dfWide <- dfWide %>%
        dplyr::mutate("{strDenominatorCol}" := strDenomType)
      return(dfWide)
    }

    # Extract unique MetricIDs from column names
    vMetricIDs <- unique(gsub("^(Numerator|Metric|Median|Lower|Upper)_", "", vWideCols))

    # Identify id columns (non-wide columns)
    vIdCols <- setdiff(vCols, vWideCols)

    # Pivot each MetricID and bind
    lPivoted <- lapply(vMetricIDs, function(strMetricID) {
      vMetricCols <- vWideCols[grepl(paste0("_", strMetricID, "$"), vWideCols)]
      vSelectCols <- c(vIdCols, vMetricCols)

      # Use dplyr::select for lazy table compatibility instead of bracket subsetting
      dfSubset <- dfWide %>%
        dplyr::select(dplyr::all_of(vSelectCols))

      # Rename columns to remove MetricID suffix using dplyr::rename for lazy table compatibility
      vNewNames <- gsub(paste0("_", strMetricID, "$"), "", vMetricCols)
      # Create named vector for rename: new_name = old_name
      vRenameVec <- stats::setNames(vMetricCols, vNewNames)
      dfSubset <- dfSubset %>%
        dplyr::rename(!!!vRenameVec)

      # Use dplyr::mutate for lazy table compatibility instead of $
      dfSubset <- dfSubset %>%
        dplyr::mutate(MetricID = strMetricID)
      
      dfSubset
    })

    # Use union_all for lazy table compatibility instead of bind_rows
    if (length(lPivoted) == 0) {
      dfLong <- NULL
    } else if (length(lPivoted) == 1) {
      dfLong <- lPivoted[[1]]
    } else {
      dfLong <- Reduce(dplyr::union_all, lPivoted)
    }
    
    # Use dplyr::mutate for lazy table compatibility instead of [[ assignment
    if (!is.null(dfLong)) {
      dfLong <- dfLong %>%
        dplyr::mutate("{strDenominatorCol}" := strDenomType)
    }
    dfLong
  })

  # Use union_all for lazy table compatibility instead of bind_rows
  if (length(lLong) == 0) {
    dfResult <- tibble::tibble()
  } else if (length(lLong) == 1) {
    dfResult <- lLong[[1]]
  } else {
    dfResult <- Reduce(dplyr::union_all, lLong)
  }

  # Reorder columns: MetricID and DenominatorType first
  vFinalCols <- colnames(dfResult)
  vPriorityCols <- c("MetricID", strDenominatorCol)
  vOtherCols <- setdiff(vFinalCols, vPriorityCols)
  # Use dplyr::select for reordering
  dfResult <- dfResult %>%
    dplyr::select(dplyr::all_of(c(vPriorityCols, vOtherCols)))

  # Return lazy table as-is, only convert regular data frames to tibble
  if (inherits(dfResult, "tbl_lazy")) {
    return(dfResult)
  }
  
  tibble::as_tibble(dfResult)
}
