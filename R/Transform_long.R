#' Transform Wide Format Results to Long Format
#'
#' @description
#' Converts wide-format dataframes from `_Wide` workflow steps back into long
#' format with standard columns (`MetricID`, `Numerator`, `Metric`, etc.).
#' Auto-detects column patterns and extracts MetricID from suffixes.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
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
#' dfLong <- Transform_long(lWide)
#' print(dfLong)
#'
#' @export
Transform_long <- function(lWide, strDenominatorCol = "DenominatorType") {
  if (!is.list(lWide) || length(lWide) == 0) {
    stop("lWide must be a non-empty named list")
  }

  if (is.null(names(lWide)) || any(names(lWide) == "")) {
    stop("lWide must have named elements (denominator types)")
  }

  lLong <- lapply(names(lWide), function(strDenomType) {
    dfWide <- lWide[[strDenomType]]

    if (!inherits(dfWide, "data.frame")) {
      stop(sprintf("Element '%s' must be a data.frame", strDenomType))
    }

    vCols <- colnames(dfWide)

    # Detect wide columns with MetricID suffixes
    vWidePatterns <- c("^Numerator_", "^Metric_", "^Median_", "^Lower_", "^Upper_")
    vWideCols <- vCols[grepl(paste(vWidePatterns, collapse = "|"), vCols)]

    if (length(vWideCols) == 0) {
      dfWide[[strDenominatorCol]] <- strDenomType
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

      dfSubset <- dfWide[, vSelectCols, drop = FALSE]

      # Rename columns to remove MetricID suffix
      vNewNames <- gsub(paste0("_", strMetricID, "$"), "", vMetricCols)
      names(dfSubset)[match(vMetricCols, names(dfSubset))] <- vNewNames

      dfSubset$MetricID <- strMetricID
      dfSubset
    })

    dfLong <- dplyr::bind_rows(lPivoted)
    dfLong[[strDenominatorCol]] <- strDenomType
    dfLong
  })

  dfResult <- dplyr::bind_rows(lLong)

  # Reorder columns: MetricID and DenominatorType first
  vFinalCols <- colnames(dfResult)
  vPriorityCols <- c("MetricID", strDenominatorCol)
  vOtherCols <- setdiff(vFinalCols, vPriorityCols)
  dfResult <- dfResult[, c(vPriorityCols, vOtherCols), drop = FALSE]

  tibble::as_tibble(dfResult)
}

