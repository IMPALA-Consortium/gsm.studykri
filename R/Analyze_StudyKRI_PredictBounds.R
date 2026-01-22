#' Calculate Confidence Intervals from Bootstrap Distribution (Internal)
#'
#' @description
#' Internal helper function that calculates percentile-based confidence intervals
#' from bootstrap distributions of study-level KRI metrics. Returns median estimates
#' with upper and lower confidence bounds for each time point. Supports both
#' single-study and multi-study comparison scenarios with full dbplyr compatibility.
#' Auto-detects multiple Metric columns (e.g., `Metric_Analysis_kri0001`) and
#' calculates bounds for each.
#'
#' This function is intended for internal use. For the main workflow, use
#' `Analyze_StudyKRI_PredictBounds()` which calls this function internally.
#'
#' @param dfInput data.frame or tbl. Bootstrapped study-level data from
#'   `Transform_CumCount` with `BootstrapRep` column. Must contain columns:
#'   `vBy` columns, `StudyMonth`, one or more `Metric_*` columns, `BootstrapRep`.
#' @param vBy character. Vector of column names for grouping CI calculation.
#'   Use `"StudyID"` for single study analysis, `character(0)` for combined
#'   multi-study analysis. Default: `"StudyID"`.
#' @param nConfLevel numeric. Confidence level between 0 and 1. Default: 0.95
#'   (95% confidence interval).
#' @param strStudyMonthCol character. Name of study month column.
#'   Default: `"StudyMonth"`.
#'
#' @return A tibble (or tbl_lazy if input was lazy) with columns:
#'   - `vBy` columns (if `length(vBy) > 0`)
#'   - `StudyMonth`: Sequential month number
#'   - `Median_*`, `Lower_*`, `Upper_*`: Bounds for each Metric column
#'     (e.g., `Median_Analysis_kri0001`, `Lower_Analysis_kri0001`, `Upper_Analysis_kri0001`)
#'   - `BootstrapCount`: Number of bootstrap samples used
#'
#' @keywords internal
#' @noRd
CalculateStudyBounds <- function(
    dfInput,
    vBy = "StudyID",
    nConfLevel = 0.95,
    strStudyMonthCol = "StudyMonth") {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data.frame or tbl object")
  }

  # Auto-detect Metric columns (matches both "Metric" and "Metric_*")
  vMetricCols <- grep("^Metric", colnames(dfInput), value = TRUE)
  if (length(vMetricCols) == 0) {
    stop("dfInput must have at least one Metric column")
  }

  # Check required columns (use colnames for lazy table compatibility)
  required_cols <- c("BootstrapRep", strStudyMonthCol)
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Check vBy columns if specified
  if (length(vBy) > 0) {
    missing_by <- setdiff(vBy, colnames(dfInput))
    if (length(missing_by) > 0) {
      stop(sprintf(
        "vBy columns not found in dfInput: %s",
        paste(missing_by, collapse = ", ")
      ))
    }
  }

  # Validate confidence level
  if (!is.numeric(nConfLevel) || length(nConfLevel) != 1 ||
    nConfLevel <= 0 || nConfLevel >= 1) {
    stop("nConfLevel must be a single numeric value between 0 and 1")
  }

  # Calculate lower and upper percentiles based on confidence level
  lower_percentile <- (1 - nConfLevel) / 2
  upper_percentile <- 1 - lower_percentile

  # Build grouping columns (vBy + StudyMonth)
  group_cols <- if (length(vBy) > 0) {
    c(vBy, strStudyMonthCol)
  } else {
    strStudyMonthCol
  }

  # Calculate summary statistics across bootstrap replicates for all Metric columns
  dfBounds <- dfInput %>%
    dplyr::summarise(
      dplyr::across(
        .cols = dplyr::all_of(.env$vMetricCols),
        .fns = list(
          Median = ~ median(.x, na.rm = TRUE),
          Lower = ~ quantile(.x, .env$lower_percentile, na.rm = TRUE),
          Upper = ~ quantile(.x, .env$upper_percentile, na.rm = TRUE)
        ),
        .names = "{.fn}_{gsub('^Metric_', '', .col)}"
      ),
      BootstrapCount = dplyr::n(),
      .by = dplyr::all_of(.env$group_cols)
    ) %>%
    # remove trailing metric from colnames which is the case for single metric
    dplyr::rename_with(~ stringr::str_replace(., "_Metric", ""))

  # Arrange by grouping columns and study month
  dfResult <- dfBounds %>%
    SortDf(dplyr::across(dplyr::all_of(.env$group_cols)))

  return(dfResult)
}

#' Calculate Bootstrap-Based Confidence Bounds for Study-Level KRI
#'
#' @description
#' Calculates confidence bounds for study-level KRI metrics using bootstrap resampling.
#' This is a convenience function that orchestrates the complete workflow:
#' \enumerate{
#'   \item Filters input data to target studies (specified in dfStudyRef or all studies if NULL)
#'   \item Generates bootstrap resamples via `BootstrapStudyKRI()`
#'   \item Aggregates to study level via `Transform_CumCount()`
#'   \item Calculates confidence intervals via `CalculateStudyBounds()`
#' }
#'
#' Note: Date normalization and filtering should be done at the input level via
#' Input_CountSiteByMonth with nMinDenominator parameter.
#'
#' @param dfInput data.frame or tbl_lazy. Group-level data from `Input_CountSiteByMonth`.
#'   Expected columns: GroupID, one or more Numerator columns, Denominator,
#'   StudyID, MonthYYYYMM, StudyMonth.
#' @param dfStudyRef data.frame or NULL. Optional study reference mapping. If provided,
#'   the unique values in the first column identify the target studies for which
#'   bounds will be calculated. If NULL (default), bounds are calculated for all
#'   studies found in dfInput.
#' @param nBootstrapReps integer. Number of bootstrap replicates to generate.
#'   Default: 1000.
#' @param nConfLevel numeric. Confidence level between 0 and 1. Default: 0.95
#'   (95% confidence interval).
#' @param seed integer or NULL. Random seed for reproducibility. Default: NULL.
#' @param tblBootstrapReps tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated bootstrap replicate indices. Default: NULL.
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated complete month sequences. Default: NULL.
#' @param strStudyCol character. Column name for study identifier in dfInput.
#'   Default: "StudyID".
#' @param strGroupCol character. Column name for group identifier. Default: "GroupID".
#' @param strStudyMonthCol character. Name of study month column. Default: "StudyMonth".
#'
#' @return A tibble (or tbl_lazy if input was lazy) with confidence intervals:
#'   - `StudyID`: Target study identifier
#'   - `StudyMonth`: Sequential month number
#'   - `Median_*`, `Lower_*`, `Upper_*`: Bounds for each Metric column
#'   - `BootstrapCount`: Number of bootstrap samples used
#'
#' @examples
#' \dontrun{
#' # Example 1: Calculate bounds for all studies in dfInput
#' Bounds_Wide <- purrr::map(
#'   lJoined,
#'   ~ Analyze_StudyKRI_PredictBounds(.)
#' )
#'
#' # Example 2: Calculate bounds for specific studies using reference mapping
#' dfStudyRef <- data.frame(
#'   study = c("AA-1", "AA-1", "AA-2", "AA-2"),
#'   studyref = c("AA-3", "AA-4", "AA-3", "AA-5")
#' )
#'
#' # Calculate bounds for target studies (AA-1 and AA-2) specified in first column
#' Bounds_Wide <- purrr::map(
#'   lJoined,
#'   ~ Analyze_StudyKRI_PredictBounds(., dfStudyRef = dfStudyRef)
#' )
#' }
#'
#' @export
Analyze_StudyKRI_PredictBounds <- function(
    dfInput,
    dfStudyRef = NULL,
    nBootstrapReps = 1000,
    nConfLevel = 0.95,
    seed = NULL,
    tblBootstrapReps = NULL,
    tblMonthSequence = NULL,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    strStudyMonthCol = "StudyMonth") {
  # Handle dfStudyRef - extract target studies
  if (is.null(dfStudyRef)) {
    # Extract all unique study IDs from input
    vTargetStudies <- dfInput %>%
      dplyr::select(dplyr::all_of(.env$strStudyCol)) %>%
      dplyr::distinct() %>%
      dplyr::pull(.env$strStudyCol)

    message(sprintf(
      "Using all %d studies found in dfInput",
      length(vTargetStudies)
    ))
  } else {
    # Input validation for dfStudyRef - accept data.frame or tbl (including tbl_lazy)
    if (!inherits(dfStudyRef, c("data.frame", "tbl"))) {
      stop("dfStudyRef must be a data.frame, tbl object, or NULL")
    }

    if (ncol(dfStudyRef) == 0) {
      stop("dfStudyRef must have at least one column")
    }

    # Extract target studies from first column of dfStudyRef using pull() for lazy table compatibility
    strStudyRefCol <- colnames(dfStudyRef)[1]
    vTargetStudies <- dfStudyRef %>%
      dplyr::select(dplyr::all_of(.env$strStudyRefCol)) %>%
      dplyr::distinct() %>%
      dplyr::pull(.env$strStudyRefCol)

    if (length(vTargetStudies) == 0) {
      stop("No target studies found in dfStudyRef")
    }
  }

  # Filter input data to target studies only
  dfFiltered <- dfInput %>%
    dplyr::filter(.data[[strStudyCol]] %in% .env$vTargetStudies)

  # Step 1: Generate bootstrap resamples
  dfBootstrapped <- BootstrapStudyKRI(
    dfInput = dfFiltered,
    nBootstrapReps = nBootstrapReps,
    nGroups = NULL,
    strStudyCol = strStudyCol,
    strGroupCol = strGroupCol,
    seed = seed,
    tblBootstrapReps = tblBootstrapReps
  )

  # Step 2: Aggregate to study level by bootstrap replicate and study
  dfStudyLevel <- Transform_CumCount(
    dfInput = dfBootstrapped,
    vBy = c("BootstrapRep", strStudyCol),
    tblMonthSequence = tblMonthSequence
  )

  # Step 3: Calculate confidence intervals
  dfBounds <- CalculateStudyBounds(
    dfInput = dfStudyLevel,
    vBy = strStudyCol,
    nConfLevel = nConfLevel,
    strStudyMonthCol = strStudyMonthCol
  )

  return(dfBounds)
}


#' Generate Bootstrap Resamples for Study-Level KRI Analysis (Internal)
#'
#' @description
#' Internal helper function that generates bootstrap resamples by resampling
#' groups (sites/countries) with replacement within each study. Uses dbplyr-compatible
#' approach with runif() for random selection. Each bootstrap replicate randomly
#' selects groups and includes all their associated data.
#'
#' This function is intended for internal use. For the main workflow, use
#' `Analyze_StudyKRI_PredictBounds()` which calls this function internally.
#'
#' @param dfInput data.frame or tbl_lazy. Group-level data from `Input_CumCountSiteByMonth`.
#'   Expected columns: GroupID, GroupLevel, Numerator, Denominator, Metric,
#'   StudyID, MonthYYYYMM (or custom column names specified in parameters).
#' @param nBootstrapReps integer. Number of bootstrap replicates to generate.
#'   Defaults to 1000.
#' @param nGroups integer or NULL. Number of groups to resample per replicate.
#'   If NULL (default), uses the actual number of groups per study. If specified,
#'   can be used to upsample (larger than actual) or downsample (smaller than actual).
#' @param strStudyCol character. Column name for study identifier. Defaults to "StudyID".
#' @param strGroupCol character. Column name for group identifier. Defaults to "GroupID".
#' @param seed integer or NULL. Random seed for reproducibility. If NULL (default),
#'   no seed is set. Note: seed only affects in-memory data frames, not SQL queries.
#' @param tblBootstrapReps tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated bootstrap replicate indices. Must have 'BootstrapRep'
#'   column with values 1 to nBootstrapReps. If NULL, attempts to create temp table
#'   (requires write privileges).
#'
#' @return A data.frame (or tbl_lazy if input was lazy) with all original columns plus:
#'   - `BootstrapRep`: integer, bootstrap replicate number (1 to nBootstrapReps)
#'   - Original data rows are replicated across bootstrap samples with groups
#'     resampled with replacement
#'
#' @keywords internal
#' @noRd
BootstrapStudyKRI <- function(
    dfInput,
    nBootstrapReps = 1000,
    nGroups = NULL,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    seed = NULL,
    tblBootstrapReps = NULL) {
  # Input Validation
  if (!is.data.frame(dfInput) && !inherits(dfInput, "tbl_lazy")) {
    stop("dfInput must be a data frame or lazy table")
  }
  if (!strStudyCol %in% colnames(dfInput)) {
    stop(sprintf("strStudyCol '%s' not found in dfInput", strStudyCol))
  }
  if (!strGroupCol %in% colnames(dfInput)) {
    stop(sprintf("strGroupCol '%s' not found in dfInput", strGroupCol))
  }
  if (!is.numeric(nBootstrapReps) || length(nBootstrapReps) != 1 || nBootstrapReps < 1) {
    stop("nBootstrapReps must be a single positive integer")
  }
  nBootstrapReps <- as.integer(nBootstrapReps)

  if (!is.null(nGroups)) {
    if (!is.numeric(nGroups) || length(nGroups) != 1 || nGroups < 1) {
      stop("nGroups must be NULL or a single positive integer")
    }
    nGroups <- as.integer(nGroups)
  }

  # Check for empty data
  if (is.data.frame(dfInput) && nrow(dfInput) == 0) {
    stop("dfInput has no rows")
  }

  # Set random seed if provided (only affects in-memory operations)
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be NULL or a single numeric value")
    }
    set.seed(seed)
  }

  # Create replicate index
  dfReps_mem <- tibble::tibble(
    BootstrapRep = seq_len(nBootstrapReps)
  )

  # HandleLazyTable handles both lazy and in-memory cases
  dfReps <- HandleLazyTable(
    tblInput = dfInput,
    tblUser = tblBootstrapReps,
    dfMem = dfReps_mem,
    strTempTableName = "bootstrap_reps",
    strTableType = "bootstrap replicate indices"
  )

  # Step 1: Number each group sequentially within study (following studykri.Rmd pattern)
  dfGroupsNumbered <- dfInput %>%
    dplyr::select(dplyr::all_of(c(.env$strStudyCol, .env$strGroupCol))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      GroupNumber = dplyr::dense_rank(.data[[strGroupCol]]),
      .by = dplyr::all_of(.env$strStudyCol)
    )

  # Calculate group counts per study
  dfGroupCounts <- dfGroupsNumbered %>%
    dplyr::summarise(
      ActualGroupCount = dplyr::n(),
      .by = dplyr::all_of(.env$strStudyCol)
    )

  # Determine effective group count for sampling
  if (is.null(nGroups)) {
    dfGroupCounts <- dfGroupCounts %>%
      dplyr::mutate(EffectiveGroupCount = .data$ActualGroupCount)
  } else {
    dfGroupCounts <- dfGroupCounts %>%
      dplyr::mutate(EffectiveGroupCount = .env$nGroups)
  }

  # Step 2: Generate bootstrap selections (matching get_boot pattern)
  # Simplified: cross_join with dfGroupsNumbered creates groups × reps rows automatically!

  if (is.null(nGroups)) {
    # Standard case: sample all groups with replacement (no positions needed!)
    dfBootstrapSelections <- dfGroupsNumbered %>%
      dplyr::cross_join(dfReps) %>% # Creates group_count × reps rows!
      dplyr::left_join(dfGroupCounts, by = strStudyCol) %>%
      dplyr::mutate(
        # runif(n()) where n() = groups (per study-rep group)
        rnd_value = stats::runif(dplyr::n()),
        # Random selection between 1 and ActualGroupCount
        GroupNumber = floor(.data$rnd_value * .data$ActualGroupCount) + 1L,
        .by = c(dplyr::all_of(.env$strStudyCol), "BootstrapRep")
      ) %>%
      dplyr::select(
        dplyr::all_of(.env$strStudyCol),
        "BootstrapRep",
        "GroupNumber"
      )
  } else {
    # nGroups specified: need exactly that many selections per study-rep
    # Note: This handles both upsampling (nGroups > actual) and downsampling
    # For lazy tables with nGroups, positions table is auto-created or user-supplied
    # This is a less common case, so positions table is acceptable

    # Get maximum nGroups for determining positions needed
    # Since EffectiveGroupCount was just set to nGroups for all rows (line 123),
    # the max is simply nGroups itself - no need to query the table
    max_effective <- nGroups

    dfPositions_mem <- tibble::tibble(Position = seq_len(as.integer(max_effective)))

    # HandleLazyTable handles both lazy and in-memory cases
    dfPositions <- HandleLazyTable(
      tblInput = dfInput,
      tblUser = NULL,
      dfMem = dfPositions_mem,
      strTempTableName = "bootstrap_positions",
      strTableType = "position indices for nGroups"
    )

    # Generate selections using positions for expansion
    dfBootstrapSelections <- dfGroupsNumbered %>%
      dplyr::distinct(.data[[strStudyCol]]) %>%
      dplyr::cross_join(dfReps) %>%
      dplyr::left_join(dfGroupCounts, by = strStudyCol) %>%
      dplyr::cross_join(dfPositions) %>%
      dplyr::filter(.data$Position <= .data$EffectiveGroupCount) %>%
      dplyr::mutate(
        rnd_value = stats::runif(dplyr::n()),
        GroupNumber = floor(.data$rnd_value * .data$ActualGroupCount) + 1L,
        .by = c(dplyr::all_of(.env$strStudyCol), "BootstrapRep")
      ) %>%
      dplyr::select(
        dplyr::all_of(.env$strStudyCol),
        "BootstrapRep",
        "GroupNumber"
      )
  }

  # Prepare lookup table with numbered groups
  dfLookup <- dfInput %>%
    dplyr::left_join(
      dfGroupsNumbered,
      by = c(strStudyCol, strGroupCol)
    )

  # Step 3: Join to get actual data
  dfBootstrapData <- dfBootstrapSelections %>%
    dplyr::left_join(
      dfLookup,
      by = c(strStudyCol, "GroupNumber"),
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-"GroupNumber")

  return(dfBootstrapData)
}
