#' Generate Bootstrap Resamples for Study-Level KRI Analysis
#'
#' @description
#' Generates bootstrap resamples by resampling groups (sites/countries) with 
#' replacement within each study. Uses dbplyr-compatible approach with runif()
#' for random selection. Each bootstrap replicate randomly selects groups and
#' includes all their associated data.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env
#' @importFrom stats runif
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
#'
#' @return A data.frame (or tbl_lazy if input was lazy) with all original columns plus:
#'   - `BootstrapRep`: integer, bootstrap replicate number (1 to nBootstrapReps)
#'   - Original data rows are replicated across bootstrap samples with groups
#'     resampled with replacement
#'
#' @examples
#' # Generate group-level data
#' dfSubjects <- clindata::rawplus_dm
#' dfNumerator <- clindata::rawplus_ae
#' dfDenominator <- clindata::rawplus_visdt
#'
#' dfInput <- Input_CumCountSiteByMonth(
#'   dfSubjects = dfSubjects,
#'   dfNumerator = dfNumerator,
#'   dfDenominator = dfDenominator,
#'   strNumeratorDateCol = "aest_dt",
#'   strDenominatorDateCol = "visit_dt"
#' )
#'
#' # Generate bootstrap samples
#' dfBootstrap <- Analyze_StudyKRI(
#'   dfInput = dfInput,
#'   nBootstrapReps = 100,
#'   seed = 42
#' )
#'
#' # Aggregate each bootstrap replicate to study level
#' dfStudyLevel <- Transform_CumCount(
#'   dfInput = dfBootstrap,
#'   vBy = c("StudyID", "BootstrapRep"),
#'   nMinDenominator = 25
#' )
#'
#' print(head(dfStudyLevel))
#'
#' @export
Analyze_StudyKRI <- function(
  dfInput,
  nBootstrapReps = 1000,
  nGroups = NULL,
  strStudyCol = "StudyID",
  strGroupCol = "GroupID",
  seed = NULL
) {
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
  
  # Helper to detect lazy tables
  is_lazy_table <- function(x) {
    inherits(x, "tbl_lazy")
  }
  
  # Set random seed if provided (only affects in-memory operations)
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be NULL or a single numeric value")
    }
    set.seed(seed)
  }
  
  # Detect if working with lazy tables
  is_lazy <- is_lazy_table(dfInput)
  
  # Create replicate index
  dfReps_mem <- data.frame(
    BootstrapRep = seq_len(nBootstrapReps)
  )
  
  # If lazy table, write helper data frames to database
  if (is_lazy) {
    con <- dbplyr::remote_con(dfInput)
    dfReps <- dplyr::copy_to(con, dfReps_mem, name = "bootstrap_reps", temporary = TRUE, overwrite = TRUE)
  } else {
    dfReps <- dfReps_mem
  }
  
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
  
  # Prepare lookup table with numbered groups
  dfLookup <- dfInput %>%
    dplyr::left_join(
      dfGroupsNumbered,
      by = c(strStudyCol, strGroupCol)
    )
  
  # Step 2: Generate bootstrap backbone with random group numbers (dbplyr compatible)
  # Create position sequence for expansion (maximum EffectiveGroupCount across all studies)
  max_effective <- if (is.data.frame(dfGroupCounts)) {
    max(dfGroupCounts$EffectiveGroupCount)
  } else {
    # For lazy tables, use a large enough number
    if (is.null(nGroups)) 1000 else nGroups
  }
  
  dfPositions_mem <- data.frame(Position = seq_len(max_effective))
  
  # If lazy table, write positions to database
  if (is_lazy) {
    dfPositions <- dplyr::copy_to(con, dfPositions_mem, name = "positions", temporary = TRUE, overwrite = TRUE)
  } else {
    dfPositions <- dfPositions_mem
  }
  
  # Following studykri.Rmd approach: cross_join creates row context for runif()
  dfBootstrapSelections <- dfGroupsNumbered %>%
    dplyr::distinct(.data[[strStudyCol]]) %>%
    dplyr::cross_join(dfReps) %>%
    dplyr::left_join(dfGroupCounts, by = strStudyCol) %>%
    dplyr::cross_join(dfPositions) %>%
    dplyr::filter(.data$Position <= .data$EffectiveGroupCount) %>%
    dplyr::mutate(
      # Key: Within each study-replicate group, runif(n()) generates as many random values
      # as there are rows (positions) - this is exactly EffectiveGroupCount
      rnd_value = runif(dplyr::n()),
      GroupNumber = floor(.data$rnd_value * .data$ActualGroupCount) + 1L,
      .by = c(dplyr::all_of(.env$strStudyCol), "BootstrapRep")
    ) %>%
    dplyr::select(
      dplyr::all_of(.env$strStudyCol),
      "BootstrapRep",
      "GroupNumber"
    )
  
  # Step 3: Join to get actual data
  dfBootstrapData <- dfBootstrapSelections %>%
    dplyr::left_join(
      dfLookup,
      by = c(strStudyCol, "GroupNumber"),
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-"GroupNumber")
  
  # Return (keeping as lazy table if input was lazy, or convert to data.frame)
  if (inherits(dfInput, "tbl_lazy")) {
    return(dfBootstrapData)
  } else {
    return(as.data.frame(dfBootstrapData))
  }
}
