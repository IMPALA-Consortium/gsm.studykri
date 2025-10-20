#' Generate Bootstrap Resamples for Study-Level KRI Analysis
#'
#' @description
#' Generates bootstrap resamples by resampling sites/groups with replacement
#' within each study. This enables calculation of confidence intervals for
#' study-level KRI metrics through Monte Carlo simulation. Each bootstrap
#' replicate randomly selects sites (with replacement) and includes all their
#' associated data.
#'
#' @importFrom dplyr %>%
#' @importFrom rlang .data .env :=
#'
#' @param dfInput data.frame. Site-level data from `Input_CumCountSiteByMonth`.
#'   Expected columns: GroupID, GroupLevel, Numerator, Denominator, Metric,
#'   StudyID, MonthYYYYMM (or custom column names specified in parameters).
#' @param nBootstrapReps integer. Number of bootstrap replicates to generate.
#'   Defaults to 1000.
#' @param nSites integer or NULL. Number of sites to resample per replicate.
#'   If NULL (default), uses the actual number of sites per study. If specified,
#'   can be used to upsample (larger than actual) or downsample (smaller than actual).
#' @param strStudyCol character. Column name for study identifier. Defaults to "StudyID".
#' @param strGroupCol character. Column name for group/site identifier. Defaults to "GroupID".
#' @param seed integer or NULL. Random seed for reproducibility. If NULL (default),
#'   no seed is set and results will vary between runs.
#'
#' @return A data.frame with all original columns plus:
#'   - `BootstrapRep`: integer, bootstrap replicate number (1 to nBootstrapReps)
#'   - Original data rows are replicated across bootstrap samples with sites
#'     resampled with replacement
#'
#' @examples
#' # Generate site-level data
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
  nSites = NULL,
  strStudyCol = "StudyID",
  strGroupCol = "GroupID",
  seed = NULL
) {
  # Input Validation
  if (!is.data.frame(dfInput)) stop("dfInput must be a data frame")
  if (!strStudyCol %in% names(dfInput)) {
    stop(sprintf("strStudyCol '%s' not found in dfInput", strStudyCol))
  }
  if (!strGroupCol %in% names(dfInput)) {
    stop(sprintf("strGroupCol '%s' not found in dfInput", strGroupCol))
  }
  if (!is.numeric(nBootstrapReps) || length(nBootstrapReps) != 1 || nBootstrapReps < 1) {
    stop("nBootstrapReps must be a single positive integer")
  }
  nBootstrapReps <- as.integer(nBootstrapReps)
  
  if (!is.null(nSites)) {
    if (!is.numeric(nSites) || length(nSites) != 1 || nSites < 1) {
      stop("nSites must be NULL or a single positive integer")
    }
    nSites <- as.integer(nSites)
  }
  
  if (nrow(dfInput) == 0) {
    stop("dfInput has no rows")
  }
  
  # Set random seed if provided
  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1) {
      stop("seed must be NULL or a single numeric value")
    }
    set.seed(seed)
  }
  
  # Create replicate index
  dfReps <- data.frame(
    BootstrapRep = seq_len(nBootstrapReps)
  )
  
  # Get unique study-site combinations and assign sequential site numbers
  dfSites <- dfInput %>%
    dplyr::distinct(.data[[strStudyCol]], .data[[strGroupCol]]) %>%
    dplyr::mutate(
      SiteNumber = dplyr::dense_rank(.data[[strGroupCol]]),
      .by = dplyr::all_of(.env$strStudyCol)
    )
  
  # Calculate number of sites per study
  dfSiteCounts <- dfSites %>%
    dplyr::summarise(
      ActualSiteCount = dplyr::n_distinct(.data[[strGroupCol]]),
      .by = dplyr::all_of(.env$strStudyCol)
    )
  
  # Determine effective site count for sampling
  # If nSites is NULL, use actual site counts; otherwise use specified value
  if (is.null(nSites)) {
    dfSiteCounts <- dfSiteCounts %>%
      dplyr::mutate(EffectiveSiteCount = .data$ActualSiteCount)
  } else {
    dfSiteCounts <- dfSiteCounts %>%
      dplyr::mutate(EffectiveSiteCount = .env$nSites)
  }
  
  # Create bootstrap backbone: study × replicate combinations
  dfBootstrapBackbone <- dfSites %>%
    dplyr::distinct(.data[[strStudyCol]]) %>%
    dplyr::cross_join(dfReps) %>%
    dplyr::left_join(
      dfSiteCounts,
      by = strStudyCol
    )
  
  # Generate random site selections for each study × replicate
  # We need one random site selection per position
  dfBootstrapSelections <- dfBootstrapBackbone %>%
    dplyr::rowwise() %>%
    dplyr::reframe(
      BootstrapRep = .data$BootstrapRep,
      !!strStudyCol := .data[[strStudyCol]],
      # Generate EffectiveSiteCount random site numbers
      SiteNumber = sample.int(
        n = .data$ActualSiteCount,
        size = .data$EffectiveSiteCount,
        replace = TRUE
      )
    ) %>%
    dplyr::ungroup()
  
  # Prepare input data with site numbers for joining
  dfInputWithSiteNum <- dfInput %>%
    dplyr::left_join(
      dfSites,
      by = c(strStudyCol, strGroupCol)
    )
  
  # Join bootstrap selections to actual data
  dfBootstrapData <- dfBootstrapSelections %>%
    dplyr::left_join(
      dfInputWithSiteNum,
      by = c(strStudyCol, "SiteNumber"),
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-"SiteNumber") %>%
    dplyr::arrange(
      .data[[strStudyCol]],
      .data$BootstrapRep,
      dplyr::across(dplyr::any_of("MonthYYYYMM")),
      .data[[strGroupCol]]
    )
  
  return(as.data.frame(dfBootstrapData))
}

