#' Predict Bounds for Combined Group of Studies
#'
#' @description
#' Calculates percentile-based confidence intervals (bounds) for a combined portfolio
#' of comparison studies. This function creates a "comparison envelope" by:
#' \enumerate{
#'   \item Finding the minimum group (site) count across selected studies
#'   \item Resampling each study with that minimum to ensure fair comparison
#'   \item Combining data across all studies
#'   \item Calculating confidence intervals for the pooled distribution
#' }
#'
#' This enables comparing one study's KRI against expected variation from a portfolio
#' of similar studies.
#'
#' @details
#' The `bMixStudies` parameter controls when studies are aggregated:
#' \itemize{
#'   \item FALSE (default): Calculate 1 timeline per reference study and bootstrap
#'    iteration then aggeregate to calculate confidence intervals. Preserves
#'    study variability.
#'   \item TRUE: Mix all sampled sites from all reference studies to calculate
#'    one common timeline per bootstrap iteration. (More efficient calculation
#'    but inter study variability not well conserved)
#' }
#'
#' @param dfInput data.frame or tbl_lazy. Site-level data from `Input_CumCountSiteByMonth`.
#'   Must contain columns: `StudyID`, `GroupID`, one or more `Numerator_*` columns,
#'   `Denominator`, `MonthYYYYMM`.
#' @param vStudyFilter character or NULL. Study IDs to include in comparison group.
#'   If NULL (default), uses all studies found in dfInput.
#'   Example: `c("STUDY1", "STUDY2", "STUDY3")`.
#' @param nBootstrapReps integer. Number of bootstrap replicates (default: 1000).
#' @param nConfLevel numeric. Confidence level for the bounds, between 0 and 1
#'   (default: 0.95 for 95% CI).
#' @param strStudyCol character. Column name for study identifier (default: "StudyID").
#' @param strGroupCol character. Column name for group identifier (default: "GroupID").
#' @param strStudyMonthCol character. Column name for sequential study month
#'   (default: "StudyMonth").
#' @param bMixStudies logical. Default: False, see Details
#' @param seed integer or NULL. Random seed for reproducibility (default: NULL).
#' @param tblBootstrapReps tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated bootstrap replicate indices with a `BootstrapRep` column
#'   containing values 1 to N (where N is the desired number of replicates).
#'   If provided, overrides the `nBootstrapReps` parameter. If NULL, attempts to
#'   create temp table (requires write privileges).
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated month sequence with only a `MonthYYYYMM` column
#'   (output of `GenerateMonthSeq()`). If NULL, attempts to create temp table
#'   (requires write privileges).
#' @param vDbIntRandomRange Numeric vector of length 2 or NULL. When using database
#'   backends that return large integers instead of 0-1 decimals for random numbers,
#'   specify the min/max range as c(min, max). Accepts both numeric and character
#'   vectors (character values are automatically converted to numeric, useful when
#'   reading from YAML files). Common values:
#'   - Snowflake: c(-9223372036854775808, 9223372036854775807) (signed 64-bit)
#'   - Other backends: c(0, 18446744073709551615) (unsigned 64-bit)
#'   Default: NULL (no normalization, assumes 0-1 decimal random values).
#' @param nMinGroups integer or NULL. Minimum number of groups for bootstrapping.
#'   If NULL (default), calculated as min(n_distinct(GroupID)) across vStudyFilter.
#'   Providing this value avoids an expensive collect() operation on database backends.
#'   This value should represent the minimum group count across all reference studies.
#'   Default: NULL.
#'
#' @return A tibble (or tbl_lazy if input was lazy) with confidence intervals.
#'   Output columns: `StudyMonth`, `Median_*`, `Lower_*`, `Upper_*` for each Metric column,
#'   `BootstrapCount`, `GroupCount`, `StudyCount`.
#'
#' @examples
#' # Create example site-level data for multiple studies
#' dfSiteLevel <- data.frame(
#'   StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 60),
#'   GroupID = rep(paste0("Site", 1:10), each = 6, times = 3),
#'   Numerator = sample(0:5, 180, replace = TRUE),
#'   Denominator = sample(10:20, 180, replace = TRUE),
#'   MonthYYYYMM = rep(rep(202301:202306, each = 10), times = 3),
#'   Metric = runif(180, 0.1, 0.5),
#'   GroupLevel = "Site",
#'   stringsAsFactors = FALSE
#' )
#'
#' # Calculate comparison envelope from 3 studies
#' dfGroupBounds <- Analyze_StudyKRI_PredictBoundsRefSet(
#'   dfInput = dfSiteLevel,
#'   vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
#'   nBootstrapReps = 100, # Use small number for example
#'   nConfLevel = 0.95,
#'   seed = 42
#' )
#'
#' # Example with Snowflake backend
#' dfGroupBounds_Snowflake <- Analyze_StudyKRI_PredictBoundsRefSet(
#'   dfInput = dfSiteLevel,
#'   vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
#'   nBootstrapReps = 100,
#'   vDbIntRandomRange = c(-9223372036854775808, 9223372036854775807)
#' )
#'
#' # Example with early study mixing (faster SQL)
#' dfGroupBounds_Mixed <- Analyze_StudyKRI_PredictBoundsRefSet(
#'   dfInput = dfSiteLevel,
#'   vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
#'   nBootstrapReps = 100,
#'   bMixStudies = TRUE,  # Aggregate studies early for SQL performance
#'   seed = 42
#' )
#'
#' print(head(dfGroupBounds))
#'
#' @export
Analyze_StudyKRI_PredictBoundsRefSet <- function(
    dfInput,
    vStudyFilter = NULL,
    nBootstrapReps = 1000,
    nConfLevel = 0.95,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    strStudyMonthCol = "StudyMonth",
    bMixStudies = FALSE,
    seed = NULL,
    tblBootstrapReps = NULL,
    tblMonthSequence = NULL,
    vDbIntRandomRange = NULL,
    nMinGroups = NULL) {
  # Input Validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfInput, c("data.frame", "tbl"))) {
    stop("dfInput must be a data.frame or tbl object")
  }

  # Auto-detect Numerator columns (matches both "Numerator" and "Numerator_*")
  vNumeratorCols <- grep("^Numerator", colnames(dfInput), value = TRUE)
  if (length(vNumeratorCols) == 0) {
    stop("dfInput must have at least one Numerator column")
  }

  # Check other required columns
  required_cols <- c(strStudyCol, strGroupCol, "Denominator", "MonthYYYYMM")
  missing_cols <- setdiff(required_cols, colnames(dfInput))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "dfInput missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Handle vStudyFilter - extract all studies if NULL
  if (is.null(vStudyFilter)) {
    # Extract all unique study IDs from input
    vStudyFilter <- dfInput %>%
      dplyr::distinct(.data[[strStudyCol]]) %>%
      dplyr::collect() %>%
      dplyr::pull(.data[[strStudyCol]])
    message(sprintf("No vStudyFilter specified. Using all %d studies.", length(vStudyFilter)))
  }

  # Validate vStudyFilter
  if (!is.character(vStudyFilter) || length(vStudyFilter) == 0) {
    stop("vStudyFilter must be a non-empty character vector")
  }

  # Validate parameters
  if (!is.numeric(nBootstrapReps) || length(nBootstrapReps) != 1 || nBootstrapReps < 1) {
    stop("nBootstrapReps must be a positive integer")
  }
  nBootstrapReps <- as.integer(nBootstrapReps)

  if (!is.numeric(nConfLevel) || length(nConfLevel) != 1 || nConfLevel <= 0 || nConfLevel >= 1) {
    stop("nConfLevel must be between 0 and 1")
  }

  # Filter to specified studies
  dfFiltered <- dfInput %>%
    dplyr::filter(.data[[strStudyCol]] %in% .env$vStudyFilter)

  # Verify studies exist (only for in-memory data frames)
  if (is.data.frame(dfFiltered) && nrow(dfFiltered) == 0) {
    stop("No data found for specified studies in vStudyFilter")
  }

  # Count unique groups per study and find minimum (ONLY if not provided)
  if (is.null(nMinGroups)) {
    dfGroupCounts <- dfFiltered %>%
      dplyr::summarise(
        GroupCount = dplyr::n_distinct(.data[[strGroupCol]]),
        .by = dplyr::all_of(.env$strStudyCol)
      ) %>%
      dplyr::collect()

    nMinGroups <- min(dfGroupCounts$GroupCount)
    message(sprintf("Calculated minimum group count: %.0f", nMinGroups))
  } else {
    # Validate provided nMinGroups
    if (!is.numeric(nMinGroups) || length(nMinGroups) != 1 || nMinGroups < 1) {
      stop("nMinGroups must be a single positive integer")
    }
    nMinGroups <- as.integer(nMinGroups)
    message(sprintf("Using provided minimum group count: %.0f", nMinGroups))
  }

  # Sample minGroups sites from each study per bootstrap iteration
  dfBootstrapped <- BootstrapStudyKRI(
    dfInput = dfFiltered,
    nBootstrapReps = nBootstrapReps,
    nGroups = nMinGroups, # Key: use minimum to ensure fair comparison
    strStudyCol = strStudyCol,
    strGroupCol = strGroupCol,
    seed = seed,
    tblBootstrapReps = tblBootstrapReps,
    vDbIntRandomRange = vDbIntRandomRange
  )

  vNumeratorCols <- grep("^Numerator", colnames(dfInput), value = TRUE)

  if (bMixStudies) {
    # APPROACH 1: Mix studies early (faster SQL, no extrapolation)
    # Step 1: Calculate StudyMonth per study+bootstrap
    dfStudyMonth <- AggrStudyMonth(
      dfInput = dfBootstrapped,
      vBy = c("StudyID", "BootstrapRep"),
      vNumeratorCols = vNumeratorCols,
      tblMonthSequence = tblMonthSequence
    )
    
    # Step 2: Aggregate across studies for each StudyMonth+BootstrapRep
    dfStudyMonthBoot <- dfStudyMonth %>%
      dplyr::summarise(
        dplyr::across(
          .cols = c(dplyr::all_of(.env$vNumeratorCols), "Denominator", "GroupCount"),
          .fns = sum
        ),
        .by = c("StudyMonth", "BootstrapRep")
      )
    
    # Step 3: Calculate cumulative counts per bootstrap replicate only
    dfCumCountBootComplete <- CumulativeCounts(
      dfAggregated = dfStudyMonthBoot,
      vBy = c("BootstrapRep"),
      vNumeratorCols = vNumeratorCols
    )
    
  } else {
    # APPROACH 2: Keep studies separate 
    # Step 1: Calculate cumulative counts per study+bootstrap
    dfCumCountBoot <- Transform_CumCount(
      dfBootstrapped,
      vBy = c("StudyID", "BootstrapRep"),
      tblMonthSequence = tblMonthSequence
    )
    
    # Step 2: Extrapolate to align study timelines
    # Study timelines will have different lengths; bring them all to the
    # same length by extrapolating the last measurement
    dfMiss <- dfCumCountBoot %>%
      distinct(.data$StudyMonth, .data$BootstrapRep) %>%
      cross_join(
        distinct(dfCumCountBoot, .data$StudyID)
      ) %>%
      left_join(
        # add the last measurement
        dfCumCountBoot %>%
          filter(.data$StudyMonth == max(.data$StudyMonth), .by = c("StudyID", "BootstrapRep")) %>%
          select(-"StudyMonth"),
        by = c("StudyID", "BootstrapRep")
      ) %>%
      anti_join(
        # only keep records not in original
        dfCumCountBoot %>%
          select(c("StudyID", "BootstrapRep", "StudyMonth")),
        by = c("StudyID", "BootstrapRep", "StudyMonth")
      )
    
    dfCumCountBootComplete <- union_all(dfCumCountBoot, dfMiss)
  }

  # Calculate CI combining data from each sample
  dfBounds <- CalculateStudyBounds(
    dfInput = dfCumCountBootComplete,
    vBy = character(0), # No additional grouping
    nConfLevel = nConfLevel,
    strStudyMonthCol = strStudyMonthCol
  )

  # Add metadata
  # Compute scalar values before mutate to avoid SQL translation issues
  nStudyCount <- length(vStudyFilter)

  dfResult <- dfBounds %>%
    dplyr::mutate(
      GroupCount = .env$nMinGroups,
      StudyCount = .env$nStudyCount
    )

  return(dfResult)
}

#' Predict Bounds for Multiple Studies Using Reference Study Mappings
#'
#' @description
#' Wrapper for `Analyze_StudyKRI_PredictBoundsRefSet` that applies study-specific
#' reference groups. For each study in `dfStudyRef`, calculates bounds using its
#' mapped reference studies.
#'
#' @details
#' For optimal performance with database backends:
#' - Include a MinGroups column in dfStudyRef (or specify custom name via strMinGroupsCol)
#'   to avoid collecting the full dataset to count groups
#' - Calculate as: min(n_distinct(GroupID)) across all reference studies per target study
#' - This avoids materializing the entire Analysis_Input table just to count groups
#'
#' @param dfInput data.frame or tbl_lazy. Site-level data from `Input_CumCountSiteByMonth`.
#'   Must contain one or more `Numerator_*` columns, `Denominator`, `MonthYYYYMM`.
#' @param dfStudyRef data.frame or tbl_lazy. Study-to-reference mappings with at least
#'   two columns: first column = target studies, second column = reference studies.
#'   Can have multiple rows per target study (one row per target-reference pair).
#'   Optional column (name specified by strMinGroupsCol): minimum group count for
#'   performance optimization.
#' @param nBootstrapReps integer. Number of bootstrap replicates (default: 1000).
#' @param nConfLevel numeric. Confidence level for the bounds (default: 0.95).
#' @param strGroupCol character. Column name for group identifier (default: "GroupID").
#' @param strStudyMonthCol character. Column name for study month (default: "StudyMonth").
#' @param strMinGroupsCol character. Column name for minimum groups in dfStudyRef (default: "MinGroups").
#'   If this column exists in dfStudyRef, its value will be used instead of calculating
#'   minimum group counts, improving performance with database backends.
#' @param bMixStudies logical. If TRUE, aggregates studies at the StudyMonth level
#'   before calculating cumulative counts (faster SQL performance, but disables
#'   extrapolation). If FALSE (default), keeps study-level granularity through
#'   extrapolation step, allowing studies with different timeline lengths to be
#'   properly aligned before aggregation. Use TRUE for database backends when
#'   SQL performance is critical and all studies have similar timeline lengths.
#'   Default: FALSE.
#' @param seed integer or NULL. Random seed (default: NULL).
#' @param tblBootstrapReps tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated bootstrap replicate indices with a `BootstrapRep` column
#'   containing values 1 to N (where N is the desired number of replicates).
#'   If provided, overrides the `nBootstrapReps` parameter. If NULL, attempts to
#'   create temp table (requires write privileges).
#' @param tblMonthSequence tbl_lazy, data.frame, or NULL. For lazy table inputs:
#'   Optional pre-generated month sequence with only a `MonthYYYYMM` column
#'   (output of `GenerateMonthSeq()`). If NULL, attempts to create temp table
#'   (requires write privileges).
#' @param vDbIntRandomRange Numeric vector of length 2 or NULL. When using database
#'   backends that return large integers instead of 0-1 decimals for random numbers,
#'   specify the min/max range as c(min, max). Accepts both numeric and character
#'   vectors (character values are automatically converted to numeric, useful when
#'   reading from YAML files). Common values:
#'   - Snowflake: c(-9223372036854775808, 9223372036854775807) (signed 64-bit)
#'   - Other backends: c(0, 18446744073709551615) (unsigned 64-bit)
#'   Default: NULL (no normalization, assumes 0-1 decimal random values).
#'
#' @return A tibble (or tbl_lazy if input was lazy) with columns: `StudyID`, `StudyRefID`, `StudyMonth`,
#'   `Median_*`, `Lower_*`, `Upper_*` for each Metric column, `BootstrapCount`,
#'   `GroupCount`, `StudyCount`.
#'
#' @examples
#' # Create study reference mapping
#' dfStudyRef <- data.frame(
#'   study = c(rep("STUDY1", 2), rep("STUDY2", 2)),
#'   studyref = c("REF1", "REF2", "REF2", "REF3")
#' )
#'
#' # Create site-level data
#' dfSiteLevel <- data.frame(
#'   StudyID = rep(c("STUDY1", "STUDY2", "REF1", "REF2", "REF3"), each = 60),
#'   GroupID = rep(paste0("Site", 1:10), each = 6, times = 5),
#'   Numerator = sample(0:5, 300, replace = TRUE),
#'   Denominator = sample(10:20, 300, replace = TRUE),
#'   MonthYYYYMM = rep(rep(202301:202306, each = 10), times = 5),
#'   Metric = runif(300, 0.1, 0.5),
#'   GroupLevel = "Site"
#' )
#'
#' # Calculate study-specific reference bounds
#' dfBounds <- Analyze_StudyKRI_PredictBoundsRef(
#'   dfInput = dfSiteLevel,
#'   dfStudyRef = dfStudyRef,
#'   nBootstrapReps = 100,
#'   seed = 42
#' )
#'
#' # Example with Snowflake backend
#' dfBounds_Snowflake <- Analyze_StudyKRI_PredictBoundsRef(
#'   dfInput = dfSiteLevel,
#'   dfStudyRef = dfStudyRef,
#'   nBootstrapReps = 100,
#'   vDbIntRandomRange = c(-9223372036854775808, 9223372036854775807)
#' )
#'
#' print(head(dfBounds))
#'
#' @export
Analyze_StudyKRI_PredictBoundsRef <- function(
    dfInput,
    dfStudyRef,
    nBootstrapReps = 1000,
    nConfLevel = 0.95,
    strGroupCol = "GroupID",
    strStudyMonthCol = "StudyMonth",
    strMinGroupsCol = "MinGroups",
    bMixStudies = FALSE,
    seed = NULL,
    tblBootstrapReps = NULL,
    tblMonthSequence = NULL,
    vDbIntRandomRange = NULL) {
  # Input validation - accept data.frame or tbl (including tbl_lazy)
  if (!inherits(dfStudyRef, c("data.frame", "tbl"))) {
    stop("dfStudyRef must be a data.frame or tbl object")
  }

  if (ncol(dfStudyRef) < 2) {
    stop("dfStudyRef must have at least 2 columns (target study in column 1, reference study in column 2)")
  }

  # Collect dfStudyRef early (materialize if lazy)
  dfStudyRefCollected <- dfStudyRef %>%
    dplyr::distinct() %>%
    dplyr::collect()

  # Use first column for target studies, second column for reference studies
  strStudyCol <- colnames(dfStudyRefCollected)[1]
  strStudyRefCol <- colnames(dfStudyRefCollected)[2]

  # Check if MinGroups column exists by name
  bHasMinGroups <- strMinGroupsCol %in% colnames(dfStudyRefCollected)

  # Get unique target studies
  vTargetStudies <- unique(dfStudyRefCollected[[strStudyCol]])

  # Initialize list to collect results
  lResults <- list()

  # Loop over each target study
  for (study in vTargetStudies) {
    # Filter to get reference studies for this target study
    studyRefRow <- dfStudyRefCollected %>%
      dplyr::filter(.data[[strStudyCol]] == .env$study)

    vRefStudies <- studyRefRow %>%
      dplyr::pull(.data[[strStudyRefCol]])

    # Get nMinGroups if available (performance optimization)
    nMinGroupsProvided <- if (bHasMinGroups) {
      studyRefRow %>%
        dplyr::pull(.data[[strMinGroupsCol]]) %>%
        unique() %>%
        as.integer()
    } else {
      NULL
    }

    # Call the set function
    dfBounds <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfInput,
      vStudyFilter = vRefStudies,
      nBootstrapReps = nBootstrapReps,
      nConfLevel = nConfLevel,
      strStudyCol = "StudyID",
      strGroupCol = strGroupCol,
      strStudyMonthCol = strStudyMonthCol,
      bMixStudies = bMixStudies,
      seed = seed,
      tblBootstrapReps = tblBootstrapReps,
      tblMonthSequence = tblMonthSequence,
      vDbIntRandomRange = vDbIntRandomRange,
      nMinGroups = nMinGroupsProvided
    )

    # Compute constant values outside mutate to avoid SQL translation issues
    # paste() with collapse is not supported in SQL translation
    strStudyRefID <- paste(vRefStudies, collapse = ", ")

    # Add StudyID and StudyRefID columns
    # Use dplyr::mutate for lazy table compatibility
    dfBounds <- dfBounds %>%
      dplyr::mutate(
        StudyID = .env$study,
        StudyRefID = .env$strStudyRefID
      )

    # Store in list
    lResults[[study]] <- dfBounds
  }

  # Combine all results - use union_all for lazy table compatibility
  dfResult <- Reduce(dplyr::union_all, lResults)

  # Return result (lazy or collected based on input)
  return(dfResult)
}
