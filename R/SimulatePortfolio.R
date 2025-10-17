#' Resample Study Data
#'
#' Creates a resampled version of a study with new subject and study IDs. Supports
#' stratified sampling based on domain activity (e.g., oversample patients with high
#' protocol deviations) and randomizes site assignments.
#'
#' @param lRaw Named list of raw data domains (e.g., Raw_SUBJ, Raw_AE, etc.)
#' @param strNewStudyID Character string for the new study ID
#' @param nSubjects Integer number of subjects to sample. NULL (default) samples same
#'   number as enrolled subjects in original data
#' @param TargetSiteCount Numeric. Approximate target number of sites in the resampled study.
#'   If NULL (default), uses sites from sampled subjects naturally.
#'   If specified, generates approximately N sites with weighted patient distributions.
#'   Note: Final site count may vary as sites with zero patients are excluded.
#'   Must be a positive integer.
#' @param replacement Logical indicating whether to sample with replacement (default: TRUE)
#' @param strOversamplDomain Character string naming a domain to use for stratified sampling.
#'   NULL (default) samples from all enrolled subjects
#' @param vOversamplQuantileRange Numeric vector of length 2 with quantile range (0-1) for
#'   oversampling. Default c(0, 1) includes all subjects
#' @param seed Integer seed for reproducibility. NULL (default) uses current random state
#'
#' @return Named list of resampled data domains with updated IDs
#'
#' @details
#' This function performs the following steps:
#' 1. Optionally filters subjects by their activity level in a specified domain
#' 2. Samples subjects with or without replacement
#' 3. Randomizes site assignments by shuffling invid values (or generates new sites if TargetSiteCount specified)
#' 4. Updates all subject, study, and site IDs across all domains
#' 5. Maintains referential integrity across domains
#'
#' When `TargetSiteCount` is specified:
#' - Generates TargetSiteCount site IDs with metadata sampled from original sites
#' - Samples patient counts per site from the distribution observed in sampled subjects
#' - Creates weighted site assignment: subjects are assigned to sites proportionally to sampled patient counts
#' - Final site count may be less than target if some sites receive no patients through sampling
#'
#' The function handles multiple subject ID formats:
#' - subjid: Simple ID (e.g., "0496")
#' - subjectid: Composite ID (e.g., "X1670496-113XXX")
#' - subject_nsv: NSV format (e.g., "0496-113XXX")
#'
#' @examples
#' # Load test data
#' lRaw <- list(
#'   Raw_SUBJ = clindata::rawplus_dm,
#'   Raw_AE = clindata::rawplus_ae,
#'   Raw_SITE = clindata::ctms_site,
#'   Raw_STUDY = clindata::ctms_study
#' )
#'
#' # Standard resampling
#' lStudy1 <- ResampleStudy(lRaw, "STUDY001", seed = 123)
#'
#' # Oversample from high-AE patients (top 25%)
#' lStudy2 <- ResampleStudy(
#'   lRaw,
#'   "STUDY002",
#'   nSubjects = 50,
#'   strOversamplDomain = "Raw_AE",
#'   vOversamplQuantileRange = c(0.75, 1.0),
#'   seed = 456
#' )
#' 
#' # Generate study with target of ~30 sites
#' lStudy3 <- ResampleStudy(
#'   lRaw,
#'   "STUDY003",
#'   nSubjects = 200,
#'   TargetSiteCount = 30,
#'   seed = 789
#' )
#'
#' @export
ResampleStudy <- function(
  lRaw,
  strNewStudyID,
  nSubjects = NULL,
  TargetSiteCount = NULL,
  replacement = TRUE,
  strOversamplDomain = NULL,
  vOversamplQuantileRange = c(0, 1),
  seed = NULL
) {
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # ---- Input Validation ----
  if (!is.list(lRaw) || is.null(names(lRaw))) {
    stop("lRaw must be a named list")
  }
  
  if (!"Raw_SUBJ" %in% names(lRaw)) {
    stop("lRaw must contain Raw_SUBJ (demographics/subject data)")
  }
  
  if (!is.character(strNewStudyID) || length(strNewStudyID) != 1) {
    stop("strNewStudyID must be a single character string")
  }
  
  if (!is.null(nSubjects) && (!is.numeric(nSubjects) || nSubjects <= 0)) {
    stop("nSubjects must be NULL or a positive integer")
  }
  
  if (!is.logical(replacement) || length(replacement) != 1) {
    stop("replacement must be a single logical value")
  }
  
  if (!is.null(strOversamplDomain)) {
    if (!is.character(strOversamplDomain) || length(strOversamplDomain) != 1) {
      stop("strOversamplDomain must be NULL or a single character string")
    }
    if (!strOversamplDomain %in% names(lRaw)) {
      stop("strOversamplDomain '", strOversamplDomain, "' not found in lRaw")
    }
    # Domain must have subjid, subjectenrollmentnumber, or subject_nsv for joining
    domain_df <- lRaw[[strOversamplDomain]]
    has_subj_col <- has_column(domain_df, "subjid") ||
      has_column(domain_df, "subjectenrollmentnumber") ||
      has_column(domain_df, "subject_nsv")
    if (!has_subj_col) {
      stop("strOversamplDomain must have a subject identifier column (subjid, subjectenrollmentnumber, or subject_nsv)")
    }
  }
  
  if (!is.numeric(vOversamplQuantileRange) || length(vOversamplQuantileRange) != 2) {
    stop("vOversamplQuantileRange must be a numeric vector of length 2")
  }
  
  if (any(vOversamplQuantileRange < 0) || any(vOversamplQuantileRange > 1)) {
    stop("vOversamplQuantileRange values must be between 0 and 1")
  }
  
  if (vOversamplQuantileRange[1] > vOversamplQuantileRange[2]) {
    stop("vOversamplQuantileRange[1] must be <= vOversamplQuantileRange[2]")
  }
  
  if (!is.null(TargetSiteCount)) {
    if (!is.numeric(TargetSiteCount) || length(TargetSiteCount) != 1 || TargetSiteCount < 1) {
      stop("TargetSiteCount must be a single positive number")
    }
    TargetSiteCount <- as.integer(TargetSiteCount)
  }
  
  # ---- Stratified Filtering (if applicable) ----
  enrolled_subj <- lRaw$Raw_SUBJ[lRaw$Raw_SUBJ$enrollyn == "Y", ]
  
  if (nrow(enrolled_subj) == 0) {
    stop("No enrolled subjects found in Raw_SUBJ (enrollyn == 'Y')")
  }
  
  if (!is.null(strOversamplDomain)) {
    # Count records per subject in oversampling domain
    domain_df <- lRaw[[strOversamplDomain]]
    
    # Detect which subject ID column to use
    if (has_column(domain_df, "subjid")) {
      subj_col <- "subjid"
      subj_ids <- domain_df$subjid
    } else if (has_column(domain_df, "subjectenrollmentnumber")) {
      # Map subjectenrollmentnumber to subjid using Raw_SUBJ
      subj_col <- "subjectenrollmentnumber"
      # Create mapping from subjectid to subjid
      subj_mapping <- setNames(lRaw$Raw_SUBJ$subjid, lRaw$Raw_SUBJ$subjectid)
      subj_ids <- subj_mapping[domain_df$subjectenrollmentnumber]
      # Remove NAs
      valid_idx <- !is.na(subj_ids)
      domain_df <- domain_df[valid_idx, ]
      subj_ids <- subj_ids[valid_idx]
    } else if (has_column(domain_df, "subject_nsv")) {
      # Map subject_nsv to subjid using Raw_SUBJ
      subj_col <- "subject_nsv"
      subj_mapping <- setNames(lRaw$Raw_SUBJ$subjid, lRaw$Raw_SUBJ$subject_nsv)
      subj_ids <- subj_mapping[domain_df$subject_nsv]
      # Remove NAs
      valid_idx <- !is.na(subj_ids)
      domain_df <- domain_df[valid_idx, ]
      subj_ids <- subj_ids[valid_idx]
    } else {
      stop("Could not find subject ID column in ", strOversamplDomain)
    }
    
    # Count records per subject
    subject_counts <- aggregate(
      rep(1, length(subj_ids)),
      by = list(subjid = subj_ids),
      FUN = length
    )
    names(subject_counts)[2] <- "n_records"
    
    # Calculate quantile thresholds
    quantiles <- quantile(
      subject_counts$n_records,
      probs = vOversamplQuantileRange,
      na.rm = TRUE
    )
    
    # Get eligible subjects within quantile range
    eligible_subjects <- subject_counts$subjid[
      subject_counts$n_records >= quantiles[1] &
      subject_counts$n_records <= quantiles[2]
    ]
    
    # Filter enrolled subjects to eligible population
    enrolled_subj <- enrolled_subj[enrolled_subj$subjid %in% eligible_subjects, ]
    
    if (nrow(enrolled_subj) == 0) {
      stop("No subjects found in specified quantile range")
    }
    
    message(sprintf(
      "Filtered to %d subjects with %s records in %.2f-%.2f quantile range (%.0f-%.0f records)",
      nrow(enrolled_subj),
      strOversamplDomain,
      vOversamplQuantileRange[1],
      vOversamplQuantileRange[2],
      quantiles[1],
      quantiles[2]
    ))
  }
  
  # ---- Subject Sampling ----
  n_to_sample <- if (is.null(nSubjects)) {
    nrow(enrolled_subj)
  } else {
    as.integer(nSubjects)
  }
  
  # Validate sample size vs replacement
  if (!replacement && n_to_sample > nrow(enrolled_subj)) {
    stop(
      "Cannot sample ", n_to_sample, " subjects without replacement from ",
      nrow(enrolled_subj), " available subjects"
    )
  }
  
  # Sample subject indices
  sampled_indices <- sample(
    seq_len(nrow(enrolled_subj)),
    size = n_to_sample,
    replace = replacement
  )
  
  # Get sampled subjects
  sampled_subj <- enrolled_subj[sampled_indices, ]
  
  # ---- Handle Site Generation/Randomization ----
  generated_sites <- NULL
  
  if (!is.null(TargetSiteCount)) {
    # Generate new sites with weighted patient distribution
    
    # 1. Generate site IDs
    new_site_nums <- seq_len(TargetSiteCount)
    
    # 2. Sample metadata for each site from original sites
    sampled_site_indices <- sample(1:nrow(lRaw$Raw_SITE), size = TargetSiteCount, replace = TRUE)
    generated_sites <- lRaw$Raw_SITE[sampled_site_indices, ]
    
    # 3. Create invid column for generated sites (this is what subjects will reference)
    new_site_ids <- paste0(strNewStudyID, "_", new_site_nums)
    generated_sites$invid <- new_site_ids
    
    # Update studyid
    if (has_column(generated_sites, "studyid")) {
      generated_sites$studyid <- strNewStudyID
    }
    
    # 4. Sample patient counts from sampled subject data
    # Count patients per site in the already-sampled subjects
    site_col_subj <- if (has_column(sampled_subj, "invid")) "invid" else "siteid"
    observed_counts <- as.vector(table(sampled_subj[[site_col_subj]]))
    
    # Sample patient counts for new sites from this vector
    sampled_counts <- sample(observed_counts, size = TargetSiteCount, replace = TRUE)
    
    # 5. Create weighted ID vector
    # Replicate each site ID by its sampled patient count
    weighted_site_ids <- rep(new_site_ids, times = sampled_counts)
    
    # 6. Assign subjects to sites via weighted sampling
    # Always use invid for the new assignments
    sampled_subj$invid <- sample(weighted_site_ids, size = nrow(sampled_subj), replace = TRUE)
    
    # Update siteid if present (extract numeric part from invid)
    if (has_column(sampled_subj, "siteid")) {
      # Extract numeric part: "STUDY001_5" -> "5"
      sampled_subj$siteid <- gsub("^.*_", "", sampled_subj$invid)
    }
    
    # Only keep sites that actually have subjects assigned
    used_site_ids <- unique(sampled_subj$invid)
    generated_sites <- generated_sites[generated_sites$invid %in% used_site_ids, ]
    
  } else {
    # Default behavior: randomize existing site assignments
    # Shuffle invid column to break original subject-site relationship
    sampled_subj$invid <- sample(sampled_subj$invid, nrow(sampled_subj))
  }
  
  # ---- Create Subject Mapping ----
  subject_mapping <- data.frame(
    old_subjid = sampled_subj$subjid,
    new_subjid = sprintf("%s_%04d", strNewStudyID, seq_len(nrow(sampled_subj))),
    old_invid = sampled_subj$invid,  # Already randomized
    row_index = seq_len(nrow(sampled_subj)),
    stringsAsFactors = FALSE
  )
  
  # ---- Process All Domains ----
  lResult <- list()
  
  for (domain_name in names(lRaw)) {
    domain_df <- lRaw[[domain_name]]
    
    # Determine domain type and process accordingly
    if (domain_name == "Raw_STUDY") {
      # Metadata: Study
      lResult[[domain_name]] <- process_study_domain(domain_df, strNewStudyID)
      
      } else if (domain_name == "Raw_SITE") {
      # Metadata: Site
      # Get site IDs from sampled subjects - use siteid if available, otherwise invid
      if (has_column(sampled_subj, "siteid")) {
        used_site_ids <- unique(sampled_subj$siteid)
      } else {
        used_site_ids <- unique(subject_mapping$old_invid)
      }
      lResult[[domain_name]] <- process_site_domain(domain_df, strNewStudyID, used_site_ids, generated_sites)
      
    } else if (domain_name == "Raw_COUNTRY") {
      # Metadata: Country - link through site
      if (has_column(lResult$Raw_SITE, "country")) {
        used_countries <- unique(lResult$Raw_SITE$country)
        country_df <- domain_df
        if (has_column(country_df, "country")) {
          country_df <- country_df[country_df$country %in% used_countries, ]
        }
        if (has_column(country_df, "studyid")) {
          country_df$studyid <- strNewStudyID
        }
        lResult[[domain_name]] <- country_df
      } else {
        # Keep as is if can't link
        lResult[[domain_name]] <- domain_df
      }
      
    } else if (has_column(domain_df, "subjid")) {
      # Subject-level domain
      lResult[[domain_name]] <- process_subject_domain(
        domain_df,
        subject_mapping,
        strNewStudyID,
        sampled_subj
      )
      
    } else if (has_column(domain_df, "subject_nsv")) {
      # Derived domain (uses subject_nsv)
      lResult[[domain_name]] <- process_derived_domain(
        domain_df,
        subject_mapping,
        strNewStudyID,
        lRaw$Raw_SUBJ
      )
      
    } else {
      # Unknown domain type - keep as is but update studyid if present
      if (has_column(domain_df, "studyid")) {
        domain_df$studyid <- strNewStudyID
      }
      lResult[[domain_name]] <- domain_df
    }
  }
  
  return(lResult)
}


# ---- Helper Functions ----

#' Check if column exists in data frame
#' @keywords internal
has_column <- function(df, col_name) {
  col_name %in% names(df)
}


#' Update composite IDs by replacing old subjid with new subjid
#' @keywords internal
update_composite_id <- function(composite_id, old_subjid, new_subjid) {
  # Use vectorized string replacement
  result <- composite_id
  for (i in seq_along(old_subjid)) {
    result <- gsub(old_subjid[i], new_subjid[i], result, fixed = TRUE)
  }
  result
}


#' Process subject-level domain
#' @keywords internal
process_subject_domain <- function(df, subject_mapping, new_study_id, sampled_subj) {
  # Inner join with subject mapping
  # Keep all records for selected subjects
  result <- df[df$subjid %in% subject_mapping$old_subjid, ]
  
  if (nrow(result) == 0) {
    # Return empty df with same structure
    return(result)
  }
  
  # Create a mapping lookup for efficiency
  old_to_new_subj <- setNames(subject_mapping$new_subjid, subject_mapping$old_subjid)
  old_to_new_site <- setNames(
    paste0(new_study_id, "_", subject_mapping$old_invid),
    subject_mapping$old_invid
  )
  
  # Update studyid
  if (has_column(result, "studyid")) {
    result$studyid <- new_study_id
  }
  
  # Store old subjid before updating for composite ID updates
  old_subjid_col <- result$subjid
  
  # Update subjid
  result$subjid <- old_to_new_subj[result$subjid]
  
  # Update invid if present (use randomized site from sampled_subj)
  if (has_column(result, "invid")) {
    # Get the new site assignment for each subject
    # Check if invid already has study prefix (from TargetSiteCount)
    sample_invid <- sampled_subj$invid[1]
    already_prefixed <- !is.na(sample_invid) && grepl(paste0("^", new_study_id, "_"), sample_invid)
    
    if (already_prefixed) {
      # invid already has the study prefix, use as-is
      subj_to_site <- setNames(sampled_subj$invid, sampled_subj$subjid)
    } else {
      # invid needs the study prefix added
      subj_to_site <- setNames(
        paste0(new_study_id, "_", sampled_subj$invid),
        sampled_subj$subjid
      )
    }
    # Map through old subjid to get correct new site
    result$invid <- subj_to_site[old_subjid_col]
  }
  
  # Update composite IDs if present
  if (has_column(result, "subjectid")) {
    for (i in seq_along(subject_mapping$old_subjid)) {
      idx <- old_subjid_col == subject_mapping$old_subjid[i]
      if (any(idx)) {
        result$subjectid[idx] <- gsub(
          subject_mapping$old_subjid[i],
          subject_mapping$new_subjid[i],
          result$subjectid[idx],
          fixed = TRUE
        )
      }
    }
  }
  
  if (has_column(result, "subject_nsv")) {
    for (i in seq_along(subject_mapping$old_subjid)) {
      idx <- old_subjid_col == subject_mapping$old_subjid[i]
      if (any(idx)) {
        result$subject_nsv[idx] <- gsub(
          subject_mapping$old_subjid[i],
          subject_mapping$new_subjid[i],
          result$subject_nsv[idx],
          fixed = TRUE
        )
      }
    }
  }
  
  return(result)
}


#' Process derived domain (uses subject_nsv for lookup)
#' @keywords internal
process_derived_domain <- function(df, subject_mapping, new_study_id, raw_subj) {
  # These domains don't have subjid directly
  # Need to extract subjid from subject_nsv to join with mapping
  
  # Extract subjid from subject_nsv in original Raw_SUBJ
  # Format is typically "subjid-siteXXX" or similar
  subj_nsv_to_subjid <- setNames(raw_subj$subjid, raw_subj$subject_nsv)
  
  # Get subjid for each record in derived domain
  df$temp_subjid <- subj_nsv_to_subjid[df$subject_nsv]
  
  # Filter to selected subjects
  result <- df[!is.na(df$temp_subjid) & df$temp_subjid %in% subject_mapping$old_subjid, ]
  
  if (nrow(result) == 0) {
    result$temp_subjid <- NULL
    return(result)
  }
  
  # Update studyid if present
  if (has_column(result, "studyid")) {
    result$studyid <- new_study_id
  }
  
  # Update subject_nsv by replacing old subjid with new subjid
  for (i in seq_along(subject_mapping$old_subjid)) {
    idx <- result$temp_subjid == subject_mapping$old_subjid[i]
    if (any(idx)) {
      result$subject_nsv[idx] <- gsub(
        subject_mapping$old_subjid[i],
        subject_mapping$new_subjid[i],
        result$subject_nsv[idx],
        fixed = TRUE
      )
    }
  }
  
  # Remove temporary column
  result$temp_subjid <- NULL
  
  return(result)
}


#' Process study metadata domain
#' @keywords internal
process_study_domain <- function(df, new_study_id) {
  # Replicate study metadata with new study ID
  result <- df
  if (has_column(result, "studyid")) {
    result$studyid <- new_study_id
  }
  return(result)
}


#' Process site metadata domain
#' @keywords internal
process_site_domain <- function(df, new_study_id, used_site_ids, generated_sites = NULL) {
  # If we have generated sites from TargetSiteCount, use them directly
  if (!is.null(generated_sites)) {
    return(generated_sites)
  }
  
  # Otherwise, use default logic
  # Detect site ID column name
  site_col <- if (has_column(df, "invid")) {
    "invid"
  } else if (has_column(df, "site_num")) {
    "site_num"
  } else {
    # Can't process without site ID column
    return(df)
  }
  
  # Keep only sites that appear in resampled data
  result <- df[df[[site_col]] %in% used_site_ids, ]
  
  if (nrow(result) == 0) {
    return(result)
  }
  
  # Update studyid
  if (has_column(result, "studyid")) {
    result$studyid <- new_study_id
  }
  
  # Only prefix invid (investigator ID), not site_num (site metadata primary key)
  # site_num is a reference ID that shouldn't change
  if (site_col == "invid") {
    result[[site_col]] <- paste0(new_study_id, "_", result[[site_col]])
  }
  
  return(result)
}

# Helper Functions for SimulatePortfolio -----------------------------------

generate_default_config <- function(lRaw, nStudies, seed) {
  if (!is.null(seed)) set.seed(seed)
  
  max_subj <- nrow(lRaw$Raw_SUBJ)
  
  data.frame(
    studyid = sprintf("PORTFOLIO%03d", seq_len(nStudies)),
    nSubjects = sample(20:min(100, max_subj), nStudies, replace = TRUE),
    TargetSiteCount = NA_integer_,
    replacement = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Simulate a Portfolio of Studies
#'
#' Creates a portfolio of multiple synthetic studies by resampling from source data.
#' Each study is generated using ResampleStudy with configurable parameters.
#'
#' @param lRaw list. Named list of raw data domains (must include Raw_SUBJ).
#' @param dfConfig data.frame. Optional configuration for each study. Must contain
#'   columns 'studyid' and 'nSubjects'. See Details.
#' @param nStudies numeric. Number of studies to generate if dfConfig is NULL. Default: 5.
#' @param seed numeric. Random seed for reproducibility. Default: NULL.
#'
#' @return list. Combined portfolio with same structure as lRaw, containing data
#'   from all simulated studies row-bound together.
#'
#' @details
#' If dfConfig is provided, it should contain:
#' - studyid: Unique study identifier
#' - nSubjects: Number of subjects per study
#' - TargetSiteCount: (Optional) Target number of sites
#' - strOversamplDomain: (Optional) Domain for stratified sampling
#' - vOversamplQuantileRange_min/max: (Optional) Quantile range for stratification
#' - replacement: (Optional) Sample with replacement (default TRUE)
#'
#' If dfConfig is NULL, generates default configuration with random parameters.
#'
#' @examples
#' # Simple portfolio with default parameters
#' lRaw <- list(
#'   Raw_SUBJ = clindata::rawplus_dm,
#'   Raw_AE = clindata::rawplus_ae,
#'   Raw_SITE = clindata::ctms_site
#' )
#' 
#' lPortfolio <- SimulatePortfolio(lRaw, nStudies = 3, seed = 123)
#' 
#' # Custom configuration
#' dfConfig <- data.frame(
#'   studyid = c("TRIAL001", "TRIAL002"),
#'   nSubjects = c(50, 75),
#'   TargetSiteCount = c(15, 20)
#' )
#' lPortfolio_custom <- SimulatePortfolio(lRaw, dfConfig = dfConfig, seed = 456)
#'
#' @export
SimulatePortfolio <- function(
  lRaw,
  dfConfig = NULL,
  nStudies = 5,
  seed = NULL
) {
  # Input validation
  if (!is.list(lRaw) || !"Raw_SUBJ" %in% names(lRaw)) {
    stop("lRaw must be a list containing at least Raw_SUBJ")
  }
  
  # Generate default config if not provided
  if (is.null(dfConfig)) {
    dfConfig <- generate_default_config(lRaw, nStudies, seed)
  }
  
  # Validate dfConfig structure
  required_cols <- c("studyid", "nSubjects")
  if (!all(required_cols %in% names(dfConfig))) {
    stop("dfConfig must contain columns: studyid, nSubjects")
  }
  
  # Check for duplicate study IDs
  if (any(duplicated(dfConfig$studyid))) {
    stop("dfConfig contains duplicate study IDs")
  }
  
  # Set seed once at start if provided
  if (!is.null(seed)) set.seed(seed)
  
  # Create list to store resampled studies
  lStudies <- vector("list", nrow(dfConfig))
  
  for (i in seq_len(nrow(dfConfig))) {
    config_row <- dfConfig[i, ]
    
    # Build ResampleStudy arguments
    resample_args <- list(
      lRaw = lRaw,
      strNewStudyID = config_row$studyid,
      nSubjects = config_row$nSubjects
    )
    
    # Add optional parameters if present
    if ("TargetSiteCount" %in% names(config_row) && !is.na(config_row$TargetSiteCount)) {
      resample_args$TargetSiteCount <- config_row$TargetSiteCount
    }
    
    if ("replacement" %in% names(config_row) && !is.na(config_row$replacement)) {
      resample_args$replacement <- config_row$replacement
    }
    
    if ("strOversamplDomain" %in% names(config_row) && !is.na(config_row$strOversamplDomain)) {
      resample_args$strOversamplDomain <- config_row$strOversamplDomain
      
      # Build quantile range
      qmin <- if ("vOversamplQuantileRange_min" %in% names(config_row) && !is.na(config_row$vOversamplQuantileRange_min)) {
        config_row$vOversamplQuantileRange_min
      } else {
        0
      }
      qmax <- if ("vOversamplQuantileRange_max" %in% names(config_row) && !is.na(config_row$vOversamplQuantileRange_max)) {
        config_row$vOversamplQuantileRange_max
      } else {
        1
      }
      resample_args$vOversamplQuantileRange <- c(qmin, qmax)
    }
    
    # Call ResampleStudy
    lStudies[[i]] <- do.call(ResampleStudy, resample_args)
  }
  
  # Combine all studies domain by domain
  domain_names <- names(lStudies[[1]])
  lPortfolio <- vector("list", length(domain_names))
  names(lPortfolio) <- domain_names
  
  for (domain_name in domain_names) {
    # Extract domain from each study
    domain_list <- lapply(lStudies, function(study) study[[domain_name]])
    
    # Row-bind all studies for this domain
    lPortfolio[[domain_name]] <- do.call(rbind, domain_list)
  }
  
  return(lPortfolio)
}

