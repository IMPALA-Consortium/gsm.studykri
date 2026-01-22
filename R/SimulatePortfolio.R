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
#' @param strOversampleDomain Character string naming a domain to use for stratified sampling.
#'   NULL (default) samples from all enrolled subjects
#' @param vOversamplQuantileRange Numeric vector of length 2 with quantile range (0-1) for
#'   oversampling. Default c(0, 1) includes all subjects
#' @param seed Integer seed for reproducibility. NULL (default) uses current random state
#' @param vSubjectIDs Character vector defining hierarchical order for subject ID column
#'   lookup. Default: c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid").
#'   **Important:** All columns in vSubjectIDs must exist in Raw_SUBJ for proper mapping.
#'   If a column is missing, add it to Raw_SUBJ before calling this function. For example:
#'   `lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv`
#' @param vSiteIDs Character vector defining hierarchical order for site ID column
#'   lookup. Default: c("invid", "siteid", "site_num")
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
#' Domains are automatically categorized by columns present:
#' - **Subject-level**: Has column in vSubjectIDs → filter to sampled subjects
#' - **Site-level**: Has column in vSiteIDs but no subject column → filter to used sites
#' - **Other**: Neither → update studyid field only
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
#' # Add required columns to Raw_SUBJ (required for vSubjectIDs)
#' lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
#' lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjid
#'
#' # Standard resampling
#' lStudy1 <- ResampleStudy(lRaw, "STUDY001", seed = 123)
#'
#' # Oversample from high-AE patients (top 25%)
#' lStudy2 <- ResampleStudy(
#'   lRaw,
#'   "STUDY002",
#'   nSubjects = 50,
#'   strOversampleDomain = "Raw_AE",
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
    strOversampleDomain = NULL,
    vOversamplQuantileRange = c(0, 1),
    seed = NULL,
    vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid"),
    vSiteIDs = c("invid", "siteid", "site_num")) {
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

  # Validate that all vSubjectIDs exist in Raw_SUBJ
  missing_cols <- setdiff(vSubjectIDs, names(lRaw$Raw_SUBJ))
  if (length(missing_cols) > 0) {
    stop(
      "vSubjectIDs validation failed: The following columns are in vSubjectIDs but missing from Raw_SUBJ: ",
      paste(missing_cols, collapse = ", "),
      "\n\nTo fix this, add the missing columns to Raw_SUBJ. For example:\n",
      "  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv"
    )
  }

  if (!is.null(strOversampleDomain)) {
    if (!is.character(strOversampleDomain) || length(strOversampleDomain) != 1) {
      stop("strOversampleDomain must be NULL or a single character string")
    }
    if (!strOversampleDomain %in% names(lRaw)) {
      stop("strOversampleDomain '", strOversampleDomain, "' not found in lRaw")
    }
    # Domain must have a subject ID column
    domain_df <- lRaw[[strOversampleDomain]]
    subj_check <- find_subject_id_column(domain_df, lRaw$Raw_SUBJ, vSubjectIDs)
    if (is.null(subj_check)) {
      stop("strOversampleDomain must have a subject identifier column: ", paste(vSubjectIDs, collapse = ", "))
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

  if (!is.null(strOversampleDomain)) {
    # Count records per subject in oversampling domain
    domain_df <- lRaw[[strOversampleDomain]]

    # Find subject ID column and map to subjid (already validated above)
    subj_info <- find_subject_id_column(domain_df, lRaw$Raw_SUBJ, vSubjectIDs)

    # Filter out invalid mappings if needed
    if (!is.null(subj_info$valid_idx)) {
      domain_df <- domain_df[subj_info$valid_idx, ]
      subj_ids <- subj_info$ids[subj_info$valid_idx]
    } else {
      subj_ids <- subj_info$ids
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

    message(sprintf(
      "Filtered to %d subjects with %s records in %.2f-%.2f quantile range (%.0f-%.0f records)",
      nrow(enrolled_subj),
      strOversampleDomain,
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
  if (!is.null(TargetSiteCount)) {
    # Generate new sites with weighted patient distribution
    site_result <- generate_sites(lRaw$Raw_SITE, sampled_subj, strNewStudyID, TargetSiteCount, vSiteIDs)
    generated_sites <- site_result$generated_sites
    sampled_subj <- site_result$updated_sampled_subj
  } else {
    # Default behavior: randomize existing site assignments
    generated_sites <- NULL
    # Create invid -> siteid mapping before shuffling
    invid_to_siteid <- setNames(sampled_subj$siteid, sampled_subj$invid)
    # Shuffle invid
    sampled_subj$invid <- sample(sampled_subj$invid, nrow(sampled_subj))
    # Update siteid to match the shuffled invid
    if (has_column(sampled_subj, "siteid")) {
      sampled_subj$siteid <- invid_to_siteid[sampled_subj$invid]
    }
  }

  # ---- Create Subject Mapping ----
  subject_mapping <- tibble::tibble(
    old_subjid = sampled_subj$subjid,
    new_subjid = sprintf("%s_%04d", strNewStudyID, seq_len(nrow(sampled_subj))),
    old_invid = sampled_subj$invid, # Already randomized
    row_index = seq_len(nrow(sampled_subj))
  )

  # ---- Process All Domains ----
  lResult <- list()

  # Get used site IDs from sampled subjects
  # For non-TargetSiteCount: use siteid (matches site_num in site table)
  # For TargetSiteCount: use invid (already set with study prefix)
  site_col_for_filter <- if (!is.null(generated_sites)) "invid" else "siteid"
  used_site_ids <- unique(sampled_subj[[site_col_for_filter]])

  for (domain_name in names(lRaw)) {
    domain_df <- lRaw[[domain_name]]

    # Generic domain processing based on columns present
    subj_info <- find_subject_id_column(domain_df, lRaw$Raw_SUBJ, vSubjectIDs)
    site_info <- find_site_id_column(domain_df, vSiteIDs)

    if (!is.null(subj_info)) {
      # Subject-level domain
      lResult[[domain_name]] <- process_subject_domain(
        domain_df,
        subject_mapping,
        strNewStudyID,
        sampled_subj,
        lRaw$Raw_SUBJ,
        vSubjectIDs
      )
    } else if (!is.null(site_info)) {
      # Site-level domain (has site ID but no subject ID)
      lResult[[domain_name]] <- process_site_domain(
        domain_df,
        strNewStudyID,
        used_site_ids,
        vSiteIDs,
        generated_sites
      )
    } else {
      # Other domain - update studyid only      
      domain_df$studyid <- strNewStudyID
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


#' Generate sites with weighted patient distribution
#' @param raw_site_df Raw site data frame
#' @param sampled_subj Data frame of sampled subjects
#' @param new_study_id New study ID string
#' @param target_site_count Target number of sites to generate
#' @param vSiteIDs Site ID column names in hierarchical order
#' @return List with generated_sites and updated_sampled_subj
#' @keywords internal
generate_sites <- function(raw_site_df, sampled_subj, new_study_id, target_site_count, vSiteIDs) {
  # 1. Generate site IDs
  new_site_nums <- seq_len(target_site_count)

  # 2. Sample metadata for each site from original sites
  sampled_site_indices <- sample(seq_len(nrow(raw_site_df)), size = target_site_count, replace = TRUE)
  generated_sites <- raw_site_df[sampled_site_indices, ]

  # 3. Update site IDs
  new_site_ids <- paste0(new_study_id, "_", new_site_nums)
  generated_sites$invid <- new_site_ids
  if (has_column(generated_sites, "studyid")) {
    generated_sites$studyid <- new_study_id
  }

  # Update site_num to match the numeric part of invid
  if (has_column(generated_sites, "site_num")) {
    generated_sites$site_num <- as.character(new_site_nums)
  }

  # 4. Sample patient counts from observed distribution
  site_info <- find_site_id_column(sampled_subj, vSiteIDs)
  site_col_subj <- if (!is.null(site_info)) site_info$column else "invid"
  observed_counts <- as.vector(table(sampled_subj[[site_col_subj]]))

  # Sample patient counts for new sites
  sampled_counts <- sample(observed_counts, size = target_site_count, replace = TRUE)

  # 5. Create weighted ID vector
  weighted_site_ids <- rep(new_site_ids, times = sampled_counts)

  # 6. Assign subjects to sites via weighted sampling
  sampled_subj$invid <- sample(weighted_site_ids, size = nrow(sampled_subj), replace = TRUE)

  # Update siteid if present (extract numeric part from invid)
  if (has_column(sampled_subj, "siteid")) {
    sampled_subj$siteid <- gsub("^.*_", "", sampled_subj$invid)
  }

  # Only keep sites that actually have subjects assigned
  used_site_ids <- unique(sampled_subj$invid)
  generated_sites <- generated_sites[generated_sites$invid %in% used_site_ids, ]

  list(
    generated_sites = generated_sites,
    updated_sampled_subj = sampled_subj
  )
}


#' Find site ID column
#' @param df Data frame to search
#' @param vSiteIDs Character vector of column names to search in hierarchical order
#' @return List with 'column' (column name found) and 'ids' (vector of site ID values), or NULL if none found
#' @keywords internal
find_site_id_column <- function(df, vSiteIDs) {
  for (col in vSiteIDs) {
    if (has_column(df, col)) {
      return(list(column = col, ids = df[[col]]))
    }
  }
  return(NULL)
}


#' Find subject ID column and map to subjid
#' @param df Data frame to search
#' @param raw_subj Raw_SUBJ data frame for mapping composite IDs
#' @param vSubjectIDs Character vector of column names to search in hierarchical order
#' @return List with 'column' (column name found) and 'ids' (vector of subjid values)
#' @keywords internal
find_subject_id_column <- function(df, raw_subj, vSubjectIDs) {
  for (col in vSubjectIDs) {
    if (!has_column(df, col)) next

    if (col == "subjid") {
      # subjid is the canonical ID, return as-is
      return(list(column = col, ids = df$subjid))
    } else {
      # Generic mapping: check if this column exists in raw_subj
      if (!has_column(raw_subj, col)) next
      
      # Create mapping from this column to subjid
      subj_mapping <- setNames(raw_subj$subjid, raw_subj[[col]])
      ids <- subj_mapping[df[[col]]]
      valid_idx <- !is.na(ids)
      return(list(column = col, ids = ids, valid_idx = valid_idx))
    }
  }

  return(NULL)
}

#' Process subject-level domain
#' @keywords internal
process_subject_domain <- function(df, subject_mapping, new_study_id, sampled_subj, raw_subj, vSubjectIDs) {
  # Find subject ID column and map to subjid
  subj_info <- find_subject_id_column(df, raw_subj, vSubjectIDs)

  # Get mapped subjids (handling invalid mappings)
  if (!is.null(subj_info$valid_idx)) {
    valid_df <- df[subj_info$valid_idx, ]
    mapped_subjids <- subj_info$ids[subj_info$valid_idx]
  } else {
    valid_df <- df
    mapped_subjids <- subj_info$ids
  }

  # Filter to selected subjects
  result <- valid_df[mapped_subjids %in% subject_mapping$old_subjid, ]

  # Update studyid (even for empty results to preserve column structure)
  result$studyid <- new_study_id

  if (nrow(result) == 0) {
    return(result)
  }

  # Store old subjid for composite ID updates
  old_subjid_col <- mapped_subjids[mapped_subjids %in% subject_mapping$old_subjid]

  # Create a mapping lookup for efficiency
  old_to_new_subj <- setNames(subject_mapping$new_subjid, subject_mapping$old_subjid)

  # Update all vSubjectID columns generically with study prefix
  for (col in vSubjectIDs) {
    if (has_column(result, col)) {
      if (col == "subjid") {
        # For subjid, use the direct mapping
        result[[col]] <- old_to_new_subj[result[[col]]]
      } else {
        # For other ID columns, add study prefix to original values
        result[[col]] <- paste0(new_study_id, "_", result[[col]])
      }
    }
  }

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

    # Update siteid to match invid if needed
    # For TargetSiteCount: extract from invid (format: STUDY_123)
    # For regular: siteid already correct in sampled_subj, but need to map through subjects
    if (has_column(result, "siteid") && has_column(sampled_subj, "siteid")) {
      # Create mapping from subjid to siteid
      subj_to_siteid <- setNames(sampled_subj$siteid, sampled_subj$subjid)
      result$siteid <- subj_to_siteid[old_subjid_col]
    }
  }

  return(result)
}


#' Process site metadata domain
#' @keywords internal
process_site_domain <- function(df, new_study_id, used_site_ids, vSiteIDs, generated_sites = NULL) {
  # If we have generated sites from TargetSiteCount, use them directly
  if (!is.null(generated_sites)) {
    return(generated_sites)
  }

  # Find site ID column - assumes site domain has one of vSiteIDs columns
  site_info <- find_site_id_column(df, vSiteIDs)

  # Keep only sites that appear in resampled data
  result <- df[df[[site_info$column]] %in% used_site_ids, ]

  if (nrow(result) == 0) {
    return(result)
  }

  result$studyid <- new_study_id

  if (site_info$column == "invid") {
    result$invid <- paste0(new_study_id, "_", result$invid)
  }

  return(result)
}

# Helper Functions for SimulatePortfolio -----------------------------------

generate_default_config <- function(lRaw, nStudies, seed) {
  if (!is.null(seed)) set.seed(seed)

  max_subj <- nrow(lRaw$Raw_SUBJ)

  tibble::tibble(
    studyid = sprintf("PORTFOLIO%03d", seq_len(nStudies)),
    nSubjects = sample(20:min(100, max_subj), nStudies, replace = TRUE),
    TargetSiteCount = NA_integer_,
    replacement = TRUE
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
#' @param vSubjectIDs Character vector defining hierarchical order for subject ID column
#'   lookup. Default: c("subjid", "subjectenrollmentnumber", "subject_nsv")
#' @param vSiteIDs Character vector defining hierarchical order for site ID column
#'   lookup. Default: c("invid", "siteid", "site_num")
#'
#' @return list. Combined portfolio with same structure as lRaw, containing data
#'   from all simulated studies row-bound together.
#'
#' @details
#' If dfConfig is provided, it should contain:
#' - studyid: Unique study identifier
#' - nSubjects: Number of subjects per study
#' - TargetSiteCount: (Optional) Target number of sites
#' - strOversampleDomain: (Optional) Domain for stratified sampling
#' - vOversamplQuantileRange_min/max: (Optional) Quantile range for stratification
#' - replacement: (Optional) Sample with replacement (default TRUE)
#'
#' If dfConfig is NULL, generates default configuration with random parameters.
#'
#' Domains are automatically processed based on columns present:
#' - Subject-level domains (have vSubjectIDs columns) are filtered to sampled subjects
#' - Site-level domains (have vSiteIDs columns but no subject columns) are filtered to used sites
#' - Other domains have studyid updated only
#'
#' @examples
#' # Simple portfolio with default parameters
#' lRaw <- list(
#'   Raw_SUBJ = clindata::rawplus_dm,
#'   Raw_AE = clindata::rawplus_ae,
#'   Raw_SITE = clindata::ctms_site
#' )
#'
#' # Add required columns to Raw_SUBJ
#' lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
#' lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjid
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
    seed = NULL,
    vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid"),
    vSiteIDs = c("invid", "siteid", "site_num")) {
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
      nSubjects = config_row$nSubjects,
      vSubjectIDs = vSubjectIDs,
      vSiteIDs = vSiteIDs
    )

    # Add optional parameters if present
    if ("TargetSiteCount" %in% names(config_row) && !is.na(config_row$TargetSiteCount)) {
      resample_args$TargetSiteCount <- config_row$TargetSiteCount
    }

    if ("replacement" %in% names(config_row) && !is.na(config_row$replacement)) {
      resample_args$replacement <- config_row$replacement
    }

    if ("strOversampleDomain" %in% names(config_row) && !is.na(config_row$strOversampleDomain)) {
      resample_args$strOversampleDomain <- config_row$strOversampleDomain

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
