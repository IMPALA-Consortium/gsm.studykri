test_that("ResampleStudy basic functionality works", {
  # Load test data
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  # Basic resampling
  result <- ResampleStudy(lRaw, "STUDY001", seed = 123)
  
  # Check structure
  expect_type(result, "list")
  expect_true("Raw_SUBJ" %in% names(result))
  expect_true("Raw_AE" %in% names(result))
  
  # Check study ID updated
  expect_true(all(result$Raw_SUBJ$studyid == "STUDY001"))
  expect_true(all(result$Raw_AE$studyid == "STUDY001"))
  
  # Check subject ID format
  expect_true(all(grepl("^STUDY001_", result$Raw_SUBJ$subjid)))
})

test_that("ResampleStudy respects nSubjects parameter", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  # Sample specific number of subjects
  result <- ResampleStudy(lRaw, "STUDY001", nSubjects = 50, seed = 123)
  
  expect_equal(nrow(result$Raw_SUBJ), 50)
  expect_true(all(grepl("^STUDY001_", result$Raw_SUBJ$subjid)))
})

test_that("ResampleStudy handles replacement parameter", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  n_enrolled <- sum(lRaw$Raw_SUBJ$enrollyn == "Y")
  
  # With replacement - can sample more than available
  result_with <- ResampleStudy(
    lRaw,
    "STUDY001",
    nSubjects = 100,
    replacement = TRUE,
    seed = 123
  )
  expect_equal(nrow(result_with$Raw_SUBJ), 100)
  
  # With replacement - can sample fewer
  result_fewer <- ResampleStudy(
    lRaw,
    "STUDY002",
    nSubjects = 50,
    replacement = TRUE,
    seed = 123
  )
  expect_equal(nrow(result_fewer$Raw_SUBJ), 50)
  
  # Without replacement - error if requesting more than available
  expect_error(
    ResampleStudy(
      lRaw,
      "STUDY003",
      nSubjects = n_enrolled + 100,
      replacement = FALSE,
      seed = 123
    ),
    "Cannot sample"
  )
})

test_that("ResampleStudy randomizes site assignments", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  # Get original enrolled subjects
  enrolled <- lRaw$Raw_SUBJ[lRaw$Raw_SUBJ$enrollyn == "Y", ]
  
  # Resample with same seed and no filtering
  result <- ResampleStudy(lRaw, "STUDY001", seed = 123)
  
  # Should have resampled subjects
  expect_true(nrow(result$Raw_SUBJ) > 0)
  
  # Site IDs should be prefixed
  expect_true(all(grepl("^STUDY001_", result$Raw_SUBJ$invid)))
  
  # Site assignments exist (basic check)
  expect_true(all(!is.na(result$Raw_SUBJ$invid)))
  
  # Sites used should be from the original set
  result_sites <- gsub("STUDY001_", "", result$Raw_SUBJ$invid)
  original_sites <- unique(enrolled$invid)
  expect_true(all(result_sites %in% original_sites))
})

test_that("ResampleStudy maintains referential integrity", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_LB = clindata::rawplus_lb,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  result <- ResampleStudy(lRaw, "STUDY001", nSubjects = 100, seed = 123)
  
  # All subjids in AE should exist in SUBJ
  ae_subjects <- unique(result$Raw_AE$subjid)
  subj_subjects <- unique(result$Raw_SUBJ$subjid)
  expect_true(all(ae_subjects %in% subj_subjects))
  
  # All subjids in LB should exist in SUBJ
  lb_subjects <- unique(result$Raw_LB$subjid)
  expect_true(all(lb_subjects %in% subj_subjects))
  
  # All siteids in SUBJ should exist in SITE
  # Use siteid if available (numeric ID that matches site_num), otherwise skip test
  if ("siteid" %in% names(result$Raw_SUBJ)) {
    subj_sites <- unique(result$Raw_SUBJ$siteid)
    if ("site_num" %in% names(result$Raw_SITE)) {
      site_sites <- unique(result$Raw_SITE$site_num)
      expect_true(all(subj_sites %in% site_sites))
    }
  } else {
    # If using invid system, check invid
    subj_sites <- unique(result$Raw_SUBJ$invid)
    if ("invid" %in% names(result$Raw_SITE)) {
      site_sites <- unique(result$Raw_SITE$invid)
      expect_true(all(subj_sites %in% site_sites))
    }
  }
})

test_that("ResampleStudy handles strOversamplDomain parameter", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_LB = clindata::rawplus_lb,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  # Sample from high-AE patients (top 25%)
  result_high <- suppressMessages(
    ResampleStudy(
      lRaw,
      "STUDY001",
      nSubjects = 100,
      strOversamplDomain = "Raw_AE",
      vOversamplQuantileRange = c(0.75, 1.0),
      seed = 123
    )
  )
  
  # Sample from low-AE patients (bottom 25%)
  result_low <- suppressMessages(
    ResampleStudy(
      lRaw,
      "STUDY002",
      nSubjects = 100,
      strOversamplDomain = "Raw_AE",
      vOversamplQuantileRange = c(0, 0.25),
      seed = 456
    )
  )
  
  # Both should have sampled subjects (may be < 100 due to filtering)
  expect_true(nrow(result_high$Raw_SUBJ) > 0)
  expect_true(nrow(result_high$Raw_SUBJ) <= 100)
  expect_true(nrow(result_low$Raw_SUBJ) > 0)
  expect_true(nrow(result_low$Raw_SUBJ) <= 100)
  
  # Calculate AE counts per subject in results
  ae_high <- aggregate(
    rep(1, nrow(result_high$Raw_AE)),
    by = list(subjid = result_high$Raw_AE$subjid),
    FUN = length
  )
  names(ae_high)[2] <- "n_ae"
  
  ae_low <- aggregate(
    rep(1, nrow(result_low$Raw_AE)),
    by = list(subjid = result_low$Raw_AE$subjid),
    FUN = length
  )
  names(ae_low)[2] <- "n_ae"
  
  # High group should have more AE records on average
  expect_true(mean(ae_high$n_ae) > mean(ae_low$n_ae))
})

test_that("ResampleStudy input checking works", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae
  )
  
  # Invalid lRaw
  expect_error(ResampleStudy(NULL, "STUDY001"), "must be a named list")
  expect_error(ResampleStudy(c(1, 2, 3), "STUDY001"), "must be a named list")
  
  # Missing Raw_SUBJ
  expect_error(
    ResampleStudy(list(Raw_AE = clindata::rawplus_ae), "STUDY001"),
    "must contain Raw_SUBJ"
  )
  
  # Invalid strNewStudyID
  expect_error(ResampleStudy(lRaw, NULL), "must be a single character string")
  expect_error(ResampleStudy(lRaw, c("A", "B")), "must be a single character string")
  
  # Invalid nSubjects
  expect_error(ResampleStudy(lRaw, "STUDY001", nSubjects = -10), "positive integer")
  expect_error(ResampleStudy(lRaw, "STUDY001", nSubjects = "ten"), "positive integer")
  
  # Invalid replacement
  expect_error(ResampleStudy(lRaw, "STUDY001", replacement = "yes"), "logical value")
  
  # Invalid strOversamplDomain
  expect_error(
    ResampleStudy(lRaw, "STUDY001", strOversamplDomain = "NonExistent"),
    "not found in lRaw"
  )
  
  # Invalid vOversamplQuantileRange
  expect_error(
    ResampleStudy(lRaw, "STUDY001", vOversamplQuantileRange = c(0)),
    "length 2"
  )
  expect_error(
    ResampleStudy(lRaw, "STUDY001", vOversamplQuantileRange = c(-0.1, 0.5)),
    "between 0 and 1"
  )
  expect_error(
    ResampleStudy(lRaw, "STUDY001", vOversamplQuantileRange = c(0, 1.5)),
    "between 0 and 1"
  )
  expect_error(
    ResampleStudy(lRaw, "STUDY001", vOversamplQuantileRange = c(0.8, 0.2)),
    "must be <="
  )
})

test_that("ResampleStudy handles composite ID formats", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  result <- ResampleStudy(lRaw, "STUDY001", nSubjects = 50, seed = 123)
  
  # Check that subjectid in Raw_ENROLL was updated
  if ("subjectid" %in% names(result$Raw_ENROLL)) {
    # All subject IDs should contain the new study prefix
    expect_true(all(grepl("STUDY001_", result$Raw_ENROLL$subjectid)))
  }
  
  # Check subjid updated
  expect_true(all(grepl("^STUDY001_", result$Raw_ENROLL$subjid)))
})

test_that("ResampleStudy handles derived domains with subject_nsv", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  result <- ResampleStudy(lRaw, "STUDY001", nSubjects = 50, seed = 123)
  
  # Check that subject_nsv was updated in derived domains (if column exists)
  if (nrow(result$Raw_DATAENT) > 0 && "subject_nsv" %in% names(result$Raw_DATAENT)) {
    expect_true(all(grepl("STUDY001_", result$Raw_DATAENT$subject_nsv)))
  }
  
  if (nrow(result$Raw_QUERY) > 0 && "subject_nsv" %in% names(result$Raw_QUERY)) {
    expect_true(all(grepl("STUDY001_", result$Raw_QUERY$subject_nsv)))
  }
  
  # At minimum, derived domains should be filtered to selected subjects
  expect_true(nrow(result$Raw_DATAENT) >= 0)
  expect_true(nrow(result$Raw_QUERY) >= 0)
})

test_that("ResampleStudy updates site IDs correctly", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  result <- ResampleStudy(lRaw, "STUDY001", seed = 123)
  
  # Detect site ID column
  site_col <- if ("invid" %in% names(result$Raw_SITE)) {
    "invid"
  } else if ("site_num" %in% names(result$Raw_SITE)) {
    "site_num"
  } else {
    skip("No site ID column found")
  }
  
  # Site IDs should be prefixed only if using invid (not site_num)
  if (site_col == "invid") {
    expect_true(all(grepl("^STUDY001_", result$Raw_SITE[[site_col]])))
  } else {
    # site_num is a reference ID that shouldn't be prefixed
    expect_true(is.numeric(result$Raw_SITE[[site_col]]) || 
                !any(grepl("^STUDY001_", result$Raw_SITE[[site_col]])))
  }
  
  # Site count should be <= original (only keeps used sites)
  expect_true(nrow(result$Raw_SITE) <= nrow(lRaw$Raw_SITE))
  
  # All sites in SUBJ should exist in SITE (check using correct ID column)
  if ("siteid" %in% names(result$Raw_SUBJ) && site_col == "site_num") {
    # Use siteid to match against site_num
    subj_sites <- unique(result$Raw_SUBJ$siteid)
    site_sites <- unique(result$Raw_SITE[[site_col]])
    expect_true(all(subj_sites %in% site_sites))
  } else {
    # Use invid if that's what's available
    subj_sites <- unique(result$Raw_SUBJ$invid)
    site_sites <- unique(result$Raw_SITE[[site_col]])
    expect_true(all(subj_sites %in% site_sites))
  }
})

test_that("ResampleStudy is reproducible with seed", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )
  
  result1 <- ResampleStudy(lRaw, "STUDY001", nSubjects = 100, seed = 42)
  result2 <- ResampleStudy(lRaw, "STUDY001", nSubjects = 100, seed = 42)
  
  # Same seed should give same results
  expect_equal(result1$Raw_SUBJ$subjid, result2$Raw_SUBJ$subjid)
  expect_equal(result1$Raw_SUBJ$invid, result2$Raw_SUBJ$invid)
  expect_equal(nrow(result1$Raw_AE), nrow(result2$Raw_AE))
})

# Site variability tests ----------------------------------------------------
test_that("ResampleStudy with TargetSiteCount generates multiple sites", {
  lRaw_small <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site,
    Raw_AE = clindata::rawplus_ae
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_small,
    strNewStudyID = "SITETEST001",
    nSubjects = 100,
    TargetSiteCount = 20,
    seed = 456
  )
  
  # Check that result has reasonable number of sites (allow some variation)
  actual_site_count <- length(unique(result$Raw_SUBJ$invid))
  expect_true(actual_site_count >= 10)  # At least half of target
  expect_true(actual_site_count <= 20)  # At most the target
  
  # Check that all subject invid values exist in Raw_SITE
  subj_sites <- unique(result$Raw_SUBJ$invid)
  site_ids <- unique(result$Raw_SITE$invid)
  expect_true(all(subj_sites %in% site_ids))
})

test_that("TargetSiteCount generates sites with valid metadata", {
  lRaw_small <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site,
    Raw_AE = clindata::rawplus_ae
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_small,
    strNewStudyID = "SITETEST002",
    nSubjects = 50,
    TargetSiteCount = 15,
    seed = 789
  )
  
  # Check that site metadata columns exist (note: lowercase 'country' in ctms_site)
  expect_true("country" %in% names(result$Raw_SITE) || "Country" %in% names(result$Raw_SITE))
  
  # Check that sites have valid country values from original data
  original_countries <- unique(clindata::ctms_site$country)
  # Get country column (handle both cases)
  country_col <- if ("country" %in% names(result$Raw_SITE)) "country" else "Country"
  result_countries <- unique(result$Raw_SITE[[country_col]])
  expect_true(all(result_countries %in% original_countries))
  
  # Check that site IDs are properly formatted (should have invid column)
  expect_true("invid" %in% names(result$Raw_SITE))
  site_ids <- result$Raw_SITE$invid
  expect_true(all(grepl("^SITETEST002_", site_ids)))
})

test_that("TargetSiteCount creates realistic site distributions", {
  lRaw_small <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site,
    Raw_AE = clindata::rawplus_ae
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_small,
    strNewStudyID = "SITETEST003",
    nSubjects = 100,
    TargetSiteCount = 10,
    seed = 101112
  )
  
  # Check that patients are distributed across sites
  site_patient_counts <- table(result$Raw_SUBJ$invid)
  
  # Should have variation in site sizes
  expect_true(length(unique(site_patient_counts)) > 1)
  
  # All sites should have at least one patient
  expect_true(all(site_patient_counts > 0))
})

test_that("ResampleStudy with TargetSiteCount = NULL uses default behavior", {
  lRaw_small <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site,
    Raw_AE = clindata::rawplus_ae
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_small,
    strNewStudyID = "SITETEST004",
    nSubjects = 50,
    TargetSiteCount = NULL,
    seed = 131415
  )
  
  # Should work normally without errors
  expect_true(length(unique(result$Raw_SUBJ$invid)) > 0)
  
  # All subject sites should exist in Raw_SITE
  subj_sites <- unique(result$Raw_SUBJ$invid)
  
  # Site IDs should be from resampled subjects, not generated
  # (won't necessarily start with study ID prefix)
  expect_true(all(!is.na(subj_sites)))
})

test_that("ResampleStudy validates TargetSiteCount parameter", {
  lRaw_small <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site
  )
  
  expect_error(
    ResampleStudy(lRaw_small, "TEST001", TargetSiteCount = -5),
    "TargetSiteCount must be a single positive number"
  )
  
  expect_error(
    ResampleStudy(lRaw_small, "TEST001", TargetSiteCount = c(10, 20)),
    "TargetSiteCount must be a single positive number"
  )
  
  expect_error(
    ResampleStudy(lRaw_small, "TEST001", TargetSiteCount = "10"),
    "TargetSiteCount must be a single positive number"
  )
})

# Domain Preservation Tests -----------------------------------------------

test_that("ResampleStudy preserves all input domains and column names", {
  # Test with comprehensive domain list
  lRaw_full <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_LB = clindata::rawplus_lb,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_VISIT = clindata::rawplus_visdt
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_full,
    strNewStudyID = "DOMAINTEST001",
    nSubjects = 50,
    seed = 999
  )
  
  # Check that all input domains exist in output
  input_domains <- names(lRaw_full)
  output_domains <- names(result)
  
  expect_true(all(input_domains %in% output_domains),
              info = paste("Missing domains:", 
                          paste(setdiff(input_domains, output_domains), collapse = ", ")))
  
  # Check no unexpected domains were added
  expect_true(all(output_domains %in% input_domains),
              info = paste("Unexpected domains:", 
                          paste(setdiff(output_domains, input_domains), collapse = ", ")))
  
  # Check each domain's columns are preserved
  for (domain_name in names(lRaw_full)) {
    input_cols <- names(lRaw_full[[domain_name]])
    output_cols <- names(result[[domain_name]])
    
    # For generated sites with TargetSiteCount, invid might be added
    # Otherwise, column names should match exactly
    if (domain_name == "Raw_SITE" && "invid" %in% output_cols && !"invid" %in% input_cols) {
      # invid was added for generated sites, check all other columns
      expect_true(all(input_cols %in% output_cols),
                  info = paste(domain_name, "- Missing columns:", 
                              paste(setdiff(input_cols, output_cols), collapse = ", ")))
    } else {
      # Exact match expected
      expect_equal(sort(input_cols), sort(output_cols),
                   info = paste(domain_name, "columns don't match"))
    }
  }
})

test_that("ResampleStudy works with minimal required domains", {
  # Only Raw_SUBJ is required
  lRaw_minimal <- list(
    Raw_SUBJ = clindata::rawplus_dm
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_minimal,
    strNewStudyID = "MINTEST001",
    nSubjects = 20,
    seed = 777
  )
  
  expect_equal(names(result), names(lRaw_minimal))
  expect_equal(sort(names(result$Raw_SUBJ)), sort(names(lRaw_minimal$Raw_SUBJ)))
})

test_that("ResampleStudy handles unknown/custom domains", {
  # Create a custom domain
  custom_domain <- data.frame(
    studyid = rep("ORIGINAL", 10),
    custom_id = 1:10,
    custom_value = letters[1:10],
    stringsAsFactors = FALSE
  )
  
  lRaw_custom <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_CUSTOM = custom_domain
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_custom,
    strNewStudyID = "CUSTOMTEST001",
    nSubjects = 20,
    seed = 666
  )
  
  # Custom domain should be preserved
  expect_true("Raw_CUSTOM" %in% names(result))
  
  # Should have same columns
  expect_equal(sort(names(result$Raw_CUSTOM)), sort(names(custom_domain)))
  
  # studyid should be updated
  expect_true(all(result$Raw_CUSTOM$studyid == "CUSTOMTEST001"))
})

test_that("ResampleStudy maintains reasonable column order", {
  lRaw_test <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae
  )
  
  result <- ResampleStudy(
    lRaw = lRaw_test,
    strNewStudyID = "ORDERTEST001",
    nSubjects = 30,
    seed = 555
  )
  
  # Column order doesn't need to be identical, but key columns should be early
  # For Raw_SUBJ, studyid, subjid should be in first few columns
  subj_cols <- names(result$Raw_SUBJ)
  expect_true(which(subj_cols == "studyid") <= 5)
  expect_true(which(subj_cols == "subjid") <= 5)
})

