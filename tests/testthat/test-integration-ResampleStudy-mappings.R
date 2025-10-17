test_that("ResampleStudy output works with mapping workflows", {
  # Load all raw data domains
  lRaw <- list(
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_AE = clindata::rawplus_ae,
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_LB = clindata::rawplus_lb,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_VISIT = clindata::rawplus_visdt
  )
  
  # Create resampled study
  lResampled <- ResampleStudy(
    lRaw,
    strNewStudyID = "RESAMPLED001",
    nSubjects = 100,
    seed = 42
  )
  
  # Verify basic structure
  expect_type(lResampled, "list")
  expect_true("Raw_SUBJ" %in% names(lResampled))
  # May be slightly less than 100 due to data filtering
  expect_true(nrow(lResampled$Raw_SUBJ) >= 90)
  expect_true(nrow(lResampled$Raw_SUBJ) <= 100)
  
  # Load mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Execute mapping workflows on resampled data
  # May have warning about unparseable dates (depends on sampled data)
  lIngest <- suppressWarnings(
    gsm.mapping::Ingest(lResampled, gsm.mapping::CombineSpecs(mapping_wf))
  )
  
  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)
  
  # Assertions
  expect_type(lMapped, "list")
  expect_true(length(lMapped) > 0)
  
  # Check that expected mapped datasets exist
  expected_outputs <- c(
    "Mapped_AE", "Mapped_COUNTRY", "Mapped_DATAENT", 
    "Mapped_ENROLL", "Mapped_LB", "Mapped_PD", 
    "Mapped_QUERY", "Mapped_Randomization", "Mapped_SDRGCOMP",
    "Mapped_SITE", "Mapped_STUDCOMP", "Mapped_STUDY", 
    "Mapped_SUBJ", "Mapped_Visit"
  )
  
  # Check each expected output exists
  for (output in expected_outputs) {
    expect_true(
      output %in% names(lMapped),
      info = sprintf("Expected output '%s' not found in lMapped", output)
    )
  }
  
  # Verify studyid updated across mapped datasets
  expect_true(all(lMapped$Mapped_SUBJ$studyid == "RESAMPLED001"))
  if (nrow(lMapped$Mapped_AE) > 0) {
    expect_true(all(lMapped$Mapped_AE$studyid == "RESAMPLED001"))
  }
})

test_that("ResampleStudy with oversampling works with mapping workflows", {
  # Load data
  lRaw <- list(
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_AE = clindata::rawplus_ae,
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_LB = clindata::rawplus_lb,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_VISIT = clindata::rawplus_visdt
  )
  
  # Create resampled study with high-AE patients
  lResampled <- suppressMessages(
    ResampleStudy(
      lRaw,
      strNewStudyID = "HIGHAE001",
      nSubjects = 50,
      strOversamplDomain = "Raw_AE",
      vOversamplQuantileRange = c(0.75, 1.0),
      seed = 999
    )
  )
  
  # Should have resampled subjects
  expect_true(nrow(lResampled$Raw_SUBJ) > 0)
  expect_true(nrow(lResampled$Raw_SUBJ) <= 50)
  
  # Load mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Execute mapping workflows - should succeed
  lIngest <- suppressWarnings(
    gsm.mapping::Ingest(lResampled, gsm.mapping::CombineSpecs(mapping_wf))
  )
  
  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)
  
  # Basic checks
  expect_type(lMapped, "list")
  expect_true("Mapped_SUBJ" %in% names(lMapped))
  expect_true("Mapped_AE" %in% names(lMapped))
  
  # Verify all subjects in mapped data have correct study ID
  expect_true(all(lMapped$Mapped_SUBJ$studyid == "HIGHAE001"))
})

