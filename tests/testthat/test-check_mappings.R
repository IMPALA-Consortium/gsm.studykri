test_that("mapping workflows execute successfully on clindata", {
  # Load test data with Raw_* naming to match YAML specs
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
    Raw_VISIT = clindata::rawplus_visdt,
    Raw_StudyRef = tibble::tibble(
      studyid = character(),
      studyrefid = character()
    )
  )

  # Load mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )

  # Execute mapping workflows
  # Expect warning about unparseable dates in source data (known data quality issue)
  expect_warning({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  }, "Field `visit_dt`: 19 unparsable Date\\(s\\) set to NA")
  
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
})

test_that("mapping workflows execute successfully on portfolio data", {
  # Load original data for portfolio generation
  lRaw_original <- list(
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
    Raw_VISIT = clindata::rawplus_visdt,
    Raw_StudyRef = tibble::tibble(
      studyid = character(),
      studyrefid = character()
    )
  )
  
  # Generate portfolio with 3 studies
  lPortfolio <- SimulatePortfolio(
    lRaw = lRaw_original,
    nStudies = 3,
    seed = 54321
  )
  
  # Optionally populate with references between the 3 studies
  study_ids <- unique(lPortfolio$Raw_SUBJ$studyid)
  lPortfolio$Raw_StudyRef <- tibble::tibble(
    studyid = rep(study_ids, each = 2),
    studyrefid = rep(study_ids[c(2, 3, 1, 3, 1, 2)], length.out = 6)
  )
  
  # Load mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Execute mapping workflows on portfolio data
  # Use suppressWarnings since portfolio data may have date quality issues
  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lPortfolio, gsm.mapping::CombineSpecs(mapping_wf))
  })
  
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
      info = sprintf("Expected output '%s' not found in lMapped from portfolio data", output)
    )
  }
  
  # Verify multiple studies are present
  study_ids <- unique(lMapped$Mapped_SUBJ$studyid)
  expect_equal(length(study_ids), 3)
  expect_true(all(grepl("^PORTFOLIO\\d{3}$", study_ids)))
  
  # Verify reasonable subject counts across portfolio
  total_subjects <- nrow(lMapped$Mapped_SUBJ)
  expect_true(total_subjects > 0)
  expect_true(total_subjects <= 300) # 3 studies * ~100 max subjects each
})

