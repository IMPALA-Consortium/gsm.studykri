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
    Raw_VISIT = clindata::rawplus_visdt
  )

  # Load mapping workflows
  # Use test_path() to find inst directory relative to tests/testthat
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = test_path("../../inst/workflow/1_mappings"),
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

