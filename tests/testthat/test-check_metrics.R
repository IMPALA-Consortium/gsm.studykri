test_that("kri0001 workflow executes successfully", {
  # Load test data
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_VISIT = clindata::rawplus_visdt,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_LB = clindata::rawplus_lb,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp
  )
  
  # Run mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  expect_warning({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  }, "Field `visit_dt`: 19 unparsable Date\\(s\\) set to NA")
  
  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)
  
  # Verify mapped data exists
  expect_true("Mapped_SUBJ" %in% names(lMapped))
  expect_true("Mapped_AE" %in% names(lMapped))
  expect_true("Mapped_Visit" %in% names(lMapped))
  
  # Run KRI workflow
  metrics_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Combine lMapped with metrics workflows for comprehensive result
  lData_combined <- lMapped
  lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lData_combined)
  
  # Validate workflow output structure
  # RunWorkflows returns results organized by workflow name (Type_ID)
  expect_type(lAnalyzed, "list")
  expect_true("Analysis_kri0001" %in% names(lAnalyzed))
  
  # The workflow result is stored under Analysis_kri0001
  lResult <- lAnalyzed$Analysis_kri0001
  expect_type(lResult, "list")
  expect_true("ID" %in% names(lResult))
  expect_true("Analysis_Input" %in% names(lResult))
  expect_equal(lResult$ID, "kri0001")
  
  # Extract Analysis_Input for detailed validation
  dfInput <- lResult$Analysis_Input
  
  # Validate structure
  expect_s3_class(dfInput, "data.frame")
  expect_true(nrow(dfInput) > 0)
  
  # Check expected columns
  expected_cols <- c("GroupID", "GroupLevel", "Numerator", "Denominator", "Metric", "StudyID", "Month")
  expect_true(all(expected_cols %in% names(dfInput)))
  
  # Check column types
  expect_type(dfInput$GroupID, "character")
  expect_type(dfInput$GroupLevel, "character")
  expect_type(dfInput$Numerator, "integer")
  expect_type(dfInput$Denominator, "integer")
  expect_type(dfInput$Metric, "double")
  expect_type(dfInput$StudyID, "character")
  expect_type(dfInput$Month, "double")
  
  # Check GroupLevel is "Site"
  expect_true(all(dfInput$GroupLevel == "Site"))
  
  # Validate cumulative property: values should be non-decreasing within each site
  dfInput_ordered <- dfInput[order(dfInput$GroupID, dfInput$Month), ]
  
  for (site in unique(dfInput_ordered$GroupID)) {
    site_data <- dfInput_ordered[dfInput_ordered$GroupID == site, ]
    
    # Numerator should be non-decreasing
    expect_true(all(diff(site_data$Numerator) >= 0))
    
    # Denominator should be non-decreasing
    expect_true(all(diff(site_data$Denominator) >= 0))
  }
  
  # Validate Metric calculation
  rows_with_denom <- dfInput$Denominator > 0
  calculated_metric <- dfInput$Numerator[rows_with_denom] / dfInput$Denominator[rows_with_denom]
  expect_equal(dfInput$Metric[rows_with_denom], calculated_metric, tolerance = 1e-10)
})

test_that("kri0001 workflow validates required mapped data", {
  # Test with missing Mapped_Visit
  lMapped_incomplete <- list(
    Mapped_SUBJ = data.frame(
      studyid = "TEST001",
      invid = "SITE01",
      subjid = "SUBJ001",
      enrollyn = "Y"
    ),
    Mapped_AE = data.frame(
      subjid = "SUBJ001",
      aest_dt = as.Date("2024-01-01")
    )
  )
  
  metrics_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Should error due to missing Mapped_Visit
  expect_error(
    gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped_incomplete)
  )
})

