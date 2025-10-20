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
  expect_true("Analysis_Transformed" %in% names(lResult))
  expect_equal(lResult$ID, "kri0001")
  
  # Extract Analysis_Input for detailed validation
  dfInput <- lResult$Analysis_Input
  
  # Validate structure
  expect_s3_class(dfInput, "data.frame")
  expect_true(nrow(dfInput) > 0)
  
  # Check expected columns
  expected_cols <- c("GroupID", "GroupLevel", "Numerator", "Denominator", "Metric", "StudyID", "MonthYYYYMM")
  expect_true(all(expected_cols %in% names(dfInput)))
  
  # Check column types
  expect_type(dfInput$GroupID, "character")
  expect_type(dfInput$GroupLevel, "character")
  expect_type(dfInput$Numerator, "integer")
  expect_type(dfInput$Denominator, "integer")
  expect_type(dfInput$Metric, "double")
  expect_type(dfInput$StudyID, "character")
  expect_type(dfInput$MonthYYYYMM, "double")
  
  # Check GroupLevel is "Site"
  expect_true(all(dfInput$GroupLevel == "Site"))
  
  # Validate cumulative property: values should be non-decreasing within each site
  dfInput_ordered <- dfInput[order(dfInput$GroupID, dfInput$MonthYYYYMM), ]
  
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
  
  # Extract Analysis_Transformed for detailed validation
  dfTransformed <- lResult$Analysis_Transformed
  
  # Validate structure
  expect_s3_class(dfTransformed, "data.frame")
  expect_true(nrow(dfTransformed) > 0)
  
  # Check expected columns
  expected_cols_transformed <- c("StudyID", "MonthYYYYMM", "StudyMonth", "Numerator", 
                                  "Denominator", "Metric", "GroupCount")
  expect_true(all(expected_cols_transformed %in% names(dfTransformed)))
  
  # Check column types
  expect_type(dfTransformed$StudyID, "character")
  expect_type(dfTransformed$MonthYYYYMM, "double")
  expect_type(dfTransformed$StudyMonth, "integer")
  expect_type(dfTransformed$Numerator, "integer")
  expect_type(dfTransformed$Denominator, "integer")
  expect_type(dfTransformed$Metric, "double")
  expect_type(dfTransformed$GroupCount, "integer")
  
  # Verify this is study-level (fewer rows than site-level)
  expect_true(nrow(dfTransformed) < nrow(dfInput))
  
  # Verify StudyMonth is sequential within each study
  for (study in unique(dfTransformed$StudyID)) {
    study_data <- dfTransformed[dfTransformed$StudyID == study, ]
    expect_equal(study_data$StudyMonth, seq_len(nrow(study_data)))
  }
  
  # Verify minimum denominator filter was applied
  expect_true(all(dfTransformed$Denominator > 25))
  
  # Verify Metric calculation
  expect_equal(
    dfTransformed$Metric,
    dfTransformed$Numerator / dfTransformed$Denominator,
    tolerance = 1e-10
  )
  
  # Extract Analysis_Bootstrapped for validation
  dfBootstrapped <- lResult$Analysis_Bootstrapped
  
  expect_s3_class(dfBootstrapped, "data.frame")
  expect_true(nrow(dfBootstrapped) > 0)
  expect_true("BootstrapRep" %in% names(dfBootstrapped))
  expect_true(all(c("GroupID", "Numerator", "Denominator", "StudyID") %in% names(dfBootstrapped)))
  
  # Should have approximately nBootstrapReps × site-level row count
  # With replacement sampling, exact count varies
  expect_true(nrow(dfBootstrapped) > nrow(dfInput) * 100)  # At least 100x for 1000 reps
  expect_equal(length(unique(dfBootstrapped$BootstrapRep)), 1000)
  
  # Extract Analysis_BootstrappedStudy for validation
  dfBootstrappedStudy <- lResult$Analysis_BootstrappedStudy
  
  expect_s3_class(dfBootstrappedStudy, "data.frame")
  expect_true(nrow(dfBootstrappedStudy) > 0)
  
  expected_cols_bootstrap <- c("StudyID", "BootstrapRep", "MonthYYYYMM", "StudyMonth",
                                "Numerator", "Denominator", "Metric", "GroupCount")
  expect_true(all(expected_cols_bootstrap %in% names(dfBootstrappedStudy)))
  
  expect_type(dfBootstrappedStudy$Numerator, "integer")
  expect_type(dfBootstrappedStudy$Denominator, "integer")
  expect_type(dfBootstrappedStudy$Metric, "double")
  expect_type(dfBootstrappedStudy$BootstrapRep, "integer")
  expect_equal(length(unique(dfBootstrappedStudy$BootstrapRep)), 1000)
  
  # Verify it's study-level (fewer rows than site-level bootstrap)
  expect_true(nrow(dfBootstrappedStudy) < nrow(dfBootstrapped))
  
  # Verify StudyMonth is sequential within each study × bootstrap replicate
  for (study in unique(dfBootstrappedStudy$StudyID)) {
    for (rep in sample(unique(dfBootstrappedStudy$BootstrapRep), min(10, length(unique(dfBootstrappedStudy$BootstrapRep))))) {
      rep_data <- dfBootstrappedStudy[dfBootstrappedStudy$StudyID == study & dfBootstrappedStudy$BootstrapRep == rep, ]
      if (nrow(rep_data) > 0) {
        expect_equal(rep_data$StudyMonth, seq_len(nrow(rep_data)))
      }
    }
  }
  
  # Verify minimum denominator filter was applied
  expect_true(all(dfBootstrappedStudy$Denominator > 25))
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

