test_that("kri0001 workflow executes successfully", {
  # Generate multi-study portfolio using SimulatePortfolio
  lRaw_original <- list(
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
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_StudyRef = tibble::tibble(
      studyid = character(),
      studyrefid = character()
    )
  )
  
  # Create a portfolio with 3 studies
  lRaw <- SimulatePortfolio(
    lRaw = lRaw_original,
    nStudies = 3,
    seed = 123
  )
  
  # Verify we have multiple studies
  study_ids <- unique(lRaw$Raw_SUBJ$studyid)
  expect_equal(length(study_ids), 3)
  
  # After SimulatePortfolio, populate with actual study references
  lRaw$Raw_StudyRef <- tibble::tibble(
    studyid = rep(study_ids, each = 2),
    studyrefid = rep(study_ids[c(2, 3, 1, 3, 1, 2)], length.out = 6)
  )
  
  # Run mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  expect_warning({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  }, "Field `visit_dt`:.*unparsable")
  
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
  
  # Reduce bootstrap iterations for faster testing
  metrics_wf$kri0001$meta$BootstrapReps <- 100
  
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
  
  # Extract Analysis_Input for detailed checks
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
  
  # Note: Input_CountSiteByMonth now returns MONTHLY counts (not cumulative)
  # Cumulative calculation happens at study level in Transform_CumCount
  # So we no longer test for monotonically increasing counts at site level
  
  # Validate Metric calculation
  rows_with_denom <- dfInput$Denominator > 0
  calculated_metric <- dfInput$Numerator[rows_with_denom] / dfInput$Denominator[rows_with_denom]
  expect_equal(dfInput$Metric[rows_with_denom], calculated_metric, tolerance = 1e-10)
  
  # Extract Analysis_Transformed for detailed checks
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
  
  # Extract Analysis_Bootstrapped for checks
  dfBootstrapped <- lResult$Analysis_Bootstrapped
  
  expect_s3_class(dfBootstrapped, "data.frame")
  expect_true(nrow(dfBootstrapped) > 0)
  expect_true("BootstrapRep" %in% names(dfBootstrapped))
  expect_true(all(c("GroupID", "Numerator", "Denominator", "StudyID") %in% names(dfBootstrapped)))
  
  # Should have approximately nBootstrapReps × site-level row count
  # With replacement sampling, exact count varies
  expect_true(nrow(dfBootstrapped) > nrow(dfInput) * 10)  # At least 10x for 100 reps
  expect_equal(length(unique(dfBootstrapped$BootstrapRep)), 100)
  
  # Extract Analysis_BootstrappedStudy for checks
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
  expect_equal(length(unique(dfBootstrappedStudy$BootstrapRep)), 100)
  
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
  
  # Extract Analysis_Bounds for checks (individual study CIs)
  dfBounds <- lResult$Analysis_Bounds
  
  expect_s3_class(dfBounds, "data.frame")
  expect_true(nrow(dfBounds) > 0)
  
  # Should have StudyID column (one CI per study)
  expect_true("StudyID" %in% names(dfBounds))
  expect_equal(length(unique(dfBounds$StudyID)), 3)  # 3 studies
  
  expected_cols_bounds <- c("StudyID", "StudyMonth", "MedianMetric", "LowerBound", 
                             "UpperBound", "BootstrapCount")
  expect_true(all(expected_cols_bounds %in% names(dfBounds)))
  
  expect_type(dfBounds$StudyID, "character")
  expect_type(dfBounds$StudyMonth, "integer")
  expect_type(dfBounds$MedianMetric, "double")
  expect_type(dfBounds$LowerBound, "double")
  expect_type(dfBounds$UpperBound, "double")
  expect_type(dfBounds$BootstrapCount, "integer")
  
  # Verify confidence intervals are sensible
  expect_true(all(dfBounds$LowerBound <= dfBounds$MedianMetric))
  expect_true(all(dfBounds$MedianMetric <= dfBounds$UpperBound))
  
  # Verify StudyMonth is sequential
  for (study in unique(dfBounds$StudyID)) {
    study_data <- dfBounds[dfBounds$StudyID == study, ]
    expect_equal(study_data$StudyMonth, seq_len(nrow(study_data)))
  }
  
  # Verify bootstrap count is reasonable (should be close to 100 per time point)
  expect_true(all(dfBounds$BootstrapCount > 0))
  expect_true(all(dfBounds$BootstrapCount <= 100))
  
  # Extract Analysis_BoundsRef for checks (study-specific reference CIs)
  dfBoundsRef <- lResult$Analysis_BoundsRef
  
  expect_s3_class(dfBoundsRef, "data.frame")
  expect_true(nrow(dfBoundsRef) > 0)
  
  # Should HAVE StudyID column (per-study reference bounds when dfStudyRef is provided)
  expect_true("StudyID" %in% names(dfBoundsRef))
  
  expected_cols_group <- c("StudyID", "StudyMonth", "MedianMetric", "LowerBound", 
                            "UpperBound", "BootstrapCount", "GroupCount", "StudyCount")
  expect_true(all(expected_cols_group %in% names(dfBoundsRef)))
  
  expect_type(dfBoundsRef$StudyID, "character")
  expect_type(dfBoundsRef$StudyMonth, "integer")
  expect_type(dfBoundsRef$MedianMetric, "double")
  expect_type(dfBoundsRef$LowerBound, "double")
  expect_type(dfBoundsRef$UpperBound, "double")
  expect_type(dfBoundsRef$BootstrapCount, "integer")
  expect_type(dfBoundsRef$GroupCount, "integer")
  expect_type(dfBoundsRef$StudyCount, "integer")
  
  # Verify metadata - each study has its own reference set (2 reference studies each)
  expect_true(all(dfBoundsRef$StudyCount >= 1))
  expect_true(all(dfBoundsRef$GroupCount > 0))
  
  # Verify confidence intervals are sensible
  expect_true(all(dfBoundsRef$LowerBound <= dfBoundsRef$MedianMetric, na.rm = TRUE))
  expect_true(all(dfBoundsRef$MedianMetric <= dfBoundsRef$UpperBound, na.rm = TRUE))
  
  # Verify StudyMonth is sequential within each study
  for (study in unique(dfBoundsRef$StudyID)) {
    study_data <- dfBoundsRef[dfBoundsRef$StudyID == study, ]
    expect_equal(study_data$StudyMonth, seq_len(nrow(study_data)))
  }
  
  # Verify bootstrap count is reasonable (should be close to 100 per time point)
  expect_true(all(dfBoundsRef$BootstrapCount > 0))
  expect_true(all(dfBoundsRef$BootstrapCount <= 100))
  
  # Test Visualize_StudyKRI with workflow output
  # Extract data for the first study
  target_study <- study_ids[1]
  
  dfStudyKRI <- dfTransformed[dfTransformed$StudyID == target_study, ]
  dfBounds <- dfBounds[dfBounds$StudyID == target_study, ]
  
  # Verify we have data for visualization
  expect_true(nrow(dfStudyKRI) > 0)
  expect_true(nrow(dfBounds) > 0)
  expect_true(nrow(dfBoundsRef) > 0)
  
  # Create visualization with all components
  p1 <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    dfBounds = dfBounds,
    strStudyID = target_study,
    strYlab = "Cumulative AE Rate per Visit"
  )
  
  # Verify plot was created
  expect_s3_class(p1, "ggplot")
  expect_true(length(p1$layers) > 0)
  
  # Verify plot labels contain study ID
  expect_true(grepl(target_study, p1$labels$title))
  expect_equal(p1$labels$y, "Cumulative AE Rate per Visit")
  
  # Test visualization without individual study bounds
  p2 <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    dfBounds = NULL,
    strStudyID = target_study,
    nMaxMonth = 12  # Limit to first 12 months
  )
  
  # Verify plot was created
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > 0)
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
  
  # Reduce bootstrap iterations for faster testing
  metrics_wf$kri0001$meta$BootstrapReps <- 100
  
  # Should error due to missing Mapped_Visit
  expect_error(
    gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped_incomplete)
  )
})
