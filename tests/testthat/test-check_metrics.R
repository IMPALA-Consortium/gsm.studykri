test_that("hand written test - minimal workflow example") {
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
    Raw_STUDCOMP = clindata::rawplus_studcomp
  )
  
  # Create a portfolio with 3 studies
  lRaw <- SimulatePortfolio(
    lRaw = lRaw_original,
    nStudies = 3,
    seed = 123
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
  
  # Run KRI workflow
  metrics_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  # Combine lMapped with metrics workflows for comprehensive result
  lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped)

  p <- Visualize_StudyKRI(
    dfStudyKRI = lAnalyzed$Analysis_kri0001$Analysis_Transformed,
    dfGroupBounds = lAnalyzed$Analysis_kri0001$Analysis_GroupBounds,
    dfStudyBounds = lAnalyzed$Analysis_kri0001$Analysis_Bounds,
    strStudyID = "PORTFOLIO001",
    strYlab = "Cumulative AE Rate per Visit"
  )
  
  expect_s3_class(p, "ggplot")

}

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
    Raw_STUDCOMP = clindata::rawplus_studcomp
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
  
  # Extract Analysis_Bounds for validation (individual study CIs)
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
  
  # Verify bootstrap count is reasonable (should be close to 1000 per time point)
  expect_true(all(dfBounds$BootstrapCount > 0))
  expect_true(all(dfBounds$BootstrapCount <= 1000))
  
  # Extract Analysis_GroupBounds for validation (combined group CIs)
  dfGroupBounds <- lResult$Analysis_GroupBounds
  
  expect_s3_class(dfGroupBounds, "data.frame")
  expect_true(nrow(dfGroupBounds) > 0)
  
  # Should NOT have StudyID column (studies are combined)
  expect_false("StudyID" %in% names(dfGroupBounds))
  
  expected_cols_group <- c("StudyMonth", "MedianMetric", "LowerBound", 
                            "UpperBound", "BootstrapCount", "GroupCount", "StudyCount")
  expect_true(all(expected_cols_group %in% names(dfGroupBounds)))
  
  expect_type(dfGroupBounds$StudyMonth, "integer")
  expect_type(dfGroupBounds$MedianMetric, "double")
  expect_type(dfGroupBounds$LowerBound, "double")
  expect_type(dfGroupBounds$UpperBound, "double")
  expect_type(dfGroupBounds$BootstrapCount, "integer")
  expect_type(dfGroupBounds$GroupCount, "integer")
  expect_type(dfGroupBounds$StudyCount, "integer")
  
  # Verify metadata
  expect_equal(unique(dfGroupBounds$StudyCount), 3)  # All 3 studies combined
  expect_true(all(dfGroupBounds$GroupCount > 0))
  
  # Verify confidence intervals are sensible
  expect_true(all(dfGroupBounds$LowerBound <= dfGroupBounds$MedianMetric, na.rm = TRUE))
  expect_true(all(dfGroupBounds$MedianMetric <= dfGroupBounds$UpperBound, na.rm = TRUE))
  
  # Verify StudyMonth is sequential
  expect_equal(dfGroupBounds$StudyMonth, seq_len(nrow(dfGroupBounds)))
  
  # Verify bootstrap count is reasonable (should be close to 1000 per time point)
  expect_true(all(dfGroupBounds$BootstrapCount > 0))
  expect_true(all(dfGroupBounds$BootstrapCount <= 1000))
  
  # Test Visualize_StudyKRI with workflow output
  # Extract data for the first study
  target_study <- study_ids[1]
  
  dfStudyKRI <- dfTransformed[dfTransformed$StudyID == target_study, ]
  dfStudyBounds <- dfBounds[dfBounds$StudyID == target_study, ]
  
  # Verify we have data for visualization
  expect_true(nrow(dfStudyKRI) > 0)
  expect_true(nrow(dfStudyBounds) > 0)
  expect_true(nrow(dfGroupBounds) > 0)
  
  # Create visualization with all components
  p1 <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    dfStudyBounds = dfStudyBounds,
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
    dfGroupBounds = dfGroupBounds,
    dfStudyBounds = NULL,
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
  
  # Should error due to missing Mapped_Visit
  expect_error(
    gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped_incomplete)
  )
})

test_that("Analyze_StudyKRI_PredictBoundsGroup works with multi-study portfolio", {
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
    Raw_STUDCOMP = clindata::rawplus_studcomp
  )
  
  # Create a portfolio with 3 studies
  lRaw_portfolio <- SimulatePortfolio(
    lRaw = lRaw_original,
    nStudies = 3,
    seed = 789
  )
  
  # Verify we have multiple studies
  study_ids <- unique(lRaw_portfolio$Raw_SUBJ$studyid)
  expect_equal(length(study_ids), 3)
  
  # Run mapping workflows on portfolio data
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  expect_warning({
    lIngest <- gsm.mapping::Ingest(lRaw_portfolio, gsm.mapping::CombineSpecs(mapping_wf))
  }, "Field `visit_dt`:.*unparsable")
  
  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)
  
  # Get Input_CumCountSiteByMonth data
  dfSiteLevel <- Input_CumCountSiteByMonth(
    dfSubjects = lMapped$Mapped_SUBJ,
    dfNumerator = lMapped$Mapped_AE,
    dfDenominator = lMapped$Mapped_Visit,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Verify site-level data has multiple studies
  expect_equal(length(unique(dfSiteLevel$StudyID)), 3)
  
  # Test Analyze_StudyKRI_PredictBoundsGroup with vStudyFilter = NULL (use all studies)
  suppressMessages({
    dfGroupBounds_all <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfSiteLevel,
      vStudyFilter = NULL,  # Test default behavior - use all studies
      nBootstrapReps = 50,  # Small for speed
      nConfLevel = 0.95,
      nMinDenominator = 10,  # Lower threshold for simulated data
      seed = 789
    )
  })
  
  # Validate output structure
  expect_s3_class(dfGroupBounds_all, "data.frame")
  expect_true(nrow(dfGroupBounds_all) > 0)
  
  expected_cols <- c("StudyMonth", "MedianMetric", "LowerBound", 
                     "UpperBound", "BootstrapCount", "GroupCount", "StudyCount")
  expect_true(all(expected_cols %in% names(dfGroupBounds_all)))
  
  # Verify no StudyID column (studies are combined)
  expect_false("StudyID" %in% names(dfGroupBounds_all))
  
  # Verify metadata
  expect_equal(unique(dfGroupBounds_all$StudyCount), 3)
  expect_true(all(dfGroupBounds_all$GroupCount > 0))
  
  # Verify confidence intervals are sensible
  expect_true(all(dfGroupBounds_all$LowerBound <= dfGroupBounds_all$MedianMetric, na.rm = TRUE))
  expect_true(all(dfGroupBounds_all$MedianMetric <= dfGroupBounds_all$UpperBound, na.rm = TRUE))
  
  # Verify StudyMonth is sequential
  expect_equal(dfGroupBounds_all$StudyMonth, seq_len(nrow(dfGroupBounds_all)))
  
  # Test with subset of studies (2 out of 3)
  suppressMessages({
    dfGroupBounds_subset <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfSiteLevel,
      vStudyFilter = study_ids[1:2],
      nBootstrapReps = 50,
      nConfLevel = 0.95,
      nMinDenominator = 10,
      seed = 789
    )
  })
  
  expect_equal(unique(dfGroupBounds_subset$StudyCount), 2)
  expect_true(nrow(dfGroupBounds_subset) > 0)
  
  # Verify bounds from subset are different from all studies
  # (different study combinations should yield different bounds)
  common_months <- intersect(dfGroupBounds_all$StudyMonth, dfGroupBounds_subset$StudyMonth)
  if (length(common_months) > 0) {
    # At least some bounds should differ
    all_medians <- dfGroupBounds_all$MedianMetric[dfGroupBounds_all$StudyMonth %in% common_months]
    subset_medians <- dfGroupBounds_subset$MedianMetric[dfGroupBounds_subset$StudyMonth %in% common_months]
    
    # Allow for some similarity but expect at least some difference
    expect_true(!all(abs(all_medians - subset_medians) < 1e-10))
  }
})

