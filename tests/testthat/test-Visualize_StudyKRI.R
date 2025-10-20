test_that("Visualize_StudyKRI creates plot with all inputs", {
  # Create test data
  dfStudyKRI <- data.frame(
    StudyID = rep("STUDY1", 5),
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    StudyCount = rep(3, 5),
    stringsAsFactors = FALSE
  )
  
  dfStudyBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    LowerBound = c(0.08, 0.10, 0.12, 0.11, 0.10),
    UpperBound = c(0.12, 0.14, 0.18, 0.17, 0.16),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    dfStudyBounds = dfStudyBounds,
    strStudyID = "STUDY1"
  )
  
  # Verify it's a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Verify plot has data
  expect_true(length(p$layers) > 0)
})

test_that("Visualize_StudyKRI works without individual study bounds", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    StudyCount = rep(3, 5),
    stringsAsFactors = FALSE
  )
  
  # Create plot without individual study bounds
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    dfStudyBounds = NULL,
    strStudyID = "STUDY1"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) > 0)
})

test_that("Visualize_StudyKRI filters to nMaxMonth correctly", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:10,
    Metric = seq(0.10, 0.19, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:10,
    MedianMetric = seq(0.11, 0.20, length.out = 10),
    LowerBound = seq(0.08, 0.17, length.out = 10),
    UpperBound = seq(0.14, 0.23, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  # Filter to first 5 months
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    strStudyID = "STUDY1",
    nMaxMonth = 5
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that data was filtered
  # The plot data should only include months 1-5
  study_data <- p$layers[[3]]$data  # Black line layer
  expect_true(all(study_data$StudyMonth <= 5))
})

test_that("Visualize_StudyKRI validates input types", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  # Test wrong type for dfStudyKRI
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = list(a = 1),
      dfGroupBounds = dfGroupBounds,
      strStudyID = "STUDY1"
    ),
    "dfStudyKRI must be a data.frame"
  )
  
  # Test wrong type for dfGroupBounds
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = list(a = 1),
      strStudyID = "STUDY1"
    ),
    "dfGroupBounds must be a data.frame"
  )
  
  # Test wrong type for dfStudyBounds
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = dfGroupBounds,
      dfStudyBounds = list(a = 1),
      strStudyID = "STUDY1"
    ),
    "dfStudyBounds must be a data.frame or NULL"
  )
  
  # Test invalid strStudyID
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = dfGroupBounds,
      strStudyID = c("STUDY1", "STUDY2")
    ),
    "strStudyID must be a single character string"
  )
  
  # Test invalid nMaxMonth
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = dfGroupBounds,
      strStudyID = "STUDY1",
      nMaxMonth = -5
    ),
    "nMaxMonth must be NULL or a positive number"
  )
})

test_that("Visualize_StudyKRI validates required columns", {
  # Missing column in dfStudyKRI
  dfStudyKRI_bad <- data.frame(
    StudyMonth = 1:5,
    # Missing Metric column
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI_bad,
      dfGroupBounds = dfGroupBounds,
      strStudyID = "STUDY1"
    ),
    "dfStudyKRI missing required columns.*Metric"
  )
  
  # Missing column in dfGroupBounds
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds_bad <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    # Missing LowerBound and UpperBound
    stringsAsFactors = FALSE
  )
  
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = dfGroupBounds_bad,
      strStudyID = "STUDY1"
    ),
    "dfGroupBounds missing required columns"
  )
})

test_that("Visualize_StudyKRI handles empty data after filtering", {
  dfStudyKRI <- data.frame(
    StudyMonth = 6:10,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 6:10,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  # Filter to months that don't exist (data starts at month 6)
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfGroupBounds = dfGroupBounds,
      strStudyID = "STUDY1",
      nMaxMonth = 5
    ),
    "No data available for dfStudyKRI after filtering"
  )
})

test_that("Visualize_StudyKRI uses custom labels", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  custom_title <- "Custom Study Title"
  custom_subtitle <- "Custom Subtitle"
  custom_ylab <- "Custom Y Label"
  custom_xlab <- "Custom X Label"
  
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    strStudyID = "STUDY1",
    strTitle = custom_title,
    strSubtitle = custom_subtitle,
    strYlab = custom_ylab,
    strXlab = custom_xlab
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that custom labels are applied
  expect_equal(p$labels$title, custom_title)
  expect_equal(p$labels$subtitle, custom_subtitle)
  expect_equal(p$labels$y, custom_ylab)
  expect_equal(p$labels$x, custom_xlab)
})

test_that("Visualize_StudyKRI works with custom column names", {
  dfStudyKRI <- data.frame(
    MyMonth = 1:5,
    MyMetric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfGroupBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    strStudyID = "STUDY1",
    strStudyMonthCol = "MyMonth",
    strMetricCol = "MyMetric"
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("Visualize_StudyKRI integration test with workflow output", {
  # Generate multi-study portfolio
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
    seed = 999
  )
  
  # Get study IDs
  study_ids <- unique(lRaw$Raw_SUBJ$studyid)
  target_study <- study_ids[1]
  
  # Run mapping workflows
  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  })
  
  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)
  
  # Run metrics workflow
  metrics_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
    strPackage = NULL
  )
  
  lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped)
  lAnalysis <- lAnalyzed$Analysis_kri0001
  
  # Extract data for visualization
  dfStudyKRI <- lAnalysis$Analysis_Transformed[
    lAnalysis$Analysis_Transformed$StudyID == target_study, ]
  
  dfStudyBounds <- lAnalysis$Analysis_Bounds[
    lAnalysis$Analysis_Bounds$StudyID == target_study, ]
  
  dfGroupBounds <- lAnalysis$Analysis_GroupBounds
  
  # Create visualization
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfGroupBounds = dfGroupBounds,
    dfStudyBounds = dfStudyBounds,
    strStudyID = target_study,
    strYlab = "Cumulative AE Rate per Visit",
    nMaxMonth = 12
  )
  
  # Verify plot was created
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) > 0)
  
  # Verify we can access plot labels
  expect_true(grepl(target_study, p$labels$title))
  expect_equal(p$labels$y, "Cumulative AE Rate per Visit")
})

