# Test Analyze_StudyKRI_PredictBounds

test_that("Analyze_StudyKRI_PredictBounds calculates confidence intervals with single study grouping", {
  # Create test data with known bootstrap distribution
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    StudyMonth = rep(1:3, each = 10),
    BootstrapRep = rep(1:10, 3),
    Metric = c(
      # Month 1: values 0.1 to 1.0 (median = 0.55)
      seq(0.1, 1.0, length.out = 10),
      # Month 2: values 0.2 to 1.1 (median = 0.65)
      seq(0.2, 1.1, length.out = 10),
      # Month 3: values 0.3 to 1.2 (median = 0.75)
      seq(0.3, 1.2, length.out = 10)
    ),
    stringsAsFactors = FALSE
  )
  
  # Calculate confidence intervals (95%)
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("StudyID", "StudyMonth", "MedianMetric", "LowerBound", "UpperBound", "BootstrapCount"))
  expect_equal(nrow(result), 3)
  
  # Check values for Month 1
  month1 <- result[result$StudyMonth == 1, ]
  expect_equal(month1$StudyID, "STUDY1")
  expect_equal(month1$MedianMetric, 0.55, tolerance = 0.01)
  expect_true(month1$LowerBound < month1$MedianMetric)
  expect_true(month1$UpperBound > month1$MedianMetric)
  expect_equal(month1$BootstrapCount, 10)
  
  # Check ordering
  expect_true(all(diff(result$StudyMonth) > 0))
})

test_that("Analyze_StudyKRI_PredictBounds works with no grouping (multi-study combined)", {
  # Create test data from multiple studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 20),
    StudyMonth = rep(1:2, each = 10, times = 2),
    BootstrapRep = rep(1:10, 4),
    Metric = runif(40, 0.1, 1.0),
    stringsAsFactors = FALSE
  )
  
  # Calculate combined confidence intervals (no StudyID grouping)
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = character(0),
    nConfLevel = 0.95
  )
  
  # Check structure - should not have StudyID column
  expect_s3_class(result, "data.frame")
  expect_named(result, c("StudyMonth", "MedianMetric", "LowerBound", "UpperBound", "BootstrapCount"))
  expect_equal(nrow(result), 2)  # Only 2 months, combined across studies
  
  # Check bootstrap count (should be 20 per month: 2 studies × 10 reps)
  expect_equal(result$BootstrapCount[1], 20)
  expect_equal(result$BootstrapCount[2], 20)
})

test_that("Analyze_StudyKRI_PredictBounds handles different confidence levels", {
  # Create test data
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 100),
    StudyMonth = rep(1, 100),
    BootstrapRep = 1:100,
    Metric = seq(0, 1, length.out = 100),  # Uniform distribution
    stringsAsFactors = FALSE
  )
  
  # Test 90% CI
  result_90 <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.90
  )
  
  # Test 95% CI
  result_95 <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )
  
  # Test 99% CI
  result_99 <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.99
  )
  
  # 99% CI should be wider than 95% CI, which should be wider than 90% CI
  expect_true(result_99$UpperBound - result_99$LowerBound > 
              result_95$UpperBound - result_95$LowerBound)
  expect_true(result_95$UpperBound - result_95$LowerBound > 
              result_90$UpperBound - result_90$LowerBound)
  
  # All should have same median
  expect_equal(result_90$MedianMetric, result_95$MedianMetric)
  expect_equal(result_95$MedianMetric, result_99$MedianMetric)
  
  # Check approximate percentiles for uniform distribution
  # 95% CI: 2.5th to 97.5th percentile
  expect_equal(result_95$LowerBound, 0.025, tolerance = 0.02)
  expect_equal(result_95$UpperBound, 0.975, tolerance = 0.02)
})

test_that("Analyze_StudyKRI_PredictBounds works with multiple grouping columns", {
  # Create test data with StudyID and Country
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY1"), each = 20),
    Country = rep(c("USA", "UK"), each = 20),
    StudyMonth = rep(1:2, each = 10, times = 2),
    BootstrapRep = rep(1:10, 4),
    Metric = runif(40, 0.1, 1.0),
    stringsAsFactors = FALSE
  )
  
  # Calculate CI grouped by StudyID and Country
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = c("StudyID", "Country"),
    nConfLevel = 0.95
  )
  
  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("StudyID", "Country", "StudyMonth") %in% names(result)))
  
  # Should have 4 rows: 2 countries × 2 months
  expect_equal(nrow(result), 4)
  
  # Check unique combinations
  expect_equal(nrow(unique(result[, c("StudyID", "Country")])), 2)
})

test_that("Analyze_StudyKRI_PredictBounds validates input", {
  # Create valid test data
  dfValid <- data.frame(
    StudyID = rep("STUDY1", 10),
    StudyMonth = rep(1, 10),
    BootstrapRep = 1:10,
    Metric = runif(10),
    stringsAsFactors = FALSE
  )
  
  # Test: Not a data frame
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = list(a = 1),
      vBy = "StudyID",
      nConfLevel = 0.95
    ),
    "dfInput must be a data.frame or tbl object"
  )
  
  # Test: Missing required column (BootstrapRep)
  dfMissing <- dfValid[, -which(names(dfValid) == "BootstrapRep")]
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfMissing,
      vBy = "StudyID",
      nConfLevel = 0.95
    ),
    "dfInput missing required columns: BootstrapRep"
  )
  
  # Test: Missing vBy column
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfValid,
      vBy = "NonExistentColumn",
      nConfLevel = 0.95
    ),
    "vBy columns not found in dfInput: NonExistentColumn"
  )
  
  # Test: Invalid confidence level (too low)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = 0
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )
  
  # Test: Invalid confidence level (too high)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = 1
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )
  
  # Test: Invalid confidence level (not numeric)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = "0.95"
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )
})

test_that("Analyze_StudyKRI_PredictBounds handles custom column names", {
  # Create test data with custom column names (must have BootstrapRep)
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 10),
    TimePoint = rep(1, 10),
    BootstrapRep = 1:10,
    Value = runif(10, 0.1, 1.0),
    stringsAsFactors = FALSE
  )
  
  # Calculate CI with custom column names
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95,
    strMetricCol = "Value",
    strStudyMonthCol = "TimePoint"
  )
  
  # Check that custom columns were used
  expect_s3_class(result, "data.frame")
  expect_true("TimePoint" %in% names(result))
  expect_true(all(!is.na(result$MedianMetric)))
  expect_equal(result$TimePoint, 1)
  expect_equal(result$BootstrapCount, 10)
})

test_that("Analyze_StudyKRI_PredictBounds handles NA values correctly", {
  # Create test data with some NA values
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    StudyMonth = rep(1, 12),
    BootstrapRep = 1:12,
    Metric = c(runif(10, 0.1, 1.0), NA, NA),
    stringsAsFactors = FALSE
  )
  
  # Calculate CI (should handle NAs)
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )
  
  # Should still return results (NAs removed)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_false(is.na(result$MedianMetric))
  expect_false(is.na(result$LowerBound))
  expect_false(is.na(result$UpperBound))
  expect_equal(result$BootstrapCount, 12)  # Count includes all rows
})

test_that("Analyze_StudyKRI_PredictBounds integration with full workflow", {
  # Skip if required packages not available
  skip_if_not_installed("clindata")
  
  # Create minimal test data
  dfSubjects <- data.frame(
    subjid = c("S1", "S2", "S3", "S4"),
    studyid = c("STUDY1", "STUDY1", "STUDY1", "STUDY1"),
    invid = c("SITE1", "SITE1", "SITE2", "SITE2"),
    stringsAsFactors = FALSE
  )
  
  dfNumerator <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S1", "S3"),
    aest_dt = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10", 
                        "2024-02-15", "2024-03-01", "2024-03-05")),
    stringsAsFactors = FALSE
  )
  
  dfDenominator <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S1", "S2", "S3", "S4"),
    visit_dt = as.Date(c("2024-01-10", "2024-01-12", "2024-02-05",
                         "2024-02-08", "2024-03-01", "2024-03-02",
                         "2024-03-03", "2024-03-04")),
    stringsAsFactors = FALSE
  )
  
  # Run full workflow
  dfInput <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Generate bootstrap samples (small number for speed)
  dfBootstrap <- Analyze_StudyKRI(
    dfInput = dfInput,
    nBootstrapReps = 10,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    seed = 123
  )
  
  # Aggregate to study level
  dfBootstrapStudy <- Transform_CumCount(
    dfInput = dfBootstrap,
    vBy = c("StudyID", "BootstrapRep"),
    nMinDenominator = 1  # Low threshold for small test data
  )
  
  # Calculate confidence intervals
  dfBounds <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfBootstrapStudy,
    vBy = "StudyID",
    nConfLevel = 0.95
  )
  
  # Validate output
  expect_s3_class(dfBounds, "data.frame")
  expect_true(all(c("StudyID", "StudyMonth", "MedianMetric", "LowerBound", 
                    "UpperBound", "BootstrapCount") %in% names(dfBounds)))
  expect_true(nrow(dfBounds) > 0)
  expect_equal(unique(dfBounds$StudyID), "STUDY1")
  
  # Check bounds are sensible
  expect_true(all(dfBounds$LowerBound <= dfBounds$MedianMetric))
  expect_true(all(dfBounds$MedianMetric <= dfBounds$UpperBound))
  
  # BootstrapCount should be positive (might vary by month due to filtering)
  expect_true(all(dfBounds$BootstrapCount > 0))
  expect_true(all(dfBounds$BootstrapCount <= 10))
})

