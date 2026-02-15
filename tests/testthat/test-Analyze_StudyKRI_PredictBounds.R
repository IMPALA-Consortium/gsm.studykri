# Test CalculateStudyBounds (internal function)

test_that("CalculateStudyBounds calculates confidence intervals with single study grouping", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
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
  result <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("StudyID", "StudyMonth", "Median", "Lower", "Upper", "BootstrapCount"))
  expect_equal(nrow(result), 3)

  # Check values for Month 1
  month1 <- result[result$StudyMonth == 1, ]
  expect_equal(month1$StudyID, "STUDY1")
  expect_equal(month1$Median, 0.55, tolerance = 0.01)
  expect_true(month1$Lower < month1$Median)
  expect_true(month1$Upper > month1$Median)
  expect_equal(month1$BootstrapCount, 10)

  # Check ordering
  expect_true(all(diff(result$StudyMonth) > 0))
})

test_that("CalculateStudyBounds works with no grouping (multi-study combined)", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  # Create test data from multiple studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 20),
    StudyMonth = rep(1:2, each = 10, times = 2),
    BootstrapRep = rep(1:10, 4),
    Metric = runif(40, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  # Calculate combined confidence intervals (no StudyID grouping)
  result <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = character(0),
    nConfLevel = 0.95
  )

  # Check structure - should not have StudyID column
  expect_s3_class(result, "data.frame")
  expect_named(result, c("StudyMonth", "Median", "Lower", "Upper", "BootstrapCount"))
  expect_equal(nrow(result), 2) # Only 2 months, combined across studies

  # Check bootstrap count (should be 10 per month: 2 studies × 10 reps)
  expect_equal(result$BootstrapCount[1], 10)
  expect_equal(result$BootstrapCount[2], 10)
})

test_that("CalculateStudyBounds handles different confidence levels", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  # Create test data
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 100),
    StudyMonth = rep(1, 100),
    BootstrapRep = 1:100,
    Metric = seq(0, 1, length.out = 100), # Uniform distribution
    stringsAsFactors = FALSE
  )

  # Test 90% CI
  result_90 <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.90
  )

  # Test 95% CI
  result_95 <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )

  # Test 99% CI
  result_99 <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.99
  )

  # 99% CI should be wider than 95% CI, which should be wider than 90% CI
  expect_true(result_99$Upper - result_99$Lower >
    result_95$Upper - result_95$Lower)
  expect_true(result_95$Upper - result_95$Lower >
    result_90$Upper - result_90$Lower)

  # All should have same median
  expect_equal(result_90$Median, result_95$Median)
  expect_equal(result_95$Median, result_99$Median)

  # Check approximate percentiles for uniform distribution
  # 95% CI: 2.5th to 97.5th percentile
  expect_equal(result_95$Lower, 0.025, tolerance = 0.02)
  expect_equal(result_95$Upper, 0.975, tolerance = 0.02)
})

test_that("CalculateStudyBounds works with multiple grouping columns", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
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
  result <- CalculateStudyBounds(
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

test_that("CalculateStudyBounds validates input", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
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
    CalculateStudyBounds(
      dfInput = list(a = 1),
      vBy = "StudyID",
      nConfLevel = 0.95
    ),
    "dfInput must be a data.frame or tbl object"
  )

  # Test: Missing required column (BootstrapRep)
  dfMissing <- dfValid[, -which(names(dfValid) == "BootstrapRep")]
  expect_error(
    CalculateStudyBounds(
      dfInput = dfMissing,
      vBy = "StudyID",
      nConfLevel = 0.95
    ),
    "dfInput missing required columns: BootstrapRep"
  )

  # Test: Missing vBy column
  expect_error(
    CalculateStudyBounds(
      dfInput = dfValid,
      vBy = "NonExistentColumn",
      nConfLevel = 0.95
    ),
    "vBy columns not found in dfInput: NonExistentColumn"
  )

  # Test: Invalid confidence level (too low)
  expect_error(
    CalculateStudyBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = 0
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )

  # Test: Invalid confidence level (too high)
  expect_error(
    CalculateStudyBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = 1
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )

  # Test: Invalid confidence level (not numeric)
  expect_error(
    CalculateStudyBounds(
      dfInput = dfValid,
      vBy = "StudyID",
      nConfLevel = "0.95"
    ),
    "nConfLevel must be a single numeric value between 0 and 1"
  )
})

test_that("CalculateStudyBounds handles custom column names", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  # Create test data with custom column names (must have BootstrapRep)
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 10),
    TimePoint = rep(1, 10),
    BootstrapRep = 1:10,
    Metric = runif(10, 0.1, 1.0),
    stringsAsFactors = FALSE
  )

  # Calculate CI with custom column names
  result <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95,
    strStudyMonthCol = "TimePoint"
  )

  # Check that custom columns were used
  expect_s3_class(result, "data.frame")
  expect_true("TimePoint" %in% names(result))
  expect_true(all(!is.na(result$Median)))
  expect_equal(result$TimePoint, 1)
  expect_equal(result$BootstrapCount, 10)
})

test_that("CalculateStudyBounds handles NA values correctly", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  # Create test data with some NA values
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    StudyMonth = rep(1, 12),
    BootstrapRep = 1:12,
    Metric = c(runif(10, 0.1, 1.0), NA, NA),
    stringsAsFactors = FALSE
  )

  # Calculate CI (should handle NAs)
  result <- CalculateStudyBounds(
    dfInput = dfTest,
    vBy = "StudyID",
    nConfLevel = 0.95
  )

  # Should still return results (NAs removed)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_false(is.na(result$Median))
  expect_false(is.na(result$Lower))
  expect_false(is.na(result$Upper))
  expect_equal(result$BootstrapCount, 12) # Count includes all rows
})

test_that("CalculateStudyBounds integration with full workflow", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
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
    studyid = c("STUDY1", "STUDY1", "STUDY1", "STUDY1", "STUDY1", "STUDY1"),
    subjid = c("S1", "S2", "S3", "S4", "S1", "S3"),
    aest_dt = as.Date(c(
      "2024-01-15", "2024-01-20", "2024-02-10",
      "2024-02-15", "2024-03-01", "2024-03-05"
    )),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    studyid = rep("STUDY1", 8),
    subjid = c("S1", "S2", "S3", "S4", "S1", "S2", "S3", "S4"),
    visit_dt = as.Date(c(
      "2024-01-10", "2024-01-12", "2024-02-05",
      "2024-02-08", "2024-03-01", "2024-03-02",
      "2024-03-03", "2024-03-04"
    )),
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
  dfBootstrap <- BootstrapStudyKRI(
    dfInput = dfInput,
    nBootstrapReps = 10,
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    seed = 123
  )

  # Aggregate to study level
  dfBootstrapStudy <- Transform_CumCount(
    dfInput = dfBootstrap,
    vBy = c("StudyID", "BootstrapRep")
  )

  # Calculate confidence intervals
  dfBounds <- CalculateStudyBounds(
    dfInput = dfBootstrapStudy,
    vBy = "StudyID",
    nConfLevel = 0.95
  )

  # Validate output
  expect_s3_class(dfBounds, "data.frame")
  expect_true(all(c(
    "StudyID", "StudyMonth", "Median", "Lower",
    "Upper", "BootstrapCount"
  ) %in% names(dfBounds)))
  expect_true(nrow(dfBounds) > 0)
  expect_equal(unique(dfBounds$StudyID), "STUDY1")

  # Check bounds are sensible
  expect_true(all(dfBounds$Lower <= dfBounds$Median))
  expect_true(all(dfBounds$Median <= dfBounds$Upper))

  # BootstrapCount should be positive (might vary by month due to filtering)
  expect_true(all(dfBounds$BootstrapCount > 0))
  expect_true(all(dfBounds$BootstrapCount <= 10))
})

# Tests for Analyze_StudyKRI_PredictBounds (main wrapper function)

test_that("Analyze_StudyKRI_PredictBounds works with NULL dfStudyRef (all studies)", {
  # Create test data for multiple studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 2),
    Numerator = sample(0:5, 60, replace = TRUE),
    Denominator = sample(10:20, 60, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202303, each = 10), times = 2),
    Metric = runif(60, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Test with NULL dfStudyRef - should use all studies
  expect_message(
    result <- Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      dfStudyRef = NULL,
      nBootstrapReps = 10,
      nConfLevel = 0.95,
      seed = 123
    ),
    "Using all 2 studies found in dfInput"
  )

  # Validate output
  expect_s3_class(result, "data.frame")
  expect_true("StudyID" %in% names(result))
  expect_equal(sort(unique(result$StudyID)), sort(c("STUDY1", "STUDY2")))
  expect_true(all(c("StudyMonth", "Median", "Lower", "Upper") %in% names(result)))
})

test_that("Analyze_StudyKRI_PredictBounds uses first column of dfStudyRef", {
  # Create test data
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 3),
    Numerator = sample(0:5, 90, replace = TRUE),
    Denominator = sample(10:20, 90, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202303, each = 10), times = 3),
    Metric = runif(90, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Create study reference with custom column names
  # First column should be used for target studies
  dfStudyRef <- data.frame(
    target_study = c("STUDY1", "STUDY2"),
    reference_study = c("STUDY3", "STUDY3"),
    stringsAsFactors = FALSE
  )

  # Should use first column (target_study) regardless of column name
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    dfStudyRef = dfStudyRef,
    nBootstrapReps = 10,
    nConfLevel = 0.95,
    seed = 456
  )

  # Should only include STUDY1 and STUDY2 (from first column)
  expect_s3_class(result, "data.frame")
  expect_equal(sort(unique(result$StudyID)), sort(c("STUDY1", "STUDY2")))
  expect_false("STUDY3" %in% result$StudyID)
})

test_that("Analyze_StudyKRI_PredictBounds validates dfStudyRef type", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = sample(0:5, 30, replace = TRUE),
    Denominator = sample(10:20, 30, replace = TRUE),
    MonthYYYYMM = rep(202301:202303, each = 10),
    Metric = runif(30, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # dfStudyRef must be data.frame or NULL
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      dfStudyRef = list(study = "STUDY1"),
      nBootstrapReps = 10
    ),
    "dfStudyRef must be a data.frame, tbl object, or NULL"
  )

  # dfStudyRef must have at least one column
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      dfStudyRef = data.frame(),
      nBootstrapReps = 10
    ),
    "dfStudyRef must have at least one column"
  )
})

test_that("Analyze_StudyKRI_PredictBounds handles empty studies in dfStudyRef", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = sample(0:5, 30, replace = TRUE),
    Denominator = sample(10:20, 30, replace = TRUE),
    MonthYYYYMM = rep(202301:202303, each = 10),
    Metric = runif(30, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Empty first column should error
  dfStudyRef <- data.frame(
    study = character(0),
    stringsAsFactors = FALSE
  )

  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      dfStudyRef = dfStudyRef,
      nBootstrapReps = 10
    ),
    "No target studies found in dfStudyRef"
  )
})

test_that("Analyze_StudyKRI_PredictBounds with dfStudyRef filters correctly", {
  # Create data with 3 studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 3),
    Numerator = sample(0:5, 90, replace = TRUE),
    Denominator = sample(10:20, 90, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202303, each = 10), times = 3),
    Metric = runif(90, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Only target STUDY1
  dfStudyRef <- data.frame(
    study = "STUDY1",
    studyref = "STUDY2",
    stringsAsFactors = FALSE
  )

  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    dfStudyRef = dfStudyRef,
    nBootstrapReps = 10,
    nConfLevel = 0.95,
    seed = 789
  )

  # Should only include STUDY1
  expect_equal(unique(result$StudyID), "STUDY1")
  expect_false("STUDY2" %in% result$StudyID)
  expect_false("STUDY3" %in% result$StudyID)
})

test_that("CalculateStudyBounds errors when no Metric column found", {
  CalculateStudyBounds <- gsm.studykri:::CalculateStudyBounds
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 10),
    StudyMonth = rep(1, 10),
    BootstrapRep = 1:10,
    SomeOtherColumn = runif(10)
  )
  expect_error(
    CalculateStudyBounds(dfTest, vBy = "StudyID"),
    "dfInput must have at least one Metric column"
  )
})

# Test vDbIntRandomRange parameter
test_that("Analyze_StudyKRI_PredictBounds works with vDbIntRandomRange parameter", {
  # Create minimal test data
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    GroupID = rep(paste0("Site", 1:3), each = 4),
    Numerator = sample(0:5, 12, replace = TRUE),
    Denominator = sample(10:20, 12, replace = TRUE),
    MonthYYYYMM = rep(202301:202302, each = 6),
    Metric = runif(12, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Test with Snowflake range
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    nBootstrapReps = 10,
    nConfLevel = 0.95,
    seed = 123,
    vDbIntRandomRange = c(-9223372036854775808, 9223372036854775807)
  )

  # Verify function completes without error and returns expected structure
  expect_s3_class(result, "data.frame")
  expect_true("StudyID" %in% colnames(result))
  expect_true("StudyMonth" %in% colnames(result))
  expect_true(any(grepl("^Median", colnames(result))))
  expect_true(any(grepl("^Lower", colnames(result))))
  expect_true(any(grepl("^Upper", colnames(result))))
  expect_equal(unique(result$StudyID), "STUDY1")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBounds handles character vDbIntRandomRange from YAML", {
  # Create minimal test data
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    GroupID = rep(paste0("Site", 1:3), each = 4),
    Numerator = sample(0:5, 12, replace = TRUE),
    Denominator = sample(10:20, 12, replace = TRUE),
    MonthYYYYMM = rep(202301:202302, each = 6),
    Metric = runif(12, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Test with character vector (as YAML would provide)
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfTest,
    nBootstrapReps = 10,
    seed = 123,
    vDbIntRandomRange = c("-9223372036854775808", "9223372036854775807")
  )

  # Verify function completes and returns valid results
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBounds rejects invalid character values", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    GroupID = rep(paste0("Site", 1:3), each = 4),
    Numerator = sample(0:5, 12, replace = TRUE),
    Denominator = sample(10:20, 12, replace = TRUE),
    MonthYYYYMM = rep(202301:202302, each = 6),
    Metric = runif(12, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Test with non-numeric character values
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      vDbIntRandomRange = c("invalid", "values")
    ),
    "non-numeric character values"
  )
})

test_that("Analyze_StudyKRI_PredictBounds rejects character vector with wrong length", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    GroupID = rep(paste0("Site", 1:3), each = 4),
    Numerator = sample(0:5, 12, replace = TRUE),
    Denominator = sample(10:20, 12, replace = TRUE),
    MonthYYYYMM = rep(202301:202302, each = 6),
    Metric = runif(12, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Test with single numeric string (wrong length)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      vDbIntRandomRange = c("100")
    ),
    "vDbIntRandomRange must be NULL or a numeric vector of length 2"
  )

  # Test with three numeric strings (wrong length)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      vDbIntRandomRange = c("1", "2", "3")
    ),
    "vDbIntRandomRange must be NULL or a numeric vector of length 2"
  )
})

test_that("Analyze_StudyKRI_PredictBounds rejects NA values after conversion", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 12),
    GroupID = rep(paste0("Site", 1:3), each = 4),
    Numerator = sample(0:5, 12, replace = TRUE),
    Denominator = sample(10:20, 12, replace = TRUE),
    MonthYYYYMM = rep(202301:202302, each = 6),
    Metric = runif(12, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Test with one valid and one invalid numeric string
  # as.numeric(c("100", "abc")) produces c(100, NA)
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      vDbIntRandomRange = c("100", "not_a_number")
    ),
    "contains NA values after conversion|non-numeric character values"
  )

  # Test with numeric vector containing NA
  expect_error(
    Analyze_StudyKRI_PredictBounds(
      dfInput = dfTest,
      vDbIntRandomRange = c(100, NA)
    ),
    "contains NA values after conversion"
  )
})
