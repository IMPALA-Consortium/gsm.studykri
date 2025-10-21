test_that("Analyze_StudyKRI_PredictBoundsGroup calculates bounds for multiple studies", {
  # Create site-level data for 3 studies with different site counts
  dfTest <- data.frame(
    StudyID = c(
      rep("STUDY1", 60),  # 10 sites * 6 months
      rep("STUDY2", 48),  # 8 sites * 6 months
      rep("STUDY3", 72)   # 12 sites * 6 months
    ),
    GroupID = c(
      rep(paste0("S1_Site", 1:10), each = 6),
      rep(paste0("S2_Site", 1:8), each = 6),
      rep(paste0("S3_Site", 1:12), each = 6)
    ),
    Numerator = c(
      rep(1:6, times = 10),  # Cumulative: 1, 2, 3, 4, 5, 6
      rep(1:6, times = 8),
      rep(1:6, times = 12)
    ),
    Denominator = c(
      rep(10:15, times = 10),  # Cumulative denominators
      rep(10:15, times = 8),
      rep(10:15, times = 12)
    ),
    MonthYYYYMM = c(
      rep(202301:202306, times = 10),
      rep(202301:202306, times = 8),
      rep(202301:202306, times = 12)
    ),
    Metric = runif(180, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  result <- Analyze_StudyKRI_PredictBoundsGroup(
    dfInput = dfTest,
    vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
    nBootstrapReps = 50,  # Small number for speed
    nConfLevel = 0.95,
    seed = 123
  )

  # Verify structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c("StudyMonth", "MedianMetric", "LowerBound", "UpperBound",
                    "BootstrapCount", "GroupCount", "StudyCount") %in% names(result)))

  # Verify no StudyID column (intentionally combined)
  expect_false("StudyID" %in% names(result))

  # Verify metadata
  expect_equal(unique(result$GroupCount), 8)  # Minimum across 10, 8, 12
  expect_equal(unique(result$StudyCount), 3)

  # Verify confidence intervals are sensible
  expect_true(all(result$LowerBound <= result$MedianMetric))
  expect_true(all(result$MedianMetric <= result$UpperBound))

  # Verify sequential StudyMonth
  expect_equal(result$StudyMonth, seq_len(nrow(result)))

  # Verify bootstrap count
  expect_true(all(result$BootstrapCount > 0))
  expect_true(all(result$BootstrapCount <= 50))
})

test_that("Analyze_StudyKRI_PredictBoundsGroup works with single study", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = rep(1:6, times = 5),
    Denominator = rep(10:15, times = 5),
    MonthYYYYMM = rep(202301:202306, times = 5),
    Metric = runif(30, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  result <- Analyze_StudyKRI_PredictBoundsGroup(
    dfInput = dfTest,
    vStudyFilter = "STUDY1",
    nBootstrapReps = 20,
    nConfLevel = 0.95,
    seed = 123
  )

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$StudyCount), 1)
  expect_equal(unique(result$GroupCount), 5)
  expect_false("StudyID" %in% names(result))
})

test_that("Analyze_StudyKRI_PredictBoundsGroup uses minimum group count", {
  # Create data with very different site counts
  dfTest <- data.frame(
    StudyID = c(
      rep("STUDY_SMALL", 12),  # 2 sites * 6 months
      rep("STUDY_LARGE", 60)   # 10 sites * 6 months
    ),
    GroupID = c(
      rep(paste0("Small_Site", 1:2), each = 6),
      rep(paste0("Large_Site", 1:10), each = 6)
    ),
    Numerator = rep(1:6, times = 12),
    Denominator = rep(10:15, times = 12),
    MonthYYYYMM = rep(202301:202306, times = 12),
    Metric = runif(72, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Suppress the informative message
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY_SMALL", "STUDY_LARGE"),
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      seed = 123
    )
  })

  # Should use minimum (2) not maximum (10)
  expect_equal(unique(result$GroupCount), 2)
})

test_that("Analyze_StudyKRI_PredictBoundsGroup validates input", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = rep(1:6, times = 5),
    Denominator = rep(10:15, times = 5),
    MonthYYYYMM = rep(202301:202306, times = 5),
    Metric = runif(30, 0.05, 0.5),
    stringsAsFactors = FALSE
  )

  # Wrong input type
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = list(a = 1),
      vStudyFilter = "STUDY1"
    ),
    "dfInput must be a data.frame or tbl object"
  )

  # Missing columns
  dfBad <- dfTest[, setdiff(names(dfTest), "Numerator")]
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfBad,
      vStudyFilter = "STUDY1"
    ),
    "dfInput missing required columns.*Numerator"
  )

  # Empty vStudyFilter
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = character(0)
    ),
    "vStudyFilter must be a non-empty character vector"
  )

  # Invalid nBootstrapReps
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = "STUDY1",
      nBootstrapReps = -10
    ),
    "nBootstrapReps must be a positive integer"
  )

  # Invalid nConfLevel
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = "STUDY1",
      nConfLevel = 1.5
    ),
    "nConfLevel must be between 0 and 1"
  )

  # No matching studies
  expect_error(
    Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = "NONEXISTENT_STUDY"
    ),
    "No data found for specified studies in vStudyFilter"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsGroup works with different confidence levels", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 2),
    Numerator = rep(1:6, times = 10),
    Denominator = rep(10:15, times = 10),
    MonthYYYYMM = rep(202301:202306, times = 10),
    Metric = runif(60, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  suppressMessages({
    result_90 <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 100,  # Increase for more stable CI estimates
      nConfLevel = 0.90,
      seed = 123
    )

    result_99 <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 100,  # Increase for more stable CI estimates
      nConfLevel = 0.99,
      seed = 123
    )
  })

  # Both should have same structure
  expect_equal(nrow(result_90), nrow(result_99))

  # 99% CI should be wider than or equal to 90% CI on average
  width_90 <- result_90$UpperBound - result_90$LowerBound
  width_99 <- result_99$UpperBound - result_99$LowerBound

  expect_true(mean(width_99, na.rm = TRUE) >= mean(width_90, na.rm = TRUE) * 0.95)
})

test_that("Analyze_StudyKRI_PredictBoundsGroup produces consistent results with seed", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 2),
    Numerator = rep(1:6, times = 10),
    Denominator = rep(10:15, times = 10),
    MonthYYYYMM = rep(202301:202306, times = 10),
    Metric = runif(60, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  suppressMessages({
    result1 <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 50,
      nConfLevel = 0.95,
      seed = 456
    )

    result2 <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 50,
      nConfLevel = 0.95,
      seed = 456
    )
  })

  # Results should be identical with same seed
  expect_equal(result1$MedianMetric, result2$MedianMetric)
  expect_equal(result1$LowerBound, result2$LowerBound)
  expect_equal(result1$UpperBound, result2$UpperBound)
})

test_that("Analyze_StudyKRI_PredictBoundsGroup integration with full workflow", {
  # Create realistic test data
  set.seed(789)
  dfSubjects <- data.frame(
    studyid = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 20),
    invid = rep(paste0("Site", 1:10), each = 2, times = 3),
    subjid = paste0("SUBJ", 1:60),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = rep(paste0("SUBJ", 1:60), each = 3),
    aest_dt = as.Date("2023-01-01") + sample(0:150, 180, replace = TRUE),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = rep(paste0("SUBJ", 1:60), each = 3),
    visit_dt = as.Date("2023-01-01") + sample(0:150, 180, replace = TRUE),
    stringsAsFactors = FALSE
  )

  # Run Input function
  dfSiteLevel <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Run group bounds function
  suppressMessages({
    dfGroupBounds <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfSiteLevel,
      vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
      nBootstrapReps = 30,  # Small for speed
      nConfLevel = 0.95,
      nMinDenominator = 5,  # Lower threshold for small test data
      seed = 789
    )
  })

  # Verify output
  expect_s3_class(dfGroupBounds, "data.frame")
  expect_true(nrow(dfGroupBounds) > 0)
  expect_true(all(c("StudyMonth", "MedianMetric", "LowerBound", "UpperBound",
                    "BootstrapCount", "GroupCount", "StudyCount") %in% names(dfGroupBounds)))
  expect_equal(unique(dfGroupBounds$StudyCount), 3)
  expect_false("StudyID" %in% names(dfGroupBounds))

  # Verify bounds are logical
  expect_true(all(dfGroupBounds$LowerBound <= dfGroupBounds$MedianMetric, na.rm = TRUE))
  expect_true(all(dfGroupBounds$MedianMetric <= dfGroupBounds$UpperBound, na.rm = TRUE))
})

test_that("Analyze_StudyKRI_PredictBoundsGroup output has correct column types", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 2),
    Numerator = rep(1:6, times = 10),
    Denominator = rep(10:15, times = 10),
    MonthYYYYMM = rep(202301:202306, times = 10),
    Metric = runif(60, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 30,
      nConfLevel = 0.95,
      seed = 999
    )
  })

  expect_type(result$StudyMonth, "integer")
  expect_type(result$MedianMetric, "double")
  expect_type(result$LowerBound, "double")
  expect_type(result$UpperBound, "double")
  expect_type(result$BootstrapCount, "integer")
  expect_type(result$GroupCount, "integer")
  expect_type(result$StudyCount, "integer")
})

test_that("Analyze_StudyKRI_PredictBoundsGroup filters studies correctly", {
  # Create data with 3 studies but only use 2
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 3),
    Numerator = rep(1:6, times = 15),
    Denominator = rep(10:15, times = 15),
    MonthYYYYMM = rep(202301:202306, times = 15),
    Metric = runif(90, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),  # Only use 2 of 3 studies
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      seed = 111
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(unique(result$StudyCount), 2)  # Should only use filtered studies
  expect_false("StudyID" %in% names(result))  # Studies are combined
})

