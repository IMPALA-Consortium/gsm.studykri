test_that("Analyze_StudyKRI_PredictBoundsRefSet calculates bounds for multiple studies", {
  # Create site-level data for 3 studies with different site counts
  dfTest <- data.frame(
    StudyID = c(
      rep("STUDY1", 60), # 10 sites * 6 months
      rep("STUDY2", 48), # 8 sites * 6 months
      rep("STUDY3", 72) # 12 sites * 6 months
    ),
    GroupID = c(
      rep(paste0("S1_Site", 1:10), each = 6),
      rep(paste0("S2_Site", 1:8), each = 6),
      rep(paste0("S3_Site", 1:12), each = 6)
    ),
    Numerator = c(
      rep(1:6, times = 10), # Cumulative: 1, 2, 3, 4, 5, 6
      rep(1:6, times = 8),
      rep(1:6, times = 12)
    ),
    Denominator = c(
      rep(10:15, times = 10), # Cumulative denominators
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

  result <- Analyze_StudyKRI_PredictBoundsRefSet(
    dfInput = dfTest,
    vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
    nBootstrapReps = 50, # Small number for speed
    nConfLevel = 0.95,
  )

  # Verify structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "StudyMonth", "Median", "Lower", "Upper",
    "BootstrapCount", "GroupCount", "StudyCount"
  ) %in% names(result)))

  # Verify no StudyID column (intentionally combined)
  expect_false("StudyID" %in% names(result))

  # Verify metadata
  expect_equal(unique(result$GroupCount), 8) # Minimum across 10, 8, 12
  expect_equal(unique(result$StudyCount), 3)

  # Verify confidence intervals are sensible
  expect_true(all(result$Lower <= result$Median))
  expect_true(all(result$Median <= result$Upper))

  # Verify sequential StudyMonth
  expect_equal(result$StudyMonth, seq_len(nrow(result)))

  # Verify bootstrap count
  expect_true(all(result$BootstrapCount > 0))
  expect_true(all(result$BootstrapCount <= 50))
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet works with single study", {
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

  result <- Analyze_StudyKRI_PredictBoundsRefSet(
    dfInput = dfTest,
    vStudyFilter = "STUDY1",
    nBootstrapReps = 20,
    nConfLevel = 0.95,
  )

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$StudyCount), 1)
  expect_equal(unique(result$GroupCount), 5)
  expect_false("StudyID" %in% names(result))
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet uses minimum group count", {
  # Create data with very different site counts
  dfTest <- data.frame(
    StudyID = c(
      rep("STUDY_SMALL", 12), # 2 sites * 6 months
      rep("STUDY_LARGE", 60) # 10 sites * 6 months
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
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY_SMALL", "STUDY_LARGE"),
      nBootstrapReps = 20,
      nConfLevel = 0.95,
    )
  })

  # Should use minimum (2) not maximum (10)
  expect_equal(unique(result$GroupCount), 2)
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet validates input", {
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
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = list(a = 1),
      vStudyFilter = "STUDY1"
    ),
    "dfInput must be a data.frame or tbl object"
  )

  # Missing columns
  dfBad <- dfTest[, setdiff(names(dfTest), "Numerator")]
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfBad,
      vStudyFilter = "STUDY1"
    ),
    "dfInput must have at least one Numerator column"
  )

  # Empty vStudyFilter
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = character(0)
    ),
    "vStudyFilter must be a non-empty character vector"
  )

  # Invalid nBootstrapReps
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = "STUDY1",
      nBootstrapReps = -10
    ),
    "nBootstrapReps must be a positive integer"
  )

  # Invalid nConfLevel
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = "STUDY1",
      nConfLevel = 1.5
    ),
    "nConfLevel must be between 0 and 1"
  )

  # No matching studies
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = "NONEXISTENT_STUDY"
    ),
    "No data found for specified studies in vStudyFilter"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet validates required columns", {
  dfTest <- data.frame(
    StudyID = "STUDY1",
    GroupID = "Site1",
    Numerator = 1,
    Denominator = 10,
    MonthYYYYMM = 202301
  )

  # Missing Denominator
  dfBad <- dfTest[, setdiff(names(dfTest), "Denominator")]
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfBad, vStudyFilter = "STUDY1"),
    "dfInput missing required columns.*Denominator"
  )

  # Missing multiple columns
  dfBad <- dfTest[, c("Numerator", "Denominator")]
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfBad, vStudyFilter = "STUDY1"),
    "dfInput missing required columns"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet integration with full workflow", {
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
    studyid = rep(rep(c("STUDY1", "STUDY2", "STUDY3"), each = 20), each = 3),
    subjid = rep(paste0("SUBJ", 1:60), each = 3),
    aest_dt = as.Date("2023-01-01") + sample(0:150, 180, replace = TRUE),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    studyid = rep(rep(c("STUDY1", "STUDY2", "STUDY3"), each = 20), each = 3),
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
    dfGroupBounds <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfSiteLevel,
      vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
      nBootstrapReps = 30, # Small for speed
      nConfLevel = 0.95,
    )
  })

  # Verify output
  expect_s3_class(dfGroupBounds, "data.frame")
  expect_true(nrow(dfGroupBounds) > 0)
  expect_true(all(c(
    "StudyMonth", "Median", "Lower", "Upper",
    "BootstrapCount", "GroupCount", "StudyCount"
  ) %in% names(dfGroupBounds)))
  expect_equal(unique(dfGroupBounds$StudyCount), 3)
  expect_false("StudyID" %in% names(dfGroupBounds))

  # Verify bounds are logical
  expect_true(all(dfGroupBounds$Lower <= dfGroupBounds$Median, na.rm = TRUE))
  expect_true(all(dfGroupBounds$Median <= dfGroupBounds$Upper, na.rm = TRUE))
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet output has correct column types", {
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
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 30,
      nConfLevel = 0.95,
    )
  })

  expect_type(result$StudyMonth, "integer")
  expect_type(result$Median, "double")
  expect_type(result$Lower, "double")
  expect_type(result$Upper, "double")
  expect_type(result$BootstrapCount, "integer")
  expect_type(result$GroupCount, "integer")
  expect_type(result$StudyCount, "integer")
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet filters studies correctly", {
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
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"), # Only use 2 of 3 studies
      nBootstrapReps = 20,
      nConfLevel = 0.95,
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(unique(result$StudyCount), 2) # Should only use filtered studies
  expect_false("StudyID" %in% names(result)) # Studies are combined
})

test_that("Analyze_StudyKRI_PredictBoundsRef wrapper works with study mapping", {
  # Create study reference mapping
  dfStudyRef <- data.frame(
    study = c(rep("AA-1", 3), rep("AA-2", 2)),
    studyref = c("AA-3", "AA-4", "AA-5", "AA-3", "AA-4")
  )

  # Create site-level data
  dfTest <- data.frame(
    StudyID = rep(c("AA-1", "AA-2", "AA-3", "AA-4", "AA-5"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 5),
    Numerator = rep(1:6, times = 25),
    Denominator = rep(10:15, times = 25),
    MonthYYYYMM = rep(202301:202306, times = 25),
    Metric = runif(150, 0.05, 0.5),
    GroupLevel = "Site"
  )

  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = dfStudyRef,
      nBootstrapReps = 20,
    )
  })

  # Verify structure
  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "StudyID", "StudyRefID", "StudyMonth", "Median",
    "Lower", "Upper"
  ) %in% names(result)))

  # Verify unique studies
  expect_equal(sort(unique(result$StudyID)), sort(c("AA-1", "AA-2")))

  # Verify StudyRefID is collapsed
  ref_aa1 <- unique(result$StudyRefID[result$StudyID == "AA-1"])
  expect_equal(ref_aa1, "AA-3, AA-4, AA-5")

  ref_aa2 <- unique(result$StudyRefID[result$StudyID == "AA-2"])
  expect_equal(ref_aa2, "AA-3, AA-4")
})

test_that("Analyze_StudyKRI_PredictBoundsRef validates input", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = rep(1:6, times = 5),
    Denominator = rep(10:15, times = 5),
    MonthYYYYMM = rep(202301:202306, times = 5),
    Metric = runif(30, 0.05, 0.5)
  )

  # Missing dfStudyRef
  expect_error(
    Analyze_StudyKRI_PredictBoundsRef(dfInput = dfTest),
    "argument \"dfStudyRef\" is missing"
  )

  # Wrong dfStudyRef type
  expect_error(
    Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = list(a = 1)
    ),
    "dfStudyRef must be a data.frame"
  )

  # dfStudyRef with only 1 column (needs at least 2)
  dfBadRef <- data.frame(wrongcol = "A")
  expect_error(
    Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = dfBadRef
    ),
    "dfStudyRef must have at least 2 columns"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsRef works with any column names", {
  # Create study reference mapping with custom column names
  # Function should use first column for target, second for reference regardless of names
  dfStudyRef <- data.frame(
    target_study = c("STUDY1", "STUDY2"),
    ref_study = c("REF1", "REF2")
  )

  # Create site-level data
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "REF1", "REF2"), each = 30),
    GroupID = rep(paste0("Site", 1:5), each = 6, times = 4),
    Numerator = rep(1:6, times = 20),
    Denominator = rep(10:15, times = 20),
    MonthYYYYMM = rep(202301:202306, times = 20),
    Metric = runif(120, 0.05, 0.5),
    GroupLevel = "Site"
  )

  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = dfStudyRef,
      nBootstrapReps = 20,
    )
  })

  expect_s3_class(result, "data.frame")
  expect_true("StudyID" %in% names(result))
  expect_true("StudyRefID" %in% names(result))
  # Should work with any column names - uses first col for target, second for ref
  expect_equal(sort(unique(result$StudyID)), sort(c("STUDY1", "STUDY2")))
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet with NULL vStudyFilter uses all studies", {
  # Create data with multiple studies
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

  # Capture the message about using all studies
  expect_message(
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = NULL,
      nBootstrapReps = 20,
      nConfLevel = 0.95,
    ),
    "No vStudyFilter specified. Using all 3 studies."
  )

  expect_s3_class(result, "data.frame")
  expect_equal(unique(result$StudyCount), 3)
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet validates vStudyFilter type", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = rep(1:6, times = 5),
    Denominator = rep(10:15, times = 5),
    MonthYYYYMM = rep(202301:202306, times = 5),
    Metric = runif(30, 0.05, 0.5),
    stringsAsFactors = FALSE
  )

  # Non-character vStudyFilter
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = 123
    ),
    "vStudyFilter must be a non-empty character vector"
  )

  # NA character - will fail on "No data found" which is also acceptable
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c(NA_character_)
    ),
    "No data found for specified studies in vStudyFilter"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet validates all numeric parameters", {
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 30),
    GroupID = rep(paste0("Site", 1:5), each = 6),
    Numerator = rep(1:6, times = 5),
    Denominator = rep(10:15, times = 5),
    MonthYYYYMM = rep(202301:202306, times = 5),
    Metric = runif(30, 0.05, 0.5),
    stringsAsFactors = FALSE
  )

  # nBootstrapReps validations
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nBootstrapReps = 0),
    "nBootstrapReps must be a positive integer"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nBootstrapReps = "ten"),
    "nBootstrapReps must be a positive integer"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nBootstrapReps = c(10, 20)),
    "nBootstrapReps must be a positive integer"
  )

  # nConfLevel validations
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nConfLevel = 0),
    "nConfLevel must be between 0 and 1"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nConfLevel = 1),
    "nConfLevel must be between 0 and 1"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nConfLevel = -0.5),
    "nConfLevel must be between 0 and 1"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nConfLevel = "0.95"),
    "nConfLevel must be between 0 and 1"
  )
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(dfInput = dfTest, vStudyFilter = "STUDY1", nConfLevel = c(0.9, 0.95)),
    "nConfLevel must be between 0 and 1"
  )
})

# Test vDbIntRandomRange parameter
test_that("Analyze_StudyKRI_PredictBoundsRef works with vDbIntRandomRange parameter", {
  # Create minimal test data
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "REF1", "REF2"), each = 12),
    GroupID = rep(paste0("Site", 1:3), each = 4, times = 3),
    Numerator = sample(0:5, 36, replace = TRUE),
    Denominator = sample(10:20, 36, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 3),
    Metric = runif(36, 0.1, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Create study reference mapping
  dfStudyRef <- data.frame(
    study = "STUDY1",
    studyref = c("REF1", "REF2"),
    stringsAsFactors = FALSE
  )

  # Test with Snowflake range
  result <- Analyze_StudyKRI_PredictBoundsRef(
    dfInput = dfTest,
    dfStudyRef = dfStudyRef,
    nBootstrapReps = 10,
    nConfLevel = 0.95,
    vDbIntRandomRange = c(-9223372036854775808, 9223372036854775807)
  )

  # Verify function completes without error and returns expected structure
  expect_s3_class(result, "data.frame")
  expect_true("StudyID" %in% colnames(result))
  expect_true("StudyRefID" %in% colnames(result))
  expect_true("StudyMonth" %in% colnames(result))
  expect_true(any(grepl("^Median", colnames(result))))
  expect_true(any(grepl("^Lower", colnames(result))))
  expect_true(any(grepl("^Upper", colnames(result))))
  expect_equal(unique(result$StudyID), "STUDY1")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet validates nMinGroups parameter", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 12),
    GroupID = rep(paste0("Site", 1:3), each = 4, times = 2),
    Numerator = sample(0:5, 24, replace = TRUE),
    Denominator = sample(10:20, 24, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 2),
    Metric = runif(24, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Test with non-numeric nMinGroups
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nMinGroups = "invalid"
    ),
    "nMinGroups must be a single positive integer"
  )

  # Test with vector length > 1
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nMinGroups = c(2, 3)
    ),
    "nMinGroups must be a single positive integer"
  )

  # Test with zero
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nMinGroups = 0
    ),
    "nMinGroups must be a single positive integer"
  )

  # Test with negative value
  expect_error(
    Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nMinGroups = -5
    ),
    "nMinGroups must be a single positive integer"
  )
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet uses provided nMinGroups", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2"), each = 12),
    GroupID = rep(paste0("Site", 1:3), each = 4, times = 2),
    Numerator = sample(0:5, 24, replace = TRUE),
    Denominator = sample(10:20, 24, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 2),
    Metric = runif(24, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Test with valid nMinGroups - should succeed and use provided value
  expect_message(
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfTest,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 10,
      nMinGroups = 2,
    ),
    "Using provided minimum group count: 2"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBoundsRef uses MinGroups column from StudyRef", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "REF1", "REF2"), each = 12),
    GroupID = rep(paste0("Site", 1:4), each = 3, times = 3),
    Numerator = sample(0:5, 36, replace = TRUE),
    Denominator = sample(10:20, 36, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 3),
    Metric = runif(36, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Create study reference mapping WITH MinGroups column
  dfStudyRef <- data.frame(
    study = "STUDY1",
    studyref = c("REF1", "REF2"),
    MinGroups = 3 # Pre-calculated value
  )

  # Should use provided MinGroups and not calculate
  expect_message(
    result <- Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = dfStudyRef,
      nBootstrapReps = 10,
    ),
    "Using provided minimum group count: 3"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBoundsRef uses custom MinGroups column name", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "REF1", "REF2"), each = 12),
    GroupID = rep(paste0("Site", 1:4), each = 3, times = 3),
    Numerator = sample(0:5, 36, replace = TRUE),
    Denominator = sample(10:20, 36, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 3),
    Metric = runif(36, 0.1, 0.5),
    GroupLevel = "Site"
  )

  # Create study reference mapping with CUSTOM column name
  dfStudyRef <- data.frame(
    study = "STUDY1",
    studyref = c("REF1", "REF2"),
    custom_min_grps = 2
  )

  # Should use custom column via strMinGroupsCol parameter
  expect_message(
    result <- Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfTest,
      dfStudyRef = dfStudyRef,
      strMinGroupsCol = "custom_min_grps",
      nBootstrapReps = 10,
    ),
    "Using provided minimum group count: 2"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet works with bMixStudies = TRUE", {
  # Create site-level data for 3 studies
  dfTest <- data.frame(
    StudyID = c(
      rep("STUDY1", 60),
      rep("STUDY2", 48),
      rep("STUDY3", 72)
    ),
    GroupID = c(
      rep(paste0("S1_Site", 1:10), each = 6),
      rep(paste0("S2_Site", 1:8), each = 6),
      rep(paste0("S3_Site", 1:12), each = 6)
    ),
    Numerator = c(
      rep(1:6, times = 10),
      rep(1:6, times = 8),
      rep(1:6, times = 12)
    ),
    Denominator = c(
      rep(10:15, times = 10),
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

  result <- Analyze_StudyKRI_PredictBoundsRefSet(
    dfInput = dfTest,
    vStudyFilter = c("STUDY1", "STUDY2", "STUDY3"),
    nBootstrapReps = 50,
    nConfLevel = 0.95,
    bMixStudies = TRUE,
  )

  # Verify structure (same as FALSE mode)
  expect_s3_class(result, "data.frame")
  expect_true(all(c(
    "StudyMonth", "Median", "Lower", "Upper",
    "BootstrapCount", "GroupCount", "StudyCount"
  ) %in% names(result)))

  # Verify no StudyID column
  expect_false("StudyID" %in% names(result))

  # Verify metadata
  expect_equal(unique(result$GroupCount), 8)
  expect_equal(unique(result$StudyCount), 3)

  # Verify confidence intervals
  expect_true(all(result$Lower <= result$Median))
  expect_true(all(result$Median <= result$Upper))
})

test_that("Analyze_StudyKRI_PredictBoundsRefSet bMixStudies modes both work", {
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

  # Run both modes
  result_mixed <- Analyze_StudyKRI_PredictBoundsRefSet(
    dfInput = dfTest,
    vStudyFilter = c("STUDY1", "STUDY2"),
    nBootstrapReps = 30,
    bMixStudies = TRUE,
  )

  result_separated <- Analyze_StudyKRI_PredictBoundsRefSet(
    dfInput = dfTest,
    vStudyFilter = c("STUDY1", "STUDY2"),
    nBootstrapReps = 30,
    bMixStudies = FALSE,
  )

  # Both should return valid results
  expect_s3_class(result_mixed, "data.frame")
  expect_s3_class(result_separated, "data.frame")

  # Both should have the same columns
  expect_equal(sort(names(result_mixed)), sort(names(result_separated)))

  # Both should have same metadata
  expect_equal(result_mixed$GroupCount, result_separated$GroupCount)
  expect_equal(result_mixed$StudyCount, result_separated$StudyCount)

  # Note: Actual values may differ due to different aggregation logic
  # We're just verifying both modes execute successfully
})

test_that("Analyze_StudyKRI_PredictBoundsRef passes through bMixStudies", {
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "REF1", "REF2"), each = 12),
    GroupID = rep(paste0("Site", 1:4), each = 3, times = 3),
    Numerator = sample(0:5, 36, replace = TRUE),
    Denominator = sample(10:20, 36, replace = TRUE),
    MonthYYYYMM = rep(rep(202301:202302, each = 6), times = 3),
    Metric = runif(36, 0.1, 0.5),
    GroupLevel = "Site"
  )

  dfStudyRef <- data.frame(
    study = "STUDY1",
    studyref = c("REF1", "REF2")
  )

  result <- Analyze_StudyKRI_PredictBoundsRef(
    dfInput = dfTest,
    dfStudyRef = dfStudyRef,
    nBootstrapReps = 20,
    bMixStudies = TRUE,
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("StudyID" %in% names(result))
})

# Lazy table tests have been moved to test-dbplyr-compatibility.R
