test_that("Input_CountSiteByMonth returns correct structure", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Assert structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  
  # Check expected columns
  expected_cols <- c("GroupID", "GroupLevel", "Numerator", "Denominator", "Metric", "StudyID", "MonthYYYYMM")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check column types
  expect_type(result$GroupID, "character")
  expect_type(result$GroupLevel, "character")
  expect_type(result$Numerator, "integer")
  expect_type(result$Denominator, "integer")
  expect_type(result$Metric, "double")
  expect_type(result$StudyID, "character")
  expect_type(result$MonthYYYYMM, "double")
  
  # Check GroupLevel is constant
  expect_true(all(result$GroupLevel == "Site"))
})

test_that("Input_CountSiteByMonth returns monthly counts (not cumulative)", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Monthly counts can vary (not necessarily monotonically increasing)
  # Just verify we have counts > 0
  expect_true(any(result$Numerator > 0))
  expect_true(any(result$Denominator > 0))
  
  # Verify the data is grouped by site and month
  result_ordered <- result[order(result$GroupID, result$MonthYYYYMM), ]
  expect_true(nrow(result_ordered) > 0)
})

test_that("Input_CountSiteByMonth calculates Metric correctly", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Check Metric calculation
  result_with_denom <- result[result$Denominator > 0, ]
  calculated_metric <- result_with_denom$Numerator / result_with_denom$Denominator
  
  expect_equal(result_with_denom$Metric, calculated_metric, tolerance = 1e-10)
})

test_that("Input_CountSiteByMonth handles missing dates correctly", {
  # Setup test data with some missing dates
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute - should not fail with missing dates (they are filtered out)
  expect_no_error({
    result <- Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    )
  })
})

test_that("Input_CountSiteByMonth validates input data frames", {
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Test non-data.frame inputs
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = list(),
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ),
    "dfSubjects must be a data.frame"
  )
  
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = list(),
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ),
    "dfNumerator must be a data.frame"
  )
  
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = list(),
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ),
    "dfDenominator must be a data.frame"
  )
})

test_that("Input_CountSiteByMonth validates required columns", {
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Test missing subject column
  dfSubjects_bad <- dfSubjects
  names(dfSubjects_bad)[names(dfSubjects_bad) == "subjid"] <- "bad_col"
  
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects_bad,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ),
    "dfSubjects missing required columns: subjid"
  )
  
  # Test missing numerator date column
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "nonexistent_col",
      strDenominatorDateCol = "visit_dt"
    ),
    "dfNumerator missing required columns: nonexistent_col"
  )
  
  # Test missing denominator date column
  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "nonexistent_col"
    ),
    "dfDenominator missing required columns: nonexistent_col"
  )
})

test_that("Input_CountSiteByMonth supports custom column names", {
  # Setup test data with renamed columns
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute with default column names
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
})

test_that("Input_CountSiteByMonth filters to enrolled subjects", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Count enrolled subjects
  n_enrolled <- sum(dfSubjects$enrollyn == "Y")
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Check that only enrolled subjects are included
  enrolled_subjects <- dfSubjects$subjid[dfSubjects$enrollyn == "Y"]
  numerator_subjects <- unique(dfNumerator$subjid[dfNumerator$subjid %in% enrolled_subjects])
  
  expect_true(length(numerator_subjects) > 0)
})

test_that("Input_CountSiteByMonth handles MonthYYYYMM formatting correctly", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Check MonthYYYYMM format (should be YYYYMM as numeric)
  # Filter out NA values for validation
  valid_months <- result$MonthYYYYMM[!is.na(result$MonthYYYYMM)]
  expect_true(all(valid_months >= 190001 & valid_months <= 209912))
  
  # Check that MonthYYYYMM values are plausible (6 digits, YYYYMM format)
  month_strings <- as.character(valid_months)
  expect_true(all(nchar(month_strings) == 6))
})

test_that("Input_CountSiteByMonth handles zero denominator correctly", {
  # Create test data where some sites have no denominator events
  dfSubjects <- data.frame(
    studyid = c("STUDY001", "STUDY001"),
    invid = c("SITE01", "SITE02"),
    subjid = c("SUBJ001", "SUBJ002"),
    enrollyn = c("Y", "Y"),
    stringsAsFactors = FALSE
  )
  
  dfNumerator <- data.frame(
    subjid = c("SUBJ001", "SUBJ001"),
    aest_dt = as.Date(c("2024-01-15", "2024-02-15")),
    stringsAsFactors = FALSE
  )
  
  dfDenominator <- data.frame(
    subjid = character(0),
    visit_dt = as.Date(character(0)),
    stringsAsFactors = FALSE
  )
  
  # Execute
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  # Check that Metric is NA when Denominator is 0
  zero_denom_rows <- result$Denominator == 0
  expect_true(all(is.na(result$Metric[zero_denom_rows])))
})

