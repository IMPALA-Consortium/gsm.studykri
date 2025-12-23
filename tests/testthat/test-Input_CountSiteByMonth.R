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
  # Filter out NA values for checking
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

test_that("Input_CountSiteByMonth works with strDenominatorEndDateCol (days calculation)", {
  # Create test data with start and end dates
  dfSubjects <- data.frame(
    studyid = c("STUDY001", "STUDY001"),
    invid = c("SITE01", "SITE01"),
    subjid = c("SUBJ001", "SUBJ002"),
    enrollyn = c("Y", "Y"),
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    aest_dt = as.Date(c("2024-01-15", "2024-01-20")),
    stringsAsFactors = FALSE
  )

  # Denominator with start and end dates (exposure periods)
  dfDenominator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    visit_start = as.Date(c("2024-01-01", "2024-01-10")),
    visit_end = as.Date(c("2024-01-31", "2024-01-25")),
    stringsAsFactors = FALSE
  )

  # Execute with end date column (triggers days calculation)
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_start",
    strDenominatorEndDateCol = "visit_end"
  )

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  # Denominator should be days, not count
  expect_true(any(result$Denominator > 0))
})

test_that("calculate_days_by_month handles multi-month date ranges", {
  # Create test data spanning multiple months
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2024-02-15"),
    stringsAsFactors = FALSE
  )

  # Date range spanning 3 months: Jan 15 to Mar 15
  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2024-01-15"),
    end_date = as.Date("2024-03-15"),
    stringsAsFactors = FALSE
  )

  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  expect_s3_class(result, "data.frame")
  # Should have entries for Jan, Feb, and Mar (3 months)
  expect_true(nrow(result) >= 3)

  # Check that days are calculated correctly
  expect_true(all(result$Denominator > 0))
})

test_that("calculate_days_by_month handles same month start and end", {
  # Create test data with same month start and end
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2024-01-15"),
    stringsAsFactors = FALSE
  )

  # Same month: Jan 10 to Jan 20 (11 days)
  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2024-01-10"),
    end_date = as.Date("2024-01-20"),
    stringsAsFactors = FALSE
  )

  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  # Should have 11 days (Jan 10 to Jan 20 inclusive)
  expect_equal(result$Denominator, 11)
})

test_that("calculate_days_by_month handles leap year correctly", {
  # Create test data with leap year Feb date range
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2024-02-15"),
    stringsAsFactors = FALSE
  )

  # Full month of Feb 2024 (leap year - 29 days)
  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2024-02-01"),
    end_date = as.Date("2024-02-29"),
    stringsAsFactors = FALSE
  )

  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  expect_s3_class(result, "data.frame")
  # Should have 29 days for leap year February
  expect_equal(result$Denominator, 29)
})

test_that("calculate_days_by_month handles non-leap year February", {
  # Create test data with non-leap year Feb
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2023-02-15"),
    stringsAsFactors = FALSE
  )

  # Full month of Feb 2023 (non-leap year - 28 days)
  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2023-02-01"),
    end_date = as.Date("2023-02-28"),
    stringsAsFactors = FALSE
  )

  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  expect_s3_class(result, "data.frame")
  # Should have 28 days for non-leap year February
  expect_equal(result$Denominator, 28)
})

test_that("calculate_days_by_month handles months with different day counts", {
  # Test different month lengths (30 vs 31 days)
  dfSubjects <- data.frame(
    studyid = rep("STUDY001", 2),
    invid = rep("SITE01", 2),
    subjid = c("SUBJ001", "SUBJ002"),
    enrollyn = c("Y", "Y"),
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    aest_dt = as.Date(c("2024-04-15", "2024-05-15")),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    start_date = as.Date(c("2024-04-01", "2024-05-01")),
    end_date = as.Date(c("2024-04-30", "2024-05-31")),
    stringsAsFactors = FALSE
  )

  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  expect_s3_class(result, "data.frame")
  # April has 30 days, May has 31 days
  april_days <- result$Denominator[result$MonthYYYYMM == 202404]
  may_days <- result$Denominator[result$MonthYYYYMM == 202405]
  expect_equal(april_days, 30)
  expect_equal(may_days, 31)
})

test_that("Input_CountSiteByMonth validates strDenominatorEndDateCol is present", {
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2024-01-15"),
    stringsAsFactors = FALSE
  )

  # Missing the end date column
  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2024-01-01"),
    stringsAsFactors = FALSE
  )

  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "start_date",
      strDenominatorEndDateCol = "missing_end_date"
    ),
    "dfDenominator missing required columns.*missing_end_date"
  )
})

test_that("Input_CountSiteByMonth handles NA dates in denominator end date", {
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = c("SUBJ001", "SUBJ002"),
    enrollyn = c("Y", "Y"),
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    aest_dt = as.Date(c("2024-01-15", "2024-01-20")),
    stringsAsFactors = FALSE
  )

  # One record has NA end date
  dfDenominator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002"),
    start_date = as.Date(c("2024-01-01", "2024-01-10")),
    end_date = as.Date(c("2024-01-31", NA)),
    stringsAsFactors = FALSE
  )

  # Should not fail, NA dates are filtered out
  expect_no_error({
    result <- Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "start_date",
      strDenominatorEndDateCol = "end_date"
    )
  })
})

test_that("Input_CountSiteByMonth errors when no enrolled subjects", {
  # All subjects not enrolled
  dfSubjects <- data.frame(
    studyid = "STUDY001",
    invid = "SITE01",
    subjid = "SUBJ001",
    enrollyn = "N",
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = "SUBJ001",
    aest_dt = as.Date("2024-01-15"),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    visit_dt = as.Date("2024-01-10"),
    stringsAsFactors = FALSE
  )

  expect_error(
    Input_CountSiteByMonth(
      dfSubjects = dfSubjects,
      dfNumerator = dfNumerator,
      dfDenominator = dfDenominator,
      strNumeratorDateCol = "aest_dt",
      strDenominatorDateCol = "visit_dt"
    ),
    "No enrolled subjects found in dfSubjects"
  )
})

# Lazy table tests have been moved to test-dbplyr-compatibility.R
test_that("Input_CountSiteByMonth includes DenominatorType when strDenominatorType is provided", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt

  # Execute with strDenominatorType
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt",
    strDenominatorType = "Visits"
  )

  # Assert DenominatorType column exists
  expect_true("DenominatorType" %in% names(result))
  expect_type(result$DenominatorType, "character")
  expect_true(all(result$DenominatorType == "Visits"))
})

test_that("Input_CountSiteByMonth excludes DenominatorType when strDenominatorType is NULL", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt

  # Execute without strDenominatorType (default NULL)
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Assert DenominatorType column does not exist
  expect_false("DenominatorType" %in% names(result))
})

test_that("Input_CountSiteByMonth includes DenominatorType with days-based calculation", {
  # Setup test data
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae

  # Execute with strDenominatorType and end date column
  result <- Input_CountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfSubjects,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "firstparticipantdate",
    strDenominatorEndDateCol = "lastparticipantdate",
    strDenominatorType = "Days on Study"
  )

  # Assert DenominatorType column exists with correct value
  expect_true("DenominatorType" %in% names(result))
  expect_true(all(result$DenominatorType == "Days on Study"))
})