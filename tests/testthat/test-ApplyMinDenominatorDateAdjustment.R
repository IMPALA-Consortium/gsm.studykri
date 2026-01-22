# Tests for ApplyMinDenominatorDateAdjustment utility function

test_that("ApplyMinDenominatorDateAdjustment adjusts dates correctly with end dates", {
  # Create test data with start and end dates
  dfSubjects <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5"),
    studyid = rep("STUDY1", 5),
    invid = c("SITE1", "SITE1", "SITE2", "SITE2", "SITE2"),
    stringsAsFactors = FALSE
  )

  # Numerator events
  dfNumerator_processed <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5"),
    studyid = rep("STUDY1", 5),
    invid = c("SITE1", "SITE1", "SITE2", "SITE2", "SITE2"),
    event_dt = as.Date(c("2024-01-05", "2024-01-10", "2024-01-15", "2024-01-20", "2024-01-25")),
    MonthYYYYMM = c(202401, 202401, 202401, 202401, 202401),
    stringsAsFactors = FALSE
  )

  # Denominator with start AND end dates (e.g., study participation periods)
  dfDenominator <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5"),
    start_dt = as.Date(c("2024-01-01", "2024-01-03", "2024-01-05", "2024-01-10", "2024-01-15")),
    end_dt = as.Date(c("2024-02-01", "2024-02-05", "2024-02-10", "2024-02-15", "2024-02-20")),
    stringsAsFactors = FALSE
  )

  # Apply date adjustment with nMinDenominator = 3
  # This should adjust the first 3 denominator events to align at the 3rd event's start date (2024-01-05)
  result <- gsm.studykri:::ApplyMinDenominatorDateAdjustment(
    dfNumerator_processed = dfNumerator_processed,
    dfDenominator = dfDenominator,
    dfSubjects = dfSubjects,
    strSubjectCol = "subjid",
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strNumeratorDateCol = "event_dt",
    strDenominatorDateCol = "start_dt",
    strDenominatorEndDateCol = "end_dt",
    nMinDenominator = 3
  )

  # Verify structure
  expect_true(is.list(result))
  expect_true(all(c("dfNumerator", "dfDenominator") %in% names(result)))

  # Verify denominator adjustments
  dfDenom_result <- result$dfDenominator

  # First 3 events should have adjusted start dates (all moved to 2024-01-05)
  expect_equal(dfDenom_result$start_dt[1], as.Date("2024-01-05")) # Was 2024-01-01, moved forward 4 days
  expect_equal(dfDenom_result$start_dt[2], as.Date("2024-01-05")) # Was 2024-01-03, moved forward 2 days
  expect_equal(dfDenom_result$start_dt[3], as.Date("2024-01-05")) # Was 2024-01-05, no change

  # Events 4 and 5 should remain unchanged (after threshold)
  expect_equal(dfDenom_result$start_dt[4], as.Date("2024-01-10"))
  expect_equal(dfDenom_result$start_dt[5], as.Date("2024-01-15"))

  # Verify END dates are adjusted by the same offset (preserving duration)
  expect_equal(dfDenom_result$end_dt[1], as.Date("2024-02-05")) # Was 2024-02-01, moved forward 4 days
  expect_equal(dfDenom_result$end_dt[2], as.Date("2024-02-07")) # Was 2024-02-05, moved forward 2 days
  expect_equal(dfDenom_result$end_dt[3], as.Date("2024-02-10")) # Was 2024-02-10, no change
  expect_equal(dfDenom_result$end_dt[4], as.Date("2024-02-15")) # Unchanged
  expect_equal(dfDenom_result$end_dt[5], as.Date("2024-02-20")) # Unchanged

  # Verify durations are preserved
  duration_original <- as.numeric(dfDenominator$end_dt - dfDenominator$start_dt)
  duration_adjusted <- as.numeric(dfDenom_result$end_dt - dfDenom_result$start_dt)
  expect_equal(duration_adjusted, duration_original)

  # Verify numerator adjustments
  dfNum_result <- result$dfNumerator

  # Events on or before threshold (2024-01-05) should be adjusted
  expect_equal(dfNum_result$MonthYYYYMM[1], 202401) # Was 2024-01-05, stays in same month
  expect_equal(dfNum_result$MonthYYYYMM[2], 202401) # Was 2024-01-10, stays in same month

  # Check that MonthYYYYMM is recalculated for adjusted events
  expect_true(all(dfNum_result$MonthYYYYMM >= 202401))
})

test_that("ApplyMinDenominatorDateAdjustment works without end dates", {
  # Create test data WITHOUT end dates
  dfSubjects <- data.frame(
    subjid = c("S1", "S2", "S3"),
    studyid = rep("STUDY1", 3),
    invid = c("SITE1", "SITE1", "SITE2"),
    stringsAsFactors = FALSE
  )

  dfNumerator_processed <- data.frame(
    subjid = c("S1", "S2", "S3"),
    studyid = rep("STUDY1", 3),
    invid = c("SITE1", "SITE1", "SITE2"),
    event_dt = as.Date(c("2024-01-05", "2024-01-10", "2024-01-15")),
    MonthYYYYMM = c(202401, 202401, 202401),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = c("S1", "S2", "S3"),
    visit_dt = as.Date(c("2024-01-01", "2024-01-03", "2024-01-05")),
    stringsAsFactors = FALSE
  )

  # Apply without end date column
  result <- gsm.studykri:::ApplyMinDenominatorDateAdjustment(
    dfNumerator_processed = dfNumerator_processed,
    dfDenominator = dfDenominator,
    dfSubjects = dfSubjects,
    strSubjectCol = "subjid",
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strNumeratorDateCol = "event_dt",
    strDenominatorDateCol = "visit_dt",
    strDenominatorEndDateCol = NULL,
    nMinDenominator = 2
  )

  # Verify structure
  expect_true(is.list(result))
  expect_true(all(c("dfNumerator", "dfDenominator") %in% names(result)))

  # Verify denominator adjustments (first 2 events to 2024-01-03)
  dfDenom_result <- result$dfDenominator
  expect_equal(dfDenom_result$visit_dt[1], as.Date("2024-01-03")) # Moved forward 2 days
  expect_equal(dfDenom_result$visit_dt[2], as.Date("2024-01-03")) # No change
  expect_equal(dfDenom_result$visit_dt[3], as.Date("2024-01-05")) # Unchanged (after threshold)

  # Verify no end date column exists in result
  expect_false("end_dt" %in% names(dfDenom_result))
})

test_that("ApplyMinDenominatorDateAdjustment works with multiple studies", {
  # Test with multiple studies - each should have independent thresholds
  dfSubjects <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
    studyid = c("STUDY1", "STUDY1", "STUDY1", "STUDY2", "STUDY2", "STUDY2"),
    invid = c("SITE1", "SITE1", "SITE2", "SITE3", "SITE3", "SITE4"),
    stringsAsFactors = FALSE
  )

  dfNumerator_processed <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
    studyid = c("STUDY1", "STUDY1", "STUDY1", "STUDY2", "STUDY2", "STUDY2"),
    invid = c("SITE1", "SITE1", "SITE2", "SITE3", "SITE3", "SITE4"),
    event_dt = as.Date(c("2024-01-05", "2024-01-10", "2024-01-15", "2024-02-05", "2024-02-10", "2024-02-15")),
    MonthYYYYMM = c(202401, 202401, 202401, 202402, 202402, 202402),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
    start_dt = as.Date(c("2024-01-01", "2024-01-03", "2024-01-05", "2024-02-01", "2024-02-03", "2024-02-05")),
    end_dt = as.Date(c("2024-03-01", "2024-03-03", "2024-03-05", "2024-04-01", "2024-04-03", "2024-04-05")),
    stringsAsFactors = FALSE
  )

  # Apply with nMinDenominator = 2 (per study)
  result <- gsm.studykri:::ApplyMinDenominatorDateAdjustment(
    dfNumerator_processed = dfNumerator_processed,
    dfDenominator = dfDenominator,
    dfSubjects = dfSubjects,
    strSubjectCol = "subjid",
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strNumeratorDateCol = "event_dt",
    strDenominatorDateCol = "start_dt",
    strDenominatorEndDateCol = "end_dt",
    nMinDenominator = 2
  )

  dfDenom_result <- result$dfDenominator

  # STUDY1: First 2 events should align to 2024-01-03
  study1_rows <- dfDenom_result$studyid == "STUDY1"
  expect_equal(dfDenom_result$start_dt[study1_rows][1], as.Date("2024-01-03"))
  expect_equal(dfDenom_result$start_dt[study1_rows][2], as.Date("2024-01-03"))
  expect_equal(dfDenom_result$start_dt[study1_rows][3], as.Date("2024-01-05")) # After threshold

  # STUDY2: First 2 events should align to 2024-02-03
  study2_rows <- dfDenom_result$studyid == "STUDY2"
  expect_equal(dfDenom_result$start_dt[study2_rows][1], as.Date("2024-02-03"))
  expect_equal(dfDenom_result$start_dt[study2_rows][2], as.Date("2024-02-03"))
  expect_equal(dfDenom_result$start_dt[study2_rows][3], as.Date("2024-02-05")) # After threshold

  # Verify end dates are adjusted independently per study
  expect_equal(dfDenom_result$end_dt[study1_rows][1], as.Date("2024-03-03")) # +2 days
  expect_equal(dfDenom_result$end_dt[study2_rows][1], as.Date("2024-04-03")) # +2 days
})

test_that("ApplyMinDenominatorDateAdjustment works with lazy tables (dbplyr)", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")

  # Create test data
  dfSubjects <- data.frame(
    subjid = c("S1", "S2", "S3"),
    studyid = rep("STUDY1", 3),
    invid = c("SITE1", "SITE1", "SITE2"),
    stringsAsFactors = FALSE
  )

  dfNumerator_processed <- data.frame(
    subjid = c("S1", "S2", "S3"),
    studyid = rep("STUDY1", 3),
    invid = c("SITE1", "SITE1", "SITE2"),
    event_dt = as.Date(c("2024-01-05", "2024-01-10", "2024-01-15")),
    MonthYYYYMM = c(202401, 202401, 202401),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = c("S1", "S2", "S3"),
    start_dt = as.Date(c("2024-01-01", "2024-01-03", "2024-01-05")),
    end_dt = as.Date(c("2024-02-01", "2024-02-03", "2024-02-05")),
    stringsAsFactors = FALSE
  )

  # Create DuckDB connection and lazy tables
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  DBI::dbWriteTable(con, "subjects", dfSubjects)
  DBI::dbWriteTable(con, "numerator", dfNumerator_processed)
  DBI::dbWriteTable(con, "denominator", dfDenominator)

  tblSubjects <- dplyr::tbl(con, "subjects")
  tblNumerator <- dplyr::tbl(con, "numerator")
  tblDenominator <- dplyr::tbl(con, "denominator")

  # Apply with lazy tables and end dates
  result <- gsm.studykri:::ApplyMinDenominatorDateAdjustment(
    dfNumerator_processed = tblNumerator,
    dfDenominator = tblDenominator,
    dfSubjects = tblSubjects,
    strSubjectCol = "subjid",
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strNumeratorDateCol = "event_dt",
    strDenominatorDateCol = "start_dt",
    strDenominatorEndDateCol = "end_dt",
    nMinDenominator = 2
  )

  # Results should still be lazy tables
  expect_s3_class(result$dfDenominator, "tbl_lazy")
  expect_s3_class(result$dfNumerator, "tbl_lazy")

  # Collect and verify
  dfDenom_result <- dplyr::collect(result$dfDenominator)

  # Verify date adjustments worked correctly
  expect_equal(dfDenom_result$start_dt[1], as.Date("2024-01-03"))
  expect_equal(dfDenom_result$start_dt[2], as.Date("2024-01-03"))
  expect_equal(dfDenom_result$end_dt[1], as.Date("2024-02-03")) # End date adjusted too
  expect_equal(dfDenom_result$end_dt[2], as.Date("2024-02-03"))

  # Cleanup
  DBI::dbDisconnect(con)
})
