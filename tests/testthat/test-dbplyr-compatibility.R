# Test dbplyr compatibility for metric functions
# Verifies that all metric functions return lazy tables when given lazy table inputs

# Skip tests if required packages not available
skip_if_not_installed("dbplyr")
skip_if_not_installed("duckdb")
skip_if_not_installed("DBI")

# Helper function to create minimal test data
create_minimal_test_data <- function() {
  list(
    dfSubjects = data.frame(
      subjid = c("S1", "S2", "S3"),
      studyid = c("STUDY1", "STUDY1", "STUDY1"),
      invid = c("SITE1", "SITE1", "SITE2"),
      stringsAsFactors = FALSE
    ),
    dfNumerator = data.frame(
      subjid = c("S1", "S2", "S3"),
      aest_dt = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10")),
      stringsAsFactors = FALSE
    ),
    dfDenominator = data.frame(
      subjid = c("S1", "S2", "S3"),
      visit_dt = as.Date(c("2024-01-10", "2024-01-12", "2024-02-05")),
      stringsAsFactors = FALSE
    )
  )
}

# Helper function to convert data frames to DuckDB lazy tables
create_lazy_tables <- function(data_list) {
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  lapply(names(data_list), function(name) {
    DBI::dbWriteTable(con, name, data_list[[name]])
  })

  list(
    con = con,
    dfSubjects = dplyr::tbl(con, "dfSubjects"),
    dfNumerator = dplyr::tbl(con, "dfNumerator"),
    dfDenominator = dplyr::tbl(con, "dfDenominator")
  )
}

# Test 1: Input_CountSiteByMonth Returns Lazy Table
test_that("Input_CountSiteByMonth returns lazy table with lazy inputs", {
  # Setup
  test_data <- create_minimal_test_data()
  lazy_tables <- create_lazy_tables(test_data)

  # Execute function
  result <- Input_CountSiteByMonth(
    dfSubjects = lazy_tables$dfSubjects,
    dfNumerator = lazy_tables$dfNumerator,
    dfDenominator = lazy_tables$dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")

  # Cleanup
  DBI::dbDisconnect(lazy_tables$con)
})

# Test 2: Transform_CumCount Returns Lazy Table
test_that("Transform_CumCount returns lazy table with lazy input", {
  # Setup
  test_data <- create_minimal_test_data()
  lazy_tables <- create_lazy_tables(test_data)

  # First get Input result as lazy table
  dfInput <- Input_CountSiteByMonth(
    dfSubjects = lazy_tables$dfSubjects,
    dfNumerator = lazy_tables$dfNumerator,
    dfDenominator = lazy_tables$dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Execute Transform
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 1
  )

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")

  # Cleanup
  DBI::dbDisconnect(lazy_tables$con)
})

# Test 3: Analyze_StudyKRI_PredictBoundsRefSet Returns Lazy Table
test_that("Analyze_StudyKRI_PredictBoundsRefSet returns lazy table with lazy input", {
  # Setup with multiple studies
  test_data_multi <- list(
    dfSubjects = data.frame(
      subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
      studyid = rep(c("STUDY1", "STUDY2"), each = 3),
      invid = c("SITE1", "SITE1", "SITE2", "SITE3", "SITE3", "SITE4"),
      stringsAsFactors = FALSE
    ),
    dfNumerator = data.frame(
      subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
      aest_dt = as.Date(c(
        "2024-01-15", "2024-01-20", "2024-02-10",
        "2024-01-18", "2024-01-25", "2024-02-12"
      )),
      stringsAsFactors = FALSE
    ),
    dfDenominator = data.frame(
      subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
      visit_dt = as.Date(c(
        "2024-01-10", "2024-01-12", "2024-02-05",
        "2024-01-11", "2024-01-15", "2024-02-08"
      )),
      stringsAsFactors = FALSE
    )
  )

  lazy_tables <- create_lazy_tables(test_data_multi)

  # Build full pipeline to get site-level data
  dfInput <- Input_CountSiteByMonth(
    dfSubjects = lazy_tables$dfSubjects,
    dfNumerator = lazy_tables$dfNumerator,
    dfDenominator = lazy_tables$dfDenominator,
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Execute Analyze_StudyKRI_PredictBoundsRefSet for lazy table check
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfInput,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 5,
      nConfLevel = 0.95,
      nMinDenominator = 1,
      seed = 123
    )
  })

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")

  # Cleanup
  DBI::dbDisconnect(lazy_tables$con)
})

test_that("Transform_CumCount comprehensive lazy table behavior", {
  # Test data with gaps and multiple sites
  test_data <- list(
    dfInput = data.frame(
      StudyID = rep("STUDY1", 8),
      GroupID = c("SITE1", "SITE1", "SITE1", "SITE1", "SITE2", "SITE2", "SITE2", "SITE2"),
      GroupLevel = "Site",
      MonthYYYYMM = c(202301, 202302, 202304, 202305, 202301, 202302, 202304, 202305),
      Numerator = c(10, 5, 8, 12, 20, 10, 15, 18),
      Denominator = c(100, 50, 80, 100, 200, 100, 150, 180),
      Metric = c(0.1, 0.1, 0.1, 0.12, 0.1, 0.1, 0.1, 0.1),
      stringsAsFactors = FALSE
    )
  )

  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "dfInput", test_data$dfInput)
  tblInput <- dplyr::tbl(con, "dfInput")

  result <- Transform_CumCount(dfInput = tblInput, vBy = "StudyID", nMinDenominator = 0)

  # 1. Verify returns lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and analyze results
  result_df <- dplyr::collect(result) %>% dplyr::arrange(MonthYYYYMM)

  # 2. Verify required columns exist
  expect_true(all(c(
    "StudyID", "MonthYYYYMM", "StudyMonth", "Numerator",
    "Denominator", "Metric", "GroupCount"
  ) %in% names(result_df)))

  # 3. Verify gap filling (missing March 202303 is filled)
  expect_equal(nrow(result_df), 5)
  expect_true(all(c(202301, 202302, 202303, 202304, 202305) %in% result_df$MonthYYYYMM))
  expect_equal(result_df$StudyMonth, 1:5)

  # 4. Verify cumulative counts are monotonically increasing
  numerator_diffs <- diff(result_df$Numerator)
  expect_true(all(numerator_diffs >= 0))

  # 5. Verify specific cumulative values (sites aggregate correctly)
  expect_equal(result_df$Numerator[1], 30)  # Jan: SITE1(10) + SITE2(20)
  expect_equal(result_df$Numerator[2], 45)  # Feb: prev + SITE1(5) + SITE2(10)
  expect_equal(result_df$Numerator[3], 45)  # Mar: gap filled, count persists
  expect_equal(result_df$Numerator[4], 68)  # Apr: prev + SITE1(8) + SITE2(15)

  DBI::dbDisconnect(con)
})

# Test 5: Analyze_StudyKRI_PredictBoundsRefSet with multiple studies
test_that("Analyze_StudyKRI_PredictBoundsRefSet works with lazy tables", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
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

  # Write to database
  DBI::dbWriteTable(con, "site_data", dfTest)

  # Create lazy table
  dfLazy <- dplyr::tbl(con, "site_data")

  # Test with lazy table
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfLazy,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      seed = 888
    )
  })

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Collect to verify data properties
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)
  expect_equal(unique(result_collected$StudyCount), 2)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 6: Analyze_StudyKRI_PredictBoundsRefSet with NULL vStudyFilter
test_that("Analyze_StudyKRI_PredictBoundsRefSet with lazy table and NULL vStudyFilter", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
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

  # Write to database
  DBI::dbWriteTable(con, "site_data", dfTest)

  # Create lazy table
  dfLazy <- dplyr::tbl(con, "site_data")

  # Test with NULL vStudyFilter on lazy table
  expect_message(
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfLazy,
      vStudyFilter = NULL,
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      seed = 777
    ),
    "No vStudyFilter specified. Using all 3 studies."
  )

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Collect to verify data properties
  result_collected <- dplyr::collect(result)
  expect_equal(unique(result_collected$StudyCount), 3)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 7: Input_CountSiteByMonth with lazy tables
test_that("Input_CountSiteByMonth works with lazy tables", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
  dfSubjects <- data.frame(
    studyid = rep("STUDY001", 3),
    invid = rep("SITE01", 3),
    subjid = c("SUBJ001", "SUBJ002", "SUBJ003"),
    enrollyn = c("Y", "Y", "Y"),
    stringsAsFactors = FALSE
  )

  dfNumerator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002", "SUBJ003"),
    aest_dt = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10")),
    stringsAsFactors = FALSE
  )

  dfDenominator <- data.frame(
    subjid = c("SUBJ001", "SUBJ002", "SUBJ003"),
    visit_dt = as.Date(c("2024-01-10", "2024-01-15", "2024-02-05")),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "subjects", dfSubjects)
  DBI::dbWriteTable(con, "numerator", dfNumerator)
  DBI::dbWriteTable(con, "denominator", dfDenominator)

  # Create lazy tables
  tblSubjects <- dplyr::tbl(con, "subjects")
  tblNumerator <- dplyr::tbl(con, "numerator")
  tblDenominator <- dplyr::tbl(con, "denominator")

  # Execute with lazy tables
  result <- Input_CountSiteByMonth(
    dfSubjects = tblSubjects,
    dfNumerator = tblNumerator,
    dfDenominator = tblDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )

  # Verify returns lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect results to verify data properties
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)
  expect_true(all(c("GroupID", "Numerator", "Denominator", "MonthYYYYMM") %in% names(result_collected)))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 8: Input_CountSiteByMonth with end date column (calculate_days_by_month)
test_that("calculate_days_by_month works with lazy tables", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
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

  dfDenominator <- data.frame(
    subjid = "SUBJ001",
    start_date = as.Date("2024-01-01"),
    end_date = as.Date("2024-02-29"),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "subjects", dfSubjects)
  DBI::dbWriteTable(con, "numerator", dfNumerator)
  DBI::dbWriteTable(con, "denominator", dfDenominator)

  # Create lazy tables
  tblSubjects <- dplyr::tbl(con, "subjects")
  tblNumerator <- dplyr::tbl(con, "numerator")
  tblDenominator <- dplyr::tbl(con, "denominator")

  # Execute with lazy tables and end date column
  result <- Input_CountSiteByMonth(
    dfSubjects = tblSubjects,
    dfNumerator = tblNumerator,
    dfDenominator = tblDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "start_date",
    strDenominatorEndDateCol = "end_date"
  )

  # Verify returns lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect results to verify data properties
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 9: Analyze_StudyKRI_PredictBounds with lazy tables
test_that("Analyze_StudyKRI_PredictBounds returns lazy table with lazy input", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
  dfTest <- data.frame(
    StudyID = rep("STUDY1", 24),
    GroupID = rep(paste0("Site", 1:4), each = 6),
    Numerator = rep(1:6, times = 4),
    Denominator = rep(10:15, times = 4),
    MonthYYYYMM = rep(202301:202306, times = 4),
    Metric = runif(24, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "site_data", dfTest)

  # Create lazy table
  dfLazy <- dplyr::tbl(con, "site_data")

  # Test with lazy table (no dfStudyRef - uses all studies)
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBounds(
      dfInput = dfLazy,
      dfStudyRef = NULL,
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      nMinDenominator = 1,
      seed = 999
    )
  })

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Collect to verify data properties
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)
  expect_true(all(c("StudyID", "StudyMonth", "Median", "Lower", "Upper") %in% names(result_collected)))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 9b: Analyze_StudyKRI_PredictBounds with lazy dfStudyRef
test_that("Analyze_StudyKRI_PredictBounds works with lazy dfStudyRef", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data with multiple studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 24),
    GroupID = rep(paste0("Site", 1:4), each = 6, times = 3),
    Numerator = rep(1:6, times = 12),
    Denominator = rep(10:15, times = 12),
    MonthYYYYMM = rep(202301:202306, times = 12),
    Metric = runif(72, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "site_data", dfTest)

  # Create lazy table for input
  dfLazy <- dplyr::tbl(con, "site_data")

  # Create study reference mapping in multi-row format (one row per study)
  # This specifies which studies to calculate bounds for
  dfStudyRef <- data.frame(
    study = c("STUDY1", "STUDY2"),
    stringsAsFactors = FALSE
  )
  
  # Write study reference to database to make it a lazy table
  DBI::dbWriteTable(con, "study_ref", dfStudyRef)
  dfStudyRefLazy <- dplyr::tbl(con, "study_ref")

  # Test with lazy table input and lazy dfStudyRef
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBounds(
      dfInput = dfLazy,
      dfStudyRef = dfStudyRefLazy,
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      nMinDenominator = 1,
      seed = 888
    )
  })

  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Collect to verify data properties
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)
  expect_true(all(c("StudyID", "StudyMonth", "Median", "Lower", "Upper") %in% names(result_collected)))
  
  # Should only have STUDY1 and STUDY2 (not STUDY3)
  expect_equal(sort(unique(result_collected$StudyID)), c("STUDY1", "STUDY2"))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 10: Analyze_StudyKRI_PredictBoundsRef with lazy tables
test_that("Analyze_StudyKRI_PredictBoundsRef returns lazy table with lazy input", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data with 3 studies
  dfTest <- data.frame(
    StudyID = rep(c("STUDY1", "STUDY2", "STUDY3"), each = 24),
    GroupID = rep(paste0("Site", 1:4), each = 6, times = 3),
    Numerator = rep(1:6, times = 12),
    Denominator = rep(10:15, times = 12),
    MonthYYYYMM = rep(202301:202306, times = 12),
    Metric = runif(72, 0.05, 0.5),
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "site_data", dfTest)

  # Create lazy table for input
  dfLazy <- dplyr::tbl(con, "site_data")

  # Create study reference mapping in multi-row format (one row per study-studyref pair)
  dfStudyRef <- data.frame(
    study = c("STUDY1", "STUDY1", "STUDY2", "STUDY2"),
    studyref = c("STUDY2", "STUDY3", "STUDY1", "STUDY3"),
    stringsAsFactors = FALSE
  )
  
  # Write study reference to database to make it a lazy table
  DBI::dbWriteTable(con, "study_ref", dfStudyRef)
  dfStudyRefLazy <- dplyr::tbl(con, "study_ref")

  # Test with lazy table input and lazy dfStudyRef
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRef(
      dfInput = dfLazy,
      dfStudyRef = dfStudyRefLazy,
      nBootstrapReps = 20,
      nConfLevel = 0.95,
      nMinDenominator = 1,
      seed = 555
    )
  })

  # Should return lazy table when input is lazy
  expect_s3_class(result, "tbl_lazy")
  
  # Collect to verify correctness
  result_collected <- dplyr::collect(result)
  expect_true(nrow(result_collected) > 0)
  expect_true(all(c("StudyID", "StudyRefID", "StudyMonth", "Median") %in% names(result_collected)))
  expect_equal(length(unique(result_collected$StudyID)), 2)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 11: JoinKRIByDenominator with lazy tables
test_that("JoinKRIByDenominator works with lazy tables", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0003"),
    Denominator = c("Visits", "Visits"),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "input", dfInput)
  DBI::dbWriteTable(con, "metrics", dfMetrics)

  # Create lazy tables
  tblInput <- dplyr::tbl(con, "input")
  tblMetrics <- dplyr::tbl(con, "metrics")

  # Test with lazy tables
  result <- JoinKRIByDenominator(tblInput, tblMetrics)

  # Should return a list
  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true("Visits" %in% names(result))

  # Each element should be a lazy table when inputs are lazy
  expect_s3_class(result[[1]], "tbl_lazy")

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 12: Transform_Long with lazy tables in list
test_that("Transform_Long works with lazy tables in list", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    Median_kri0003 = c(0.2, 0.25),
    Lower_kri0003 = c(0.1, 0.15),
    Upper_kri0003 = c(0.3, 0.35),
    BootstrapCount = c(1000, 1000),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "wide_data", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "wide_data")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with lazy table in list
  result <- Transform_Long(lWide)

  # Should return lazy table when input is lazy
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_s3_class(result_collected, "data.frame")
  expect_true(nrow(result_collected) > 0)
  expect_true(all(c("MetricID", "DenominatorType", "StudyID", "Median", "Lower", "Upper") %in% names(result_collected)))
  expect_equal(length(unique(result_collected$MetricID)), 2)

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 13: Transform_Long with no wide columns and lazy table
test_that("Transform_Long handles no wide columns with lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Data frame with only id columns, no wide columns
  dfNoWide <- data.frame(
    StudyID = c("AA-1", "AA-2", "AA-3"),
    StudyMonth = c(1, 1, 2),
    BootstrapCount = c(1000, 1000, 1000),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "no_wide_data", dfNoWide)

  # Create lazy table
  tblNoWide <- dplyr::tbl(con, "no_wide_data")

  # Create list with lazy table
  lWide <- list(Visits = tblNoWide)

  # Test with lazy table (should hit early return path)
  result <- Transform_Long(lWide)

  # Should return lazy table with DenominatorType added
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_true("DenominatorType" %in% names(result_collected))
  expect_equal(unique(result_collected$DenominatorType), "Visits")
  expect_equal(nrow(result_collected), 3)
  expect_true(all(c("StudyID", "StudyMonth", "BootstrapCount") %in% names(result_collected)))
  
  # MetricID should NOT be in the result since there were no wide columns
  expect_false("MetricID" %in% names(result_collected))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 14: Transform_Long with multiple denominator types and lazy tables
test_that("Transform_Long handles multiple denominator types with lazy tables", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data for Visits
  dfVisits <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    stringsAsFactors = FALSE
  )

  # Create test data for Days
  dfDays <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0003 = c(0.2, 0.25),
    Lower_kri0003 = c(0.1, 0.15),
    Upper_kri0003 = c(0.3, 0.35),
    stringsAsFactors = FALSE
  )

  # Create test data for Subjects
  dfSubjects <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Numerator_kri0005 = c(10, 12),
    Metric_kri0005 = c(0.1, 0.12),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "visits_data", dfVisits)
  DBI::dbWriteTable(con, "days_data", dfDays)
  DBI::dbWriteTable(con, "subjects_data", dfSubjects)

  # Create lazy tables
  tblVisits <- dplyr::tbl(con, "visits_data")
  tblDays <- dplyr::tbl(con, "days_data")
  tblSubjects <- dplyr::tbl(con, "subjects_data")

  # Create list with multiple lazy tables
  lWide <- list(Visits = tblVisits, Days = tblDays, Subjects = tblSubjects)

  # Test with multiple lazy tables
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_equal(nrow(result_collected), 6)  # 2 rows * 3 denominator types
  expect_equal(sort(unique(result_collected$DenominatorType)), c("Days", "Subjects", "Visits"))
  expect_equal(length(unique(result_collected$MetricID)), 3)
  
  # Verify each denominator type has correct data
  visits_data <- result_collected[result_collected$DenominatorType == "Visits", ]
  expect_equal(unique(visits_data$MetricID), "kri0001")
  expect_true(all(c("Median", "Lower", "Upper") %in% names(visits_data)))

  days_data <- result_collected[result_collected$DenominatorType == "Days", ]
  expect_equal(unique(days_data$MetricID), "kri0003")

  subjects_data <- result_collected[result_collected$DenominatorType == "Subjects", ]
  expect_equal(unique(subjects_data$MetricID), "kri0005")
  expect_true(all(c("Numerator", "Metric") %in% names(subjects_data)))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 15: Transform_Long with single MetricID and lazy table
test_that("Transform_Long handles single MetricID with lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data with only one MetricID
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2", "AA-3"),
    StudyMonth = c(1, 1, 2),
    Median_kri0001 = c(0.5, 0.6, 0.55),
    Lower_kri0001 = c(0.3, 0.4, 0.35),
    Upper_kri0001 = c(0.7, 0.8, 0.75),
    BootstrapCount = c(1000, 1000, 1000),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "single_metric", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "single_metric")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with lazy table
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_equal(nrow(result_collected), 3)
  expect_equal(unique(result_collected$MetricID), "kri0001")
  expect_equal(unique(result_collected$DenominatorType), "Visits")
  expect_true(all(c("Median", "Lower", "Upper") %in% names(result_collected)))
  
  # Verify data values
  expect_equal(sort(result_collected$Median), c(0.5, 0.55, 0.6))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 16: Transform_Long with custom denominator column name and lazy table
test_that("Transform_Long handles custom denominator column with lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "wide_data", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "wide_data")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with custom denominator column name
  result <- Transform_Long(lWide, strDenominatorCol = "CustomDenomType")

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_true("CustomDenomType" %in% names(result_collected))
  expect_false("DenominatorType" %in% names(result_collected))
  expect_equal(result_collected$CustomDenomType, rep("Visits", 2))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 17: Transform_Long with mixed wide column patterns and lazy table
test_that("Transform_Long handles mixed wide column patterns with lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data with mixed patterns
  # Some metrics have Median/Lower/Upper, others have only Numerator/Metric
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    Numerator_kri0003 = c(10, 12),
    Metric_kri0003 = c(0.2, 0.24),
    Median_kri0005 = c(0.15, 0.18),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "mixed_data", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "mixed_data")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with lazy table
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_equal(nrow(result_collected), 6)  # 2 rows * 3 MetricIDs
  expect_equal(sort(unique(result_collected$MetricID)), c("kri0001", "kri0003", "kri0005"))

  # kri0001 should have Median/Lower/Upper
  kri0001_data <- result_collected[result_collected$MetricID == "kri0001", ]
  expect_true(all(c("Median", "Lower", "Upper") %in% names(kri0001_data)))
  expect_equal(sort(kri0001_data$Median), c(0.5, 0.6))

  # kri0003 should have Numerator/Metric
  kri0003_data <- result_collected[result_collected$MetricID == "kri0003", ]
  expect_true(all(c("Numerator", "Metric") %in% names(kri0003_data)))
  expect_equal(sort(kri0003_data$Numerator), c(10, 12))

  # kri0005 should have Median (only one pattern column)
  kri0005_data <- result_collected[result_collected$MetricID == "kri0005", ]
  expect_true("Median" %in% names(kri0005_data))
  expect_equal(sort(kri0005_data$Median), c(0.15, 0.18))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 18: Transform_Long column ordering with lazy table
test_that("Transform_Long maintains correct column order with lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data with various column orders
  dfWide <- data.frame(
    BootstrapCount = c(1000, 1000),
    StudyID = c("AA-1", "AA-2"),
    Median_kri0001 = c(0.5, 0.6),
    StudyMonth = c(1, 1),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "wide_data", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "wide_data")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with lazy table
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify column ordering
  result_collected <- dplyr::collect(result)
  col_names <- names(result_collected)
  
  # MetricID and DenominatorType should be first two columns
  expect_equal(col_names[1], "MetricID")
  expect_equal(col_names[2], "DenominatorType")
  
  # Other columns should follow
  expect_true(all(c("StudyID", "StudyMonth", "BootstrapCount", "Median", "Lower", "Upper") %in% col_names[3:length(col_names)]))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 19: Transform_Long with empty lazy table
test_that("Transform_Long handles empty lazy table", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create empty data frame with correct structure
  dfWide <- data.frame(
    StudyID = character(0),
    StudyMonth = integer(0),
    Median_kri0001 = numeric(0),
    Lower_kri0001 = numeric(0),
    Upper_kri0001 = numeric(0),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "empty_data", dfWide)

  # Create lazy table
  tblWide <- dplyr::tbl(con, "empty_data")

  # Create list with lazy table
  lWide <- list(Visits = tblWide)

  # Test with empty lazy table
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify structure
  result_collected <- dplyr::collect(result)
  expect_equal(nrow(result_collected), 0)
  expect_true(all(c("MetricID", "DenominatorType") %in% names(result_collected)))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})

# Test 20: Transform_Long integration with JoinKRIByDenominator output
test_that("Transform_Long works with JoinKRIByDenominator lazy table output", {
  skip_if_not_installed("dbplyr")
  skip_if_not_installed("duckdb")

  # Create in-memory database
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

  # Create test data that mimics JoinKRIByDenominator output
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0003"),
    Denominator = c("Visits", "Days"),
    stringsAsFactors = FALSE
  )

  # Write to database
  DBI::dbWriteTable(con, "input", dfInput)
  DBI::dbWriteTable(con, "metrics", dfMetrics)

  # Create lazy tables
  tblInput <- dplyr::tbl(con, "input")
  tblMetrics <- dplyr::tbl(con, "metrics")

  # Use JoinKRIByDenominator to create list of lazy tables
  lJoined <- JoinKRIByDenominator(tblInput, tblMetrics)

  # Verify JoinKRIByDenominator returns list of lazy tables
  expect_type(lJoined, "list")
  expect_true(all(c("Visits", "Days") %in% names(lJoined)))
  
  # Each element should be lazy
  lapply(lJoined, function(x) expect_s3_class(x, "tbl_lazy"))

  # Now test if we can process this with some wide columns
  # In real workflow, there would be analysis steps that create wide columns
  # For this test, we'll create mock wide data with the expected structure
  dfWideVisits <- data.frame(
    StudyID = "AA-1",
    StudyMonth = 1,
    Median_kri0001 = 0.5,
    Lower_kri0001 = 0.3,
    Upper_kri0001 = 0.7,
    stringsAsFactors = FALSE
  )

  dfWideDays <- data.frame(
    StudyID = "AA-1",
    StudyMonth = 1,
    Median_kri0003 = 0.2,
    Lower_kri0003 = 0.1,
    Upper_kri0003 = 0.3,
    stringsAsFactors = FALSE
  )

  # Write mock wide data
  DBI::dbWriteTable(con, "wide_visits", dfWideVisits, overwrite = TRUE)
  DBI::dbWriteTable(con, "wide_days", dfWideDays, overwrite = TRUE)

  # Create lazy tables
  lWide <- list(
    Visits = dplyr::tbl(con, "wide_visits"),
    Days = dplyr::tbl(con, "wide_days")
  )

  # Test Transform_Long with this structure
  result <- Transform_Long(lWide)

  # Should return lazy table
  expect_s3_class(result, "tbl_lazy")

  # Collect and verify
  result_collected <- dplyr::collect(result)
  expect_equal(nrow(result_collected), 2)
  expect_true(all(c("MetricID", "DenominatorType") %in% names(result_collected)))
  expect_equal(sort(unique(result_collected$DenominatorType)), c("Days", "Visits"))

  # Clean up
  DBI::dbDisconnect(con, shutdown = TRUE)
})
