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

