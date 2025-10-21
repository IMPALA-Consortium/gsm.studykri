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

# Test 3: Analyze_StudyKRI Returns Lazy Table
test_that("Analyze_StudyKRI returns lazy table with lazy input", {
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
  
  # Execute Analyze
  result <- Analyze_StudyKRI(
    dfInput = dfInput,
    nBootstrapReps = 10,  # Small number for speed
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    seed = 123
  )
  
  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Cleanup
  DBI::dbDisconnect(lazy_tables$con)
})

# Test 4: Analyze_StudyKRI_PredictBounds Returns Lazy Table
test_that("Analyze_StudyKRI_PredictBounds returns lazy table with lazy input", {
  # Setup
  test_data <- create_minimal_test_data()
  lazy_tables <- create_lazy_tables(test_data)
  
  # Build full pipeline to get bootstrap data
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
  
  dfBootstrap <- Analyze_StudyKRI(
    dfInput = dfInput,
    nBootstrapReps = 5,  # Very small number for speed
    strStudyCol = "StudyID",
    strGroupCol = "GroupID",
    seed = 123
  )
  
  dfBootstrapStudy <- Transform_CumCount(
    dfInput = dfBootstrap,
    vBy = c("StudyID", "BootstrapRep"),
    nMinDenominator = 1
  )
  
  # Execute Analyze_StudyKRI_PredictBounds
  result <- Analyze_StudyKRI_PredictBounds(
    dfInput = dfBootstrapStudy,
    vBy = "StudyID",
    nConfLevel = 0.95
  )
  
  # Verify result is lazy table
  expect_s3_class(result, "tbl_lazy")
  
  # Cleanup
  DBI::dbDisconnect(lazy_tables$con)
})

# Test 5: Analyze_StudyKRI_PredictBoundsGroup Returns Lazy Table
test_that("Analyze_StudyKRI_PredictBoundsGroup returns lazy table with lazy input", {
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
      aest_dt = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10",
                          "2024-01-18", "2024-01-25", "2024-02-12")),
      stringsAsFactors = FALSE
    ),
    dfDenominator = data.frame(
      subjid = c("S1", "S2", "S3", "S4", "S5", "S6"),
      visit_dt = as.Date(c("2024-01-10", "2024-01-12", "2024-02-05",
                           "2024-01-11", "2024-01-15", "2024-02-08")),
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
  
  # Execute Analyze_StudyKRI_PredictBoundsGroup
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsGroup(
      dfInput = dfInput,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 5,  # Very small number for speed
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

