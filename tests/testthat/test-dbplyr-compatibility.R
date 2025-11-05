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
    nBootstrapReps = 10, # Small number for speed
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
    nBootstrapReps = 5, # Very small number for speed
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

# Test 5: Analyze_StudyKRI_PredictBoundsRef Returns Lazy Table
test_that("Analyze_StudyKRI_PredictBoundsRef returns lazy table with lazy input", {
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

  # Execute Analyze_StudyKRI_PredictBoundsRefSet (core function) for lazy table check
  suppressMessages({
    result <- Analyze_StudyKRI_PredictBoundsRefSet(
      dfInput = dfInput,
      vStudyFilter = c("STUDY1", "STUDY2"),
      nBootstrapReps = 5, # Very small number for speed
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

# Test 6: Transform_CumCount lazy table month sequence auto-generation
test_that("Transform_CumCount auto-generates month sequences for lazy tables", {
  test_data <- list(
    dfInput = data.frame(
      StudyID = rep("STUDY1", 6),
      GroupID = rep(c("SITE1", "SITE2"), each = 3),
      GroupLevel = "Site",
      MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
      Numerator = c(5, 10, 15, 3, 8, 12),
      Denominator = c(10, 20, 30, 10, 20, 30),
      Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4),
      stringsAsFactors = FALSE
    )
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "dfInput", test_data$dfInput)
  tblInput <- dplyr::tbl(con, "dfInput")
  
  result <- Transform_CumCount(
    dfInput = tblInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  expect_s3_class(result, "tbl_lazy")
  
  result_df <- dplyr::collect(result)
  expect_s3_class(result_df, "data.frame")
  expect_true(nrow(result_df) > 0)
  expect_true(all(c("StudyID", "MonthYYYYMM", "StudyMonth", "Numerator",
                    "Denominator", "Metric", "GroupCount") %in% names(result_df)))
  expect_true(all(diff(result_df$Numerator) >= 0))
  
  DBI::dbDisconnect(con)
})

# Test 7: Transform_CumCount lazy table with multiple studies  
test_that("Transform_CumCount handles multiple studies with lazy tables", {
  test_data <- list(
    dfInput = data.frame(
      StudyID = rep(c("STUDY1", "STUDY2"), each = 6),
      GroupID = rep(c("SITE1", "SITE2"), each = 3, times = 2),
      GroupLevel = "Site",
      MonthYYYYMM = rep(c(202301, 202302, 202303), 4),
      Numerator = rep(c(5, 10, 15), 4),
      Denominator = rep(c(10, 20, 30), 4),
      Metric = rep(c(0.5, 0.5, 0.5), 4),
      stringsAsFactors = FALSE
    )
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "dfInput", test_data$dfInput)
  tblInput <- dplyr::tbl(con, "dfInput")
  
  result <- Transform_CumCount(dfInput = tblInput, vBy = "StudyID", nMinDenominator = 25)
  
  expect_s3_class(result, "tbl_lazy")
  result_df <- dplyr::collect(result)
  
  expect_equal(length(unique(result_df$StudyID)), 2)
  expect_true(all(c("STUDY1", "STUDY2") %in% result_df$StudyID))
  
  for (study in c("STUDY1", "STUDY2")) {
    study_data <- result_df[result_df$StudyID == study, ]
    expect_equal(min(study_data$StudyMonth), 1)
  }
  
  DBI::dbDisconnect(con)
})

# Test 8: Transform_CumCount lazy table cumulative persistence
test_that("Transform_CumCount lazy table produces monotonically increasing cumulative counts", {
  test_data <- list(
    dfInput = data.frame(
      StudyID = rep("STUDY1", 5),
      GroupID = c("SITE1", "SITE1", "SITE1", "SITE2", "SITE2"),
      GroupLevel = "Site",
      MonthYYYYMM = c(202301, 202302, 202303, 202301, 202302),
      Numerator = c(10, 5, 8, 20, 10),
      Denominator = c(100, 50, 80, 200, 100),
      Metric = c(0.1, 0.1, 0.1, 0.1, 0.1),
      stringsAsFactors = FALSE
    )
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "dfInput", test_data$dfInput)
  tblInput <- dplyr::tbl(con, "dfInput")
  
  result <- Transform_CumCount(dfInput = tblInput, vBy = "StudyID", nMinDenominator = 0)
  
  expect_s3_class(result, "tbl_lazy")
  result_df <- dplyr::collect(result) %>% dplyr::arrange(MonthYYYYMM)
  
  numerator_diffs <- diff(result_df$Numerator)
  expect_true(all(numerator_diffs >= 0))
  
  expect_equal(result_df$Numerator[1], 30)
  expect_equal(result_df$Numerator[2], 45)
  expect_equal(result_df$Numerator[3], 53) # Site2 contribution persists!
  
  DBI::dbDisconnect(con)
})

# Test 9: Transform_CumCount lazy table filters gaps correctly
test_that("Transform_CumCount lazy table fills gaps in calendar months", {
  test_data <- list(
    dfInput = data.frame(
      StudyID = rep("STUDY1", 6),
      GroupID = rep(c("SITE1", "SITE2"), each = 3),
      GroupLevel = "Site",
      MonthYYYYMM = rep(c(202301, 202303, 202305), 2),
      Numerator = c(5, 10, 15, 3, 8, 12),
      Denominator = c(10, 20, 30, 10, 20, 30),
      Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4),
      stringsAsFactors = FALSE
    )
  )
  
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  DBI::dbWriteTable(con, "dfInput", test_data$dfInput)
  tblInput <- dplyr::tbl(con, "dfInput")
  
  result <- Transform_CumCount(dfInput = tblInput, vBy = "StudyID", nMinDenominator = 0)
  
  expect_s3_class(result, "tbl_lazy")
  result_df <- dplyr::collect(result) %>% dplyr::arrange(MonthYYYYMM)
  
  expect_equal(nrow(result_df), 5)
  expect_true(all(c(202301, 202302, 202303, 202304, 202305) %in% result_df$MonthYYYYMM))
  expect_equal(result_df$StudyMonth, 1:5)
  
  DBI::dbDisconnect(con)
})
