test_that("validate_month_sequence detects gaps", {
  tbl_with_gap <- data.frame(
    StudyID = "STUDY001",
    MonthYYYYMM = c(202301, 202303, 202304)  # Missing Feb
  )
  
  expect_error(
    validate_month_sequence(tbl_with_gap, "StudyID"),
    "has gaps.*202302"
  )
})

test_that("validate_month_sequence accepts complete sequences", {
  tbl_complete <- data.frame(
    StudyID = "STUDY001",
    MonthYYYYMM = c(202301, 202302, 202303)
  )
  
  expect_silent(validate_month_sequence(tbl_complete, "StudyID"))
})

test_that("validate_month_sequence checks for required columns", {
  tbl_missing_month <- data.frame(
    StudyID = "STUDY001",
    SomeOtherCol = c(1, 2, 3)
  )
  
  expect_error(
    validate_month_sequence(tbl_missing_month, "StudyID"),
    "must have 'MonthYYYYMM' column"
  )
})

test_that("validate_month_sequence checks for grouping columns", {
  tbl_missing_group <- data.frame(
    MonthYYYYMM = c(202301, 202302, 202303)
  )
  
  expect_error(
    validate_month_sequence(tbl_missing_group, "StudyID"),
    "must have 'StudyID' column"
  )
})

test_that("validate_month_sequence handles empty table", {
  tbl_empty <- data.frame(
    StudyID = character(0),
    MonthYYYYMM = numeric(0)
  )
  
  expect_error(
    validate_month_sequence(tbl_empty, "StudyID"),
    "empty"
  )
})

test_that("validate_month_sequence handles multiple studies", {
  tbl_multi <- data.frame(
    StudyID = c(rep("STUDY001", 3), rep("STUDY002", 4)),
    MonthYYYYMM = c(
      202301, 202302, 202303,  # Complete for STUDY001
      202401, 202402, 202404, 202405  # Missing March for STUDY002
    )
  )
  
  expect_error(
    validate_month_sequence(tbl_multi, "StudyID"),
    "STUDY002.*202403"
  )
})

test_that("expand_lazy_table rejects invalid expansion types", {
  skip_if_not_installed("duckdb")
  
  # Create a simple lazy table
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbWriteTable(con, "test_data", data.frame(x = 1:5))
  tbl_lazy <- dplyr::tbl(con, "test_data")
  
  # Try with invalid expansion type (not data.frame or tbl_lazy)
  expect_error(
    expand_lazy_table(
      tblInput = tbl_lazy,
      tblExpansion = list(x = 1:10),  # Invalid type
      dfExpansion_mem = data.frame(x = 1:10),
      strTempTableName = "test_temp",
      strExpansionType = "test expansion"
    ),
    "must be a data.frame or tbl_lazy"
  )
})

test_that("expand_lazy_table uses user-supplied data.frame", {
  skip_if_not_installed("duckdb")
  
  # Create a simple lazy table
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbWriteTable(con, "test_data", data.frame(x = 1:5))
  tbl_lazy <- dplyr::tbl(con, "test_data")
  
  # Provide user data.frame - should write it to temp table
  user_df <- data.frame(BootstrapRep = 1:3)
  
  result <- expand_lazy_table(
    tblInput = tbl_lazy,
    tblExpansion = user_df,
    dfExpansion_mem = data.frame(BootstrapRep = 1:10),  # Should be ignored
    strTempTableName = "test_reps",
    strExpansionType = "test replicates"
  )
  
  # Result should be a lazy table
  expect_true(inherits(result, "tbl_lazy"))
  
  # Should have 3 rows (from user_df, not dfExpansion_mem)
  expect_equal(nrow(dplyr::collect(result)), 3)
})

test_that("expand_lazy_table auto-generates when no user table provided", {
  skip_if_not_installed("duckdb")
  
  # Create a simple lazy table
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbWriteTable(con, "test_data", data.frame(x = 1:5))
  tbl_lazy <- dplyr::tbl(con, "test_data")
  
  # No user table - should auto-generate from dfExpansion_mem
  result <- expand_lazy_table(
    tblInput = tbl_lazy,
    tblExpansion = NULL,
    dfExpansion_mem = data.frame(BootstrapRep = 1:10),
    strTempTableName = "auto_reps",
    strExpansionType = "bootstrap replicates"
  )
  
  # Result should be a lazy table
  expect_true(inherits(result, "tbl_lazy"))
  
  # Should have 10 rows from dfExpansion_mem
  expect_equal(nrow(dplyr::collect(result)), 10)
})

test_that("expand_lazy_table verifies connection match for user lazy tables", {
  skip_if_not_installed("duckdb")
  
  # Create two separate connections
  con1 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  con2 <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit({
    DBI::dbDisconnect(con1)
    DBI::dbDisconnect(con2)
  })
  
  DBI::dbWriteTable(con1, "test_data", data.frame(x = 1:5))
  DBI::dbWriteTable(con2, "expansion_data", data.frame(y = 1:3))
  
  tbl_lazy1 <- dplyr::tbl(con1, "test_data")
  tbl_lazy2 <- dplyr::tbl(con2, "expansion_data")
  
  # Should error because connections don't match
  expect_error(
    expand_lazy_table(
      tblInput = tbl_lazy1,
      tblExpansion = tbl_lazy2,
      dfExpansion_mem = data.frame(y = 1:10),
      strTempTableName = "test_temp",
      strExpansionType = "test expansion"
    ),
    "same database connection"
  )
})

test_that("expand_lazy_table returns user-supplied lazy table when connection matches", {
  skip_if_not_installed("duckdb")
  
  # Create connection and tables
  con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")
  on.exit(DBI::dbDisconnect(con))
  
  DBI::dbWriteTable(con, "test_data", data.frame(x = 1:5))
  DBI::dbWriteTable(con, "user_expansion", data.frame(Rep = 1:7))
  
  tbl_input <- dplyr::tbl(con, "test_data")
  tbl_expansion <- dplyr::tbl(con, "user_expansion")
  
  # Should use user-supplied lazy table directly
  result <- expand_lazy_table(
    tblInput = tbl_input,
    tblExpansion = tbl_expansion,
    dfExpansion_mem = data.frame(Rep = 1:10),  # Should be ignored
    strTempTableName = "test_temp",
    strExpansionType = "test replicates"
  )
  
  # Result should be the same as user-supplied table
  expect_equal(nrow(dplyr::collect(result)), 7)
})

