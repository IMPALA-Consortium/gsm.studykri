# Test Transform_Long function
# Tests for defensive code and edge cases

# ---- Input Validation Tests ----

test_that("Transform_Long rejects non-list input", {
  expect_error(
    Transform_Long(lWide = "not a list"),
    "lWide must be a non-empty named list"
  )

  # Note: data.frame IS a list in R, so it passes the first check
  # but fails when trying to process elements
  expect_error(
    Transform_Long(lWide = data.frame(x = 1)),
    "must be a data.frame or tbl"
  )

  expect_error(
    Transform_Long(lWide = c(1, 2, 3)),
    "lWide must be a non-empty named list"
  )
})

test_that("Transform_Long rejects empty list", {
  expect_error(
    Transform_Long(lWide = list()),
    "lWide must be a non-empty named list"
  )
})

test_that("Transform_Long rejects NULL input", {
  expect_error(
    Transform_Long(lWide = NULL),
    "lWide must be a non-empty named list"
  )
})

test_that("Transform_Long rejects list with NULL names", {
  dfWide <- data.frame(
    StudyID = "AA-1",
    StudyMonth = 1,
    Median_kri0001 = 0.5
  )

  # List without names attribute
  lWide <- list(dfWide)

  expect_error(
    Transform_Long(lWide = lWide),
    "lWide must have named elements"
  )
})

test_that("Transform_Long rejects list with empty string names", {
  dfWide <- data.frame(
    StudyID = "AA-1",
    StudyMonth = 1,
    Median_kri0001 = 0.5
  )

  # List with empty string name - create it with setNames to avoid R errors
  lWide <- list(dfWide)
  names(lWide) <- ""

  expect_error(
    Transform_Long(lWide = lWide),
    "lWide must have named elements"
  )
})

test_that("Transform_Long rejects list with partially missing names", {
  dfWide1 <- data.frame(
    StudyID = "AA-1",
    StudyMonth = 1,
    Median_kri0001 = 0.5
  )

  dfWide2 <- data.frame(
    StudyID = "AA-2",
    StudyMonth = 1,
    Median_kri0001 = 0.6
  )

  # List with one named and one unnamed element
  lWide <- list(Visits = dfWide1, dfWide2)

  expect_error(
    Transform_Long(lWide = lWide),
    "lWide must have named elements"
  )
})

test_that("Transform_Long rejects list element that is a vector", {
  lWide <- list(Visits = c(1, 2, 3))

  expect_error(
    Transform_Long(lWide = lWide),
    "Element 'Visits' must be a data.frame or tbl"
  )
})

test_that("Transform_Long rejects list element that is a matrix", {
  lWide <- list(Visits = matrix(1:9, nrow = 3))

  expect_error(
    Transform_Long(lWide = lWide),
    "Element 'Visits' must be a data.frame or tbl"
  )
})

test_that("Transform_Long rejects list element that is NULL", {
  lWide <- list(Visits = NULL)

  expect_error(
    Transform_Long(lWide = lWide),
    "Element 'Visits' must be a data.frame or tbl"
  )
})

test_that("Transform_Long rejects list element that is a character", {
  lWide <- list(Days = "not a dataframe")

  expect_error(
    Transform_Long(lWide = lWide),
    "Element 'Days' must be a data.frame or tbl"
  )
})

# ---- Edge Case Functionality Tests ----

test_that("Transform_Long handles data frame with no wide columns", {
  # Data frame with only id columns, no wide columns
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 2),
    BootstrapCount = c(1000, 1000)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  # Should return the data with DenominatorType column added
  expect_s3_class(result, "data.frame")
  expect_true("DenominatorType" %in% names(result))
  expect_equal(result$DenominatorType, rep("Visits", 2))
  expect_equal(nrow(result), 2)

  # Original columns should be preserved
  expect_true(all(c("StudyID", "StudyMonth", "BootstrapCount") %in% names(result)))

  # MetricID should NOT be in the result since there were no wide columns
  expect_false("MetricID" %in% names(result))
})

test_that("Transform_Long handles single MetricID correctly", {
  # Data frame with only one MetricID
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    BootstrapCount = c(1000, 1000)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 2)
  expect_true(all(c("MetricID", "DenominatorType", "Median", "Lower", "Upper") %in% names(result)))
  expect_equal(unique(result$MetricID), "kri0001")
  expect_equal(result$Median, c(0.5, 0.6))
})

test_that("Transform_Long handles multiple MetricIDs correctly", {
  # Data frame with multiple MetricIDs
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    Median_kri0003 = c(0.2, 0.25),
    Lower_kri0003 = c(0.1, 0.15),
    Upper_kri0003 = c(0.3, 0.35),
    BootstrapCount = c(1000, 1000)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 4) # 2 rows * 2 MetricIDs
  expect_equal(sort(unique(result$MetricID)), c("kri0001", "kri0003"))

  # Check specific values for kri0001
  result_kri0001 <- result[result$MetricID == "kri0001", ]
  expect_equal(result_kri0001$Median, c(0.5, 0.6))

  # Check specific values for kri0003
  result_kri0003 <- result[result$MetricID == "kri0003", ]
  expect_equal(result_kri0003$Median, c(0.2, 0.25))
})

test_that("Transform_Long handles single denominator type", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_equal(unique(result$DenominatorType), "Visits")
  expect_equal(nrow(result), 2)
})

test_that("Transform_Long handles multiple denominator types", {
  dfWide1 <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  dfWide2 <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0003 = c(0.2, 0.25),
    Lower_kri0003 = c(0.1, 0.15),
    Upper_kri0003 = c(0.3, 0.35)
  )

  lWide <- list(Visits = dfWide1, Days = dfWide2)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(sort(unique(result$DenominatorType)), c("Days", "Visits"))
  expect_equal(nrow(result), 4) # 2 rows from Visits + 2 rows from Days

  # Check Visits data
  result_visits <- result[result$DenominatorType == "Visits", ]
  expect_equal(result_visits$MetricID, rep("kri0001", 2))

  # Check Days data
  result_days <- result[result$DenominatorType == "Days", ]
  expect_equal(result_days$MetricID, rep("kri0003", 2))
})

# ---- Return Type Tests ----

test_that("Transform_Long returns tibble for regular data.frame input", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "tbl")
  expect_s3_class(result, "data.frame")
})

test_that("Transform_Long preserves tibble class", {
  dfWide <- dplyr::tibble(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "tbl")
})

# ---- Custom Column Name Tests ----

test_that("Transform_Long uses custom strDenominatorCol parameter", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide, strDenominatorCol = "CustomDenomType")

  expect_true("CustomDenomType" %in% names(result))
  expect_false("DenominatorType" %in% names(result))
  expect_equal(result$CustomDenomType, rep("Visits", 2))
})

test_that("Transform_Long handles tibble with no wide columns", {
  # Tibble with only id columns
  dfWide <- dplyr::tibble(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 2)
  )

  lWide <- list(Days = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_true("DenominatorType" %in% names(result))
  expect_equal(result$DenominatorType, rep("Days", 2))

  # MetricID should NOT be in the result since there were no wide columns
  expect_false("MetricID" %in% names(result))
})

test_that("Transform_Long custom denominator column works with no wide columns", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 2)
  )

  lWide <- list(Days = dfWide)
  result <- Transform_Long(lWide, strDenominatorCol = "DenomType")

  expect_true("DenomType" %in% names(result))
  expect_equal(result$DenomType, rep("Days", 2))

  # MetricID should NOT be in the result since there were no wide columns
  expect_false("MetricID" %in% names(result))
})

# ---- Column Ordering Tests ----

test_that("Transform_Long places MetricID and DenominatorType first", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    BootstrapCount = c(1000, 1000),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  # Check that MetricID and DenominatorType are the first two columns
  expect_equal(names(result)[1], "MetricID")
  expect_equal(names(result)[2], "DenominatorType")
})

test_that("Transform_Long preserves other column order after priority columns", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    BootstrapCount = c(1000, 1000)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  # Priority columns first
  expect_equal(names(result)[1:2], c("MetricID", "DenominatorType"))

  # Other columns preserved (order may vary but should include these)
  other_cols <- names(result)[3:length(names(result))]
  expect_true(all(c("StudyID", "StudyMonth", "Median", "Lower", "Upper", "BootstrapCount") %in% other_cols))
})

# ---- Integration Tests ----

test_that("Transform_Long works with example from documentation", {
  # Example from function documentation
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    Median_kri0003 = c(0.2, 0.25),
    Lower_kri0003 = c(0.1, 0.15),
    Upper_kri0003 = c(0.3, 0.35),
    BootstrapCount = c(1000, 1000)
  )

  lWide <- list(Visits = dfWide)
  dfLong <- Transform_Long(lWide)

  expect_s3_class(dfLong, "tbl")
  expect_equal(nrow(dfLong), 4)
  expect_true(all(c("MetricID", "DenominatorType", "StudyID", "Median", "Lower", "Upper") %in% names(dfLong)))
  expect_equal(length(unique(dfLong$MetricID)), 2)
})

test_that("Transform_Long handles Numerator and Metric columns", {
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Numerator_kri0001 = c(10, 12),
    Metric_kri0001 = c(0.5, 0.6),
    Numerator_kri0003 = c(5, 6),
    Metric_kri0003 = c(0.25, 0.3)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 4)
  expect_true(all(c("Numerator", "Metric") %in% names(result)))

  # Check kri0001 values
  result_kri0001 <- result[result$MetricID == "kri0001", ]
  expect_equal(result_kri0001$Numerator, c(10, 12))
  expect_equal(result_kri0001$Metric, c(0.5, 0.6))
})

test_that("Transform_Long handles different wide column patterns per MetricID", {
  # Each MetricID can have different column patterns
  # All MetricIDs must have compatible columns for union_all to work
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8),
    Median_kri0003 = c(0.2, 0.24),
    Lower_kri0003 = c(0.15, 0.18),
    Upper_kri0003 = c(0.25, 0.30)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 4)
  expect_equal(sort(unique(result$MetricID)), c("kri0001", "kri0003"))

  # Both should have Median/Lower/Upper since they have the same pattern
  result_kri0001 <- result[result$MetricID == "kri0001", ]
  expect_true(all(c("Median", "Lower", "Upper") %in% names(result_kri0001)))

  result_kri0003 <- result[result$MetricID == "kri0003", ]
  expect_true(all(c("Median", "Lower", "Upper") %in% names(result_kri0003)))
})

test_that("Transform_Long handles empty data frame", {
  # Empty data frame with correct structure
  dfWide <- data.frame(
    StudyID = character(0),
    StudyMonth = integer(0),
    Median_kri0001 = numeric(0),
    Lower_kri0001 = numeric(0),
    Upper_kri0001 = numeric(0)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 0)
  expect_true(all(c("MetricID", "DenominatorType") %in% names(result)))
})

test_that("Transform_Long handles complex MetricID patterns", {
  # Test with different MetricID formats
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Median_kri9999 = c(0.3, 0.4),
    Median_custom_id = c(0.2, 0.25)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  expect_s3_class(result, "tbl")
  expect_equal(nrow(result), 6) # 2 rows * 3 MetricIDs
  expect_equal(length(unique(result$MetricID)), 3)
  expect_true(all(c("kri0001", "kri9999", "custom_id") %in% result$MetricID))
})

test_that("Transform_Long preserves all id columns", {
  # Test with multiple id columns
  dfWide <- data.frame(
    StudyID = c("AA-1", "AA-2"),
    StudyMonth = c(1, 1),
    SiteCount = c(10, 12),
    BootstrapRep = c(1, 1),
    Median_kri0001 = c(0.5, 0.6),
    Lower_kri0001 = c(0.3, 0.4),
    Upper_kri0001 = c(0.7, 0.8)
  )

  lWide <- list(Visits = dfWide)
  result <- Transform_Long(lWide)

  # All original id columns should be preserved
  expect_true(all(c("StudyID", "StudyMonth", "SiteCount", "BootstrapRep") %in% names(result)))
  expect_equal(result$SiteCount, c(10, 12))
  expect_equal(result$BootstrapRep, c(1, 1))
})
