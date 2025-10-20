test_that("Transform_CumCount returns correct structure", {
  # Create simple input data
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(all(c("StudyID", "MonthYYYYMM", "StudyMonth", "Numerator", 
                    "Denominator", "Metric", "GroupCount") %in% names(result)))
})

test_that("Transform_CumCount aggregates across sites correctly", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # Should sum numerators across sites for each month
  # Month 202301: 5 + 3 = 8, Month 202302: 10 + 8 = 18, Month 202303: 15 + 12 = 27
  expect_equal(result$Numerator[result$MonthYYYYMM == 202302], 18)
  expect_equal(result$Numerator[result$MonthYYYYMM == 202303], 27)
  
  # Should sum denominators across sites
  expect_equal(result$Denominator[result$MonthYYYYMM == 202302], 40)
  expect_equal(result$Denominator[result$MonthYYYYMM == 202303], 60)
  
  # Should count sites
  expect_equal(result$GroupCount[1], 2)
})

test_that("Transform_CumCount creates sequential StudyMonth", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # After filtering (only months with > 25 denominator), should be sequential 1, 2, ...
  expect_equal(result$StudyMonth, seq_len(nrow(result)))
  
  # StudyMonth should start at 1
  expect_equal(min(result$StudyMonth), 1)
})

test_that("Transform_CumCount applies minimum denominator filter", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 9),
    GroupID = rep(c("SITE01", "SITE02", "SITE03"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 3),
    Numerator = rep(c(2, 5, 10), 3),
    Denominator = rep(c(5, 15, 30), 3),
    Metric = rep(c(0.4, 0.33, 0.33), 3)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # Month 202301: 3 sites * 5 = 15 denominator (< 25, should be filtered)
  # Month 202302: 3 sites * 15 = 45 denominator (> 25, should be kept)
  # Month 202303: 3 sites * 30 = 90 denominator (> 25, should be kept)
  
  expect_false(202301 %in% result$MonthYYYYMM)
  expect_true(202302 %in% result$MonthYYYYMM)
  expect_true(202303 %in% result$MonthYYYYMM)
  expect_equal(nrow(result), 2)
})

test_that("Transform_CumCount calculates Metric correctly", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # Month 202302: (10 + 8) / (20 + 20) = 18/40 = 0.45
  # Month 202303: (15 + 12) / (30 + 30) = 27/60 = 0.45
  
  expect_equal(result$Metric[result$MonthYYYYMM == 202302], 18/40)
  expect_equal(result$Metric[result$MonthYYYYMM == 202303], 27/60)
})

test_that("Transform_CumCount handles multiple grouping columns", {
  dfInput <- data.frame(
    StudyID = rep(c("STUDY001", "STUDY002"), each = 6),
    BootstrapRep = rep(rep(1:2, each = 3), 2),
    GroupID = rep(c("SITE01", "SITE02"), each = 3, times = 2),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 4),
    Numerator = rep(c(10, 15, 20), 4),
    Denominator = rep(c(20, 30, 40), 4),
    Metric = rep(c(0.5, 0.5, 0.5), 4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = c("StudyID", "BootstrapRep"),
    nMinDenominator = 25
  )
  
  expect_true("StudyID" %in% names(result))
  expect_true("BootstrapRep" %in% names(result))
  
  # Should have results for each combination of StudyID and BootstrapRep
  expect_true(nrow(result) > 0)
  
  # Check that StudyMonth restarts for each group
  study1_rep1 <- result[result$StudyID == "STUDY001" & result$BootstrapRep == 1, ]
  expect_equal(min(study1_rep1$StudyMonth), 1)
})

test_that("Transform_CumCount handles single site", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 3),
    GroupID = rep("SITE01", 3),
    GroupLevel = "Site",
    MonthYYYYMM = c(202301, 202302, 202303),
    Numerator = c(10, 15, 20),
    Denominator = c(20, 30, 40),
    Metric = c(0.5, 0.5, 0.5)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_equal(result$GroupCount[1], 1)
})

test_that("Transform_CumCount handles zero denominators", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 0, 30, 10, 0, 30),
    Metric = c(0.5, NA, 0.5, 0.3, NA, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 5
  )
  
  # Month with zero total denominator should have NA metric (though filtered by > threshold)
  # This tests the if_else logic for zero denominators
  expect_type(result$Metric, "double")
})

test_that("Transform_CumCount validates dfInput parameter", {
  expect_error(
    Transform_CumCount(dfInput = "not a data frame", vBy = "StudyID"),
    "dfInput must be a data frame"
  )
  
  dfInput <- data.frame(
    StudyID = "STUDY001",
    GroupID = "SITE01"
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "StudyID"),
    "dfInput missing required columns"
  )
})

test_that("Transform_CumCount validates vBy parameter", {
  dfInput <- data.frame(
    StudyID = "STUDY001",
    GroupID = "SITE01",
    MonthYYYYMM = 202301,
    Numerator = 10,
    Denominator = 20,
    Metric = 0.5
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = 123),
    "vBy must be a non-empty character vector"
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = character(0)),
    "vBy must be a non-empty character vector"
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "NotAColumn"),
    "vBy columns not found in dfInput"
  )
})

test_that("Transform_CumCount validates nMinDenominator parameter", {
  dfInput <- data.frame(
    StudyID = "STUDY001",
    GroupID = "SITE01",
    MonthYYYYMM = 202301,
    Numerator = 10,
    Denominator = 30,
    Metric = 0.33
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "StudyID", nMinDenominator = "25"),
    "nMinDenominator must be a single non-negative numeric value"
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "StudyID", nMinDenominator = -5),
    "nMinDenominator must be a single non-negative numeric value"
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "StudyID", nMinDenominator = c(10, 20)),
    "nMinDenominator must be a single non-negative numeric value"
  )
})

test_that("Transform_CumCount handles empty data frame", {
  dfInput <- data.frame(
    StudyID = character(0),
    GroupID = character(0),
    MonthYYYYMM = numeric(0),
    Numerator = numeric(0),
    Denominator = numeric(0),
    Metric = numeric(0)
  )
  
  expect_error(
    Transform_CumCount(dfInput = dfInput, vBy = "StudyID"),
    "dfInput has no rows"
  )
})

test_that("Transform_CumCount filters all data when threshold too high", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(1, 2, 3, 1, 2, 3),
    Denominator = c(5, 10, 15, 5, 10, 15),
    Metric = c(0.2, 0.2, 0.2, 0.2, 0.2, 0.2)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 100
  )
  
  # All data should be filtered out
  expect_equal(nrow(result), 0)
})

test_that("Transform_CumCount preserves MonthYYYYMM in output", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  expect_true("MonthYYYYMM" %in% names(result))
  expect_true(all(result$MonthYYYYMM %in% c(202302, 202303)))
})

test_that("Transform_CumCount works with actual Input_CumCountSiteByMonth output", {
  # Use actual clindata to test integration
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  dfInput <- Input_CumCountSiteByMonth(
    dfSubjects = dfSubjects,
    dfNumerator = dfNumerator,
    dfDenominator = dfDenominator,
    strNumeratorDateCol = "aest_dt",
    strDenominatorDateCol = "visit_dt"
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(c("StudyID", "MonthYYYYMM", "StudyMonth", "Numerator",
                    "Denominator", "Metric", "GroupCount") %in% names(result)))
  
  # StudyMonth should be sequential starting from 1 within each study
  for (study in unique(result$StudyID)) {
    study_data <- result[result$StudyID == study, ]
    expect_equal(min(study_data$StudyMonth), 1)
    expect_equal(max(study_data$StudyMonth), nrow(study_data))
    expect_equal(study_data$StudyMonth, seq_len(nrow(study_data)))
  }
  
  # All denominators should be > 25
  expect_true(all(result$Denominator > 25))
  
  # Metric should match manual calculation
  expect_equal(
    result$Metric,
    result$Numerator / result$Denominator
  )
})

test_that("Transform_CumCount handles non-consecutive calendar months", {
  # Test with gaps in calendar months (e.g., 202301, 202303, skipping 202302)
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202303, 202305), 2),  # Non-consecutive
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # StudyMonth should still be sequential 1, 2, 3... despite gaps in calendar months
  expect_equal(result$StudyMonth, seq_len(nrow(result)))
  
  # MonthYYYYMM should preserve the original calendar months
  expect_true(all(result$MonthYYYYMM %in% c(202301, 202303, 202305)))
})

