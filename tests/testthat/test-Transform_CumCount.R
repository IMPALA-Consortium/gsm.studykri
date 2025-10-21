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

test_that("Transform_CumCount aggregates across sites and calculates cumulative counts correctly", {
  # Input is monthly counts per site
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),  # Monthly counts per site
    Denominator = c(10, 20, 30, 10, 20, 30),  # Monthly counts per site
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # After aggregation and cumsum:
  # Month 202301: (5+3)=8 -> cumsum=8 (but filtered out, < 25 denominator)
  # Month 202302: (10+8)=18 -> cumsum=8+18=26 (first month in result)
  # Month 202303: (15+12)=27 -> cumsum=8+18+27=53 (second month)
  
  # First month (202302) should have cumulative sum including 202301
  expect_equal(result$Numerator[result$MonthYYYYMM == 202302], 8 + 18)
  # Second month (202303) should have cumulative sum of all
  expect_equal(result$Numerator[result$MonthYYYYMM == 202303], 8 + 18 + 27)
  
  # Same for denominators
  expect_equal(result$Denominator[result$MonthYYYYMM == 202302], 20 + 40)
  expect_equal(result$Denominator[result$MonthYYYYMM == 202303], 20 + 40 + 60)
  
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

test_that("Transform_CumCount calculates Metric correctly with cumulative counts", {
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2),
    Numerator = c(5, 10, 15, 3, 8, 12),  # Monthly counts
    Denominator = c(10, 20, 30, 10, 20, 30),  # Monthly counts
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 25
  )
  
  # Cumulative totals after aggregation:
  # Month 202302: cumsum(8, 18) = 26 numerator, cumsum(20, 40) = 60 denominator
  # Month 202303: cumsum(8, 18, 27) = 53 numerator, cumsum(20, 40, 60) = 120 denominator
  
  expect_equal(result$Metric[result$MonthYYYYMM == 202302], 26/60)
  expect_equal(result$Metric[result$MonthYYYYMM == 202303], 53/120)
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

test_that("Transform_CumCount works with actual Input_CountSiteByMonth output", {
  # Use actual clindata to test integration
  dfSubjects <- clindata::rawplus_dm
  dfNumerator <- clindata::rawplus_ae
  dfDenominator <- clindata::rawplus_visdt
  
  dfInput <- Input_CountSiteByMonth(
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

test_that("Transform_CumCount fills gaps in calendar months with zeros", {
  # Test with gaps in calendar months (e.g., 202301, 202303, 202305 skipping Feb and Apr)
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 6),
    GroupID = rep(c("SITE01", "SITE02"), each = 3),
    GroupLevel = "Site",
    MonthYYYYMM = rep(c(202301, 202303, 202305), 2),  # Non-consecutive (missing Feb and Apr)
    Numerator = c(5, 10, 15, 3, 8, 12),
    Denominator = c(10, 20, 30, 10, 20, 30),
    Metric = c(0.5, 0.5, 0.5, 0.3, 0.4, 0.4)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 0  # Don't filter to see all months including filled gaps
  )
  
  # Should have all months from Jan to May: 202301, 202302, 202303, 202304, 202305
  expect_equal(nrow(result), 5)
  expect_true(all(c(202301, 202302, 202303, 202304, 202305) %in% result$MonthYYYYMM))
  
  # StudyMonth should be 1, 2, 3, 4, 5 (not compressed to 1, 2, 3)
  expect_equal(result$StudyMonth, 1:5)
  
  # Months with original data should have aggregated counts
  expect_equal(result$Numerator[result$MonthYYYYMM == 202301], 8)  # 5 + 3
  expect_equal(result$Numerator[result$MonthYYYYMM == 202303], 8 + 18)  # cumsum(8, 0, 18)
  
  # Gap months (202302, 202304) should have cumulative values maintained
  expect_equal(result$Numerator[result$MonthYYYYMM == 202302], 8)  # Same as 202301 (no new data)
  expect_equal(result$Numerator[result$MonthYYYYMM == 202304], 8 + 18)  # Same as 202303 (no new data)
  
  # Final month should have all cumulative data
  expect_equal(result$Numerator[result$MonthYYYYMM == 202305], 8 + 18 + 27)
})

test_that("Cumulative counts are monotonically increasing within each study", {
  # This test validates the core fix: when sites drop out, cumulative counts persist
  # Create realistic scenario with sites dropping in/out
  dfInput <- data.frame(
    StudyID = rep(c("STUDY001", "STUDY002"), each = 12),
    GroupID = c(
      # Study 1: Site 1 all months, Site 2 stops after month 2, Site 3 starts month 3
      rep("SITE01", 4), rep("SITE02", 2), rep("SITE03", 2), rep("SITE04", 4),
      # Study 2: Similar pattern
      rep("SITEA", 4), rep("SITEB", 2), rep("SITEC", 2), rep("SITED", 4)
    ),
    GroupLevel = "Site",
    MonthYYYYMM = c(
      # Study 1 months
      202301, 202302, 202303, 202304,  # Site 1
      202301, 202302,                   # Site 2 (drops out)
      202303, 202304,                   # Site 3 (starts late)
      202301, 202302, 202303, 202304,  # Site 4
      # Study 2 months
      202301, 202302, 202303, 202304,  # Site A
      202301, 202302,                   # Site B (drops out)
      202303, 202304,                   # Site C (starts late)
      202301, 202302, 202303, 202304   # Site D
    ),
    Numerator = rep(c(10, 15, 12, 18), 6),      # Monthly counts
    Denominator = rep(c(20, 30, 25, 35), 6),    # Monthly counts
    Metric = rep(c(0.5, 0.5, 0.48, 0.51), 6)
  )
  
  # Transform to study level with cumulative counts
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 0  # Don't filter any data
  )
  
  # Check each study has monotonically increasing (non-decreasing) cumulative counts
  for (study_id in unique(result$StudyID)) {
    study_data <- result %>%
      dplyr::filter(.data$StudyID == study_id) %>%
      dplyr::arrange(.data$StudyMonth)
    
    # Numerator should be non-decreasing (each value >= previous value)
    if (nrow(study_data) > 1) {
      numerator_diffs <- diff(study_data$Numerator)
      expect_true(
        all(numerator_diffs >= 0),
        info = sprintf("Study %s: Numerator not monotonically increasing. Diffs: %s",
                      study_id, paste(numerator_diffs, collapse = ", "))
      )
      
      # Denominator should be non-decreasing (each value >= previous value)
      denominator_diffs <- diff(study_data$Denominator)
      expect_true(
        all(denominator_diffs >= 0),
        info = sprintf("Study %s: Denominator not monotonically increasing. Diffs: %s",
                      study_id, paste(denominator_diffs, collapse = ", "))
      )
    }
  }
  
  # Verify we have data for both studies
  expect_equal(length(unique(result$StudyID)), 2)
  expect_true(all(c("STUDY001", "STUDY002") %in% result$StudyID))
})

test_that("Cumulative counts persist when sites drop out (integration test)", {
  # Create scenario that would fail with old implementation
  # Site 1 reports in months 1-3, Site 2 reports only in months 1-2
  dfInput <- data.frame(
    StudyID = rep("STUDY001", 5),
    GroupID = c("SITE01", "SITE01", "SITE01", "SITE02", "SITE02"),
    GroupLevel = "Site",
    MonthYYYYMM = c(202301, 202302, 202303, 202301, 202302),
    Numerator = c(10, 5, 8, 20, 10),      # Monthly counts
    Denominator = c(100, 50, 80, 200, 100), # Monthly counts
    Metric = c(0.1, 0.1, 0.1, 0.1, 0.1)
  )
  
  result <- Transform_CumCount(
    dfInput = dfInput,
    vBy = "StudyID",
    nMinDenominator = 0
  )
  
  # Month 1: Site1=10 + Site2=20 = 30 (cumsum=30)
  # Month 2: Site1=5 + Site2=10 = 15 (cumsum=45)
  # Month 3: Site1=8 + Site2=0 = 8 (cumsum=53) <- Site2's contribution persists!
  
  result_ordered <- result[order(result$MonthYYYYMM), ]
  
  # Verify cumulative sums are correct and increasing
  expect_equal(result_ordered$Numerator[1], 30)    # Month 1
  expect_equal(result_ordered$Numerator[2], 45)    # Month 2
  expect_equal(result_ordered$Numerator[3], 53)    # Month 3 - includes inactive Site2!
  
  expect_equal(result_ordered$Denominator[1], 300)   # Month 1
  expect_equal(result_ordered$Denominator[2], 450)   # Month 2
  expect_equal(result_ordered$Denominator[3], 530)   # Month 3
})

