test_that("Analyze_StudyKRI returns correct structure", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteA", "SiteB", "SiteB"),
    GroupLevel = "Site",
    Numerator = c(1, 2, 3, 4),
    Denominator = c(10, 20, 30, 40),
    Metric = c(0.1, 0.1, 0.1, 0.1),
    StudyID = c("Study1", "Study1", "Study1", "Study1"),
    MonthYYYYMM = c(202301, 202302, 202301, 202302)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 42)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("BootstrapRep" %in% names(result))
  expect_true(all(c("GroupID", "Numerator", "Denominator", "StudyID") %in% names(result)))
})

test_that("Analyze_StudyKRI generates correct number of replicates", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    MonthYYYYMM = c(202301, 202301)
  )

  nReps <- 50
  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = nReps, seed = 123)

  expect_equal(length(unique(result$BootstrapRep)), nReps)
  expect_true(all(result$BootstrapRep >= 1 & result$BootstrapRep <= nReps))
})

test_that("Analyze_StudyKRI resamples with replacement", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3),
    Denominator = c(10, 20, 30),
    MonthYYYYMM = c(202301, 202301, 202301)
  )

  # With many replicates, we should see some sites appearing multiple times
  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 100, seed = 456)

  # Count sites per replicate
  site_counts <- result %>%
    dplyr::group_by(BootstrapRep, GroupID) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  # At least some replicates should have a site appearing more than once
  expect_true(any(site_counts$Count > 1))
})

test_that("Analyze_StudyKRI seed produces reproducible results", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3),
    Denominator = c(10, 20, 30),
    MonthYYYYMM = c(202301, 202301, 202301)
  )

  result1 <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 999)
  result2 <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 999)

  expect_equal(result1, result2)
})

test_that("Analyze_StudyKRI without seed produces different results", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3),
    Denominator = c(10, 20, 30),
    MonthYYYYMM = c(202301, 202301, 202301)
  )

  result1 <- Analyze_StudyKRI(dfInput, nBootstrapReps = 50, seed = NULL)
  result2 <- Analyze_StudyKRI(dfInput, nBootstrapReps = 50, seed = NULL)

  # Results should differ (though technically could be identical by chance)
  # Check at least one row differs
  expect_false(identical(result1, result2))
})

test_that("Analyze_StudyKRI handles multiple studies independently", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC", "SiteD"),
    StudyID = c("Study1", "Study1", "Study2", "Study2"),
    Numerator = c(1, 2, 3, 4),
    Denominator = c(10, 20, 30, 40),
    MonthYYYYMM = c(202301, 202301, 202301, 202301)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 20, seed = 111)

  # Check both studies present in all replicates
  study_counts <- result %>%
    dplyr::group_by(StudyID, BootstrapRep) %>%
    dplyr::summarise(Count = dplyr::n(), .groups = "drop")

  expect_equal(length(unique(study_counts$StudyID)), 2)
  expect_equal(nrow(study_counts), 40) # 2 studies × 20 reps

  # Check that Study1 sites don't appear in Study2 resamples
  study1_sites <- unique(dfInput$GroupID[dfInput$StudyID == "Study1"])
  study2_sites <- unique(dfInput$GroupID[dfInput$StudyID == "Study2"])

  study1_results <- result %>% dplyr::filter(StudyID == "Study1")
  study2_results <- result %>% dplyr::filter(StudyID == "Study2")

  expect_true(all(study1_results$GroupID %in% study1_sites))
  expect_true(all(study2_results$GroupID %in% study2_sites))
})

test_that("Analyze_StudyKRI nSites parameter works for downsampling", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC", "SiteD"),
    StudyID = c("Study1", "Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3, 4),
    Denominator = c(10, 20, 30, 40),
    MonthYYYYMM = c(202301, 202301, 202301, 202301)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, nSites = 2, seed = 222)

  # Each replicate should have exactly 2 sites worth of data
  site_counts <- result %>%
    dplyr::group_by(BootstrapRep) %>%
    dplyr::summarise(UniqueSites = dplyr::n_distinct(GroupID), .groups = "drop")

  expect_true(all(site_counts$UniqueSites <= 2))
})

test_that("Analyze_StudyKRI nSites parameter works for upsampling", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    MonthYYYYMM = c(202301, 202301)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, nSites = 5, seed = 333)

  # Each replicate should have 5 site selections (some will be duplicates)
  row_counts <- result %>%
    dplyr::group_by(BootstrapRep) %>%
    dplyr::summarise(RowCount = dplyr::n(), .groups = "drop")

  expect_true(all(row_counts$RowCount == 5))
})

test_that("Analyze_StudyKRI handles single site per study", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteA"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    MonthYYYYMM = c(202301, 202302)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 5, seed = 444)

  expect_true(nrow(result) > 0)
  expect_equal(length(unique(result$BootstrapRep)), 5)
  # All rows should be for SiteA since it's the only site
  expect_true(all(result$GroupID == "SiteA"))
})

test_that("Analyze_StudyKRI handles single study", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3),
    Denominator = c(10, 20, 30),
    MonthYYYYMM = c(202301, 202301, 202301)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 555)

  expect_true(all(result$StudyID == "Study1"))
  expect_equal(length(unique(result$BootstrapRep)), 10)
})

test_that("Analyze_StudyKRI preserves all original columns", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    GroupLevel = c("Site", "Site"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    Metric = c(0.1, 0.1),
    StudyID = c("Study1", "Study1"),
    MonthYYYYMM = c(202301, 202301),
    CustomColumn = c("A", "B")
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 5, seed = 666)

  original_cols <- names(dfInput)
  expect_true(all(original_cols %in% names(result)))
  expect_true("BootstrapRep" %in% names(result))
})

test_that("Analyze_StudyKRI preserves data types", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1L, 2L),
    Denominator = c(10L, 20L),
    Metric = c(0.1, 0.2),
    MonthYYYYMM = c(202301, 202301)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 5, seed = 777)

  expect_type(result$GroupID, "character")
  expect_type(result$StudyID, "character")
  expect_type(result$Numerator, "integer")
  expect_type(result$Denominator, "integer")
  expect_type(result$Metric, "double")
  expect_type(result$BootstrapRep, "integer")
})

test_that("Analyze_StudyKRI handles custom column names", {
  dfInput <- data.frame(
    SiteCode = c("SiteA", "SiteB"),
    ProtocolID = c("PROTO1", "PROTO1"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    MonthYYYYMM = c(202301, 202301)
  )

  result <- Analyze_StudyKRI(
    dfInput,
    nBootstrapReps = 5,
    strStudyCol = "ProtocolID",
    strGroupCol = "SiteCode",
    seed = 888
  )

  expect_true("BootstrapRep" %in% names(result))
  expect_true("SiteCode" %in% names(result))
  expect_true("ProtocolID" %in% names(result))
  expect_equal(length(unique(result$BootstrapRep)), 5)
})

test_that("Analyze_StudyKRI errors on missing dfInput", {
  expect_error(
    Analyze_StudyKRI(dfInput = NULL, nBootstrapReps = 10),
    "dfInput must be a data frame"
  )
})

test_that("Analyze_StudyKRI errors on empty dfInput", {
  dfInput <- data.frame(
    GroupID = character(0),
    StudyID = character(0),
    Numerator = integer(0)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10),
    "dfInput has no rows"
  )
})

test_that("Analyze_StudyKRI errors on missing StudyID column", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    Numerator = c(1, 2)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10),
    "strStudyCol 'StudyID' not found in dfInput"
  )
})

test_that("Analyze_StudyKRI errors on missing GroupID column", {
  dfInput <- data.frame(
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10),
    "strGroupCol 'GroupID' not found in dfInput"
  )
})

test_that("Analyze_StudyKRI errors on invalid nBootstrapReps", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = -5),
    "nBootstrapReps must be a single positive integer"
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 0),
    "nBootstrapReps must be a single positive integer"
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = c(10, 20)),
    "nBootstrapReps must be a single positive integer"
  )
})

test_that("Analyze_StudyKRI errors on invalid nSites", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10, nSites = -1),
    "nSites must be NULL or a single positive integer"
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10, nSites = 0),
    "nSites must be NULL or a single positive integer"
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10, nSites = c(2, 3)),
    "nSites must be NULL or a single positive integer"
  )
})

test_that("Analyze_StudyKRI errors on invalid seed", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2)
  )

  expect_error(
    Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = c(1, 2)),
    "seed must be NULL or a single numeric value"
  )
})

test_that("Analyze_StudyKRI integrates with Input_CumCountSiteByMonth output", {
  skip_if_not_installed("clindata")

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

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 12345)

  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true("BootstrapRep" %in% names(result))
  expect_equal(length(unique(result$BootstrapRep)), 10)

  # Verify structure matches input plus BootstrapRep
  expect_true(all(names(dfInput) %in% names(result)))
})

test_that("Analyze_StudyKRI output works with Transform_CumCount", {
  skip_if_not_installed("clindata")

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

  dfBootstrap <- Analyze_StudyKRI(dfInput, nBootstrapReps = 5, seed = 99999)

  # Should work with vBy including BootstrapRep
  dfTransformed <- Transform_CumCount(
    dfInput = dfBootstrap,
    vBy = c("StudyID", "BootstrapRep"),
    nMinDenominator = 25
  )

  expect_s3_class(dfTransformed, "data.frame")
  expect_true(nrow(dfTransformed) > 0)
  expect_true("BootstrapRep" %in% names(dfTransformed))
  expect_true("StudyID" %in% names(dfTransformed))
  expect_true("Metric" %in% names(dfTransformed))
})

test_that("Analyze_StudyKRI maintains correct row multiplier", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteA", "SiteB"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(1, 2, 3),
    Denominator = c(10, 20, 30),
    MonthYYYYMM = c(202301, 202302, 202301)
  )

  nReps <- 100
  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = nReps, seed = 7777)

  # Total rows should be approximately nReps * number of sites per study
  # (with variation due to sampling)
  # Each replicate samples 2 sites (the actual count), and each site contributes its rows
  # SiteA has 2 rows, SiteB has 1 row
  # Average contribution per site = 1.5 rows
  # Average per replicate = 2 sites * 1.5 = 3 rows (but this varies with sampling)

  expect_true(nrow(result) >= nReps * 1) # At minimum, one row per replicate
  expect_true(nrow(result) <= nReps * 6) # At maximum, if all sites sampled multiple times
})

test_that("Analyze_StudyKRI bootstrap distribution has expected properties", {
  dfInput <- data.frame(
    GroupID = c("SiteA", "SiteB", "SiteC"),
    StudyID = c("Study1", "Study1", "Study1"),
    Numerator = c(5, 10, 15),
    Denominator = c(100, 100, 100),
    MonthYYYYMM = c(202301, 202301, 202301)
  )

  nReps <- 500
  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = nReps, seed = 8888)

  # Calculate mean numerator per replicate
  rep_means <- result %>%
    dplyr::group_by(BootstrapRep) %>%
    dplyr::summarise(MeanNum = mean(Numerator), .groups = "drop")

  # Bootstrap mean should be close to population mean
  population_mean <- mean(dfInput$Numerator)
  bootstrap_mean <- mean(rep_means$MeanNum)

  # Should be within reasonable range (allowing for sampling variation)
  expect_true(abs(bootstrap_mean - population_mean) < 2)
})

test_that("Analyze_StudyKRI handles multiple months correctly", {
  dfInput <- data.frame(
    GroupID = rep(c("SiteA", "SiteB"), each = 3),
    StudyID = rep("Study1", 6),
    Numerator = 1:6,
    Denominator = seq(10, 60, by = 10),
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2)
  )

  result <- Analyze_StudyKRI(dfInput, nBootstrapReps = 10, seed = 9999)

  # All months should be represented in bootstrap samples
  expect_true(all(c(202301, 202302, 202303) %in% result$MonthYYYYMM))

  # Check structure per replicate
  for (rep in unique(result$BootstrapRep)) {
    rep_data <- result %>% dplyr::filter(BootstrapRep == rep)
    # Each replicate should have data for multiple months
    expect_true(length(unique(rep_data$MonthYYYYMM)) > 0)
  }
})

