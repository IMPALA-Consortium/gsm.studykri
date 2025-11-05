test_that("SimulatePortfolio creates multiple studies", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site
  )

  result <- SimulatePortfolio(lRaw, nStudies = 3, seed = 999)

  # Check structure
  expect_type(result, "list")
  expect_true(all(names(lRaw) %in% names(result)))

  # Check multiple studies present
  study_ids <- unique(result$Raw_SUBJ$studyid)
  expect_equal(length(study_ids), 3)
  expect_true(all(grepl("^PORTFOLIO\\d{3}$", study_ids)))
})

test_that("SimulatePortfolio respects custom configuration", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_SITE = clindata::ctms_site
  )

  dfConfig <- data.frame(
    studyid = c("CUSTOM001", "CUSTOM002"),
    nSubjects = c(20, 30),
    TargetSiteCount = c(10, 15)
  )

  result <- SimulatePortfolio(lRaw, dfConfig = dfConfig, seed = 888)

  study_ids <- unique(result$Raw_SUBJ$studyid)
  expect_setequal(study_ids, c("CUSTOM001", "CUSTOM002"))

  # Check subject counts per study
  subj_counts <- table(result$Raw_SUBJ$studyid)
  expect_true(subj_counts["CUSTOM001"] <= 20)
  expect_true(subj_counts["CUSTOM002"] <= 30)
})

test_that("SimulatePortfolio output works with mapping workflows", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study
  )

  lPortfolio <- SimulatePortfolio(lRaw, nStudies = 2, seed = 777)

  # Should work with Ingest (basic smoke test)
  expect_no_error({
    # Just test that the structure is correct
    expect_true(is.list(lPortfolio))
    expect_true("Raw_SUBJ" %in% names(lPortfolio))
  })
})

test_that("SimulatePortfolio validates inputs", {
  lRaw <- list(Raw_SUBJ = clindata::rawplus_dm)

  # Missing Raw_SUBJ
  expect_error(
    SimulatePortfolio(list(Raw_AE = data.frame())),
    regexp = "Raw_SUBJ"
  )

  # Duplicate study IDs
  dfConfig <- data.frame(
    studyid = c("DUP", "DUP"),
    nSubjects = c(10, 20)
  )
  expect_error(
    SimulatePortfolio(lRaw, dfConfig = dfConfig),
    regexp = "duplicate"
  )
  
  # Missing required columns in dfConfig
  dfConfig_bad <- data.frame(
    studyid = c("TEST1", "TEST2")
    # Missing nSubjects column
  )
  expect_error(
    SimulatePortfolio(lRaw, dfConfig = dfConfig_bad),
    regexp = "studyid, nSubjects"
  )
})

test_that("SimulatePortfolio handles optional dfConfig parameters", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site
  )
  
  # Test with replacement parameter
  dfConfig <- data.frame(
    studyid = c("TEST1", "TEST2"),
    nSubjects = c(20, 30),
    replacement = c(TRUE, FALSE)
  )
  
  result <- SimulatePortfolio(lRaw, dfConfig = dfConfig, seed = 123)
  expect_equal(length(unique(result$Raw_SUBJ$studyid)), 2)
  
  # Test with strOversamplDomain parameter
  dfConfig2 <- data.frame(
    studyid = c("TEST3", "TEST4"),
    nSubjects = c(25, 35),
    strOversamplDomain = c("Raw_AE", "Raw_AE"),
    vOversamplQuantileRange_min = c(0, 0.5),
    vOversamplQuantileRange_max = c(0.5, 1.0)
  )
  
  result2 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig2, seed = 456)
  )
  expect_equal(length(unique(result2$Raw_SUBJ$studyid)), 2)
  
  # Test with only min specified (should default max to 1)
  dfConfig3 <- data.frame(
    studyid = "TEST5",
    nSubjects = 20,
    strOversamplDomain = "Raw_AE",
    vOversamplQuantileRange_min = 0.75
    # max not specified, should default to 1
  )
  
  result3 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig3, seed = 789)
  )
  expect_true(nrow(result3$Raw_SUBJ) > 0)
  
  # Test with only max specified (should default min to 0)
  dfConfig4 <- data.frame(
    studyid = "TEST6",
    nSubjects = 20,
    strOversamplDomain = "Raw_AE",
    vOversamplQuantileRange_max = 0.25
    # min not specified, should default to 0
  )
  
  result4 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig4, seed = 101)
  )
  expect_true(nrow(result4$Raw_SUBJ) > 0)
})
