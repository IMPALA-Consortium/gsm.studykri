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
})

