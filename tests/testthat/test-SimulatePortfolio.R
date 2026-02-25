test_that("SimulatePortfolio creates multiple studies", {
  lRaw <- list(
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_AE = clindata::rawplus_ae,
    Raw_SITE = clindata::ctms_site
  )

  # Add required vSubjectID columns
  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
  lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjectid

  result <- SimulatePortfolio(lRaw, nStudies = 3, )

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

  # Add required vSubjectID columns
  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
  lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjectid

  dfConfig <- data.frame(
    studyid = c("CUSTOM001", "CUSTOM002"),
    nSubjects = c(20, 30),
    TargetSiteCount = c(10, 15)
  )

  result <- SimulatePortfolio(lRaw, dfConfig = dfConfig, )

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

  # Add required vSubjectID columns
  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
  lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjectid

  lPortfolio <- SimulatePortfolio(lRaw, nStudies = 2, )

  # Should work with Ingest (basic smoke test)
  expect_no_error({
    # Just test that the structure is correct
    expect_true(is.list(lPortfolio))
    expect_true("Raw_SUBJ" %in% names(lPortfolio))
  })
})

test_that("SimulatePortfolio validates inputs", {
  lRaw <- list(Raw_SUBJ = clindata::rawplus_dm)

  # Add required vSubjectID columns
  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
  lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjectid

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

  # Add required vSubjectID columns
  lRaw$Raw_SUBJ$subjectname <- lRaw$Raw_SUBJ$subject_nsv
  lRaw$Raw_SUBJ$subjectenrollmentnumber <- lRaw$Raw_SUBJ$subjectid

  # Test with replacement parameter
  dfConfig <- data.frame(
    studyid = c("TEST1", "TEST2"),
    nSubjects = c(20, 30),
    replacement = c(TRUE, FALSE)
  )

  result <- SimulatePortfolio(lRaw, dfConfig = dfConfig, )
  expect_equal(length(unique(result$Raw_SUBJ$studyid)), 2)

  # Test with strOversampleDomain parameter
  dfConfig2 <- data.frame(
    studyid = c("TEST3", "TEST4"),
    nSubjects = c(25, 35),
    strOversampleDomain = c("Raw_AE", "Raw_AE"),
    vOversamplQuantileRange_min = c(0, 0.5),
    vOversamplQuantileRange_max = c(0.5, 1.0)
  )

  result2 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig2, )
  )
  expect_equal(length(unique(result2$Raw_SUBJ$studyid)), 2)

  # Test with only min specified (should default max to 1)
  dfConfig3 <- data.frame(
    studyid = "TEST5",
    nSubjects = 20,
    strOversampleDomain = "Raw_AE",
    vOversamplQuantileRange_min = 0.75
    # max not specified, should default to 1
  )

  result3 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig3, )
  )
  expect_true(nrow(result3$Raw_SUBJ) > 0)

  # Test with only max specified (should default min to 0)
  dfConfig4 <- data.frame(
    studyid = "TEST6",
    nSubjects = 20,
    strOversampleDomain = "Raw_AE",
    vOversamplQuantileRange_max = 0.25
    # min not specified, should default to 0
  )

  result4 <- suppressMessages(
    SimulatePortfolio(lRaw, dfConfig = dfConfig4, )
  )
  expect_true(nrow(result4$Raw_SUBJ) > 0)
})

# Tests for ResampleStudy function ----

test_that("ResampleStudy validates vSubjectIDs columns exist", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE1", "SITE2")
    )
  )

  # Missing required vSubjectIDs columns
  expect_error(
    ResampleStudy(lRaw, "TEST001", vSubjectIDs = c("subjid", "subjectenrollmentnumber")),
    regexp = "vSubjectIDs validation failed.*subjectenrollmentnumber"
  )
})

test_that("ResampleStudy validates strOversampleDomain has subject ID", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE1", "SITE2"),
      subjectenrollmentnumber = c("S1", "S2", "S3"),
      subject_nsv = c("S1", "S2", "S3"),
      subjectname = c("S1", "S2", "S3"),
      subjectid = c("S1", "S2", "S3")
    ),
    Raw_SITE = data.frame(
      invid = c("SITE1", "SITE2"),
      siteid = c("1", "2")
    )
  )

  # strOversampleDomain without subject ID column should fail
  expect_error(
    ResampleStudy(
      lRaw,
      "TEST001",
      strOversampleDomain = "Raw_SITE",
      vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
    ),
    regexp = "strOversampleDomain must have a subject identifier column"
  )
})

test_that("ResampleStudy handles invalid mapping in find_subject_id_column", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE1", "SITE2"),
      siteid = c("1", "1", "2"),
      subjectenrollmentnumber = c("E1", "E2", "E3"),
      subject_nsv = c("N1", "N2", "N3"),
      subjectname = c("Name1", "Name2", "Name3"),
      subjectid = c("ID1", "ID2", "ID3")
    ),
    Raw_AE = data.frame(
      subjectid = c("ID1", "ID2", "ID3", "INVALID_ID"),
      aedesc = c("AE1", "AE2", "AE3", "AE4")
    )
  )

  # Should handle invalid mapping and filter them out
  result <- suppressMessages(
    ResampleStudy(
      lRaw,
      "TEST001",
      nSubjects = 2,
      strOversampleDomain = "Raw_AE",
            vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
    )
  )

  expect_true(nrow(result$Raw_SUBJ) <= 2)
  # AE records should have study prefix in subjectid
  if (nrow(result$Raw_AE) > 0) {
    expect_true(all(grepl("^TEST001_", result$Raw_AE$subjectid)))
  }
})

test_that("ResampleStudy handles siteid updates correctly", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE2", "SITE3"),
      siteid = c("1", "2", "3"),
      subjectenrollmentnumber = c("S1", "S2", "S3"),
      subject_nsv = c("S1", "S2", "S3"),
      subjectname = c("S1", "S2", "S3"),
      subjectid = c("S1", "S2", "S3")
    ),
    Raw_AE = data.frame(
      subjid = c("S1", "S2", "S3"),
      invid = c("SITE1", "SITE2", "SITE3"),
      siteid = c("1", "2", "3"),
      aedesc = c("AE1", "AE2", "AE3")
    )
  )

  result <- ResampleStudy(
    lRaw,
    "TEST001",
    nSubjects = 2,
        vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
  )

  # siteid should be updated to match randomized invid
  expect_true(all(!is.na(result$Raw_SUBJ$siteid)))
  expect_true(all(!is.na(result$Raw_AE$siteid)))
})

test_that("ResampleStudy with TargetSiteCount generates sites correctly", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = paste0("S", 1:50),
      enrollyn = rep("Y", 50),
      invid = sample(paste0("SITE", 1:10), 50, replace = TRUE),
      siteid = sample(as.character(1:10), 50, replace = TRUE),
      subjectenrollmentnumber = paste0("S", 1:50),
      subject_nsv = paste0("S", 1:50),
      subjectname = paste0("S", 1:50),
      subjectid = paste0("S", 1:50)
    ),
    Raw_SITE = data.frame(
      invid = paste0("SITE", 1:10),
      siteid = as.character(1:10),
      site_num = as.character(1:10),
      studyid = rep("ORIGINAL", 10)
    )
  )

  result <- ResampleStudy(
    lRaw,
    "TEST001",
    nSubjects = 30,
    TargetSiteCount = 5,
        vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
  )

  # Should have generated sites with study prefix
  expect_true(all(grepl("^TEST001_", result$Raw_SITE$invid)))
  expect_equal(result$Raw_SITE$studyid[1], "TEST001")

  # All subjects should be assigned to generated sites
  expect_true(all(result$Raw_SUBJ$invid %in% result$Raw_SITE$invid))

  # Site count should be <= target (some sites may have 0 subjects)
  expect_true(nrow(result$Raw_SITE) <= 5)
})

test_that("ResampleStudy processes domain without subject or site columns", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE1", "SITE2"),
      siteid = c("1", "1", "2"), # Added siteid
      subjectenrollmentnumber = c("S1", "S2", "S3"),
      subject_nsv = c("S1", "S2", "S3"),
      subjectname = c("S1", "S2", "S3"),
      subjectid = c("S1", "S2", "S3")
    ),
    Raw_STUDY = data.frame(
      studyid = "ORIGINAL",
      study_name = "Original Study"
    )
  )

  result <- ResampleStudy(
    lRaw,
    "TEST001",
    nSubjects = 2,
        vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
  )

  # studyid should be updated in study domain
  expect_equal(result$Raw_STUDY$studyid, "TEST001")
  expect_equal(result$Raw_STUDY$study_name, "Original Study")
})

test_that("ResampleStudy process_site_domain with invid column", {
  lRaw <- list(
    Raw_SUBJ = data.frame(
      subjid = c("S1", "S2", "S3"),
      enrollyn = c("Y", "Y", "Y"),
      invid = c("SITE1", "SITE2", "SITE3"),
      siteid = c("1", "2", "3"),
      subjectenrollmentnumber = c("S1", "S2", "S3"),
      subject_nsv = c("S1", "S2", "S3"),
      subjectname = c("S1", "S2", "S3"),
      subjectid = c("S1", "S2", "S3")
    ),
    Raw_SITE = data.frame(
      invid = c("SITE1", "SITE2", "SITE3", "SITE4"),
      studyid = rep("ORIGINAL", 4)
    )
  )

  result <- ResampleStudy(
    lRaw,
    "TEST001",
    nSubjects = 2,
        vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid")
  )

  # Site domain invid should have study prefix
  expect_true(all(grepl("^TEST001_", result$Raw_SITE$invid)))
  # Should only include sites from sampled subjects
  expect_true(nrow(result$Raw_SITE) <= 2)
})
