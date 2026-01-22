test_that("AddScreeningFailures basic functionality works", {
  # Create test data
  # dfEnrolled: only Y patients across 3 sites
  dfEnrolled <- data.frame(
    subjid = paste0("S", 1:30),
    studyid = "Study1",
    siteid = rep(c("Site1", "Site2", "Site3"), times = c(10, 12, 8)),
    enrollyn = "Y",
    enroll_dt = as.Date("2024-01-01") + 1:30,
    stringsAsFactors = FALSE
  )
  
  # dfSource: mix of Y and N patients across multiple sites with different ratios
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:100),
    studyid = "SourceStudy",
    siteid = rep(c("SiteA", "SiteB", "SiteC", "SiteD"), each = 25),
    enrollyn = c(
      rep("Y", 20), rep("N", 5),   # SiteA: 80% Y, 20% N
      rep("Y", 15), rep("N", 10),  # SiteB: 60% Y, 40% N
      rep("Y", 18), rep("N", 7),   # SiteC: 72% Y, 28% N
      rep("Y", 10), rep("N", 15)   # SiteD: 40% Y, 60% N
    ),
    enroll_dt = as.Date("2024-01-01") + 1:100,
    stringsAsFactors = FALSE
  )
  
  # Call function with seed for reproducibility
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 123
  )
  
  # Basic checks
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > nrow(dfEnrolled))
  
  # Check that both Y and N patients are present
  enrollyn_table <- table(result$enrollyn)
  expect_true("Y" %in% names(enrollyn_table))
  expect_true("N" %in% names(enrollyn_table))
  
  # Check that all original enrolled patients are still there
  expect_equal(sum(result$enrollyn == "Y"), nrow(dfEnrolled))
  
  # Check that N patients were added
  expect_true(sum(result$enrollyn == "N") > 0)
  
  # Check that sites from dfEnrolled are present
  expect_true(all(c("Site1", "Site2", "Site3") %in% result$siteid))
  
  # Check that study from dfEnrolled is maintained
  expect_true(all(result$studyid == "Study1"))
  
  # Test reproducibility
  result2 <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 123
  )
  
  expect_equal(nrow(result), nrow(result2))
  expect_equal(sum(result$enrollyn == "N"), sum(result2$enrollyn == "N"))
})

test_that("AddScreeningFailures works without seed", {
  # Simple test without seed to cover that branch
  dfEnrolled <- data.frame(
    subjid = paste0("S", 1:10),
    studyid = "Study1",
    siteid = rep("Site1", 10),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )
  
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:20),
    studyid = "SourceStudy",
    siteid = rep("SiteA", 20),
    enrollyn = c(rep("Y", 15), rep("N", 5)),
    stringsAsFactors = FALSE
  )
  
  # Call without seed
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = NULL
  )
  
  expect_true(nrow(result) >= nrow(dfEnrolled))
})

test_that("AddScreeningFailures handles edge case with zero screening failures calculated", {
  # Create scenario where ratio might result in very few or zero screening failures
  dfEnrolled <- data.frame(
    subjid = paste0("S", 1:2),
    studyid = "Study1",
    siteid = rep("Site1", 2),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )
  
  # Source with very high enrollment ratio (close to 1.0)
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:100),
    studyid = "SourceStudy",
    siteid = rep("SiteA", 100),
    enrollyn = c(rep("Y", 99), rep("N", 1)),
    stringsAsFactors = FALSE
  )
  
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 456
  )
  
  # Should still return a valid dataframe
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(dfEnrolled))
})

test_that("AddScreeningFailures handles sites with zero enrolled patients in source", {
  # This tests the fix for the infinite sample size bug
  dfEnrolled <- data.frame(
    subjid = paste0("S", 1:10),
    studyid = "Study1",
    siteid = rep("Site1", 10),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )
  
  # Source with one site having ONLY screening failures (0 enrolled)
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:40),
    studyid = "SourceStudy",
    siteid = c(rep("SiteA", 20), rep("SiteB", 20)),
    enrollyn = c(
      rep("Y", 15), rep("N", 5),   # SiteA: has enrolled patients
      rep("N", 20)                  # SiteB: ONLY screening failures (this caused the bug)
    ),
    stringsAsFactors = FALSE
  )
  
  # This should not error with "vector size cannot be infinite"
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 789
  )
  
  # Should return valid results
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) >= nrow(dfEnrolled))
  expect_true(all(result$enrollyn %in% c("Y", "N")))
})

test_that("AddScreeningFailures handles multi-study scenarios", {
  # Create dfEnrolled with multiple studies, multiple sites per study
  dfEnrolled <- data.frame(
    subjid = paste0("E", 1:80),
    studyid = c(rep("Study1", 50), rep("Study2", 30)),
    siteid = c(
      rep(c("Site1", "Site2", "Site3"), times = c(20, 15, 15)),  # Study1
      rep(c("Site1", "Site2"), times = c(15, 15))                 # Study2
    ),
    enrollyn = "Y",
    enroll_dt = as.Date("2024-01-01") + 1:80,
    stringsAsFactors = FALSE
  )
  
  # Source data with various ratios (study ID irrelevant)
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:100),
    studyid = "SourceStudy",
    siteid = rep(c("SiteA", "SiteB", "SiteC", "SiteD"), each = 25),
    enrollyn = c(
      rep("Y", 20), rep("N", 5),   # SiteA: 80% Y, 20% N
      rep("Y", 15), rep("N", 10),  # SiteB: 60% Y, 40% N
      rep("Y", 18), rep("N", 7),   # SiteC: 72% Y, 28% N
      rep("Y", 10), rep("N", 15)   # SiteD: 40% Y, 60% N
    ),
    enroll_dt = as.Date("2024-01-01") + 1:100,
    stringsAsFactors = FALSE
  )
  
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 999
  )
  
  # Basic checks
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > nrow(dfEnrolled))
  
  # Check that all original enrolled patients are preserved
  expect_equal(sum(result$enrollyn == "Y"), nrow(dfEnrolled))
  
  # Check that screening failures were added
  expect_true(sum(result$enrollyn == "N") > 0)
  
  # Verify study-site combinations are maintained correctly
  # Study1 should have Site1, Site2, Site3
  study1_sites <- unique(result$siteid[result$studyid == "Study1"])
  expect_true(all(c("Site1", "Site2", "Site3") %in% study1_sites))
  
  # Study2 should have Site1, Site2
  study2_sites <- unique(result$siteid[result$studyid == "Study2"])
  expect_true(all(c("Site1", "Site2") %in% study2_sites))
  
  # Each study-site combination should have both Y and N patients (or just Y if ratio was very high)
  study_site_combos <- result %>%
    dplyr::group_by(.data$studyid, .data$siteid) %>%
    dplyr::summarise(
      n_enrolled = sum(.data$enrollyn == "Y"),
      n_screening = sum(.data$enrollyn == "N"),
      .groups = "drop"
    )
  
  # All combinations should exist
  expect_equal(nrow(study_site_combos), 5)  # Study1: 3 sites, Study2: 2 sites
  
  # All should have enrolled patients
  expect_true(all(study_site_combos$n_enrolled > 0))
})

test_that("AddScreeningFailures handles overlapping site names across studies", {
  # Critical test: Same site name appears in multiple studies
  # Each study-site should get independent screening failures
  dfEnrolled <- data.frame(
    subjid = paste0("E", 1:60),
    studyid = c(rep("StudyA", 30), rep("StudyB", 30)),
    siteid = c(
      rep(c("Site1", "Site2"), each = 15),  # StudyA
      rep(c("Site1", "Site2"), each = 15)   # StudyB (same site names!)
    ),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )
  
  # Source with different ratios
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:60),
    studyid = "Source",
    siteid = rep(c("X", "Y", "Z"), each = 20),
    enrollyn = c(
      rep("Y", 16), rep("N", 4),   # X: 80% Y
      rep("Y", 10), rep("N", 10),  # Y: 50% Y
      rep("Y", 15), rep("N", 5)    # Z: 75% Y
    ),
    stringsAsFactors = FALSE
  )
  
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 111
  )
  
  # Check that we have 4 distinct study-site combinations
  combinations <- result %>%
    dplyr::distinct(.data$studyid, .data$siteid) %>%
    dplyr::arrange(.data$studyid, .data$siteid)
  
  expect_equal(nrow(combinations), 4)
  expect_equal(combinations$studyid, c("StudyA", "StudyA", "StudyB", "StudyB"))
  expect_equal(combinations$siteid, c("Site1", "Site2", "Site1", "Site2"))
  
  # Each study-site should have screening failures independently sampled
  study_site_counts <- result %>%
    dplyr::group_by(.data$studyid, .data$siteid, .data$enrollyn) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  
  # All 4 study-site combos should have Y patients
  y_counts <- study_site_counts %>%
    dplyr::filter(.data$enrollyn == "Y")
  expect_equal(nrow(y_counts), 4)
  expect_true(all(y_counts$n == 15))  # Each has 15 enrolled
  
  # Screening failures should be present (unless very high ratios were sampled)
  n_counts <- study_site_counts %>%
    dplyr::filter(.data$enrollyn == "N")
  expect_true(nrow(n_counts) > 0)  # At least some screening failures
})

test_that("AddScreeningFailures handles studies with different numbers of sites", {
  # Study1: 4 sites, Study2: 2 sites, Study3: 1 site
  dfEnrolled <- data.frame(
    subjid = paste0("E", 1:70),
    studyid = c(rep("Study1", 40), rep("Study2", 20), rep("Study3", 10)),
    siteid = c(
      rep(paste0("S", 1:4), each = 10),  # Study1: 4 sites
      rep(paste0("S", 1:2), each = 10),  # Study2: 2 sites (overlapping names)
      rep("S1", 10)                       # Study3: 1 site (overlapping name)
    ),
    enrollyn = "Y",
    stringsAsFactors = FALSE
  )
  
  dfSource <- data.frame(
    subjid = paste0("SRC", 1:50),
    studyid = "Source",
    siteid = rep("X", 50),
    enrollyn = c(rep("Y", 40), rep("N", 10)),  # 80% enrollment ratio
    stringsAsFactors = FALSE
  )
  
  result <- AddScreeningFailures(
    dfEnrolled = dfEnrolled,
    dfSource = dfSource,
    strSiteCol = "siteid",
    strStudyCol = "studyid",
    seed = 222
  )
  
  # Should have 7 study-site combinations total
  # Study1: S1, S2, S3, S4 (4)
  # Study2: S1, S2 (2)
  # Study3: S1 (1)
  combinations <- result %>%
    dplyr::distinct(.data$studyid, .data$siteid)
  
  expect_equal(nrow(combinations), 7)
  
  # Verify each study has correct sites
  study1_sites <- combinations %>%
    dplyr::filter(.data$studyid == "Study1") %>%
    dplyr::pull(.data$siteid) %>%
    sort()
  expect_equal(study1_sites, c("S1", "S2", "S3", "S4"))
  
  study2_sites <- combinations %>%
    dplyr::filter(.data$studyid == "Study2") %>%
    dplyr::pull(.data$siteid) %>%
    sort()
  expect_equal(study2_sites, c("S1", "S2"))
  
  study3_sites <- combinations %>%
    dplyr::filter(.data$studyid == "Study3") %>%
    dplyr::pull(.data$siteid)
  expect_equal(study3_sites, "S1")
  
  # All original enrolled patients should be present
  expect_equal(sum(result$enrollyn == "Y"), nrow(dfEnrolled))
})
