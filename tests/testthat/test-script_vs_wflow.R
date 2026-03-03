# Data ----------------------------------------------------------
lRaw_original <- list(
  Raw_SUBJ = clindata::rawplus_dm,
  Raw_AE = clindata::rawplus_ae,
  Raw_VISIT = clindata::rawplus_visdt,
  Raw_SITE = clindata::ctms_site,
  Raw_STUDY = clindata::ctms_study,
  Raw_PD = clindata::ctms_protdev,
  Raw_DATAENT = clindata::edc_data_pages,
  Raw_DATACHG = clindata::edc_data_points,
  Raw_QUERY = clindata::edc_queries,
  Raw_ENROLL = clindata::rawplus_enroll,
  Raw_Randomization = clindata::rawplus_ixrsrand,
  Raw_LB = clindata::rawplus_lb,
  Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
  Raw_STUDCOMP = clindata::rawplus_studcomp,
  Raw_IE = clindata::rawplus_ie
)

lRaw_original$Raw_SUBJ$subjectname <- lRaw_original$Raw_SUBJ$subject_nsv
lRaw_original$Raw_SUBJ$subjectenrollmentnumber <- lRaw_original$Raw_SUBJ$subjid

# Create a portfolio with 3 studies
lRaw <- SimulatePortfolio(
  lRaw = lRaw_original,
  nStudies = 3,
  vSubjectIDs = c("subjid", "subjectenrollmentnumber", "subject_nsv", "subjectname", "subjectid"),
  dfConfig = tibble(
    studyid = c("AA-1", "AA-2", "AA-3", "AA-4", "AA-5", "AA-6"),
    nSubjects = c(500, 750, 150, 200, 250, 300),
    strOversampleDomain = rep("Raw_AE", 6),
    vOversamplQuantileRange_min = c(0, 0, 0, 0, 0, 0),
    vOversamplQuantileRange_max = c(1, 0.75, 1, 1, 0.95, 0.9)
  )
)

lRaw$Raw_StudyRef <- tibble(
  studyid = c(rep("AA-1", 3), rep("AA-2", 4)),
  studyrefid = c("AA-3", "AA-4", "AA-5", "AA-3", "AA-4", "AA-5", "AA-6")
)


test_that("script_vs_wflow", {
  # Input

  dfInputS <- gsm.studykri::Input_CountSiteByMonth(
    dfSubjects = lRaw$Raw_SUBJ %>% filter(enrollyn == "Y"),
    dfNumerator = lRaw$Raw_STUDCOMP %>% filter(compyn == "N") %>% mutate(mincreated_dts = as.Date(mincreated_dts)),
    dfDenominator = lRaw$Raw_SUBJ %>% filter(enrollyn == "Y"),
    strStudyCol = "studyid",
    strGroupCol = "invid",
    strGroupLevel = "Site",
    strSubjectCol = "subjid",
    strNumeratorDateCol = "mincreated_dts",
    strDenominatorDateCol = "firstparticipantdate",
    nMinDenominator = 5 # Normalize dates at the threshold
  )

  names(dfInputS$GroupID) <- NULL

  mapping_wf <- gsm.core::MakeWorkflowList(
    strNames = NULL,
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
    strPackage = NULL
  )

  # Expect warning about unparseable dates in source data (known data quality issue)
  expect_warning(
    {
      lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
    },
    "Field `visit_dt`"
  )

  lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)

  metrics_wf <- gsm.core::MakeWorkflowList(
    strNames = c("kri0006", "kri0007", "kri0014"),
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
    strPackage = NULL
  )

  lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped)

  dfInputW <- gsm.studykri::BindResults(lAnalyzed, "Analysis_Input")

  expect_equal(
    dfInputS,
    dfInputW %>%
      filter(MetricID == "Analysis_kri0006") %>%
      select(-DenominatorType, -MetricID, -SnapshotDate)
  )

  dfTransformedS <- gsm.studykri::Transform_CumCount(
    dfInputS,
    vBy = c("StudyID")
  )

  dfTransformedW <- gsm.studykri::BindResults(lAnalyzed, "Analysis_Transformed")

  expect_equal(
    dfTransformedS,
    dfTransformedW %>%
      filter(MetricID == "Analysis_kri0006") %>%
      select(-MetricID, -SnapshotDate)
  )

  dfBoundsS <- dfInputS %>%
    gsm.studykri::Analyze_StudyKRI_PredictBounds(dfStudyRef = lRaw$Raw_StudyRef)


  # bootstrapped study bounds should be same length as original data
  expect_equal(
    dfTransformedS %>%
      filter(StudyID %in% unique(lRaw$Raw_StudyRef$studyid)) %>%
      nrow(),
    dfBoundsS %>%
      nrow()
  )

  dfBoundsRefS <- gsm.studykri::Analyze_StudyKRI_PredictBoundsRef(dfInputS, dfStudyRef = lRaw$Raw_StudyRef)


  dfMetrics <- gsm.reporting::MakeMetric(metrics_wf)

  lJoined <- gsm.studykri::JoinKRIByDenominator(dfInputW, dfMetrics)

  Bounds_Wide <- purrr::map(lJoined, ~ Analyze_StudyKRI_PredictBounds(., dfStudyRef = lRaw$Raw_StudyRef))
  BoundsRef_Wide <- purrr::map(lJoined, ~ Analyze_StudyKRI_PredictBoundsRef(., dfStudyRef = lRaw$Raw_StudyRef))

  dfBoundsW <- Transform_Long(Bounds_Wide)
  dfBoundsRefW <- Transform_Long(BoundsRef_Wide)

  dfTransformedW6 <- dfTransformedW %>%
    filter(MetricID == "Analysis_kri0006")

  dfBoundsW6 <- dfBoundsW %>%
    filter(MetricID == "Analysis_kri0006")

  dfBoundsRefW6 <- dfBoundsRefW %>%
    filter(MetricID == "Analysis_kri0006")

  # by joining KRI with the same Denominator extra study months may be added by calendar months with only numerator events
  expect_true(
    dfTransformedW6 %>%
      filter(StudyID %in% unique(lRaw$Raw_StudyRef$studyid)) %>%
      nrow() <= nrow(dfBoundsW6)
  )

  expect_true(dplyr::near(mean(dfTransformedS$Metric), mean(dfTransformedW6$Metric)))
  expect_true(dplyr::near(mean(dfBoundsS$Upper), mean(dfBoundsW6$Upper), tol = 0.01))
  expect_true(dplyr::near(mean(dfBoundsS$Lower), mean(dfBoundsW6$Lower), tol = 0.01))
  expect_true(dplyr::near(mean(dfBoundsRefS$Upper), mean(dfBoundsRefW6$Upper), tol = 0.01))
  expect_true(dplyr::near(mean(dfBoundsRefS$Lower), mean(dfBoundsRefW6$Lower), tol = 0.01))
  expect_true(dplyr::near(mean(dfBoundsRefS$Median), mean(dfBoundsRefW6$Median), tol = 0.01))

  # p_script <- gsm.studykri::Visualize_StudyKRI(
  #   dfStudyKRI = dfTransformedS ,
  #   dfBounds = dfBoundsS,
  #   dfBoundsRef = dfBoundsRefS,
  #   strStudyID = "AA-1"
  # )

  # p_wflw <- gsm.studykri::Visualize_StudyKRI(
  #   dfStudyKRI = dfTransformedW6,
  #   dfBounds = dfBoundsW6,
  #   dfBoundsRef = dfBoundsRefW6,
  #   strStudyID = "AA-1"
  # )
})
