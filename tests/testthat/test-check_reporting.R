test_that("Reporting workflow executes successfully", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.reporting")
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("clindata")
  skip_if_not_installed("rmarkdown")
  
  # Full pipeline test
  lRaw <- list(
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_AE = clindata::rawplus_ae,
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_LB = clindata::rawplus_lb,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_VISIT = clindata::rawplus_visdt
  )
  
  # 1. Mappings
  mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri")
  )
  
  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  })
  
  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)
  
  # 2. Metrics
  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri")
  )
  
  lAnalyzed <- gsm.core::RunWorkflows(metrics_wf, lMapped)
  
  # 3. Reporting (bind results)
  reporting_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/3_reporting", package = "gsm.studykri")
  )
  
  lReporting <- gsm.core::RunWorkflows(
    lWorkflows = reporting_wf,
    lData = c(
      lMapped,
      list(
        lAnalyzed = lAnalyzed,
        lWorkflows = metrics_wf
      )
    )
  )
  
  # Verify all three reporting outputs exist
  expect_type(lReporting, "list")
  expect_true("Reporting_Results" %in% names(lReporting))
  expect_true("Reporting_Bounds" %in% names(lReporting))
  expect_true("Reporting_BoundsRef" %in% names(lReporting))
  
  # Extract for verification
  lResults <- lReporting$Reporting_Results$lResults
  lBounds <- lReporting$Reporting_Bounds$lBounds
  lBoundsRef <- lReporting$Reporting_BoundsRef$lBoundsRef
  
  # Verify structure
  expect_true("dfResults" %in% names(lResults))
  expect_true("dfBounds" %in% names(lBounds))
  expect_true("dfBounds" %in% names(lBoundsRef))
  expect_true("dfMetadata" %in% names(lResults))
})

test_that("StudyKRI module workflow generates report", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.reporting")
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("clindata")
  skip_if_not_installed("rmarkdown")
  
  # Full pipeline test
  lRaw <- list(
    Raw_SITE = clindata::ctms_site,
    Raw_STUDY = clindata::ctms_study,
    Raw_PD = clindata::ctms_protdev,
    Raw_DATAENT = clindata::edc_data_pages,
    Raw_QUERY = clindata::edc_queries,
    Raw_AE = clindata::rawplus_ae,
    Raw_SUBJ = clindata::rawplus_dm,
    Raw_ENROLL = clindata::rawplus_enroll,
    Raw_Randomization = clindata::rawplus_ixrsrand,
    Raw_LB = clindata::rawplus_lb,
    Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
    Raw_STUDCOMP = clindata::rawplus_studcomp,
    Raw_VISIT = clindata::rawplus_visdt
  )
  
  # 1. Mappings
  mapping_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/1_mappings", package = "gsm.studykri")
  )
  
  suppressWarnings({
    lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))
  })
  
  lMapped <- gsm.core::RunWorkflows(mapping_wf, lIngest)
  
  # 2. Metrics
  metrics_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/2_metrics", package = "gsm.studykri")
  )
  
  lAnalyzed <- gsm.core::RunWorkflows(metrics_wf, lMapped)
  
  # 3. Reporting (bind results)
  reporting_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/3_reporting", package = "gsm.studykri")
  )
  
  lReporting <- gsm.core::RunWorkflows(
    lWorkflows = reporting_wf,
    lData = c(
      lMapped,
      list(
        lAnalyzed = lAnalyzed,
        lWorkflows = metrics_wf
      )
    )
  )
  
  # 4. Module (charts + report)
  module_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/4_modules", package = "gsm.studykri")
  )
  
  # Set output path
  tmpfile <- tempfile(fileext = ".html")
  
  # Update workflow with output path
  module_wf$Module_StudyKRI$steps[[2]]$params$strReportPath <- tmpfile
  
  lModule <- gsm.core::RunWorkflows(module_wf, lReporting)
  
  # Verify report was generated
  expect_true(file.exists(tmpfile))
  expect_true(file.size(tmpfile) > 0)
  
  # Verify charts were generated
  expect_type(lModule$Module_StudyKRI$lCharts, "list")
  expect_true(length(lModule$Module_StudyKRI$lCharts) > 0)
  
  unlink(tmpfile)
})

