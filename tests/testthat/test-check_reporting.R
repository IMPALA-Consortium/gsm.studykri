test_that("Complete YAML workflow generates HTML report", {
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
  
  # Reduce bootstrap iterations for faster testing
  metrics_wf$kri0001$meta$BootstrapReps <- 100
  
  lAnalyzed <- gsm.core::RunWorkflows(metrics_wf, lMapped)
  
  # 3. Reporting (YAML workflows)
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
  
  # 4. Modules (charts + report via YAML)
  module_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/4_modules", package = "gsm.studykri")
  )
  
  # Set output path - use simple filename not tempfile()
  tmpfile <- "test_complete_studykri_report.html"
  
  # Get report template path
  report_path <- system.file("report", "Report_KRI.Rmd", package = "gsm.studykri")
  expect_true(nchar(report_path) > 0, info = "Report template path should not be empty")
  expect_true(file.exists(report_path), info = "Report template file should exist")
  
  # Update workflow with output path and input template path
  module_wf$StudyKRI$steps[[2]]$params$strOutputFile <- tmpfile
  module_wf$StudyKRI$steps[[2]]$params$strInputPath <- report_path
  
  # Run module workflow - pass all reporting outputs
  lModule <- gsm.core::RunWorkflows(
    lWorkflows = module_wf,
    lData = lReporting
  )
  
  # Verify report was generated
  expect_true(file.exists(tmpfile))
  expect_true(file.size(tmpfile) > 0)
  
  # Verify report output path was returned (workflow returns only final step output)
  expect_type(lModule$Module_StudyKRI, "character")
  expect_true(nchar(lModule$Module_StudyKRI) > 0)
  
  unlink(tmpfile)
})
