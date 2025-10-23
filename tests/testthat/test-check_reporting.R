test_that("Script-based reporting workflow executes successfully", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.reporting")
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("clindata")
  
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
  
  # 3. Script-based reporting approach (from Cookbook)
  dfResults <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_Transformed"
  )
  
  dfBounds <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_Bounds"
  )
  
  dfBoundsRef <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_BoundsRef"
  )
  
  dfMetrics <- gsm.reporting::MakeMetric(metrics_wf)
  
  # Verify structure
  expect_s3_class(dfResults, "data.frame")
  expect_s3_class(dfBounds, "data.frame")
  expect_s3_class(dfBoundsRef, "data.frame")
  expect_s3_class(dfMetrics, "data.frame")
  
  expect_true("StudyID" %in% names(dfResults))
  expect_true("Metric" %in% names(dfResults))
  expect_true(nrow(dfResults) > 0)
  expect_true(nrow(dfBounds) > 0)
  expect_true(nrow(dfBoundsRef) > 0)
})

test_that("MakeCharts_StudyKRI and Report_KRI generate report", {
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
  
  # 3. Bind results
  dfResults <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_Transformed"
  )
  
  dfBounds <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_Bounds"
  )
  
  dfBoundsRef <- gsm.reporting::BindResults(
    lAnalyzed, 
    "Analysis_BoundsRef"
  )
  
  dfMetrics <- gsm.reporting::MakeMetric(metrics_wf)
  
  dfGroups <- dplyr::bind_rows(
    lMapped$Mapped_STUDY,
    lMapped$Mapped_SITE,
    lMapped$Country
  )
  
  # Generate charts
  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )
  
  expect_type(lCharts, "list")
  expect_true(length(lCharts) > 0)
  
  # Generate report
  tmpfile <- "test_studykri_report.html"
  
  gsm.kri::Report_KRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = tmpfile,
    strInputPath = system.file("report", "Report_KRI.Rmd", package = "gsm.studykri")
  )
  
  expect_true(file.exists(tmpfile))
  expect_true(file.size(tmpfile) > 0)
  
  unlink(tmpfile)
})

test_that("YAML-based reporting workflow executes successfully", {
  skip_if_not_installed("gsm.core")
  skip_if_not_installed("gsm.mapping")
  skip_if_not_installed("gsm.reporting")
  skip_if_not_installed("clindata")
  
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
  
  # 3. Reporting (YAML workflows)
  reporting_wf <- gsm.core::MakeWorkflowList(
    strPath = system.file("workflow/3_reporting", package = "gsm.studykri")
  )
  
  # Pass both lMapped and lAnalyzed, plus workflows
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
  
  # Verify all 5 reporting outputs exist
  expect_type(lReporting, "list")
  expect_true("Reporting_Results" %in% names(lReporting))
  expect_true("Reporting_Bounds" %in% names(lReporting))
  expect_true("Reporting_BoundsRef" %in% names(lReporting))
  expect_true("Reporting_Metrics" %in% names(lReporting))
  expect_true("Reporting_Groups" %in% names(lReporting))
  
  # Verify structure - workflows return output directly
  expect_type(lReporting$Reporting_Results, "list")  # BindResults returns list
  expect_type(lReporting$Reporting_Bounds, "list")
  expect_type(lReporting$Reporting_BoundsRef, "list")
  expect_s3_class(lReporting$Reporting_Metrics, "data.frame")  # MakeMetric returns df
  expect_s3_class(lReporting$Reporting_Groups, "data.frame")  # bind_rows returns df
  
  # Verify data frames exist
  expect_s3_class(lReporting$Reporting_Results, "data.frame")
  expect_s3_class(lReporting$Reporting_Bounds, "data.frame")
  expect_s3_class(lReporting$Reporting_BoundsRef, "data.frame")
  
  # Verify content
  expect_true(nrow(lReporting$Reporting_Results) > 0)
  expect_true(nrow(lReporting$Reporting_Metrics) > 0)
  expect_true(nrow(lReporting$Reporting_Groups) > 0)
})

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
