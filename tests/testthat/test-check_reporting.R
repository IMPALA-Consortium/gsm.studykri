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
  tmpfile <- tempfile(fileext = ".html")
  
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
  expect_true("lResults" %in% names(lReporting$Reporting_Results))
  expect_true("lBounds" %in% names(lReporting$Reporting_Bounds))
  expect_true("lBoundsRef" %in% names(lReporting$Reporting_BoundsRef))
  expect_true("dfMetrics" %in% names(lReporting$Reporting_Metrics))
  expect_true("dfGroups" %in% names(lReporting$Reporting_Groups))
  
  # Verify data frames exist - outputs are data frames directly
  expect_s3_class(lReporting$Reporting_Results$lResults, "data.frame")
  expect_s3_class(lReporting$Reporting_Bounds$lBounds, "data.frame")
  expect_s3_class(lReporting$Reporting_BoundsRef$lBoundsRef, "data.frame")
  expect_s3_class(lReporting$Reporting_Metrics$dfMetrics, "data.frame")
  expect_s3_class(lReporting$Reporting_Groups$dfGroups, "data.frame")
  
  # Verify content
  expect_true(nrow(lReporting$Reporting_Results$lResults) > 0)
  expect_true(nrow(lReporting$Reporting_Metrics$dfMetrics) > 0)
  expect_true(nrow(lReporting$Reporting_Groups$dfGroups) > 0)
})
