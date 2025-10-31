suppressPackageStartupMessages({
  library(dplyr)
  library(gsm.core)
  library(gsm.mapping)
  library(gsm.kri)
  library(gsm.reporting)
  library(gsm.studykri)
})

# Generate multi-study portfolio
lRaw_original <- list(
  Raw_SUBJ = clindata::rawplus_dm,
  Raw_AE = clindata::rawplus_ae,
  Raw_VISIT = clindata::rawplus_visdt,
  Raw_SITE = clindata::ctms_site,
  Raw_STUDY = clindata::ctms_study,
  Raw_PD = clindata::ctms_protdev,
  Raw_DATAENT = clindata::edc_data_pages,
  Raw_QUERY = clindata::edc_queries,
  Raw_ENROLL = clindata::rawplus_enroll,
  Raw_Randomization = clindata::rawplus_ixrsrand,
  Raw_LB = clindata::rawplus_lb,
  Raw_SDRGCOMP = clindata::rawplus_sdrgcomp,
  Raw_STUDCOMP = clindata::rawplus_studcomp
)

lRaw <- SimulatePortfolio(
  lRaw = lRaw_original,
  nStudies = 3,
  seed = 123,
  dfConfig = tibble(
    studyid = c("AA-1", "AA-2", "AA-3", "AA-4", "AA-5", "AA-6"),
    nSubjects = c(500, 750, 150, 200, 250, 300),
    strOversamplDomain = rep("Raw_AE", 6),
    vOversamplQuantileRange_min = c(0, 0, 0, 0, 0, 0),
    vOversamplQuantileRange_max = c(1, 0.75, 1, 1, 0.95, 0.9)
  )
)

lRaw$Raw_StudyRef <- tibble(
  studyid = c(rep("AA-1", 3), rep("AA-2", 4)),
  studyrefid = c("AA-3", "AA-4", "AA-5", "AA-3", "AA-4", "AA-5", "AA-6")
)

# Run mapping workflows
mapping_wf <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/1_mappings", package = "gsm.studykri"),
  strPackage = NULL
)

lIngest <- gsm.mapping::Ingest(lRaw, gsm.mapping::CombineSpecs(mapping_wf))

lMapped <- gsm.core::RunWorkflows(lWorkflows = mapping_wf, lData = lIngest)

# Run KRI workflow
metrics_wf <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
  strPackage = NULL
)

lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped)

# Run reporting workflows
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

# Generate reports using module workflow
module_wf_gsm <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/4_modules", package = "gsm.studykri"),
  strPackage = NULL
)

report_path <- system.file("report", "Report_KRI.Rmd", package = "gsm.studykri")
n_steps <- length(module_wf_gsm$StudyKRI$steps)

module_wf_gsm$StudyKRI$steps[[n_steps]]$params$strInputPath <- report_path

lModule <- gsm.core::RunWorkflows(module_wf_gsm, lReporting)

message("Reports generated successfully for studies AA-1 and AA-2")

