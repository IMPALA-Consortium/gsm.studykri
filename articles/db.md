# In-Database Compatibility

## Installation

``` r
install.packages("pak")
pak::pak("Gilead-BioStats/clindata")
pak::pak("Gilead-BioStats/gsm.core")
pak::pak("Gilead-BioStats/gsm.mapping")
pak::pak("Gilead-BioStats/gsm.kri")
pak::pak("Gilead-BioStats/gsm.reporting")
pak::pak("IMPALA-Consortium/gsm.simaerep")
```

## Load

``` r
suppressPackageStartupMessages(library(dplyr))
library(gsm.core)
library(gsm.mapping)
library(gsm.kri)
library(gsm.reporting)
library(gsm.studykri)
#> 
#> Attaching package: 'gsm.studykri'
#> The following object is masked from 'package:gsm.reporting':
#> 
#>     BindResults
```

## Introduction

{gsm.studykir} functions can be executed on a database back-end instead
of relying on in-memory calculations

## Simulate Data Set and Save to DuckDb

We use the clindata package which includes data from one clinical study
to simulate new study data. We create a portfolio with study AA-4
oversampling patients with low AE counts.

``` r
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
  Raw_VISIT = clindata::rawplus_visdt %>%
    mutate(visit_dt = lubridate::ymd(visit_dt))
)

lPortfolio <- SimulatePortfolio(
  lRaw = lRaw,
  nStudies = 4,
  dfConfig = tibble(
    studyid = c("AA-1", "AA-2", "AA-3", "AA-4"),
    nSubjects = c(500, 750, 150, 200),
    strOversamplDomain = rep("Raw_AE", 4),
    vOversamplQuantileRange_min = c(0, 0, 0, 0),
    vOversamplQuantileRange_max = c(1, 1, 1, 0.75)
  )
)

con <- DBI::dbConnect(duckdb::duckdb(), ":memory:")

purrr::walk2(names(lPortfolio), lPortfolio, ~ DBI::dbWriteTable(con, .x, .y))

dbPortfolio <- purrr::map(names(lPortfolio), ~ tbl(con, .x))
names(dbPortfolio) <- names(lPortfolio)
```

Here we create our data set.

``` r

tblInput1 <- gsm.studykri::Input_CountSiteByMonth(
  dfSubjects = dbPortfolio$Raw_SUBJ,
  dfNumerator = dbPortfolio$Raw_AE,
  dfDenominator = dbPortfolio$Raw_VISIT,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "visit_dt",
  strDenominatorType = "Visit"
)

tblInput2 <- gsm.studykri::Input_CountSiteByMonth(
  dfSubjects = dbPortfolio$Raw_SUBJ,
  dfNumerator = dbPortfolio$Raw_AE %>%
    filter(aeser == "Y"),
  dfDenominator = dbPortfolio$Raw_VISIT,
  strStudyCol = "studyid",
  strGroupCol = "invid",
  strGroupLevel = "Site",
  strSubjectCol = "subjid",
  strNumeratorDateCol = "aest_dt",
  strDenominatorDateCol = "visit_dt",
  strDenominatorType = "Visit"
)

tblInput <- dplyr::union_all(
  tblInput1 %>%
    mutate(MetricID = "kri0001"),
  tblInput2 %>%
    mutate(MetricID = "kri0002"),
)

dfMetrics <- tibble(
  MetricID = c("kri0001", "kri0002"),
  Denominator = "Visits"
)

DBI::dbWriteTable(con, "Metrics", dfMetrics)


lJoined <- JoinKRIByDenominator(tblInput)

lJoined
#> $Visit
#> # Source:   SQL [?? x 7]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>    GroupID    GroupLevel Denominator StudyID MonthYYYYMM Numerator_kri0002
#>    <chr>      <chr>            <dbl> <chr>         <dbl>             <dbl>
#>  1 AA-1_0X170 Site                 1 AA-1         201208                 0
#>  2 AA-1_0X178 Site                 2 AA-1         200606                 0
#>  3 AA-1_0X069 Site                 2 AA-1         200803                 0
#>  4 AA-1_0X162 Site                 1 AA-1         201106                 0
#>  5 AA-1_0X162 Site                 1 AA-1         201211                 0
#>  6 AA-1_0X076 Site                 2 AA-1         200808                 0
#>  7 AA-1_0X178 Site                 1 AA-1         200504                 0
#>  8 AA-1_0X003 Site                 2 AA-1         200605                 0
#>  9 AA-1_0X126 Site                 3 AA-1         200602                 0
#> 10 AA-1_0X180 Site                 2 AA-1         201205                 0
#> # ℹ more rows
#> # ℹ 1 more variable: Numerator_kri0001 <dbl>
```

Next we prepare for the actual bootstrap simulation. For this we need to
perform multiple cross joins that require the creation of temporary
tables in the backend. {gsm.studykri} will attempt to create them using
the connection attached to the lazy tables. However if write priveleges
are not sufficient we can also create the tables manually and pass them
to the relevant functions.

``` r
dfStudyRef <- tibble(
  StudyID = "AA-1",
  StudyRef = c("AA-2", "AA-3", "AA-4")
)

dfRep <- tibble(
  BootstrapRep = seq(1, 1000)
)

dfMonths <- tibble(
  YYYY = seq(2003, 2020)
) %>%
cross_join(
  tibble(
    MM = seq(1, 12)
  )
) %>%
mutate(
  MonthYYYYMM = YYYY * 100 + MM
) %>%
select(MonthYYYYMM)

DBI::dbWriteTable(con, "StudyRef", dfStudyRef, overwrite = TRUE)
DBI::dbWriteTable(con, "Rep", dfRep, overwrite = TRUE)
DBI::dbWriteTable(con, "Months", dfMonths, overwrite = TRUE)


tblStudyRef <- tbl(con, "StudyRef")
tblRep <- tbl(con, "Rep")
tblMonths <- tbl(con, "Months")


Bounds_Wide <- purrr::map(
  lJoined, ~ Analyze_StudyKRI_PredictBounds(
    .,
    dfStudyRef = tblStudyRef,
    tblBootstrapReps = tblRep,
    tblMonthSequence = tblMonths
    )
  )

BoundsRef_Wide <- purrr::map(
  lJoined, ~ Analyze_StudyKRI_PredictBoundsRef(
    .,
    dfStudyRef = tblStudyRef,
    tblBootstrapReps = tblRep,
    tblMonthSequence = tblMonths
    )
  )
#> Resampling with minimum group count: 78

tblBounds <- Transform_Long(Bounds_Wide)

tblBoundsRef <- Transform_Long(BoundsRef_Wide)

tblTransformed <- union_all(
  tblInput1 %>%
    Transform_CumCount(vBy = "StudyID", tblMonthSequence = tblMonths) %>%
    mutate(MetricID = "kri0001"),
  tblInput2 %>%
    Transform_CumCount(vBy = "StudyID", tblMonthSequence = tblMonths) %>%
    mutate(MetricID = "kri0002"),
)


tblBounds
#> # Source:     SQL [?? x 8]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyID, StudyMonth
#>    MetricID DenominatorType StudyID StudyMonth BootstrapCount  Median    Lower
#>    <chr>    <chr>           <chr>        <dbl>          <dbl>   <dbl>    <dbl>
#>  1 kri0002  Visit           AA-1             2           1000 0       0       
#>  2 kri0002  Visit           AA-1            10           1000 0       0       
#>  3 kri0002  Visit           AA-1            18           1000 0.00166 0       
#>  4 kri0002  Visit           AA-1            28           1000 0.00295 0       
#>  5 kri0002  Visit           AA-1            29           1000 0.00312 0.000671
#>  6 kri0002  Visit           AA-1            34           1000 0.00321 0.000582
#>  7 kri0002  Visit           AA-1            39           1000 0.00274 0.000490
#>  8 kri0002  Visit           AA-1            42           1000 0.00247 0.000450
#>  9 kri0002  Visit           AA-1            47           1000 0.00266 0.000526
#> 10 kri0002  Visit           AA-1            49           1000 0.00291 0.000945
#> # ℹ more rows
#> # ℹ 1 more variable: Upper <dbl>

tblBoundsRef
#> # Source:     SQL [?? x 11]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyMonth
#>    MetricID DenominatorType StudyMonth BootstrapCount GroupCount StudyCount
#>    <chr>    <chr>                <dbl>          <dbl>      <dbl>      <int>
#>  1 kri0002  Visit                    5           1000         78          3
#>  2 kri0002  Visit                    9           1000         78          3
#>  3 kri0002  Visit                   11           1000         78          3
#>  4 kri0002  Visit                   14           1000         78          3
#>  5 kri0002  Visit                   15           1000         78          3
#>  6 kri0002  Visit                   16           1000         78          3
#>  7 kri0002  Visit                   17           1000         78          3
#>  8 kri0002  Visit                   22           1000         78          3
#>  9 kri0002  Visit                   26           1000         78          3
#> 10 kri0002  Visit                   30           1000         78          3
#> # ℹ more rows
#> # ℹ 5 more variables: StudyID <chr>, StudyRefID <chr>, Median <dbl>,
#> #   Lower <dbl>, Upper <dbl>

tblTransformed
#> # Source:     SQL [?? x 8]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyID, StudyMonth
#>    StudyID MonthYYYYMM StudyMonth Numerator Denominator Metric GroupCount
#>    <chr>         <dbl>      <dbl>     <dbl>       <dbl>  <dbl>      <dbl>
#>  1 AA-2         200402          1         4          31  0.129          7
#>  2 AA-2         200403          2         8          44  0.182          9
#>  3 AA-2         200404          3        11          57  0.193         11
#>  4 AA-2         200405          4        14          75  0.187         14
#>  5 AA-2         200406          5        18          94  0.191         16
#>  6 AA-2         200407          6        23         121  0.190         20
#>  7 AA-2         200408          7        26         155  0.168         25
#>  8 AA-2         200409          8        30         187  0.160         25
#>  9 AA-2         200410          9        41         227  0.181         30
#> 10 AA-2         200411         10        44         266  0.165         30
#> # ℹ more rows
#> # ℹ 1 more variable: MetricID <chr>
```

Finally before plotting we need to collect the results into memory.

``` r

metrics_wf <- gsm.core::MakeWorkflowList(
  strNames = NULL,
  strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
  strPackage = NULL
)

dfMetrics <- gsm.reporting::MakeMetric(metrics_wf)


lCharts <- gsm.studykri::MakeCharts_StudyKRI(
    dfResults = collect(tblTransformed), 
    dfBounds = collect(tblBounds),
    dfBoundsRef = collect(tblBoundsRef),
    dfMetrics = dfMetrics
  )

lCharts
#> $`AA-1_kri0001`
```

![](db_files/figure-html/unnamed-chunk-4-1.png)

    #> 
    #> $`AA-1_kri0002`

![](db_files/figure-html/unnamed-chunk-4-2.png)

## Using workflows

Unfortunately the data specfication checks from gsm.core will not work
on lazy tables. We therefore need to skip the mapping workflow and
ignore the warnings for the missing specifications.

We might also scrub some of the specifications from the yaml files to
begin with.

``` r

lMapped <- list(
  Mapped_AE = dbPortfolio$Raw_AE,
  Mapped_Visit = dbPortfolio$Raw_VISIT,
  Mapped_SUBJ = dbPortfolio$Raw_SUBJ %>%
    filter(enrollyn == "Y"),
  Mapped_StudyRef =  tbl(con, "StudyRef")
)

metrics_wf <- gsm.core::MakeWorkflowList(
  strNames = "kri0001.yaml",
  strPath = system.file("workflow/2_metrics", package = "gsm.studykri"),
  strPackage = NULL
)

lAnalyzed <- gsm.core::RunWorkflows(lWorkflows = metrics_wf, lData = lMapped)
#> 
#> ── Running 1 Workflows ─────────────────────────────────────────────────────────
#> 
#> ── Initializing `Analysis_kri0001` Workflow ────────────────────────────────────
#> 
#> ── Checking data against spec
#> → All 3 data.frame(s) in the spec are present in the data: Mapped_AE, Mapped_SUBJ, Mapped_Visit
#> Warning: Not all columns of Mapped_AE in the spec are in the expected format, improperly
#> formatted columns are: subjid
#> Not all columns of Mapped_AE in the spec are in the expected format, improperly
#> formatted columns are: aest_dt
#> Warning: Not all columns of Mapped_SUBJ in the spec are in the expected format,
#> improperly formatted columns are: subjid
#> Not all columns of Mapped_SUBJ in the spec are in the expected format,
#> improperly formatted columns are: invid
#> Not all columns of Mapped_SUBJ in the spec are in the expected format,
#> improperly formatted columns are: studyid
#> Warning: Not all columns of Mapped_Visit in the spec are in the expected format,
#> improperly formatted columns are: subjid
#> Not all columns of Mapped_Visit in the spec are in the expected format,
#> improperly formatted columns are: visit_dt
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_AE$subjid
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_AE$aest_dt
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_SUBJ$subjid
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_SUBJ$invid
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_SUBJ$studyid
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_Visit$subjid
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_Visit$visit_dt
#> 
#> ── Workflow Step 1 of 3: `gsm.studykri::Input_CountSiteByMonth` ──
#> 
#> ── Evaluating 10 parameter(s) for `gsm.studykri::Input_CountSiteByMonth`
#> ✔ dfSubjects = Mapped_SUBJ: Passing lData$Mapped_SUBJ.
#> ✔ dfNumerator = Mapped_AE: Passing lData$Mapped_AE.
#> ✔ dfDenominator = Mapped_Visit: Passing lData$Mapped_Visit.
#> ℹ strStudyCol = studyid: No matching data found. Passing 'studyid' as a string.
#> ℹ strGroupCol = invid: No matching data found. Passing 'invid' as a string.
#> ✔ strGroupLevel = GroupLevel: Passing lMeta$GroupLevel.
#> ℹ strSubjectCol = subjid: No matching data found. Passing 'subjid' as a string.
#> ℹ strNumeratorDateCol = aest_dt: No matching data found. Passing 'aest_dt' as a string.
#> ℹ strDenominatorDateCol = visit_dt: No matching data found. Passing 'visit_dt' as a string.
#> ✔ strDenominatorType = Denominator: Passing lMeta$Denominator.
#> 
#> ── Calling `gsm.studykri::Input_CountSiteByMonth`
#> 
#> ── list of length 2 saved as `lData$Analysis_Input`.
#> 
#> ── Workflow Step 2 of 3: `gsm.studykri::Transform_CumCount` ──
#> 
#> ── Evaluating 3 parameter(s) for `gsm.studykri::Transform_CumCount`
#> ✔ dfInput = Analysis_Input: Passing lData$Analysis_Input.
#> ℹ vBy = StudyID: No matching data found. Passing 'StudyID' as a string.
#> ✔ nMinDenominator = AccrualThreshold: Passing lMeta$AccrualThreshold.
#> 
#> ── Calling `gsm.studykri::Transform_CumCount`
#> 
#> ── list of length 2 saved as `lData$Analysis_Transformed`.
#> 
#> ── Workflow Step 3 of 3: `list` ──
#> 
#> ── Evaluating 3 parameter(s) for `list`
#> ✔ ID = ID: Passing lMeta$ID.
#> ✔ Analysis_Input = Analysis_Input: Passing lData$Analysis_Input.
#> ✔ Analysis_Transformed = Analysis_Transformed: Passing lData$Analysis_Transformed.
#> 
#> ── Calling `list`
#> 
#> ── list of length 3 saved as `lData$lAnalysis`.
#> 
#> ── Returning results from final step: list of length 3`. ──
#> 
#> ── Completed `Analysis_kri0001` Workflow ───────────────────────────────────────

reporting_wf <- gsm.core::MakeWorkflowList(
  strNames = c("Bounds", "BoundsRef", "Input", "Join", "Metrics", "Results"),
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
#> 
#> ── Running 6 Workflows ─────────────────────────────────────────────────────────
#> 
#> ── Initializing `Reporting_Results` Workflow ───────────────────────────────────
#> 
#> ── No spec found in workflow. Proceeding without checking data.
#> 
#> ── Workflow Step 1 of 1: `gsm.studykri::BindResults` ──
#> 
#> ── Evaluating 2 parameter(s) for `gsm.studykri::BindResults`
#> ✔ lAnalysis = lAnalyzed: Passing lData$lAnalyzed.
#> ℹ strName = Analysis_Transformed: No matching data found. Passing 'Analysis_Transformed' as a string.
#> 
#> ── Calling `gsm.studykri::BindResults`
#> 
#> ── list of length 2 saved as `lData$lResults`.
#> 
#> ── Returning results from final step: list of length 2`. ──
#> 
#> ── Completed `Reporting_Results` Workflow ──────────────────────────────────────
#> 
#> ── Initializing `Reporting_Input` Workflow ─────────────────────────────────────
#> 
#> ── No spec found in workflow. Proceeding without checking data.
#> 
#> ── Workflow Step 1 of 1: `gsm.studykri::BindResults` ──
#> 
#> ── Evaluating 2 parameter(s) for `gsm.studykri::BindResults`
#> ✔ lAnalysis = lAnalyzed: Passing lData$lAnalyzed.
#> ℹ strName = Analysis_Input: No matching data found. Passing 'Analysis_Input' as a string.
#> 
#> ── Calling `gsm.studykri::BindResults`
#> 
#> ── list of length 2 saved as `lData$Reporting_Input`.
#> 
#> ── Returning results from final step: list of length 2`. ──
#> 
#> ── Completed `Reporting_Input` Workflow ────────────────────────────────────────
#> 
#> ── Initializing `Reporting_Metrics` Workflow ───────────────────────────────────
#> 
#> ── No spec found in workflow. Proceeding without checking data.
#> 
#> ── Workflow Step 1 of 1: `gsm.reporting::MakeMetric` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.reporting::MakeMetric`
#> ✔ lWorkflows = lWorkflows: Passing lData$lWorkflows.
#> 
#> ── Calling `gsm.reporting::MakeMetric`
#> 
#> ── 1x16 data.frame saved as `lData$dfMetrics`.
#> 
#> ── Returning results from final step: 1x16 data.frame`. ──
#> 
#> ── Completed `Reporting_Metrics` Workflow ──────────────────────────────────────
#> 
#> ── Initializing `Reporting_Join` Workflow ──────────────────────────────────────
#> 
#> ── Checking data against spec
#> → All 1 data.frame(s) in the spec are present in the data: Reporting_Input
#> Warning: Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: MetricID
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: GroupID
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: GroupLevel
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: Numerator
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: Denominator
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: StudyID
#> Not all columns of Reporting_Input in the spec are in the expected format,
#> improperly formatted columns are: MonthYYYYMM
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$MetricID
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$GroupID
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$GroupLevel
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$Numerator
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$Denominator
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$StudyID
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Reporting_Input$MonthYYYYMM
#> 
#> ── Workflow Step 1 of 1: `gsm.studykri::JoinKRIByDenominator` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.studykri::JoinKRIByDenominator`
#> ✔ dfInput = Reporting_Input: Passing lData$Reporting_Input.
#> 
#> ── Calling `gsm.studykri::JoinKRIByDenominator`
#> 
#> ── list of length 1 saved as `lData$Joined_Analysis_Input`.
#> 
#> ── Returning results from final step: list of length 1`. ──
#> 
#> ── Completed `Reporting_Join` Workflow ─────────────────────────────────────────
#> 
#> ── Initializing `Reporting_Bounds` Workflow ────────────────────────────────────
#> 
#> ── Checking data against spec
#> → All 2 data.frame(s) in the spec are present in the data: Reporting_Join, Mapped_StudyRef
#> → All specified columns in Mapped_StudyRef are in the expected format
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_StudyRef$study
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_StudyRef$studyref
#> 
#> ── Workflow Step 1 of 3: `gsm.studykri::ParseFunction` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.studykri::ParseFunction`
#> ℹ strFunction = gsm.studykri::Analyze_StudyKRI_PredictBounds: No matching data found. Passing 'gsm.studykri::Analyze_StudyKRI_PredictBounds' as a string.
#> 
#> ── Calling `gsm.studykri::ParseFunction`
#> 
#> ── closure of length 1 saved as `lData$PredictBounds_Func`.
#> 
#> ── Workflow Step 2 of 3: `purrr::map` ──
#> 
#> ── Evaluating 7 parameter(s) for `purrr::map`
#> ✔ .x = Reporting_Join: Passing lData$Reporting_Join.
#> ✔ .f = PredictBounds_Func: Passing lData$PredictBounds_Func.
#> ✔ dfStudyRef = Mapped_StudyRef: Passing lData$Mapped_StudyRef.
#> ✔ nBootstrapReps = BootstrapReps: Passing lMeta$BootstrapReps.
#> ✔ nConfLevel = Threshold: Passing lMeta$Threshold.
#> ✔ nMinDenominator = AccrualThreshold: Passing lMeta$AccrualThreshold.
#> ℹ seed = 42: No matching data found. Passing '42' as a string.
#> 
#> ── Calling `purrr::map`
#> 
#> ── list of length 1 saved as `lData$Analysis_Bounds_Wide`.
#> 
#> ── Workflow Step 3 of 3: `gsm.studykri::Transform_Long` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.studykri::Transform_Long`
#> ✔ lWide = Analysis_Bounds_Wide: Passing lData$Analysis_Bounds_Wide.
#> 
#> ── Calling `gsm.studykri::Transform_Long`
#> 
#> ── list of length 2 saved as `lData$Reporting_Bounds`.
#> 
#> ── Returning results from final step: list of length 2`. ──
#> 
#> ── Completed `Reporting_Bounds` Workflow ───────────────────────────────────────
#> 
#> ── Initializing `Reporting_BoundsRef` Workflow ─────────────────────────────────
#> 
#> ── Checking data against spec
#> → All 2 data.frame(s) in the spec are present in the data: Reporting_Join, Mapped_StudyRef
#> → All specified columns in Mapped_StudyRef are in the expected format
#> Warning: Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_StudyRef$study
#> Not all specified columns in the spec are present in the data, missing columns
#> are: Mapped_StudyRef$studyref
#> 
#> ── Workflow Step 1 of 3: `gsm.studykri::ParseFunction` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.studykri::ParseFunction`
#> ℹ strFunction = gsm.studykri::Analyze_StudyKRI_PredictBoundsRef: No matching data found. Passing 'gsm.studykri::Analyze_StudyKRI_PredictBoundsRef' as a string.
#> 
#> ── Calling `gsm.studykri::ParseFunction`
#> 
#> ── closure of length 1 saved as `lData$PredictBoundsRef_Func`.
#> 
#> ── Workflow Step 2 of 3: `purrr::map` ──
#> 
#> ── Evaluating 7 parameter(s) for `purrr::map`
#> ✔ .x = Reporting_Join: Passing lData$Reporting_Join.
#> ✔ .f = PredictBoundsRef_Func: Passing lData$PredictBoundsRef_Func.
#> ✔ dfStudyRef = Mapped_StudyRef: Passing lData$Mapped_StudyRef.
#> ✔ nBootstrapReps = BootstrapReps: Passing lMeta$BootstrapReps.
#> ✔ nConfLevel = Threshold: Passing lMeta$Threshold.
#> ✔ nMinDenominator = AccrualThreshold: Passing lMeta$AccrualThreshold.
#> ℹ seed = 42: No matching data found. Passing '42' as a string.
#> 
#> ── Calling `purrr::map`
#> Resampling with minimum group count: 78
#> 
#> ── list of length 1 saved as `lData$Analysis_BoundsRef_Wide`.
#> 
#> ── Workflow Step 3 of 3: `gsm.studykri::Transform_Long` ──
#> 
#> ── Evaluating 1 parameter(s) for `gsm.studykri::Transform_Long`
#> ✔ lWide = Analysis_BoundsRef_Wide: Passing lData$Analysis_BoundsRef_Wide.
#> 
#> ── Calling `gsm.studykri::Transform_Long`
#> 
#> ── list of length 2 saved as `lData$Reporting_BoundsRef`.
#> 
#> ── Returning results from final step: list of length 2`. ──
#> 
#> ── Completed `Reporting_BoundsRef` Workflow ────────────────────────────────────

lReporting
#> $Reporting_Results
#> # Source:     SQL [?? x 9]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyID, StudyMonth
#>    StudyID MonthYYYYMM StudyMonth Numerator Denominator Metric GroupCount
#>    <chr>         <dbl>      <dbl>     <dbl>       <dbl>  <dbl>      <dbl>
#>  1 AA-1         200404          1         6          39  0.154         13
#>  2 AA-1         200405          2        12          60  0.2           16
#>  3 AA-1         200406          3        12          80  0.15          16
#>  4 AA-1         200407          4        16         106  0.151         18
#>  5 AA-1         200408          5        19         137  0.139         22
#>  6 AA-1         200409          6        26         168  0.155         23
#>  7 AA-1         200410          7        31         200  0.155         24
#>  8 AA-1         200411          8        34         233  0.146         24
#>  9 AA-1         200412          9        36         268  0.134         22
#> 10 AA-1         200501         10        40         300  0.133         23
#> # ℹ more rows
#> # ℹ 2 more variables: MetricID <chr>, SnapshotDate <date>
#> 
#> $Reporting_Input
#> # Source:   SQL [?? x 10]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>    GroupID    GroupLevel Numerator Denominator Metric StudyID MonthYYYYMM
#>    <chr>      <chr>          <dbl>       <dbl>  <dbl> <chr>         <dbl>
#>  1 AA-1_0X140 Site               2           2  1     AA-1         200903
#>  2 AA-1_0X163 Site               2           1  2     AA-1         200405
#>  3 AA-1_0X163 Site               1           1  1     AA-1         200506
#>  4 AA-1_0X163 Site               1           2  0.5   AA-1         200603
#>  5 AA-1_0X124 Site               1           3  0.333 AA-1         200512
#>  6 AA-1_0X124 Site               1           3  0.333 AA-1         200608
#>  7 AA-1_0X102 Site               1           1  1     AA-1         200605
#>  8 AA-1_0X052 Site               1           4  0.25  AA-1         201110
#>  9 AA-1_0X052 Site               1           1  1     AA-1         201112
#> 10 AA-1_0X156 Site               1           1  1     AA-1         200906
#> # ℹ more rows
#> # ℹ 3 more variables: DenominatorType <chr>, MetricID <chr>,
#> #   SnapshotDate <date>
#> 
#> $Reporting_Metrics
#> # A tibble: 1 × 16
#>   Type    ID    GroupLevel Abbreviation Metric Numerator Denominator Model Score
#>   <chr>   <chr> <chr>      <chr>        <chr>  <chr>     <chr>       <chr> <chr>
#> 1 Analys… kri0… Site       AE           Adver… Adverse … Visits      Boot… Boot…
#> # ℹ 7 more variables: AnalysisType <chr>, Threshold <dbl>,
#> #   AccrualThreshold <int>, AccrualMetric <chr>, BootstrapReps <int>,
#> #   Priority <dbl>, MetricID <chr>
#> 
#> $Reporting_Join
#> $Reporting_Join$Visits
#> # Source:   SQL [?? x 6]
#> # Database: DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#>    GroupID    GroupLevel Denominator StudyID MonthYYYYMM Numerator_Analysis_kr…¹
#>    <chr>      <chr>            <dbl> <chr>         <dbl>                   <dbl>
#>  1 AA-1_0X013 Site                 1 AA-1         201411                       1
#>  2 AA-1_0X076 Site                 2 AA-1         200409                       2
#>  3 AA-1_0X124 Site                 4 AA-1         200601                       3
#>  4 AA-1_0X035 Site                 2 AA-1         201511                       1
#>  5 AA-1_0X170 Site                 1 AA-1         201212                       2
#>  6 AA-1_0X093 Site                 7 AA-1         201709                       1
#>  7 AA-1_0X162 Site                 1 AA-1         201112                       3
#>  8 AA-1_0X162 Site                 1 AA-1         201307                       1
#>  9 AA-1_0X016 Site                 2 AA-1         200905                       1
#> 10 AA-1_0X016 Site                 2 AA-1         201007                       1
#> # ℹ more rows
#> # ℹ abbreviated name: ¹​Numerator_Analysis_kri0001
#> 
#> 
#> $Reporting_Bounds
#> # Source:     SQL [?? x 8]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyID, StudyMonth
#>    MetricID      DenominatorType StudyID StudyMonth BootstrapCount Median  Lower
#>    <chr>         <chr>           <chr>        <dbl>          <dbl>  <dbl>  <dbl>
#>  1 Analysis_kri… Visits          AA-1             2           1000  0.159 0.0392
#>  2 Analysis_kri… Visits          AA-1            10           1000  0.132 0.0600
#>  3 Analysis_kri… Visits          AA-1            18           1000  0.174 0.115 
#>  4 Analysis_kri… Visits          AA-1            28           1000  0.160 0.121 
#>  5 Analysis_kri… Visits          AA-1            29           1000  0.158 0.119 
#>  6 Analysis_kri… Visits          AA-1            34           1000  0.148 0.112 
#>  7 Analysis_kri… Visits          AA-1            39           1000  0.147 0.115 
#>  8 Analysis_kri… Visits          AA-1            42           1000  0.147 0.117 
#>  9 Analysis_kri… Visits          AA-1            47           1000  0.151 0.123 
#> 10 Analysis_kri… Visits          AA-1            49           1000  0.152 0.125 
#> # ℹ more rows
#> # ℹ 1 more variable: Upper <dbl>
#> 
#> $Reporting_BoundsRef
#> # Source:     SQL [?? x 11]
#> # Database:   DuckDB 1.4.3 [unknown@Linux 6.11.0-1018-azure:R 4.5.2/:memory:]
#> # Ordered by: StudyMonth
#>    MetricID      DenominatorType StudyMonth BootstrapCount GroupCount StudyCount
#>    <chr>         <chr>                <dbl>          <dbl>      <dbl>      <int>
#>  1 Analysis_kri… Visits                 145           1000         78          3
#>  2 Analysis_kri… Visits                 150           1000         78          3
#>  3 Analysis_kri… Visits                 156           1000         78          3
#>  4 Analysis_kri… Visits                 158           1000         78          3
#>  5 Analysis_kri… Visits                 159           1000         78          3
#>  6 Analysis_kri… Visits                 167           1000         78          3
#>  7 Analysis_kri… Visits                 170           1000         78          3
#>  8 Analysis_kri… Visits                 173           1000         78          3
#>  9 Analysis_kri… Visits                 174           1000         78          3
#> 10 Analysis_kri… Visits                 181           1000         78          3
#> # ℹ more rows
#> # ℹ 5 more variables: StudyID <chr>, StudyRefID <chr>, Median <dbl>,
#> #   Lower <dbl>, Upper <dbl>
```

Next we collect the data and create the charts.

``` r

lCharts <- gsm.studykri::MakeCharts_StudyKRI(
    dfResults = collect(lReporting$Reporting_Results), 
    dfBounds = collect(lReporting$Reporting_Bounds),
    dfBoundsRef = collect(lReporting$Reporting_BoundsRef),
    dfMetrics = lReporting$Reporting_Metrics
  )

lCharts
#> $`AA-1_Analysis_kri0001`
```

![](db_files/figure-html/unnamed-chunk-6-1.png)
