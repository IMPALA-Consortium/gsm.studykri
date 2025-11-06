test_that("Report_KRI_StudyKRI returns empty character vector for empty lCharts", {
  result <- Report_KRI_StudyKRI(
    lCharts = list(),
    dfResults = data.frame(),
    dfGroups = data.frame(),
    dfMetrics = data.frame(),
    strOutputFile = "test.html",
    strInputPath = "dummy.Rmd"
  )

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("Report_KRI_StudyKRI returns empty character vector for NULL lCharts", {
  result <- Report_KRI_StudyKRI(
    lCharts = NULL,
    dfResults = data.frame(),
    dfGroups = data.frame(),
    dfMetrics = data.frame(),
    strOutputFile = "test.html",
    strInputPath = "dummy.Rmd"
  )

  expect_type(result, "character")
  expect_length(result, 0)
})

test_that("Report_KRI_StudyKRI returns empty character vector for non-list lCharts", {
  # Test with string input
  result <- Report_KRI_StudyKRI(
    lCharts = "not a list",
    dfResults = data.frame(),
    dfGroups = data.frame(),
    dfMetrics = data.frame(),
    strOutputFile = "test.html",
    strInputPath = "dummy.Rmd"
  )

  expect_type(result, "character")
  expect_length(result, 0)

  # Test with numeric input
  result2 <- Report_KRI_StudyKRI(
    lCharts = 123,
    dfResults = data.frame(),
    dfGroups = data.frame(),
    dfMetrics = data.frame(),
    strOutputFile = "test.html",
    strInputPath = "dummy.Rmd"
  )

  expect_type(result2, "character")
  expect_length(result2, 0)
})

test_that("Report_KRI_StudyKRI handles dfResults without StudyID column", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create minimal test data without StudyID in dfResults
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot()
  )

  dfResults <- data.frame(
    MetricID = "kri0001",
    GroupID = "Site_0001",
    Numerator = 10,
    Denominator = 100,
    Metric = 0.1,
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = "Site_0001",
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Test Metric",
    Numerator = "Events",
    Denominator = "Total",
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file in current directory
  temp_rmd <- "test_report_temp.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp.html"

  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  expect_type(result, "character")
  expect_length(result, 1)
  expect_named(result, "STUDY1")

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})

test_that("Report_KRI_StudyKRI handles filename without extension", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create minimal test data
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot()
  )

  dfResults <- data.frame(
    StudyID = "STUDY1",
    MetricID = "kri0001",
    GroupID = "Site_0001",
    Numerator = 10,
    Denominator = 100,
    Metric = 0.1,
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = "Site_0001",
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Test Metric",
    Numerator = "Events",
    Denominator = "Total",
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file in current directory
  temp_rmd <- "test_report_temp.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp_no_ext"

  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  expect_type(result, "character")
  expect_true(grepl("_STUDY1\\.html$", result["STUDY1"]))

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})

test_that("Report_KRI_StudyKRI handles filename with extension", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create minimal test data
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot()
  )

  dfResults <- data.frame(
    StudyID = "STUDY1",
    MetricID = "kri0001",
    GroupID = "Site_0001",
    Numerator = 10,
    Denominator = 100,
    Metric = 0.1,
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = "Site_0001",
    GroupLevel = "Site",
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Test Metric",
    Numerator = "Events",
    Denominator = "Total",
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file in current directory
  temp_rmd <- "test_report_temp2.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp2.html"

  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  expect_type(result, "character")
  expect_true(grepl("_STUDY1\\.html$", result["STUDY1"]))

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})

test_that("Report_KRI_StudyKRI processes multiple studies", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create test data with multiple studies
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot(),
    "STUDY1_kri0002" = ggplot2::ggplot(),
    "STUDY2_kri0001" = ggplot2::ggplot()
  )

  dfResults <- data.frame(
    StudyID = c("STUDY1", "STUDY1", "STUDY2"),
    MetricID = c("kri0001", "kri0002", "kri0001"),
    GroupID = c("Site_0001", "Site_0001", "Site_0002"),
    Numerator = c(10, 20, 15),
    Denominator = c(100, 200, 150),
    Metric = c(0.1, 0.1, 0.1),
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = c("Site_0001", "Site_0002"),
    GroupLevel = c("Site", "Site"),
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    Metric = c("Test Metric 1", "Test Metric 2"),
    Numerator = c("Events", "Events"),
    Denominator = c("Total", "Total"),
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file in current directory
  temp_rmd <- "test_report_temp.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp.html"

  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  expect_type(result, "character")
  expect_length(result, 2)
  expect_setequal(names(result), c("STUDY1", "STUDY2"))

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})

test_that("Report_KRI_StudyKRI skips studies with no matching charts after filtering", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create charts including one with non-standard name
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot(),
    "INVALID" = ggplot2::ggplot() # This will be parsed as study "INVALID"
  )

  dfResults <- data.frame(
    StudyID = c("STUDY1", "INVALID"),
    MetricID = c("kri0001", "kri0001"),
    GroupID = c("Site_0001", "Site_0002"),
    Numerator = c(10, 15),
    Denominator = c(100, 150),
    Metric = c(0.1, 0.1),
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = c("Site_0001", "Site_0002"),
    GroupLevel = c("Site", "Site"),
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Test Metric",
    Numerator = "Events",
    Denominator = "Total",
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file in current directory
  temp_rmd <- "test_report_temp.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp.html"

  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  # Should only have STUDY1 in results (INVALID has no matching charts with underscore)
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true("STUDY1" %in% names(result))
  # INVALID should NOT be in results because "INVALID" doesn't match prefix "INVALID_"
  expect_false("INVALID" %in% names(result))

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})

test_that("Report_KRI_StudyKRI correctly filters results by StudyID", {
  skip_if_not_installed("gsm.kri")
  skip_if_not_installed("rmarkdown")

  # Create test data with multiple studies in dfResults
  lCharts <- list(
    "STUDY1_kri0001" = ggplot2::ggplot()
  )

  # dfResults has data for both STUDY1 and STUDY2, but only STUDY1 has charts
  dfResults <- data.frame(
    StudyID = c("STUDY1", "STUDY2"),
    MetricID = c("kri0001", "kri0001"),
    GroupID = c("Site_0001", "Site_0002"),
    Numerator = c(10, 20),
    Denominator = c(100, 200),
    Metric = c(0.1, 0.1),
    stringsAsFactors = FALSE
  )

  dfGroups <- data.frame(
    GroupID = c("Site_0001", "Site_0002"),
    GroupLevel = c("Site", "Site"),
    stringsAsFactors = FALSE
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Test Metric",
    Numerator = "Events",
    Denominator = "Total",
    stringsAsFactors = FALSE
  )

  # Create a temporary Rmd file
  temp_rmd <- "test_report_temp.Rmd"
  writeLines(c(
    "---",
    "output: html_document",
    "---",
    "",
    "# Test Report",
    "",
    "This is a test."
  ), temp_rmd)

  temp_out <- "test_report_temp.html"

  # Call function - should only generate report for STUDY1
  result <- Report_KRI_StudyKRI(
    lCharts = lCharts,
    dfResults = dfResults,
    dfGroups = dfGroups,
    dfMetrics = dfMetrics,
    strOutputFile = temp_out,
    strInputPath = temp_rmd
  )

  expect_type(result, "character")
  expect_length(result, 1)
  expect_true("STUDY1" %in% names(result))
  expect_true(file.exists(result["STUDY1"]))

  # Clean up
  unlink(temp_rmd)
  unlink(result, force = TRUE)
})
