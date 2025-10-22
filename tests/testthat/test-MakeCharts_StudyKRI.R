test_that("MakeCharts_StudyKRI generates charts", {
  # Create mock bound results
  dfResults <- data.frame(
    MetricID = rep("kri0001", 6),
    StudyID = rep(c("STUDY001", "STUDY002"), each = 3),
    StudyMonth = rep(1:3, 2),
    Metric = runif(6, 0.1, 0.3),
    Numerator = rpois(6, 10),
    Denominator = rpois(6, 100),
    GroupCount = 5,
    MonthYYYYMM = rep(c(202301, 202302, 202303), 2)
  )
  
  dfBounds <- data.frame(
    MetricID = rep("kri0001", 6),
    StudyID = rep(c("STUDY001", "STUDY002"), each = 3),
    StudyMonth = rep(1:3, 2),
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )
  
  dfBoundsRef <- data.frame(
    MetricID = "kri0001",
    StudyMonth = 1:3,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18
  )
  
  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Adverse Event Rate",
    Abbreviation = "AE",
    Numerator = "AEs",
    Denominator = "Days"
  )
  
  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )
  
  expect_type(lCharts, "list")
  expect_equal(length(lCharts), 2)  # 2 studies
  expect_true("STUDY001_kri0001" %in% names(lCharts))
  expect_true("STUDY002_kri0001" %in% names(lCharts))
  expect_s3_class(lCharts$STUDY001_kri0001, "ggplot")
})

test_that("MakeCharts_StudyKRI handles empty data gracefully", {
  dfResults <- data.frame(
    MetricID = character(0),
    StudyID = character(0),
    StudyMonth = integer(0),
    Metric = numeric(0)
  )
  
  expect_error(
    MakeCharts_StudyKRI(
      dfResults = dfResults,
      dfBounds = data.frame(),
      dfBoundsRef = data.frame(),
      dfMetrics = data.frame()
    ),
    "non-empty"
  )
})

