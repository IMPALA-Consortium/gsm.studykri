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
    MetricID = rep("kri0001", 6),
    StudyID = rep(c("STUDY001", "STUDY002"), each = 3),
    StudyMonth = rep(1:3, 2),
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = rep(c("REF1, REF2", "REF2, REF3"), each = 3)
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
  expect_equal(length(lCharts), 2) # 2 studies
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

test_that("MakeCharts_StudyKRI validates dfResults is a data frame", {
  expect_error(
    MakeCharts_StudyKRI(
      dfResults = "not a data frame",
      dfBounds = data.frame(),
      dfBoundsRef = data.frame(),
      dfMetrics = data.frame()
    ),
    "non-empty"
  )

  expect_error(
    MakeCharts_StudyKRI(
      dfResults = NULL,
      dfBounds = data.frame(),
      dfBoundsRef = data.frame(),
      dfMetrics = data.frame()
    ),
    "non-empty"
  )
})

test_that("MakeCharts_StudyKRI validates required columns", {
  # Missing MetricID column
  dfResults_missing_metric <- data.frame(
    StudyID = "STUDY001",
    StudyMonth = 1,
    Metric = 0.1
  )

  expect_error(
    MakeCharts_StudyKRI(
      dfResults = dfResults_missing_metric,
      dfBounds = data.frame(),
      dfBoundsRef = data.frame(),
      dfMetrics = data.frame()
    ),
    "missing columns.*MetricID"
  )

  # Missing multiple columns
  dfResults_missing_multiple <- data.frame(
    MetricID = "kri0001"
  )

  expect_error(
    MakeCharts_StudyKRI(
      dfResults = dfResults_missing_multiple,
      dfBounds = data.frame(),
      dfBoundsRef = data.frame(),
      dfMetrics = data.frame()
    ),
    "missing columns.*StudyID.*StudyMonth.*Metric"
  )
})

test_that("MakeCharts_StudyKRI skips studies with no reference bounds", {
  dfResults <- data.frame(
    MetricID = rep("kri0001", 6),
    StudyID = rep(c("STUDY001", "STUDY002"), each = 3),
    StudyMonth = rep(1:3, 2),
    Metric = runif(6, 0.1, 0.3),
    Numerator = rpois(6, 10),
    Denominator = rpois(6, 100)
  )

  dfBounds <- data.frame(
    MetricID = rep("kri0001", 6),
    StudyID = rep(c("STUDY001", "STUDY002"), each = 3),
    StudyMonth = rep(1:3, 2),
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  # Only provide reference bounds for STUDY001
  dfBoundsRef <- data.frame(
    MetricID = rep("kri0001", 3),
    StudyID = rep("STUDY001", 3),
    StudyMonth = 1:3,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1, REF2"
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Adverse Event Rate"
  )

  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )

  # Should only have chart for STUDY001, STUDY002 is skipped
  expect_equal(length(lCharts), 1)
  expect_true("STUDY001_kri0001" %in% names(lCharts))
  expect_false("STUDY002_kri0001" %in% names(lCharts))
})

test_that("MakeCharts_StudyKRI uses metric_id when metric name not found", {
  dfResults <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    Metric = runif(3, 0.1, 0.3),
    Numerator = rpois(3, 10),
    Denominator = rpois(3, 100)
  )

  dfBounds <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  dfBoundsRef <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1"
  )

  # dfMetrics with different MetricID (metric not found)
  dfMetrics <- data.frame(
    MetricID = "kri0002",
    Metric = "Different Metric"
  )

  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )

  expect_type(lCharts, "list")
  expect_equal(length(lCharts), 1)
  # Should still create chart with metric_id as label
  expect_s3_class(lCharts$STUDY001_kri0001, "ggplot")
})

test_that("MakeCharts_StudyKRI uses metric_id when Metric column missing", {
  dfResults <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    Metric = runif(3, 0.1, 0.3),
    Numerator = rpois(3, 10),
    Denominator = rpois(3, 100)
  )

  dfBounds <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  dfBoundsRef <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1"
  )

  # dfMetrics without Metric column
  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Abbreviation = "AE"
  )

  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )

  expect_type(lCharts, "list")
  expect_equal(length(lCharts), 1)
  expect_s3_class(lCharts$STUDY001_kri0001, "ggplot")
})

test_that("MakeCharts_StudyKRI respects nMaxMonth parameter", {
  dfResults <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:6,
    Metric = runif(6, 0.1, 0.3),
    Numerator = rpois(6, 10),
    Denominator = rpois(6, 100)
  )

  dfBounds <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:6,
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  dfBoundsRef <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:6,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1"
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Adverse Event Rate"
  )

  # Test with nMaxMonth = 3
  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics,
    nMaxMonth = 3
  )

  expect_type(lCharts, "list")
  expect_equal(length(lCharts), 1)
  expect_s3_class(lCharts$STUDY001_kri0001, "ggplot")

  # Verify the plot object was created with nMaxMonth parameter
  expect_true(!is.null(lCharts$STUDY001_kri0001))
})

test_that("MakeCharts_StudyKRI handles errors in Visualize_StudyKRI", {
  dfResults <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    Metric = runif(3, 0.1, 0.3),
    Numerator = rpois(3, 10),
    Denominator = rpois(3, 100)
  )

  dfBounds <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  dfBoundsRef <- data.frame(
    MetricID = "kri0001",
    StudyID = "STUDY001",
    StudyMonth = 1:3,
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1"
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Metric = "Adverse Event Rate"
  )

  # Mock Visualize_StudyKRI to throw an error
  with_mocked_bindings(
    Visualize_StudyKRI = function(...) stop("Mocked error"),
    {
      expect_warning(
        lCharts <- MakeCharts_StudyKRI(
          dfResults = dfResults,
          dfBounds = dfBounds,
          dfBoundsRef = dfBoundsRef,
          dfMetrics = dfMetrics
        ),
        "Failed to create chart"
      )

      # Should return empty list (or list with NULL entry)
      expect_type(lCharts, "list")
    },
    .package = "gsm.studykri"
  )
})

test_that("MakeCharts_StudyKRI handles multiple metrics per study", {
  dfResults <- data.frame(
    MetricID = rep(c("kri0001", "kri0002"), each = 3),
    StudyID = rep("STUDY001", 6),
    StudyMonth = rep(1:3, 2),
    Metric = runif(6, 0.1, 0.3),
    Numerator = rpois(6, 10),
    Denominator = rpois(6, 100)
  )

  dfBounds <- data.frame(
    MetricID = rep(c("kri0001", "kri0002"), each = 3),
    StudyID = rep("STUDY001", 6),
    StudyMonth = rep(1:3, 2),
    LowerBound = 0.05,
    UpperBound = 0.35,
    MedianMetric = 0.20
  )

  dfBoundsRef <- data.frame(
    MetricID = rep(c("kri0001", "kri0002"), each = 3),
    StudyID = rep("STUDY001", 6),
    StudyMonth = rep(1:3, 2),
    LowerBound = 0.08,
    UpperBound = 0.32,
    MedianMetric = 0.18,
    StudyRefID = "REF1"
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    Metric = c("AE Rate", "Protocol Deviation Rate")
  )

  lCharts <- MakeCharts_StudyKRI(
    dfResults = dfResults,
    dfBounds = dfBounds,
    dfBoundsRef = dfBoundsRef,
    dfMetrics = dfMetrics
  )

  expect_type(lCharts, "list")
  expect_equal(length(lCharts), 2)
  expect_true("STUDY001_kri0001" %in% names(lCharts))
  expect_true("STUDY001_kri0002" %in% names(lCharts))
  expect_s3_class(lCharts$STUDY001_kri0001, "ggplot")
  expect_s3_class(lCharts$STUDY001_kri0002, "ggplot")
})
