test_that("Visualize_StudyKRI creates plot with all inputs", {
  # Create test data
  dfStudyKRI <- data.frame(
    StudyID = rep("STUDY1", 5),
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    StudyCount = rep(3, 5),
    stringsAsFactors = FALSE
  )
  
  dfBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    LowerBound = c(0.08, 0.10, 0.12, 0.11, 0.10),
    UpperBound = c(0.12, 0.14, 0.18, 0.17, 0.16),
    stringsAsFactors = FALSE
  )
  
  # Create plot
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    dfBounds = dfBounds,
    strStudyID = "STUDY1"
  )
  
  # Verify it's a ggplot object
  expect_s3_class(p, "ggplot")
  
  # Verify plot has data
  expect_true(length(p$layers) > 0)
})

test_that("Visualize_StudyKRI works without individual study bounds", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    StudyCount = rep(3, 5),
    stringsAsFactors = FALSE
  )
  
  # Create plot without individual study bounds
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    dfBounds = NULL,
    strStudyID = "STUDY1"
  )
  
  expect_s3_class(p, "ggplot")
  expect_true(length(p$layers) > 0)
})

test_that("Visualize_StudyKRI filters to nMaxMonth correctly", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:10,
    Metric = seq(0.10, 0.19, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:10,
    MedianMetric = seq(0.11, 0.20, length.out = 10),
    LowerBound = seq(0.08, 0.17, length.out = 10),
    UpperBound = seq(0.14, 0.23, length.out = 10),
    stringsAsFactors = FALSE
  )
  
  # Filter to first 5 months
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    strStudyID = "STUDY1",
    nMaxMonth = 5
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that data was filtered
  # The plot data should only include months 1-5
  study_data <- p$layers[[3]]$data  # Black line layer
  expect_true(all(study_data$StudyMonth <= 5))
})

test_that("Visualize_StudyKRI validates input types", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  # Test wrong type for dfStudyKRI
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = list(a = 1),
      dfBoundsRef = dfBoundsRef,
      strStudyID = "STUDY1"
    ),
    "dfStudyKRI must be a data.frame"
  )
  
  # Test wrong type for dfBoundsRef
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = list(a = 1),
      strStudyID = "STUDY1"
    ),
    "dfBoundsRef must be a data.frame or NULL"
  )
  
  # Test wrong type for dfBounds
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = dfBoundsRef,
      dfBounds = list(a = 1),
      strStudyID = "STUDY1"
    ),
    "dfBounds must be a data.frame or NULL"
  )
  
  # Test invalid strStudyID
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = dfBoundsRef,
      strStudyID = c("STUDY1", "STUDY2")
    ),
    "strStudyID must be a single character string"
  )
  
  # Test invalid nMaxMonth
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = dfBoundsRef,
      strStudyID = "STUDY1",
      nMaxMonth = -5
    ),
    "nMaxMonth must be NULL or a positive number"
  )
})

test_that("Visualize_StudyKRI validates required columns", {
  # Missing column in dfStudyKRI
  dfStudyKRI_bad <- data.frame(
    StudyMonth = 1:5,
    # Missing Metric column
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI_bad,
      dfBoundsRef = dfBoundsRef,
      strStudyID = "STUDY1"
    ),
    "dfStudyKRI missing required columns.*Metric"
  )
  
  # Missing column in dfBoundsRef
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef_bad <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    # Missing LowerBound and UpperBound
    stringsAsFactors = FALSE
  )
  
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = dfBoundsRef_bad,
      strStudyID = "STUDY1"
    ),
    "dfBoundsRef missing required columns"
  )
})

test_that("Visualize_StudyKRI handles empty data after filtering", {
  dfStudyKRI <- data.frame(
    StudyMonth = 6:10,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 6:10,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  # Filter to months that don't exist (data starts at month 6)
  expect_error(
    Visualize_StudyKRI(
      dfStudyKRI = dfStudyKRI,
      dfBoundsRef = dfBoundsRef,
      strStudyID = "STUDY1",
      nMaxMonth = 5
    ),
    "No data available for dfStudyKRI after filtering"
  )
})

test_that("Visualize_StudyKRI uses custom labels", {
  dfStudyKRI <- data.frame(
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  custom_title <- "Custom Study Title"
  custom_subtitle <- "Custom Subtitle"
  custom_ylab <- "Custom Y Label"
  custom_xlab <- "Custom X Label"
  
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    strStudyID = "STUDY1",
    strTitle = custom_title,
    strSubtitle = custom_subtitle,
    strYlab = custom_ylab,
    strXlab = custom_xlab
  )
  
  expect_s3_class(p, "ggplot")
  
  # Check that custom labels are applied
  expect_equal(p$labels$title, custom_title)
  expect_equal(p$labels$subtitle, custom_subtitle)
  expect_equal(p$labels$y, custom_ylab)
  expect_equal(p$labels$x, custom_xlab)
})

test_that("Visualize_StudyKRI works with custom column names", {
  dfStudyKRI <- data.frame(
    MyMonth = 1:5,
    MyMetric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBoundsRef <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.11, 0.13, 0.14, 0.15, 0.14),
    LowerBound = c(0.08, 0.10, 0.11, 0.12, 0.11),
    UpperBound = c(0.14, 0.16, 0.17, 0.18, 0.17),
    stringsAsFactors = FALSE
  )
  
  p <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = dfBoundsRef,
    strStudyID = "STUDY1",
    strStudyMonthCol = "MyMonth",
    strMetricCol = "MyMetric"
  )
  
  expect_s3_class(p, "ggplot")
})

test_that("Visualize_StudyKRI works without reference bounds (dfBoundsRef = NULL)", {
  dfStudyKRI <- data.frame(
    StudyID = rep("STUDY1", 5),
    StudyMonth = 1:5,
    Metric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    stringsAsFactors = FALSE
  )
  
  dfBounds <- data.frame(
    StudyMonth = 1:5,
    MedianMetric = c(0.10, 0.12, 0.15, 0.14, 0.13),
    LowerBound = c(0.08, 0.10, 0.12, 0.11, 0.10),
    UpperBound = c(0.12, 0.14, 0.18, 0.17, 0.16),
    stringsAsFactors = FALSE
  )
  
  # Plot with study bounds but no reference bounds
  p1 <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = NULL,
    dfBounds = dfBounds,
    strStudyID = "STUDY1"
  )
  
  expect_s3_class(p1, "ggplot")
  expect_true(length(p1$layers) > 0)
  
  # Plot with only study data (no bounds at all)
  p2 <- Visualize_StudyKRI(
    dfStudyKRI = dfStudyKRI,
    dfBoundsRef = NULL,
    dfBounds = NULL,
    strStudyID = "STUDY1"
  )
  
  expect_s3_class(p2, "ggplot")
  expect_true(length(p2$layers) > 0)
  
  # Verify plot renders without reference bounds - should have at least line and point layers
  layer_types <- vapply(p2$layers, function(x) class(x$geom)[1], character(1))
  expect_true("GeomLine" %in% layer_types)
  expect_true("GeomPoint" %in% layer_types)
})
