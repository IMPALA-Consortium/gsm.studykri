test_that("JoinKRIByDenominator joins KRIs by denominator type", {
  # Mock dfInput with 2 KRIs sharing same denominator (now includes DenominatorType)
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Visits"
  )

  # dfMetrics with matching AccrualThreshold
  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0003"),
    Denominator = c("Visits", "Visits"),
    AccrualThreshold = c(180, 180)
  )

  lResult <- JoinKRIByDenominator(dfInput, dfMetrics)

  # Output is list with correct denominator names
  expect_type(lResult, "list")
  expect_equal(names(lResult), "Visits")

  # Numerator columns renamed correctly
  dfVisits <- lResult$Visits
  expect_true("Numerator_kri0001" %in% names(dfVisits))
  expect_true("Numerator_kri0003" %in% names(dfVisits))

  # Values are correct
  expect_equal(nrow(dfVisits), 2)
  expect_equal(dfVisits$Numerator_kri0001, c(5, 3))
  expect_equal(dfVisits$Numerator_kri0003, c(2, 1))
})

test_that("JoinKRIByDenominator handles multiple denominator types", {
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    GroupID = c("Site1", "Site1"),
    GroupLevel = "Site",
    Numerator = c(5, 10),
    Denominator = c(100, 500),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = c("Visits", "Days on Study")
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    Denominator = c("Visits", "Days on Study"),
    AccrualThreshold = c(180, 180)
  )

  lResult <- JoinKRIByDenominator(dfInput, dfMetrics)

  expect_equal(length(lResult), 2)
  expect_true("Visits" %in% names(lResult))
  expect_true("Days on Study" %in% names(lResult))

  expect_true("Numerator_kri0001" %in% names(lResult$Visits))
  expect_true("Numerator_kri0002" %in% names(lResult$`Days on Study`))
})

test_that("JoinKRIByDenominator errors when DenominatorType column is missing", {
  # dfInput without DenominatorType column
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001"),
    GroupID = c("Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3),
    Denominator = c(100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001"),
    Denominator = c("Visits"),
    AccrualThreshold = c(180)
  )

  expect_error(
    JoinKRIByDenominator(dfInput, dfMetrics),
    "dfInput must contain a 'DenominatorType' column"
  )
})

test_that("JoinKRIByDenominator errors when KRIs have different AccrualThreshold for same DenominatorType", {
  # dfInput with 2 KRIs sharing same DenominatorType
  dfInput <- data.frame(
    MetricID = c("kri0006", "kri0006", "kri0007", "kri0007"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Enrolled Subjects"
  )

  # dfMetrics with DIFFERENT AccrualThreshold values
  dfMetrics <- data.frame(
    MetricID = c("kri0006", "kri0007"),
    Denominator = c("Enrolled Subjects", "Enrolled Subjects"),
    AccrualThreshold = c(25, 180)  # Different thresholds!
  )

  expect_error(
    JoinKRIByDenominator(dfInput, dfMetrics),
    "Cannot join KRIs with DenominatorType 'Enrolled Subjects' because they have different AccrualThreshold values"
  )
})

test_that("JoinKRIByDenominator succeeds when KRIs have matching AccrualThreshold for same DenominatorType", {
  # dfInput with 2 KRIs sharing same DenominatorType
  dfInput <- data.frame(
    MetricID = c("kri0006", "kri0006", "kri0007", "kri0007"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Enrolled Subjects"
  )

  # dfMetrics with MATCHING AccrualThreshold values
  dfMetrics <- data.frame(
    MetricID = c("kri0006", "kri0007"),
    Denominator = c("Enrolled Subjects", "Enrolled Subjects"),
    AccrualThreshold = c(25, 25)  # Same threshold!
  )

  # Should not throw error
  lResult <- JoinKRIByDenominator(dfInput, dfMetrics)

  expect_type(lResult, "list")
  expect_equal(names(lResult), "Enrolled Subjects")
  expect_true("Numerator_kri0006" %in% names(lResult$`Enrolled Subjects`))
  expect_true("Numerator_kri0007" %in% names(lResult$`Enrolled Subjects`))
})

test_that("JoinKRIByDenominator errors when KRIs have different GroupLevel for same DenominatorType", {
  dfInput <- data.frame(
    MetricID = c("kri0006", "kri0006", "kri0014", "kri0014"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = c("Site", "Site", "Country", "Country"),  # Different GroupLevels!
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Enrolled Subjects"
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0006", "kri0014"),
    Denominator = c("Enrolled Subjects", "Enrolled Subjects"),
    AccrualThreshold = c(25, 25)
  )

  expect_error(
    JoinKRIByDenominator(dfInput, dfMetrics),
    "Cannot join KRIs with DenominatorType 'Enrolled Subjects' because they have different GroupLevel values"
  )
})

test_that("JoinKRIByDenominator errors when KRIs have missing GroupLevel", {
  dfInput <- data.frame(
    MetricID = c("kri0006", "kri0006", "kri0014", "kri0014"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = c("Site", "Site", NA, NA),  # Missing GroupLevel!
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Enrolled Subjects"
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0006", "kri0014"),
    Denominator = c("Enrolled Subjects", "Enrolled Subjects"),
    AccrualThreshold = c(25, 25)
  )

  expect_error(
    JoinKRIByDenominator(dfInput, dfMetrics),
    "have missing or empty GroupLevel values"
  )
})

test_that("JoinKRIByDenominator errors when KRIs have different Denominator values for same id_cols", {
  # Two KRIs with same DenominatorType but different Denominator values
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001", "kri0002", "kri0002"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 8, 4),
    Denominator = c(100, 80, 150, 120),  # Different denominators!
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Days on Study"
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    Denominator = c("Days on Study", "Days on Study"),
    AccrualThreshold = c(180, 180)
  )

  expect_error(
    JoinKRIByDenominator(dfInput, dfMetrics),
    "have different Denominator values for the same"
  )
})
