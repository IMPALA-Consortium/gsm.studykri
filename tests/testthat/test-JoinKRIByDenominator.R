test_that("JoinKRIByDenominator joins KRIs by denominator type", {
  # Mock dfInput with 2 KRIs sharing same denominator
  dfInput <- data.frame(
    MetricID = c("kri0001", "kri0001", "kri0003", "kri0003"),
    GroupID = c("Site1", "Site2", "Site1", "Site2"),
    GroupLevel = "Site",
    Numerator = c(5, 3, 2, 1),
    Denominator = c(100, 80, 100, 80),
    StudyID = "AA-1",
    MonthYYYYMM = 202301
  )

  # Mock dfMetrics mapping KRIs to denominator
  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0003"),
    Denominator = c("Visits", "Visits")
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
    MonthYYYYMM = 202301
  )

  dfMetrics <- data.frame(
    MetricID = c("kri0001", "kri0002"),
    Denominator = c("Visits", "Days on Study")
  )

  lResult <- JoinKRIByDenominator(dfInput, dfMetrics)

  expect_equal(length(lResult), 2)
  expect_true("Visits" %in% names(lResult))
  expect_true("Days on Study" %in% names(lResult))

  expect_true("Numerator_kri0001" %in% names(lResult$Visits))
  expect_true("Numerator_kri0002" %in% names(lResult$`Days on Study`))
})





