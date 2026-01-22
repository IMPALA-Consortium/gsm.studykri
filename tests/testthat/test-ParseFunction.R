test_that("ParseFunction resolves simple function names", {
  # Test with base R function
  func <- ParseFunction("mean")
  expect_type(func, "closure")
  expect_equal(func(1:10), mean(1:10))

  # Test with stats function
  func2 <- ParseFunction("median")
  expect_type(func2, "closure")
  expect_equal(func2(1:10), median(1:10))
})

test_that("ParseFunction resolves package-qualified function names", {
  # Test with stats package
  func <- ParseFunction("stats::sd")
  expect_type(func, "closure")
  expect_equal(func(1:10), stats::sd(1:10))

  # Test with base package (sum is a primitive/builtin, not closure)
  func2 <- ParseFunction("base::mean")
  expect_type(func2, "closure")
  expect_equal(func2(1:10), base::mean(1:10))
})

test_that("ParseFunction resolves gsm.studykri functions", {
  # Test with a known gsm.studykri function
  func <- ParseFunction("gsm.studykri::Transform_Long")
  expect_type(func, "closure")
  expect_identical(func, gsm.studykri::Transform_Long)
})

test_that("ParseFunction errors on non-existent functions", {
  expect_error(
    ParseFunction("nonexistent_function_xyz123"),
    "Failed to parse"
  )

  expect_error(
    ParseFunction("fake.package::fake_function"),
    "Failed to parse"
  )
})

test_that("ParseFunction errors on non-function objects", {
  # Create a non-function object
  test_var <- 42
  assign("test_var", test_var, envir = .GlobalEnv)

  expect_error(
    ParseFunction("test_var"),
    "resolved to.*numeric.*not a function"
  )

  # Clean up
  rm(test_var, envir = .GlobalEnv)
})

test_that("ParseFunction validates input is character", {
  expect_error(
    ParseFunction(123),
    "must be a character string.*not.*numeric"
  )

  expect_error(
    ParseFunction(NULL),
    "must be a character string.*not.*NULL"
  )

  expect_error(
    ParseFunction(list("mean")),
    "must be a character string.*not.*list"
  )
})

test_that("ParseFunction validates input is single string", {
  expect_error(
    ParseFunction(c("mean", "median")),
    "must be a single character string.*not a vector of length 2"
  )

  expect_error(
    ParseFunction(character(0)),
    "must be a single character string.*not a vector of length 0"
  )
})

test_that("ParseFunction works with operators", {
  # Test with operator function
  func <- ParseFunction("base::`+`")
  expect_type(func, "builtin")
  expect_equal(func(2, 3), 5)
})

test_that("ParseFunction can be used in purrr::map workflow", {
  # Simulate YAML workflow pattern
  test_data <- list(
    data1 = 1:5,
    data2 = 6:10
  )

  # Step 1: Parse function
  func <- ParseFunction("mean")

  # Step 2: Use in purrr::map
  result <- purrr::map(test_data, func)

  expect_equal(result$data1, mean(1:5))
  expect_equal(result$data2, mean(6:10))
})

test_that("ParseFunction works with gsm.studykri functions in workflow", {
  skip_if_not_installed("gsm.studykri")

  # Parse a gsm.studykri function
  func <- ParseFunction("gsm.studykri::JoinKRIByDenominator")

  # Verify it's the correct function
  expect_type(func, "closure")
  expect_identical(func, gsm.studykri::JoinKRIByDenominator)

  # Create simple test data (DenominatorType column is now required)
  dfInput <- data.frame(
    MetricID = "kri0001",
    GroupID = "Site1",
    GroupLevel = "Site",
    Numerator = 5,
    Denominator = 100,
    StudyID = "AA-1",
    MonthYYYYMM = 202301,
    DenominatorType = "Visits"
  )

  dfMetrics <- data.frame(
    MetricID = "kri0001",
    Denominator = "Visits",
    AccrualThreshold = 180
  )

  # Use the parsed function
  result <- func(dfInput, dfMetrics)

  expect_type(result, "list")
  expect_true("Visits" %in% names(result))
})
