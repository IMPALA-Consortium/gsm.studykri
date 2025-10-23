# Helper functions for parameter validation tests
# Reduces boilerplate code across test files

#' Test parameter validation for a function
#'
#' Helper to reduce boilerplate in validation tests. Note: This helper is designed
#' to be used within test_that() blocks where testthat functions are available.
#'
#' @param fn Function to test
#' @param valid_args Named list of valid arguments that work
#' @param invalid_tests List of lists, each with 'args' (arguments to override) and 'error' (expected error pattern)
#'
#' @examples
#' \dontrun{
#' test_that("validates parameters", {
#'   test_parameter_validation(
#'     fn = my_function,
#'     valid_args = list(x = 1, y = 2),
#'     invalid_tests = list(
#'       list(args = list(x = "invalid"), error = "x must be numeric"),
#'       list(args = list(y = NULL), error = "y cannot be NULL")
#'     )
#'   )
#' })
#' }
test_parameter_validation <- function(fn, valid_args, invalid_tests) {
  # Note: expect_error is from testthat, available in test context
  for (test_case in invalid_tests) {
    # Merge invalid args with valid baseline
    test_args <- modifyList(valid_args, test_case$args)
    
    # Test that error is thrown with expected pattern
    testthat::expect_error(
      do.call(fn, test_args),
      test_case$error
    )
  }
}

#' Create minimal test data for Input_CountSiteByMonth
#'
#' @return List with dfSubjects, dfNumerator, dfDenominator
create_minimal_input_data <- function() {
  list(
    dfSubjects = data.frame(
      subjid = c("S1", "S2", "S3"),
      studyid = c("STUDY1", "STUDY1", "STUDY1"),
      invid = c("SITE1", "SITE1", "SITE2"),
      enrollyn = c("Y", "Y", "Y"),
      stringsAsFactors = FALSE
    ),
    dfNumerator = data.frame(
      subjid = c("S1", "S2", "S3"),
      aest_dt = as.Date(c("2024-01-15", "2024-01-20", "2024-02-10")),
      stringsAsFactors = FALSE
    ),
    dfDenominator = data.frame(
      subjid = c("S1", "S2", "S3"),
      visit_dt = as.Date(c("2024-01-10", "2024-01-12", "2024-02-05")),
      stringsAsFactors = FALSE
    )
  )
}

#' Create minimal test data for Analyze_StudyKRI
#'
#' @return Data frame suitable for Analyze_StudyKRI
create_minimal_analyze_data <- function() {
  data.frame(
    GroupID = c("SiteA", "SiteB"),
    StudyID = c("Study1", "Study1"),
    Numerator = c(1, 2),
    Denominator = c(10, 20),
    MonthYYYYMM = c(202301, 202301),
    stringsAsFactors = FALSE
  )
}

