#' Parse Function String to Function Object
#'
#' @description
#' Converts a character string representing a function name into an actual function object.
#' Supports both simple function names (e.g., "mean") and package-qualified names
#' (e.g., "gsm.studykri::Transform_Long"). This is particularly useful in YAML workflows
#' where function references need to be stored as strings and later resolved.
#'
#' @param strFunction `character` A single character string representing a function name.
#'   Can be a simple name like "mean" or package-qualified like "pkg::function".
#'
#' @return A function object resolved from the input string.
#'
#' @details
#' The function uses `eval(parse(text=...))` to resolve the string to a function object.
#' This approach successfully handles the `::` operator for package-qualified names.
#'
#' If the string does not resolve to a function object, an error is thrown.
#'
#' @examples
#' # Parse a base R function
#' func <- ParseFunction("mean")
#' func(1:10)
#'
#' # Parse a package-qualified function
#' func <- ParseFunction("gsm.studykri::Transform_Long")
#'
#' \dontrun{
#' # Use in YAML workflow pattern:
#' # Step 1: Parse function string
#' # - output: MyFunc
#' #   name: gsm.studykri::ParseFunction
#' #   params:
#' #     strFunction: package::function_name
#' #
#' # Step 2: Use parsed function
#' # - output: Result
#' #   name: purrr::map
#' #   params:
#' #     .x: MyData
#' #     .f: MyFunc
#' }
#'
#' @export
ParseFunction <- function(strFunction) {
  # Validate input
  if (!is.character(strFunction)) {
    stop(
      "strFunction must be a character string, not ", class(strFunction)[1], ".",
      call. = FALSE
    )
  }

  if (length(strFunction) != 1) {
    stop(
      "strFunction must be a single character string, not a vector of length ", length(strFunction), ".",
      call. = FALSE
    )
  }

  # Parse and evaluate the string to get the function
  func <- tryCatch(
    eval(parse(text = strFunction)),
    error = function(e) {
      stop(
        "Failed to parse '", strFunction, "' as a function. Error: ", e$message,
        call. = FALSE
      )
    }
  )

  # Verify the result is a function
  if (!is.function(func)) {
    stop(
      "'", strFunction, "' resolved to ", class(func)[1], ", not a function.",
      call. = FALSE
    )
  }

  return(func)
}



