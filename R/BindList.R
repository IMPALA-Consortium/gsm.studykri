#' BindLists
#' @description passes arguments to c but ignores argument names
#' @param ... lists to be combined
#' @export
BindLists <- function(...) {
  ls <- list(...)
  # unnames first layer of list
  ls <- unname(ls)
  # merge first layer of list
  ls <- do.call(c, ls)

  return(ls)
}
