#' Determine relationships between sets
#'
#' @param s object describing sets to compare. Type can be one of: ...
#' @param ...
#'
#' @return Returns something...
#' @export
#'
#' @examples
#' # Example
compare_sets <- function(s, ...) {
  if (is.vector(s, mode = "numeric")) {
    if(is.null(names(s))) stop(sprintf("s must be a named numeric vector"))
    out <- s
  } else {
    message("s is not a numeric vector")
  }

  out
}
