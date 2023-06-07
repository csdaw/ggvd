#' Polygon clipping
#'
#' @description A wrapper around [polyclip::polyclip()], which is an interface
#'   to the *Clipper* C++ library.
#'
#' @details Each of the arguments `a` and `b` represents a region in the
#'   Euclidean plane bounded by closed polygons. The format of these arguments
#'   is either:
#'
#'   - a `list` containing two numeric vectors `x` and `y` giving the
#'   coordinates of the vertices of a single polygon. The last vertex
#'   **should not** repeat the first vertex.
#'   - a `list of list(x,y)` structures giving the coordinates of the vertices
#'   of several polygons.
#'
#' @param a,b `list` or `list of list`, specifying polygons. See details.
#' @param op `string`, set operation to be performed to combine `a` and `b`.
#'   One of `"intersection"`, `"union"`, `"minus"`, `"xor"` (partially matched).
#'
#' @return Returns a `list of lists`.
#' @source Slightly modified from `eulerr::poly_clip()` which is distributed
#'   under the GPLv3 license.
#'
#' @export
#'
#' @examples
#' #
poly_clip <- function(a, b, op = c("intersection", "union", "minus", "xor")) {
  op <- match.arg(op)
  a0 <- identical(length(a), 0L)
  b0 <- identical(length(b), 0L)

  if (op == "intersection") {
    if (a0 || b0)
      return(list())
  } else if (op == "union") {
    if (a0 && !b0)
      return(b)
    else if (!a0 && b0)
      return(a)
    else if (a0 && b0)
      return(list())
  } else if (op == "minus") {
    if (!a0 && b0)
      return(a)
    else if (a0)
      return(list())
  }
  polyclip::polyclip(a, b, op = op)
}
