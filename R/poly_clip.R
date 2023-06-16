#' Polygon clipping
#'
#' @description A wrapper around [polyclip::polyclip()], which is an interface
#'   to the *Clipper* C++ library.
#'
#' @details Each of the arguments `a` and `b` represents a region in the
#'   Euclidean plane bounded by closed polygons. The format of these arguments
#'   is either:
#'
#'.  - a `data.frame` containing two columns `x` and `y` or a `list` containing
#'.  two numeric vectors `x` and `y` giving the coordinates of the vertices of a
#'   single polygon. The last vertex **should not** repeat the first vertex.
#'   - a `list of lists(x,y)` where `x` and `y` are numeric vectors giving the
#'   coordinates of the vertices of several polygons.
#'
#' @param a,b `data.frame` or `list` or `list of lists`, specifying polygons.
#'   See details below.
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
#' library(ggplot2)
#'
#' triangle <- list(
#'   x = c(0, 1, 3),
#'   y = c(0, 1, 0)
#' )
#'
#' circle <- ellipse()
#'
#' ggplot(circle, aes(x = x, y = y)) +
#'   geom_polygon(colour = "red") +
#'   geom_polygon(data = data.frame(triangle), colour = "blue") +
#'   coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-1.5, 1.5)) +
#'   theme_bw()
#'
#' # 4 set operations:
#' # intersection
#' s1 <- poly_clip(triangle, circle, "intersection")
#'
#' ggplot(data.frame(s1), aes(x = x, y = y)) +
#'   geom_polygon(colour = "green") +
#'   coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-1.5, 1.5)) +
#'   theme_bw()
#'
#' # union
#' s2 <- poly_clip(triangle, circle, "union")
#'
#' ggplot(data.frame(s2), aes(x = x, y = y)) +
#'   geom_polygon(colour = "green") +
#'   coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-1.5, 1.5)) +
#'   theme_bw()
#'
#'
#' # minus (subtract circle from triangle)
#' s3 <- poly_clip(triangle, circle, "minus")
#'
#' ggplot(data.frame(s3), aes(x = x, y = y)) +
#'   geom_polygon(colour = "green") +
#'   coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-1.5, 1.5)) +
#'   theme_bw()
#'
#' # xor (result is 2 shapes here)
#' s4 <- poly_clip(triangle, circle, "xor")
#'
#' ggplot(data.frame(s4[[1]]), aes(x = x, y = y)) +
#'   geom_polygon(colour = "red") +
#'   geom_polygon(data = data.frame(s4[[2]]), colour = "blue") +
#'   coord_cartesian(xlim = c(-1.5, 3.5), ylim = c(-1.5, 1.5)) +
#'   theme_bw()
#'
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
