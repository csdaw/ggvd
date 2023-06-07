#' Generate an ellipse
#'
#' @description Given a set of geometric parameters, generate the x,y coordinates
#' of an ellipse or super ellipse.
#'
#' @param x0 `numeric`, x coordinate for ellipse centre.
#' @param y0 `numeric`, y coordinate for ellipse centre.
#' @param a `numeric`, length of semi-major axis.
#' @param b `numeric`, length of semi-minor axis.
#' @param angle `numeric`, rotation of ellipse in radians. Rotation occurs
#' in counter-clockwise direction as per the unit circle.
#' @param n `integer`, number of points around the ellipse to use.
#' @param m1 `numeric`, major semi-diameter of super ellipse. Default is `2`,
#' which produces just a plain ellipse.
#' @param m2 `numeric`, minor semi-diameter of super ellipse. Default is to
#' match `m1`.
#'
#' @return Returns a `data.frame` containing coordinates,
#' with two columns: `x` and `y`, and `n + 1` rows.
#' @source Code modified from `ggforce::StatEllip` which is distributed under
#' the MIT license.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # default is a unit circle
#' ggplot(ellipse(), aes(x, y)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed()
#'
#' # angle is in radians, and rotation occurs counter-clockwise
#' ggplot(ellipse(a = 2, angle = pi / 4), aes(x, y)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed()
#'
#' # try a super ellipse
#' ggplot(ellipse(a = 2, angle = pi / 4, m1 = 3), aes(x, y)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed()
#'
ellipse <- function(x0 = 0, y0 = 0, a = 1, b = 1, angle = 0, n = 360L, m1 = 2, m2) {
  # define m2 default
  m2 <- ifelse(missing(m2), m1, m2)

  points <- seq(0, 2 * pi, length.out = n + 1)[seq_len(n)]
  cos_p <- cos(points)
  sin_p <- sin(points)
  x_tmp <- abs(cos_p)^(2 / m1) * a * sign(cos_p)
  y_tmp <- abs(sin_p)^(2 / m2) * b * sign(sin_p)

  # angle is in radians, rotation occurs counter-clockwise as per the unit circle
  out <- data.frame(
    x = x0 + x_tmp * cos(angle) - y_tmp * sin(angle),
    y = y0 + x_tmp * sin(angle) + y_tmp * cos(angle)
  )
  out
}
