#' Generate ellipses from a data frame
#'
#' @description Given a data frame containing sets of geometric
#'   parameters, generate the x,y coordinates of ellipses/super ellipses.
#'
#'   The input data frame can have the following `numeric` columns:
#'
#'   - `x0` (required) = x coordinate for ellipse centre.
#'   - `y0` (required) = y coordinate for ellipse centre.
#'   - `a` (optional) = length of semi-major axis. Default is `1`.
#'   - `b` (optional) = length of semi-minor axis. Default is `1`.
#'   - `angle` (optional) = rotation of the ellipse in radians. Rotation occurs
#'   in a counter-clockwise direction as per the unit circle.
#'   - `m1` (optional) = major semi-diameter of super ellipse. Default is `2`,
#'   which produces just a normal ellipse.
#'   - `m2` (optional) = minor semi-diameter of super ellipse. Default is to
#'   match `m1`.
#'
#' @param df `data.frame`, each row describes one ellipse. Minimum required info
#'   is 2 columns, `x0` and `y0` describing the (x, y) coordinates of the centre
#'   of the ellipse.
#' @param n `integer`, number of points around the ellipse to use. Default is `360L`.
#'
#' @return Returns `df` with the following additional numeric columns:
#'
#'  - `x` = x coordinate of points describing the ellipse.
#'  - `y` = y coordinate of points describing the ellipse.
#'  - `group` = numeric indicating which points belong to which ellipse.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x0 = c(0, 1),
#'   y0 = c(0, 0)
#' )
#' df
#'
#' ellipses <- df2ellipse(df)
#' head(ellipses)
#'
#' ggplot(ellipses, aes(x, y, group = group)) +
#'   geom_polygon(colour = "black", fill = NA) +
#'   coord_fixed()
#'
df2ellipse <- function(df, n = 360L) {
  stopifnot(all(c("x0", "y0") %in% colnames(df)))
  og_colnames <- colnames(df)
  n_ellipses <- nrow(df)

  df$a <- if (is.null(df$a)) 1 else df$a
  df$b <- if (is.null(df$b)) 1 else df$b
  df$angle <- if (is.null(df$angle)) 0 else df$angle
  df$m1 <- if (is.null(df$m1)) 2 else df$m1
  df$m2 <- if (is.null(df$m2)) df$m1 else df$m2
  df$group <- if (is.null(df$group) | all(df$group == -1)) seq_len(n_ellipses) else df$group

  df <- df[rep(seq_len(n_ellipses), each = n), ]
  rownames(df) <- NULL

  points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)],
                n_ellipses)
  cos_p <- cos(points)
  sin_p <- sin(points)
  x_tmp <- abs(cos_p)^(2 / df$m1) * df$a * sign(cos_p)
  y_tmp <- abs(sin_p)^(2 / df$m2) * df$b * sign(sin_p)
  df$x <- df$x0 + x_tmp * cos(df$angle) - y_tmp * sin(df$angle)
  df$y <- df$y0 + x_tmp * sin(df$angle) + y_tmp * cos(df$angle)
  df[, c(og_colnames, "x", "y", "group")]
}
