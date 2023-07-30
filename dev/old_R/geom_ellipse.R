#' @title ggvd Stats
#' @name ggvd-stat
#' @rdname ggvd-stat
#' @format NULL
#' @usage NULL
#' @description ggvd uses the ggproto class system to extend the functionality
#' of ggplot2. The package includes a number of `Stat` objects.
#' @inheritSection ggplot2::Stat Stats
#' @export
StatEllipse2 <- ggproto(
  "StatEllipse2", Stat,
  required_aes = c("x0", "y0", "a", "b", "angle"),
  default_aes = aes(m1 = NA, m2 = NA),
  setup_data = function(data, params) {
    # From ggforce::StatEllip
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2

    # Remove NA rows to avoid warning when
    # NAs are automatically removed somewhere between here
    # and compute_panel
    # (otherwise must use na.rm = TRUE)
    data <- data[!is.na(data$x0), ]
    data
  },
  compute_panel = function(data, scales, n = 360L) {
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))

    ellipses <- lapply(seq_len(nrow(data)), function(i) {
      ellipse_path <- ellipse(
        x0 = data$x0[i],
        y0 = data$y0[i],
        a = data$a[i],
        b = data$b[i],
        angle = data$angle[i],
        n = n,
        m1 = data$m1[i],
        m2 = data$m2[i]
      )

      cbind(ellipse_path, unclass(data[i, cols_to_keep]))
    })

    do.call(rbind, ellipses)
  }
)

#' Ellipses, superellipses, and circles
#'
#' @description This is a slight modification of `ggforce::geom_ellipse()` that
#' allows you to draw ellipses at a specified angle and center relative to the
#' coordinate system. Apart from letting you draw regular ellipsis, the stat is
#' using the generalised formula for superellipses which can be utilised by
#' setting the `m1` and `m2` aesthetics. If you only set the m1 the m2 value
#' will follow that to ensure a symmetric appearance.
#'
#' @section Aesthetics:
#' geom_ellipse understand the following aesthetics (required aesthetics are in
#' bold):
#'
#' - **x0**
#' - **y0**
#' - **a**
#' - **b**
#' - **angle**
#' - m1
#' - m2
#' - alpha
#' - colour
#' - fill
#' - group
#' - linetype
#' - linewidth
#'
#' @section Computed variables:
#'
#' \describe{
#'  \item{x, y}{The coordinates for the points along the ellipse.}
#' }
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @param n `integer`, the number of points to sample along the ellipse.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   set = c(1, 2),
#'   fill = c("red", "blue"),
#'   x0 = c(-0.5, 0.5),
#'   y0 = c(0, 0),
#'   a = c(1, 1),
#'   b = c(1, 1),
#'   angle = c(0, 0)
#' )
#'
#' ggplot(df) +
#'   aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle,
#'       fill = fill, group = set) +
#'   geom_ellipse(colour = "black", alpha = 0.5) +
#'   scale_fill_identity()
#'
geom_ellipse <- function(mapping = NULL, data = NULL,
                         stat = "ellipse2", position = "identity",
                         ...,
                         n = 360L, na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}
