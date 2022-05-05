GeomEuler <- ggproto("GeomEuler", GeomPolygon,
                     required_aes = c("x", "y"),
                     default_aes = aes(colour = "black", fill = NA, size = 0.5,
                                       linetype = 1, alpha = NA))

#' Plot Euler diagram
#'
#' @inheritParams ggplot2::geom_polygon
#'
#' @return Returns a `ggplot` object.
#' @export
#'
#' @examples
#' # Examples will go here
geom_euler <- function(mapping = NULL,
                       data = NULL,
                       stat = "identity",
                       position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    stat = stat, geom = GeomEuler, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
