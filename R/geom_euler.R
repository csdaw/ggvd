GeomEuler <- ggproto("GeomEuler", GeomPolygon,
                     required_aes = c("x", "y"),
                     default_aes = aes(colour = "black", fill = NA, size = 0.5,
                                       linetype = 1, alpha = NA),
                     extra_params = c("nset", "method", "na.rm"))

#' Plot Euler diagram
#'
#' @inheritParams ggplot2::geom_polygon
#' @param method `string`, method for fitting Euler diagram. Should be either
#' `"VennDiagram"` or `"eulerr"`.
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
                       method = "VennDiagram",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  # check arguments
  match.arg(method, c("VennDiagram", "eulerr"), several.ok = FALSE)

  # number of sets to compare
  nset <- nrow(data)

  layer(
    stat = stat, geom = GeomEuler, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      nset = nset,
      method = method,
      na.rm = na.rm,
      ...
    )
  )
}
