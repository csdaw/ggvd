GeomEuler <- ggproto("GeomEuler", GeomPolygon,
                     required_aes = c("x", "y"),
                     default_aes = aes(colour = "black", fill = NA, size = 0.5,
                                       linetype = 1, alpha = NA))

StatEuler <- ggproto("StatEuler", Stat,
                     required_aes = c("x", "y"),
                     extra_params = c("method", "na.rm"),
                     setup_params = function(data, params) {
                       params$nset = nrow(data)
                       print(params)
                       params
                     },
                     setup_data = function(data, params) {
                       print(data)
                       print(params)

                       actual_input <- tibble::tibble(
                         set = c("A", "B"),
                         n = c(3, 3),
                         count = list(c(2, 1, 2))
                       )

                       if (params$method == "VennDiagram") {
                         ellipses <- generate_vd_euler(
                           area1 = actual_input$count[[1]][1],
                           area2 = actual_input$count[[1]][3],
                           cross.area = actual_input$count[[1]][2],
                           max.circle.size = 0.2
                         )
                         print(ellipses)
                       }

                       data
                     },
                     compute_group  = function(data, scales) {
                       data
                     }
)

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

  layer(
    stat = StatEuler, geom = GeomEuler, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      method = method,
      na.rm = na.rm,
      ...
    )
  )
}
