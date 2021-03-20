StatVenn <- ggproto("StatVenn", Stat,
                    setup_data = function(data, params) {
                      print("Here!")
                      str(data)
                      data
                    },
                    compute_panel = function(data, scales, n = 360) {
                      data$start <- 0
                      data$end <- 2 * pi
                      print("Now here!")
                      str(data)
                      ggforce:::arcPaths(data, n + 1)
                    },
                    required_aes = c("x0", "y0", "r")
)

GeomVenn <- ggproto("GeomVenn", ggforce::GeomShape,
                    default_aes = aes(colour = "black", fill = NA, size = 0.5, linetype = 1,
                                      alpha = NA)
)

#' Title
#'
#' @description Description.
#'
#' @param mapping Description.
#' @param data Description.
#' @param position Description.
#' @param na.rm Description.
#' @param show.legend Description.
#' @param inherit.aes Description.
#' @param ... Description.
#'
#' @return Description.
#' @export
geom_venn <- function(mapping = NULL, data = NULL,
                      position = "identity", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  layer(
    stat = StatVenn, geom = GeomVenn, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
