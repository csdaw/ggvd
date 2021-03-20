StatVenn <- ggproto("StatVenn", Stat,
                    setup_data = function(data, params) {
                      data$m1 <- ifelse(is.null(data$m1), 2, data$m1)
                      data$m2 <- ifelse(is.null(data$m2), data$m1, data$m2)
                      print("Here!")
                      str(data)
                      data
                    },
                    compute_panel = function(self, data, scales, n = 360) {
                      if (is.null(data)) return(data)

                      data$group <- make.unique(as.character(data$group))
                      n_ellipses <- nrow(data)
                      print(n_ellipses)
                      data <- data[rep(seq_len(n_ellipses), each = n), ]
                      points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)],
                                    n_ellipses)
                      cos_p <- cos(points)
                      sin_p <- sin(points)
                      x_tmp <- abs(cos_p)^(2 / data$m1) * data$a * sign(cos_p)
                      y_tmp <- abs(sin_p)^(2 / data$m2) * data$b * sign(sin_p)
                      data$x <- data$x0 + x_tmp * cos(data$angle) - y_tmp * sin(data$angle)
                      data$y <- data$y0 + x_tmp * sin(data$angle) + y_tmp * cos(data$angle)
                      ## convert x and y columns to list of matrices separated by group
                      ## convert matrices to list of polygons
                      # polygons <- lapply(list_of_matrices, function(x) sf::st_polygon(x))
                      ## perform the intersecting/setdiffing potentially by...
                      # if (n_ellipses = 2) {do this}
                      # else if (n_ellipses = 3) {do this}
                      # else if (n_ellipses = 4) {do this}

                      print("Now here!!!")
                      str(data)
                      print(head(data))
                      print(tail(data))
                      data
                    },
                    required_aes = c('x0', 'y0', 'a', 'b', 'angle'),
                    default_aes = aes(m1 = NA, m2 = NA),
                    extra_params = c('n', 'na.rm')
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
