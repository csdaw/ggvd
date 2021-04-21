StatVenn <- ggproto("StatVenn", Stat,

                    required_aes = c('x0', 'y0', 'a', 'b', 'angle'),
                    default_aes = aes(m1 = NA, m2 = NA), # needed for super ellipse in ggforce. should delete later once I customise geom_shape

                    setup_params = function(data, params) {
                      print("params!!")
                      str(params)
                      params
                    },

                    extra_params = c('n', 'na.rm'),

                    setup_data = function(data, params) {
                      data$m1 <- ifelse(is.null(data$m1), 2, data$m1) # super ellipse stuff. should delete later once I customise geom_shape
                      data$m2 <- ifelse(is.null(data$m2), data$m1, data$m2) # super ellipse stuff. should delete later once I customise geom_shape
                      print("setup_data!!!")
                      print(ggplot2::scale_type(data$group))
                      str(data)
                      data
                    },

                    compute_panel = function(self, data, scales, type = "discrete", n = 360) {
                      if (is.null(data)) return(data)

                      print("self!!")
                      str(self)

                      print("scales!!")
                      str(scales)

                      data$group <- make.unique(as.character(data$group))

                      print("group!!")
                      str(data$group)

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

                      print("data!!!!!!")
                      str(data)
                      print(unique(data$group))
                      print(unique(data$PANEL))

                      # keep only necessary columns
                      out <- data[ ,c("x", "y", "group", "fill", "colour", "PANEL")]

                      #out

                      if (type == "discrete") {
                        out
                      } else if (type == "continuous") {
                        # replace group to change drawing order
                        out$group <- gsub("1", "Y", out$group)
                        out$group <- gsub("2", "Z", out$group)

                        # remove fill for circle outlines
                        out$fill <- NA

                        data_list <- split(data[c("x", "y", "group")], f = data$group)

                        circles <- lapply(data_list, function(x) {
                          # repeat first polygon point to close polygon
                          x[nrow(x) + 1, ] <- x[1, ]

                          list(as.matrix(x[c("x", "y")]))
                        })

                        polygons <- lapply(circles, function(x) sf::st_polygon(x))

                        if (n_ellipses == 2) {
                          polygon_list <- make_2d_venn(polygons)
                        } else if (n_ellipses == 3) {
                          polygon_list <- make_3d_venn(polygons)
                        } else if (n_ellipses == 4) {
                          polygon_list <- make_4d_venn(polygons)
                        } else {
                          stop("geom_venn can only compare 2-4 sets")
                        }
                        #polygon_counts <- factor(c(100, 200, 300))
                        polygon_counts <- seq(100, length(polygon_list) * 100, 100)
                        print("polygon_counts!!")
                        str(polygon_counts)
                        polygon_names <- names(polygon_list)

                        test_name <- c("X", "Y", "Z")

                        polygon_dfs <- lapply(1:length(polygon_list), function(i) {
                          df <- as.data.frame(matrix(unlist(polygon_list[[i]]), ncol = 2))
                          colnames(df) <- c("x","y")
                          df$group <- polygon_names[[i]]
                          df$fill <- polygon_counts[[i]]
                          df$colour <- NA
                          df$PANEL <- 1
                          df
                        })
                        data_polygons <- do.call(rbind, polygon_dfs)

                        print("data_polygons!!!")
                        str(data_polygons)

                        test <- rbind(out, data_polygons)
                        print("test!!!")
                        str(test)
                        test
                      }
                    }
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
                      position = "identity", ...,
                      type = "discrete",
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  print("mapping!!")
  print(names(mapping))
  str(mapping)
  str(mapping$fill)

  layer(
    stat = StatVenn, geom = GeomVenn, data = data, mapping = mapping,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(
      type = type,
      na.rm = na.rm,
      ...
    )
  )
}
