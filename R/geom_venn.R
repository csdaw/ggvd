StatVenn <- ggproto("StatVenn", Stat,

                    required_aes = c("set_names"),
                    optional_aes = c(),

                    setup_params = function(data, params) {
                      params
                    },

                    extra_params = c('n_ellipses', 'n', 'na.rm'),

                    setup_data = function(data, params) {
                      data
                    },

                    compute_panel = function(self, data, scales, n_ellipses = 2,
                                             type = "discrete", n = 360) {
                      if (is.null(data)) return(data)

                      data2 <- generate_ellipses(data, n_ellipses = n_ellipses, n = n)
                      print("str of data2")
                      str(data2)

                      if (type == "discrete") {
                        data2
                      } else if (type == "continuous") {
                        stop("work in progress")
                        # keep only necessary columns
                        out <- data[ ,c("x", "y", "group", "fill", "colour", "PANEL")]

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

GeomVenn <- ggproto("GeomVenn", GeomPolygon,
                    extra_params = c("n_ellipses", "type"),
                    draw_panel = function(data, panel_params, coord, type = "discrete", n_ellipses = 1) {

                      n <- nrow(data)
                      if (n == 1) return(ggplot2::zeroGrob())

                      munched <- ggplot2::coord_munch(coord, data, panel_params)
                      munched <- munched[order(munched$group), ]
                      if (!is.integer(munched$group)) {
                        munched$group <- match(munched$group, unique(munched$group))
                      }

                      # For gpar(), there is one entry per polygon (not one entry per point).
                      # We'll pull the first value from each group, and assume all these values
                      # are the same within each group.
                      first_idx <- !duplicated(munched$group)
                      first_rows <- munched[first_idx, ]

                      circle_fill <- grid::polygonGrob(
                        x = munched$x, y = munched$y,
                        id = munched$group, default.units = 'native',
                        gp = grid::gpar(
                          col = NA,
                          fill = alpha(first_rows$fill, first_rows$alpha)
                        ))

                      circle_outline <- grid::polygonGrob(
                        x = munched$x, y = munched$y,
                        id = munched$group, default.units = 'native',
                        gp = grid::gpar(
                          col = first_rows$colour,
                          fill = NA,
                          lwd = first_rows$size * ggplot2::.pt,
                          lty = first_rows$linetype
                        ))

                      set_munched <- generate_set_pos(
                        coord = coord, panel_params = panel_params,
                        munched = munched, n_ellipses = n_ellipses
                      )

                      set_names <- grid::textGrob(
                        set_munched$set_names,
                        x = set_munched$x, set_munched$y, default.units = "native",
                        gp = grid::gpar(
                          col = "black",
                          fontsize = 6 * ggplot2::.pt
                        )
                      )

                      ggplot2:::ggname("geom_venn",
                                       grid::grobTree(circle_fill, circle_outline,
                                                      set_names))
                    }
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
  n_ellipses <- nrow(data)

  list(
    layer(
      stat = StatVenn, geom = GeomVenn, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        type = type,
        n_ellipses = n_ellipses,
        na.rm = na.rm,
        ...
      )
    ),
    scale_x_continuous(limits = c(-2, 2)),
    scale_y_continuous(limits = c(-1.75, 1.75)),
    coord_fixed()
  )

}
