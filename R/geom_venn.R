GeomVenn <- ggproto("GeomVenn", GeomPolygon,

                    required_aes = c("set_names", "elements"),
                    optional_aes = c(),

                    extra_params = c('type', 'n', 'na.rm'),
                    setup_params = function(data, params) {
                      params$n_sets <- nrow(data)
                      params$count_matrix <- generate_count(data$elements)
                      params
                    },
                    setup_data = function(data, params, n = 360) {
                      if (is.null(data)) return(data)

                      # drop list-column as we don't need it anymore
                      data <- data[, !names(data) %in% "elements"]

                      data <- generate_ellipses(data, n_sets = params$n_sets, n = n)

                      data
                    },
                    draw_panel = function(data, panel_params, coord, count_matrix,
                                          type = "discrete", n_sets = 1,
                                          set_name_colour = "black", set_name_size = 5) {
                      if (nrow(data) == 1) return(ggplot2::zeroGrob())

                      munched <- ggplot2::coord_munch(coord, data, panel_params)

                      # This line was screwing up the order of circles/labels
                      # munched <- munched[order(munched$group), ]

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
                        munched = munched, n_sets = n_sets
                      )

                      set_names <- grid::textGrob(
                        set_munched$set_names,
                        x = set_munched$x, y = set_munched$y, default.units = "native",
                        gp = grid::gpar(
                          col = set_name_colour,
                          fontsize = set_name_size * ggplot2::.pt
                        )
                      )

                      count_munched <- ggplot2::coord_munch(coord, count_matrix, panel_params)

                      counts <- grid::textGrob(
                        count_munched$count,
                        x = count_munched$x, y = count_munched$y, default.units = "npc",
                        gp = grid::gpar(
                          col = "black",
                          fontsize = set_name_size * ggplot2::.pt
                        )
                      )

                      ggplot2:::ggname("geom_venn",
                                       grid::grobTree(circle_fill, circle_outline,
                                                      set_names, counts))
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
                      stat = "identity",
                      position = "identity", ...,
                      type = "discrete",
                      set_name_colour = "black",
                      set_name_size = 5,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  list(
    layer(
      stat = stat, geom = GeomVenn, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        type = type,
        set_name_colour = set_name_colour,
        set_name_size = set_name_size,
        na.rm = na.rm,
        ...
      )
    ),
    scale_x_continuous(limits = c(-2, 2)),
    scale_y_continuous(limits = c(-1.75, 1.75)),
    coord_fixed()
  )

}
