GeomVenn <- ggproto("GeomVenn", GeomPolygon,

                    required_aes = c("set_names", "elements"),
                    default_aes = aes(colour = "black", fill = NA, alpha = 0.5,
                                      size = 0.5, linetype = 1, fontface = "plain", family = ""),

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
                                          n_sets = 1, type = "discrete",
                                          set_name_pos = NULL,
                                          set_name_colour = NULL, set_name_size = 5,
                                          set_name_face = NULL,
                                          set_name_family = NULL,
                                          count_colour = "black", count_size = 5,
                                          count_face = NULL, count_family = NULL,
                                          count_nudge = 0.04,
                                          percentage = TRUE,
                                          percentage_digits = 1,
                                          percentage_colour = "black",
                                          percentage_size = 3, percentage_face = NULL,
                                          percentage_family = NULL,
                                          percentage_nudge = -count_nudge) {
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
                        munched = munched, n_sets = n_sets, pos = set_name_pos
                      )

                      set_names <- grid::textGrob(
                        set_munched$set_names,
                        x = set_munched$x, y = set_munched$y, default.units = "native",
                        gp = grid::gpar(
                          col = if (is.null(set_name_colour)) first_rows$colour else set_name_colour,
                          fontsize = set_name_size * ggplot2::.pt,
                          fontface = if (is.null(set_name_face)) first_rows$fontface else set_name_face,
                          fontfamily = if (is.null(set_name_family)) first_rows$family else set_name_family
                        )
                      )

                      count_y_nudged <- count_matrix$y + count_nudge
                      pct_y_nudged <- count_matrix$y + percentage_nudge

                      count_matrix$y <- count_y_nudged
                      count_munched <- ggplot2::coord_munch(coord, count_matrix, panel_params)
                      count_matrix$y <- pct_y_nudged
                      pct_munched <- ggplot2::coord_munch(coord, count_matrix, panel_params)

                      counts <- grid::textGrob(
                        count_munched$count,
                        x = count_munched$x, y = count_munched$y, default.units = "npc",
                        gp = grid::gpar(
                          col = count_colour,
                          fontsize = count_size * ggplot2::.pt,
                          fontface = if (is.null(count_face)) first_rows$fontface[1] else count_face,
                          fontfamily = if (is.null(count_family)) first_rows$family[1] else count_family
                        )
                      )

                      if (percentage) {
                        percentages <- grid::textGrob(
                          paste0("(", round(pct_munched$percentage, digits = percentage_digits), "%)"),
                          x = pct_munched$x, y = pct_munched$y, default.units = "npc",
                          gp = grid::gpar(
                            col = percentage_colour,
                            fontsize = percentage_size * ggplot2::.pt,
                            fontface = if (is.null(percentage_face)) first_rows$fontface[1] else percentage_face,
                            fontfamily = if (is.null(percentage_family)) first_rows$family[1] else percentage_family
                          )
                        )
                      } else {
                        percentages <- grid::nullGrob()
                      }

                      ggplot2:::ggname("geom_venn",
                                       grid::grobTree(circle_fill, circle_outline,
                                                      set_names, counts, percentages))
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
                      set_name_pos = NULL,
                      set_name_colour = NULL,
                      set_name_size = 5,
                      set_name_face = NULL,
                      set_name_family = NULL,
                      count_colour = "black",
                      count_size = 5,
                      count_face = NULL,
                      count_family = NULL,
                      count_nudge = 0.06,
                      percentage = TRUE,
                      percentage_digits = 1,
                      percentage_colour = "black",
                      percentage_size = 3,
                      percentage_face = NULL,
                      percentage_family = NULL,
                      percentage_nudge = -count_nudge,
                      na.rm = FALSE,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  list(
    layer(
      stat = stat, geom = GeomVenn, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        type = type,
        set_name_pos = set_name_pos,
        set_name_colour = set_name_colour,
        set_name_size = set_name_size,
        set_name_face = set_name_face,
        set_name_family = set_name_family,
        count_colour = count_colour,
        count_size = count_size,
        count_face = count_face,
        count_family = count_family,
        count_nudge = count_nudge,
        percentage = percentage,
        percentage_digits = percentage_digits,
        percentage_colour = percentage_colour,
        percentage_size = percentage_size,
        percentage_face = percentage_face,
        percentage_family = percentage_family,
        percentage_nudge = percentage_nudge,
        na.rm = na.rm,
        ...
      )
    ),
    scale_x_continuous(limits = c(-2, 2)),
    scale_y_continuous(limits = c(-1.75, 1.75)),
    coord_fixed()
  )

}
