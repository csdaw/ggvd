GeomVenn <- ggproto("GeomVenn", GeomPolygon,

                    required_aes = c("set_name", "elements"),
                    default_aes = aes(colour = "black", fill = NA, alpha = 0.5,
                                      size = 0.5, linetype = 1, fontface = "plain", family = ""),

                    extra_params = c('type', 'n', 'na.rm'),
                    setup_params = function(data, params) {
                      params$n_sets <- nrow(data)
                      params$count_matrix <- generate_count(data$elements)
                      params$set_totals <- paste0("(", lengths(data$elements), ")")
                      params
                    },
                    setup_data = function(data, params, n = 360) {
                      if (is.null(data)) return(data)

                      # drop list-column as we don't need it anymore
                      data <- data[, !names(data) %in% "elements"]

                      data <- generate_ellipses(data, n_sets = params$n_sets, n = n)
                      data$segment <- NA_character_

                      if (params$type == "continuous") {

                        data_list <- split(data, f = data$group)
                        max_group <- max(as.integer(data$group))

                        ellipses <- lapply(data_list, function(x) {
                          # repeat first polygon point to close polygon
                          x[nrow(x) + 1, ] <- x[1, ]

                          list(as.matrix(x[c("x", "y")]))
                        })

                        polygons <- lapply(ellipses, function(x) sf::st_polygon(x))

                        gen_segments <- generate_segments(params)
                        polygon_list <- gen_segments(polygons)

                        # reorder according to count_matrix (IMPORTANT!)
                        polygon_list <- polygon_list[params$count_matrix$segment[2:2^params$n_sets]]

                        polygon_dfs <- lapply(seq_along(polygon_list), function(i) {
                          df <- as.data.frame(matrix(unlist(polygon_list[[i]]), ncol = 2))
                          colnames(df) <- c("x", "y")
                          df$group <- as.character(max_group + i)
                          df$segment <- names(polygon_list)[[i]]
                          df$PANEL <- factor("1")
                          df$set_name <- factor("thisisaplaceholder")
                          df
                        })

                        fill_df <- do.call(rbind, polygon_dfs)
                        fill_df <- merge(fill_df, params$count_matrix[, c("count", "segment")], by = "segment")
                        names(fill_df)[names(fill_df) == "count"] <- "fill"
                        data <- rbind(data, fill_df)
                      }

                      data
                    },
                    draw_panel = function(data, panel_params, coord, count_matrix,
                                          n_sets = 1, set_totals = NULL,
                                          type = "discrete",
                                          set_name_pos = NULL,
                                          set_name_colour = NULL,
                                          set_name_size = 5,
                                          set_name_face = NULL,
                                          set_name_family = NULL,
                                          set_total = FALSE,
                                          set_total_pos = c(0, -0.15),
                                          set_total_colour = NULL,
                                          set_total_size = 3,
                                          set_total_face = NULL,
                                          set_total_family = NULL,
                                          count_colour = "black",
                                          count_size = 5,
                                          count_face = NULL,
                                          count_family = NULL,
                                          count_nudge = 0.04,
                                          percentage = TRUE,
                                          percentage_digits = 1,
                                          percentage_colour = "black",
                                          percentage_size = 3,
                                          percentage_face = NULL,
                                          percentage_family = NULL,
                                          percentage_nudge = -count_nudge) {
                      if (nrow(data) == 1) return(ggplot2::zeroGrob())

                      munched <- ggplot2::coord_munch(coord, data[is.na(data$segment), ], panel_params)

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

                      if (type == "continuous") {
                        fill_munched <- ggplot2::coord_munch(coord, data[!is.na(data$segment), ], panel_params)

                        if (!is.integer(fill_munched$group)) {
                          fill_munched$group <- as.integer(fill_munched$group)
                        }
                        fill_first_idx <- !duplicated(fill_munched$group)
                        fill_first_rows <- fill_munched[fill_first_idx, ]

                        circle_fill <- grid::polygonGrob(
                          x = fill_munched$x, y = fill_munched$y,
                          id = fill_munched$group, default.units = 'native',
                          gp = grid::gpar(
                            col = NA,
                            fill = alpha(fill_first_rows$fill, fill_first_rows$alpha)
                          ))
                      } else {
                        circle_fill <- grid::polygonGrob(
                          x = munched$x, y = munched$y,
                          id = munched$group, default.units = 'native',
                          gp = grid::gpar(
                            col = NA,
                            fill = alpha(first_rows$fill, first_rows$alpha)
                          ))
                      }

                      circle_outline <- grid::polygonGrob(
                        x = munched$x, y = munched$y,
                        id = munched$group, default.units = 'native',
                        gp = grid::gpar(
                          col = first_rows$colour,
                          fill = NA,
                          lwd = first_rows$size * ggplot2::.pt,
                          lty = first_rows$linetype
                        ))

                      set_pos <- generate_set_pos(
                        coord = coord, panel_params = panel_params,
                        munched = munched, n_sets = n_sets, pos = set_name_pos
                      )
                      set_munched <- ggplot2::coord_munch(coord, set_pos, panel_params)

                      set_names <- grid::textGrob(
                        set_munched$set_name,
                        x = set_munched$x, y = set_munched$y, default.units = "native",
                        gp = grid::gpar(
                          col = if (is.null(set_name_colour)) first_rows$colour else set_name_colour,
                          fontsize = set_name_size * ggplot2::.pt,
                          fontface = if (is.null(set_name_face)) first_rows$fontface else set_name_face,
                          fontfamily = if (is.null(set_name_family)) first_rows$family else set_name_family
                        )
                      )

                      if (set_total) {
                        stopifnot(is.numeric(set_total_pos) && length(set_total_pos) == 2)

                        set_pos$x <- set_pos$x + set_total_pos[1]
                        set_pos$y <- set_pos$y + set_total_pos[2]
                        set_total_munched <- ggplot2::coord_munch(coord, set_pos, panel_params)

                        set_totals <- grid::textGrob(
                          set_totals,
                          x = set_total_munched$x, y = set_total_munched$y,
                          default.units = "native",
                          gp = grid::gpar(
                            col = if (is.null(set_total_colour)) first_rows$colour else set_total_colour,
                            fontsize = set_total_size * ggplot2::.pt,
                            fontface = if (is.null(set_total_face)) first_rows$fontface else set_total_face,
                            fontfamily = if (is.null(set_total_family)) first_rows$family else set_total_family
                          )
                        )
                      } else {
                        set_totals <- grid::nullGrob()
                      }

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
                                                      set_names, set_totals, counts, percentages))
                    }
)


#' Title
#'
#' @description Description.
#'
#' @inheritParams ggplot2::geom_point
#' @param type `string`, type of Venn diagram to plot. Either `"discrete"`
#' (the default) or `"continuous"`.
#' @param set_name_pos Optional. `data.frame` with the same nrow as `data` and
#' two columns: `x` and `y` containing `numeric` coordinates to customize the
#' position of the set name text.
#' @param set_name_colour `string`, colour of the set name text (default is `"black"`).
#' @param set_name_size `numeric`, size of the set name text (default is `5`).
#' @param set_name_face `string`, font face of set name text (default is `"plain"`)
#' @param set_name_family `string`, font family of set name text (default is `"sans"`)
#' @param set_total `logical`, should the total number of elements in each set
#' be shown? (default is `FALSE`).
#' @param set_total_pos Optional. `numeric` of length 2 specifying the position
#' of set total text relative to the center of the set name text (default is
#' `c(0, -0.15)`).
#' @param set_total_colour `string`, colour of the set total text (default is `"black"`).
#' @param set_total_size `numeric`, size of the set total text (default is `3`).
#' @param set_total_face `string`, font face of set total text (default is `"plain"`).
#' @param set_total_family `string`, font family of set total text (default is `"sans"`).
#' @param count_colour `string`, colour of the segment count text (default is `"black"`).
#' @param count_size `numeric`, size of the segment count text (default is `5`).
#' @param count_face `string`, font face of segment count text (default is `"plain"`)
#' @param count_family `string`, font family of segment count text (default is `"sans"`).
#' @param count_nudge `numeric`, amount to nudge segment count text in the y
#' direction (default is `0.06`).
#' @param percentage `logical`, should the percentage of elements in each
#' segment be shown? (default is `TRUE`).
#' @param percentage_digits `numeric`, number of decimal places to show in the
#' percentage text (default is `1`).
#' @param percentage_colour `string`, colour of the percentage text (default is `"black"`).
#' @param percentage_size `numeric`, size of the percentage text (default is `3`).
#' @param percentage_face `string`, font face of percentage text (default is `"plain"`).
#' @param percentage_family `string`, font family of percentage text (default is `"sans"`).
#' @param percentage_nudge `numeric`, amount to nudge percentage text in the y direction
#' (default is `-0.06`).
#'
#' @return Description.
#' @export
#'
#' @examples
#' #
geom_venn <- function(mapping = NULL, data = NULL,
                      stat = "identity",
                      position = "identity", ...,
                      type = "discrete",
                      set_name_pos = NULL,
                      set_name_colour = NULL,
                      set_name_size = 5,
                      set_name_face = NULL,
                      set_name_family = NULL,
                      set_total = FALSE,
                      set_total_pos = c(0, -0.15),
                      set_total_colour = NULL,
                      set_total_size = 4,
                      set_total_face = NULL,
                      set_total_family = NULL,
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
        set_total = set_total,
        set_total_pos = set_total_pos,
        set_total_colour = set_total_colour,
        set_total_size = set_total_size,
        set_total_face = set_total_face,
        set_total_family = set_total_family,
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
