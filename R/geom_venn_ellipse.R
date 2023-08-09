#' @export
StatVennEllipse <- ggproto(
  "StatVennEllipse", Stat,
  required_aes = c("x0", "y0"),
  optional_aes = c("a", "b", "angle", "m1", "m2"),
  extra_params = c("n", "na.rm"),
  compute_panel = function(self, data, scales, n = 360L) {
    if (length(data) == 0 || nrow(data) == 0) return(data)

    n_ellipses <- nrow(data)
    n_segments <- 2^n_ellipses - 1
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))

    if (!is.null(data$fill) & class(data$fill) == "list") {
      data$part <- "set"
      counts <- data$fill[1][[1]]
      data$fill <- NULL

      data <- df2ellipse(data, n = n)

      data_segments <- poly_segment(
        split(data[, c("x", "y")], data$group),
        tt = bit_comb(n_ellipses, boolean = TRUE)[-1, ]
      )
      data2 <- vector(mode = "list", length = n_segments)

      for (i in seq_len(n_segments)) {
        data2[[i]] <- cbind.data.frame(
          data_segments[[i]],
          list(group = i+n_ellipses, fill = counts[i], PANEL = factor(1), part = "segment")
        ) # Not sure if hard-coding panel is okay..., but it is necessary for now
      }

      data <- vctrs::vec_rbind(data, do.call(rbind, data2))[, c("x", "y", "part", cols_to_keep)]

      # This is a bit janky, deal with NAs in discrete scale prior to scale determination
      if (class(data$fill) == "character") data$fill[is.na(data$fill)] <- unique(data$fill)[2]
      if (!is.null(data$colour) & class(data$colour) == "character") data$colour[is.na(data$colour)] <- unique(data$colour)[2]
      data
    } else {
      df2ellipse(data, n = n)[, c("x", "y", cols_to_keep)]
    }
  }
)

#' @export
GeomVennEllipse <- ggproto(
  "GeomVennEllipse", GeomPolygon,
  default_aes = aes(colour = "black", fill = NA, linewidth = 0.5, linetype = 1,
                    alpha = NA, subgroup = NULL),
  draw_panel = function(self, data, panel_params, coord) {
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)
    munched <- munched[order(munched$group), ]

    if (!is.null(munched$part)) {
      seg_munched <- munched[munched$part == "segment", ]
      seg_first_idx <- !duplicated(seg_munched$group)
      seg_first_rows <- seg_munched[seg_first_idx, ]

      fills <- grid::polygonGrob(
        seg_munched$x, seg_munched$y,
        default.units = "native",
        id = seg_munched$group,
        gp = grid::gpar(
          col = NA,
          fill = alpha(seg_first_rows$fill, seg_first_rows$alpha),
          lwd = seg_first_rows$linewidth * .pt,
          lty = seg_first_rows$linetype
        )
      )

      set_munched <- munched[munched$part == "set", ]
      set_first_idx <- !duplicated(set_munched$group)
      set_first_rows <- set_munched[set_first_idx, ]

      outlines <- grid::polygonGrob(
        set_munched$x, set_munched$y,
        default.units = "native",
        id = set_munched$group,
        gp = grid::gpar(
          fill = NA,
          col = set_first_rows$colour,
          lwd = set_first_rows$linewidth * .pt,
          lty = set_first_rows$linetype
        )
      )

    } else {
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      fills <- grid::polygonGrob(
        munched$x, munched$y,
        default.units = "native",
        id = munched$group,
        gp = gpar(
          col = first_rows$colour,
          fill = alpha(first_rows$fill, first_rows$alpha),
          lwd = first_rows$linewidth * .pt,
          lty = first_rows$linetype
        )
      )

      outlines <- grid::polygonGrob(
        munched$x, munched$y,
        default.units = "native",
        id = munched$group,
        gp = gpar(
          fill = NA,
          col = first_rows$colour,
          lwd = first_rows$linewidth * .pt,
          lty = first_rows$linetype
        )
      )
    }

    gTree(name = "geom_venn_ellipse", children = grid::gList(fills, outlines))
  }
)

#' @export
geom_venn_ellipse <- function(mapping = NULL, data = NULL,
                              position = "identity", n = 360L, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = StatVennEllipse, geom = GeomVennEllipse,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}
