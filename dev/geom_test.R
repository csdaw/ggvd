library(ggvd)
library(ggplot2)
library(tibble)
library(grid)
library(ggtrace)



#debugonce(df2ellipse)

df <- tibble(
  x0 = c(0, 1, 0.5),
  y0 = c(0, 0, -0.5),
  count = list(seq(from = 5, by = 5, length.out = 7)),
  test = list(c("orange", "black", "blue", "red", "green", "purple", "pink")),
  var = c("red", "green", "purple"),
  set = c(3, 1, 2)
)

StatTest <- ggproto(
  "StatTest", Stat,
  required_aes = c("x0", "y0", "group"),
  setup_data = function(data, params) {
    data$a <- if (is.null(data$a)) 1 else data$a
    data$b <- if (is.null(data$b)) 1 else data$b
    data$angle <- if (is.null(data$angle)) 0 else data$angle
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data
  },
  compute_panel = function(data, scales, n = 360L) {
    if (length(data) == 0 || nrow(data) == 0) return(data)
    #browser()
    n_ellipses <- nrow(data)
    n_segments <- 2^n_ellipses - 1
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))

    if (!is.null(data$fill) & class(data$fill) == "list") {
      data$part <- "set"
      counts <- data$fill[1][[1]]
      data$fill <- NULL

      data <- df2ellipse(data, n = n)
      fills <- poly_segment(
        split(data[, c("x", "y")], data$group),
        tt = bit_comb(n_ellipses, boolean = TRUE)[-1, ]
      )

      test <- vector(mode = "list", length = n_segments)

      for (i in seq_len(n_segments)) {
        test[[i]] <- cbind.data.frame(fills[[i]], list(group = i+n_ellipses, fill = counts[i], PANEL = factor(1), part = "segment")) # Not sure if hard-coding panel is okay..., but it is necessary for now
      }

      out <- vctrs::vec_rbind(data, do.call(rbind, test))[, c("x", "y", "part", cols_to_keep)]

      # This is a bit janky, deal with NAs in discrete scale prior to scale determination
      if (class(out$fill) == "character") out$fill[is.na(out$fill)] <- unique(out$fill)[2]
      if (!is.null(out$colour) & class(out$colour) == "character") out$colour[is.na(out$colour)] <- unique(out$colour)[2]
      out
    } else {
      df2ellipse(data, n = n)[, c("x", "y", cols_to_keep)]
    }
  }
)

GeomTest <- ggproto(
  "GeomTest", GeomPolygon,
  required_aes = c("x", "y"),
  default_aes = aes(colour = NA, fill = NA, linewidth = 0.5, linetype = 1,
                    alpha = NA, subgroup = NULL),
  setup_params = function(data, params) {
    #browser()
    params
  },
  draw_panel = function(data, panel_params, coord) {
    #browser()
    n <- nrow(data)
    if (n == 1) return(zeroGrob())

    munched <- coord_munch(coord, data, panel_params)
    munched <- munched[order(munched$group), ]

    if (!is.null(munched$part)) {
      #browser()
      seg_munched <- munched[munched$part == "segment", ]
      seg_first_idx <- !duplicated(seg_munched$group)
      seg_first_rows <- seg_munched[seg_first_idx, ]

      segments <- grid::polygonGrob(
        seg_munched$x, seg_munched$y,
        default.units = "native",
        id = seg_munched$group, gp = gpar(
          col = NA,
          fill = alpha(seg_first_rows$fill, seg_first_rows$alpha),
          lwd = seg_first_rows$linewidth * .pt,
          lty = seg_first_rows$linetype
        )
      )

      set_munched <- munched[munched$part == "set", ]
      set_first_idx <- !duplicated(set_munched$group)
      set_first_rows <- set_munched[set_first_idx, ]

      sets <- grid::polygonGrob(
        set_munched$x, set_munched$y,
        default.units = "native",
        id = set_munched$group, gp = gpar(
          col = set_first_rows$colour,
          fill = NA,
          lwd = set_first_rows$linewidth * .pt,
          lty = set_first_rows$linetype
        )
      )

      ggplot2:::ggname("geom_test", gTree(children = gList(segments, sets)))

    } else {
      first_idx <- !duplicated(munched$group)
      first_rows <- munched[first_idx, ]

      grid::polygonGrob(
        munched$x, munched$y,
        default.units = "native",
        id = munched$group, gp = gpar(
          col = first_rows$colour,
          fill = alpha(first_rows$fill, first_rows$alpha),
          lwd = first_rows$linewidth * .pt,
          lty = first_rows$linetype
        )
      )
    }
  }
)

geom_test <- function(mapping = NULL, data = NULL,
                      position = "identity", n = 360L, na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = "test", geom = "test",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}


# > ggtrace::get_method_inheritance(GeomTest)
# $Geom
# [1] "aesthetics"      "draw_group"      "draw_layer"      "draw_panel"      "extra_params"    "non_missing_aes"
# [7] "optional_aes"    "parameters"      "required_aes"    "setup_data"      "setup_params"    "use_defaults"
#
# $GeomPolygon
# [1] "default_aes" "draw_key"    "handle_na"   "rename_size"
#
# $GeomTest
# [1] "draw_panel"   "required_aes" "setup_params"


# Top right circle group # = 1, therefore drawn first
# Bottom circle group # = 2, therefore drawn second
# Top left circle group # = 3, therefore drawn third
# Need to generate truth table with counts in a way that can be
# joined in the correct order
# data$group is all -1 unless we set group explicitly
ggplot(df, aes(x0 = x0, y0 = y0, group = set, fill = count)) +
  geom_test() +
  scale_fill_continuous()

ggplot(df, aes(x0 = x0, y0 = y0, group = set, fill = test)) +
  geom_test(colour = "red") +
  scale_fill_discrete()

ggplot(df, aes(x0 = x0, y0 = y0, group = set, fill = count, colour = var)) +
  geom_test(linewidth = 2) +
  scale_fill_continuous() +
  scale_color_discrete()

# data$group is all -1
ggplot(df, aes(x0 = x0, y0 = y0, fill = count)) +
  geom_test() +
  scale_fill_continuous()

# data$group is all -1
ggplot(df, aes(x0 = x0, y0 = y0, fill = count)) +
  geom_test()

# data$group is 3,1,2 (alphabetical order of colour names)
ggplot(df, aes(x0 = x0, y0 = y0, fill = var)) +
  geom_test()

# data$group is all -1, circle are draw in row order, and filled with numbers in set
ggplot(df, aes(x0 = x0, y0 = y0, fill = set)) +
  geom_test()

# data$group is 3, 1, 2, circles are drawn in set order and filled with numbers in set
ggplot(df, aes(x0 = x0, y0 = y0, fill = set, group = set)) +
  geom_test()
