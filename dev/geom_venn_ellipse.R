library(ggplot2)
library(grid)
library(tibble)
# library(ggdebug)

df_test <- data.frame(
  x = c(
    0, 2, 2, 0,
    1, 3, 3, 1,
    0.5, 1.5, 1.5, 0.5
  ),
  y = c(
    0, 0, 2, 2,
    -0.5, -0.5, 2, 2,
    -1, -1, 1, 1
  ),
  fill = rep(c("red", "green", "blue"), each = 4),
  id = rep(c(2, 1, 3), each = 4),
  id2 = rep(c("B", "A", "C"), each = 4)
)

# unless you set aes(group = id) or aes(group = id2),
# the shapes are drawn in reverse row order i.e. blue, then green, then red.
ggplot(df_test, aes(x=x, y=y, fill=fill)) +
  geom_polygon(alpha = 1, colour = "black") +
  scale_fill_identity()



# This function is like base::make.unique, but it
# maintains the ordering of the original names if the values
# are sorted.
make_unique <- function(x, sep = '.') {
  if (!anyDuplicated(x)) return(x)
  groups <- match(x, unique(x))
  suffix <- unsplit(lapply(split(x, groups), seq_along), groups)
  max_chars <- nchar(max(suffix))
  suffix_format <- paste0('%0', max_chars, 'd')
  paste0(x, sep, sprintf(suffix_format, suffix))
}

StatVennEllipse <- ggproto(
  "StatVennEllipse", Stat,
  setup_data = function(data, params) {
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data
  },
  compute_panel = function(data, scales, n = 360L) { # n goes here...
    if (length(data) == 0 || nrow(data) == 0) return(data)
    data$group <- make_unique(as.character(data$group))
    n_ellipses <- nrow(data)
    data <- data[rep(seq_len(n_ellipses), each = n), ]
    points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)],
                  n_ellipses)
    cos_p <- cos(points)
    sin_p <- sin(points)
    x_tmp <- abs(cos_p)^(2 / data$m1) * data$a * sign(cos_p)
    y_tmp <- abs(sin_p)^(2 / data$m2) * data$b * sign(sin_p)
    data$x <- data$x0 + x_tmp * cos(data$angle) - y_tmp * sin(data$angle)
    data$y <- data$y0 + x_tmp * sin(data$angle) + y_tmp * cos(data$angle)
    data
  }
)

# OR a different way, as per StatSpring https://ggplot2-book.org/ext-springs
StatVennEllipse2 <- ggproto(
  "StatVennEllipse", Stat,
  setup_params = function(data, params) {
    # could have an ifelse and store segment info in params for the time being
    # e.g.
    # if (type = "continuous") {
    # params$segfills <- some_vector
    #
    # }
    browser()

  },
  setup_data = function(data, params) {
    browser()

    data # if using segment filles, it will complain about NAs here unless we remove them silently but carefully
    # if segment fills are passed as aes(fill = ...), then you might deal with NAs
    # using ggplot2::remove_missing() as per https://ggplot2-book.org/ext-springs#sec-spring3
    # but with na.rm = TRUE?

    # if (type = "continuous") {
    # remove NA rows that don't have NA in segment related data
    # remove segment related columns?
    # }
  },
  compute_panel = function(data, scales, n = 360L) {

  },
  required_aes = c("x0", "y0"),
  optional_aes = c("a", "b", "angle", "m1", "m2")
)

GeomVennEllipse <- ggproto(
  "GeomVennEllipse", GeomPolygon,
  extra_params = c("na.rm", "n", "count"),
  required_aes = c("x0", "y0"),
  optional_aes = c("a", "b", "angle", "m1", "m2"),
  setup_params = function(data, params) {
    if (class(data$fill) == "list") {
      params$count <- unique(unlist(data$fill))
    }
    params
  },
  setup_data = function(data, params, n = 360L) {
    #browser()
    data$a <- if (is.null(data$a)) 1 else data$a
    data$b <- if (is.null(data$b)) 1 else data$b
    data$angle <- if (is.null(data$angle)) 0 else data$angle
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data$fill <- if (!is.null(data$fill) & class(data$fill) == "list") NULL else data$fill

    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))
    n_ellipses <- nrow(data)
    n_segments <- 2^n_ellipses - 1
    ellipses <- lapply(seq_len(n_ellipses), function(i) {
      #browser()
      ellipse_path <- ggvd::ellipse(
        data$x0[i],
        data$y0[i],
        data$a[i],
        data$b[i],
        data$angle[i],
        n,
        data$m1[i],
        data$m2[i]
      )
      cbind(ellipse_path, unclass(data[i, cols_to_keep]))
    })
    do.call(rbind, ellipses)
  },
  draw_panel = function(data, panel_params, coord, count = NULL) {
    browser()
    print(count)
    if (is.null(data) || nrow(data) == 0) return(zeroGrob())

    # is there some sorting on group necessary here before munching?
    # as per geom-path
    munched <- coord_munch(coord, data, panel_params) # should I use is_closed arg? See https://github.com/tidyverse/ggplot2/blob/main/R/geom-polygon.R
    # or some sorting on group necessary after munching?
    # as per geom-polygon/ggforce::geom_shape?
    # geom_shape also has a strange matching step here is group is not integers...
    # I'll follow geom_shape for now..
    munched <- munched[order(munched$group), ]
    if (!is.integer(munched$group)) {
      munched$group <- match(munched$group, unique0(munched$group))
    }

    aaa <- with(munched[, c("x", "y", "group")], split(munched[, c("x", "y")], group))

    fills <- poly_segment(
      aaa,
      bit_comb(3, boolean = TRUE)[-1, ] # to do: do not hard-code this!
    )

    counts <- count

    test <- vector(mode = "list", length = 7) # to do: do not hard-code this!

    for (i in seq_len(7)) { # to do: do not hard code this!
      test[[i]] <- cbind.data.frame(fills[[i]], list(group = i+3, PANEL = 1, fill = counts[i])) # to do: do not hard-code this
    }

    yyy <- do.call(rbind, test)

    # as per geom_shape
    first_idx <- !duplicated(munched$group)
    first_rows <- munched[first_idx, ]


    # if type = "continous"
    # calculate segments
    # grab fills from params
    # polygonGrob()


    #browser()
    venn_outline <- polygonGrob(
      munched$x, munched$y,
      default.units = "native",
      id = munched$group,
      gp = gpar(
        col = first_rows$colour,
        # fill = alpha(first_rows$fill, first_rows$alpha), #to do: do not hard-code, add some ifelse continuous clause here
        fill = NA,
        lwd = (if (is.null(first_rows$linewidth)) first_rows$size else first_rows$linewidth) * .pt,
        lty = first_rows$linetype
      )
    )


    first_idx2 <- !duplicated(yyy$group)
    first_rows2 <- yyy[first_idx2, ]

    venn_fill <- polygonGrob(
      yyy$x, yyy$y,
      default.units = "native",
      id = yyy$group,
      gp = gpar(
        col = NA,
        fill =first_rows2$fill,
        lwd = 0.5,
        lty = 1
      )
    )



    # I think best to draw segments first if necessary, then outlines
    # To add separate fills use gTree as per https://github.com/tidyverse/ggplot2/blob/main/R/geom-pointrange.R I think?

    ggplot2:::ggname(
      "geom_venn_ellipse",
      grid::grobTree(venn_fill, venn_outline)
    )
  }
)



geom_venn_ellipse <- function(mapping = NULL, data = NULL, geom = "polygon",
                              position = "identity", n = 360L, na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = "VennEllipse", geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

geom_venn_ellipse2 <- function(mapping = NULL, data = NULL, geom = "polygon",
                               position = "identity", n = 360L, na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = "VennEllipse2", geom = "VennEllipse",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

geom_venn_ellipse3 <- function(mapping = NULL, data = NULL, geom = "polygon",
                               position = "identity", n = 360L, na.rm = FALSE,
                               show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = "identity", geom = "VennEllipse",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

df <- data.frame( # row 1 = top left, row 2 = top right, row 3 = bottom centre
  id = c(2, 1, 3, NA),
  id2 = c("B", "D", "C", NA),
  x0 = c(0, 1, 0.5, NA),
  y0 = c(0.5, 0.5, -0.5, NA),
  a = 1,
  b = 1,
  angle = 0,
  alpha = 0.5,
  colour = "black",
  fill = c("red", "green", "blue", NA),
  linetype = "solid",
  linewidth = 2
)

# unless you set aes(group = id) or aes(group = id2),
# the shapes are drawn in reverse row order i.e. blue, then green, then red.
ggplot(df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, fill = fill, group = id2)) +
  geom_venn_ellipse2(alpha = 1, linetype = "solid", colour = "black") +
  scale_fill_identity()

## Trying out segmented fills

# in GeomVennEllipse@draw_panel, after munching you have things in roughly the following format
munched_test <- data.frame(
  fill = rep(c("red", "green", "blue"), each = 4),
  x = c(
    0, 2, 2, 0,
    1, 3, 3, 1,
    0.5, 1.5, 1.5, 0.5
  ),
  y = c(
    0, 0, 2, 2,
    -0.5, -0.5, 2, 2,
    -1, -1, 1, 1
  ),
  group = rep(c(1L, 3L, 2L), each = 4),
  PANEL = factor(1, levels = 1),
  colour = "black",
  linewidth = 0.5,
  linetype = "solid",
  alpha = 1
)

# have a look
ggplot(munched_test, aes(x = x, y = y, fill = fill, group = group)) +
  geom_polygon() +
  scale_fill_identity()

# as per geom_shape
first_idx_test <- !duplicated(munched_test$group)
first_rows_test <- munched_test[first_idx_test, ]

aaa <- split(munched_test[, c("x", "y")], munched_test$group)
source("R/bit_comb.R")
bbb <- bit_comb(n = length(aaa), boolean = TRUE)[-1, ]
bbb
ccc <- ggvd::poly_segment(aaa, bbb)
ccc

# from dev/poly_segment
ddd <- do.call(rbind.data.frame, ccc)

ddd$group <- sub("\\..*$", "", rownames(ddd))
rownames(ddd) <- NULL

ggplot(data = ddd, aes(x, y, fill = group)) +
  geom_polygon()

## TRy different df input

df2 <- tibble( # row 1 = top left, row 2 = top right, row 3 = bottom centre
  id = c(2, 1, 3),
  id2 = c("B", "D", "C"),
  x0 = c(0, 1, 0.5),
  y0 = c(0.5, 0.5, -0.5),
  a = 1,
  b = 1,
  angle = 0,
  alpha = 0.5,
  colour = "black",
  fill = c("red", "green", "blue"),
  linetype = "solid",
  linewidth = 2,
  # segs = rep(unlist(lcombn(1:3)), 3),
  count = list(rep(c(11, 22, 33, 3, 4, 5, 6), 2))
)

ggplot(df2, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, fill = count, group = id)) +
  geom_venn_ellipse3(colour = "black")


f1 <- function(n, boolean = TRUE) {
  tt <- bit_comb(n = n, boolean = boolean)[-1, ]
  apply(tt, 1, function(x) seq_len(ncol(tt))[x])
}

f1(3)

f2 <- function(vv) {
  do.call("c", lapply(seq_along(vv), function(i) combn(vv, i, FUN = list)))
}

xxx <- f2(1:3)

microbenchmark::microbenchmark(
  f1(10),
  f2(1:10)
)


