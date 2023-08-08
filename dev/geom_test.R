library(ggvd)
library(ggplot2)
library(tibble)

df2ellipse <- function(df, n = 360L) {
  stopifnot(all(c("x0", "y0", "a", "b", "angle", "m1", "m2") %in% colnames(df)))

  n_ellipses <- nrow(df)
  df$group <- if (is.null(df$group) | all(df$group == -1)) seq_len(n_ellipses) else df$group
  df <- df[rep(seq_len(n_ellipses), each = n), ]

  points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)],
                n_ellipses)
  cos_p <- cos(points)
  sin_p <- sin(points)
  x_tmp <- abs(cos_p)^(2 / df$m1) * df$a * sign(cos_p)
  y_tmp <- abs(sin_p)^(2 / df$m2) * df$b * sign(sin_p)
  df$x <- df$x0 + x_tmp * cos(df$angle) - y_tmp * sin(df$angle)
  df$y <- df$y0 + x_tmp * sin(df$angle) + y_tmp * cos(df$angle)
  df
}
# debug(df2ellipse)

df <- tibble(
  x0 = c(0, 1, 0.5),
  y0 = c(0, 0, -0.5),
  count = list(seq(from = 5, by = 5, length.out = 7)),
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
    browser()
    n_ellipses <- nrow(data)
    n_segments <- 2^n_ellipses - 1

    if (!is.null(data$fill) & class(data$fill) == "list") {
      counts <- data$fill[1][[1]]
      data$fill <- NULL
      data <- df2ellipse(data, n = n)
      fills <- poly_segment(
        split(data[, c("x", "y")], data$group),
        tt = bit_comb(n_ellipses, boolean = TRUE)[-1, ]
      )
      test <- vector(mode = "list", length = n_segments)

      for (i in seq_len(n_segments)) {
        test[[i]] <- cbind.data.frame(fills[[i]], list(group = i+n_ellipses, PANEL = 1, fill = counts[i])) # Not sure if hard-coding panel is okay...
      }

      do.call(rbind, test)
    } else {
      df2ellipse(data, n = n)
    }
  }
)

GeomTest <- ggproto(
  "GeomTest", GeomPolygon,
  required_aes = c("x", "y"),
  setup_params = function(data, params) {
    browser()
    params
  }
)

geom_test <- function(mapping = NULL, data = NULL, geom = "polygon",
                      position = "identity", n = 360L, na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data, mapping = mapping, stat = "test", geom = "test",
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

# Top right circle group # = 1, therefore drawn first
# Bottom circle group # = 2, therefore drawn second
# Top left circle group # = 3, therefore drawn third
# Need to generate truth table with counts in a way that can be
# joined in the correct order
# data$group is all -1 unless we set group explicitly
ggplot(df, aes(x0 = x0, y0 = y0, group = set, fill = count)) +
  geom_test() +
  scale_fill_continuous()

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
