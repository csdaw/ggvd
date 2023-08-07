library(ggvd)
library(ggplot2)
library(tibble)

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

df <- tibble(
  x0 = c(0, 1, 0.5),
  y0 = c(0, 0, -0.5),
  count = list(c(11, 22, 33, 1+2, 1+3, 2+3, 1+2+3)),
  var = c("red", "green", "purple")
)

StatTest <- ggproto(
  "StatTest", Stat,
  setup_data = function(data, params) {
    data$a <- if (is.null(data$a)) 1 else data$a
    data$b <- if (is.null(data$b)) 1 else data$b
    data$angle <- if (is.null(data$angle)) 0 else data$angle
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data
  },
  compute_panel = function(data, scales, n = 360L) { # n goes here...
    if (length(data) == 0 || nrow(data) == 0) return(data)
    browser()
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

# data$group is all -1
ggplot(df, aes(x0 = x0, y0 = y0)) +
  geom_test()

# data$group is all -1
ggplot(df, aes(x0 = x0, y0 = y0, fill = count)) +
  geom_test()

# data$group is 3,1,2 (alphabetical order of colour names)
ggplot(df, aes(x0 = x0, y0 = y0, fill = var)) +
  geom_test()
