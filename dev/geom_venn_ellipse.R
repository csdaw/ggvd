library(ggplot2)
# library(ggdebug)

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
  compute_panel = function(self, data, scales, n = 360L) { # n goes here...
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
  setup_data = function(data, params) {
    data$a <- if (is.null(data$a)) 1 else data$a
    data$b <- if (is.null(data$b)) 1 else data$b
    data$angle <- if (is.null(data$angle)) 0 else data$angle
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data
  },
  compute_panel = function(self, data, scales, n = 360L) {
    #browser()
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))
    ellipses <- lapply(seq_len(nrow(data)), function(i) {
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
  required_aes = c("x0", "y0"),
  optional_aes = c("a", "b", "angle", "m1", "m2")
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
    data = data, mapping = mapping, stat = "VennEllipse2", geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(n = n, na.rm = na.rm, ...)
  )
}

df <- data.frame(
  id = c(2, 1, 3),
  x0 = c(0, 1, 0.5),
  y0 = c(0.5, 0.5, -0.5),
  a = 1,
  b = 1,
  angle = 0,
  alpha = 0.5,
  colour = "black",
  fill = c("red", "green", "blue"),
  linetype = "solid",
  linewidth = 2
)

#apply(df[, c("x0", "y0")], 1, ggvd::ellipse)

#df



ggplot(df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, group = id)) +
  geom_venn_ellipse(alpha = 0.5, aes(fill = as.character(id)),
                    linetype = "solid", colour = "black")

p <- ggplot(df, aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, group = id))

microbenchmark::microbenchmark(
  p + geom_venn_ellipse(),
  p + geom_venn_ellipse2(),
  times = 1000L
)



