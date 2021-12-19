#### Description ####
# This script generates the x, y coordinates used to draw the ellipses in
# geom_venn(). I've abstracted this step outside of the package to avoid
# having the `sf` package as a dependency.

#### Setup ####
# define number of points to use for ellipse polygons
n <- 360

# define allowed Venn types
venn_types <- list("discrete" = data.frame(), "continuous" = data.frame())

# define allowed number of Venn sets
venn_sets <- 2:4

# define an empty list to contain the ellipse data frames
ggvd_data <- rep(list(venn_types), times = length(venn_sets))
names(ggvd_data) <- venn_sets

#### 2-way Venn ####
n_sets <- 2
points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)], n_sets)
cos_p <- cos(points)
sin_p <- sin(points)

# special number to define circle overlap, must be between 0.5 and 1
overlap <- 0.6

ellipses <- tibble::tibble(
  # left, right
  group = as.character(1:n_sets),
  x0 = c(-overlap, overlap),
  y0 = c(0, 0),
  a = c(1, 1),
  b = c(1, 1),
  angle = c(0, 0)
)

ellipses <- ellipses[rep(seq_len(n_sets), each = n), ]

x_tmp <- abs(cos_p) * ellipses$a * sign(cos_p)
y_tmp <- abs(sin_p) * ellipses$b * sign(sin_p)
ellipses$x <- ellipses$x0 + x_tmp * cos(ellipses$angle) - y_tmp * sin(ellipses$angle)
ellipses$y <- ellipses$y0 + x_tmp * sin(ellipses$angle) + y_tmp * cos(ellipses$angle)

# save discrete ellipses in ggvd_data
ggvd_data[[names(ggvd_data) == n_sets]][["discrete"]] <- ellipses

# 3-way Venn

# 4-way Venn
