library(ggplot2)
library(ggforce)
library(magrittr)
library(eulerr)

###

# Have a look at eulerr:::parse_list for parsing the list input more quickly
# than my count venn

# Input must be tibble to accommodate list columns
# Input rows == number of sets to be comparse == number of circles to draw
test_df <- tibble::tibble(
  set = c("A", "B"),
  fill = c("red", "blue"),
  colour = c("black", "black"),
  n = c(3, 3),
  seg = list(c("A", "AB", "B"), c("A", "AB", "B")),
  seg_count = list(c(2, 1, 2), c(2, 1, 2)),
  seg_fill = c(NA, NA),
  h = c(-0.54, 0.54),
  k = c(0, 0),
  a = c(0.97, 0.97),
  b = c(0.97, 0.97),
  phi = c(-1.39, -1.39),
)

my_sets <- list(
  A = 1:3,
  B = 3:6,
  C = 1:20
)

circle_coords <- euler(my_sets)
circle_plot <- plot(circle_coords, quantities = TRUE)
debugonce(eulerr:::plot.euler)
plot(circle_coords, quantities = TRUE)

test_seg_labs <- data.frame(
  seg_count = circle_plot$data$centers$quantities,
  seg_count_x = circle_plot$data$centers$x,
  seg_count_y = circle_plot$data$centers$y
)

test_input <- tibble::tibble(
  set = names(my_sets),
  fill = c("red", "green", "blue"),
  n = lengths(my_sets),
  seg_count = list(rep(circle_plot$data$centers$quantities, 2)),
  seg_count_x = list(rep(circle_plot$data$centers$x, 2)),
  seg_count_y = list(rep(circle_plot$data$centers$y, 2))
) %>%
  cbind(circle_coords$ellipses)

test_input %>%
  ggplot() +
  geom_ellipse(aes(x0 = h, y0 = k, a = a, b = b, angle = phi, fill = fill),
               alpha = 0.2) +
  # I am unsure where the x y coords for A B C are stored
  geom_text(aes(label = seg_count, x = seg_count_x, y = seg_count_y), data = test_seg_labs, size = 6) +
  geom_text(aes(label = set), x)

## test plot_euler()
debug(setup_geometry)
aaa <- plot_euler(circle_coords, quantities = TRUE, labels = FALSE)





