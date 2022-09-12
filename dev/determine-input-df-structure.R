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
  B = 3:6
)

circle_coords <- euler(my_sets)
circle_plot <- plot(circle_coords, quantities = TRUE)
circle_plot
# debugonce(eulerr:::plot.euler)
plot(circle_coords, quantities = TRUE)

test_seg_labs <- data.frame(
  seg_count = circle_plot$data$centers$quantities,
  seg_count_x = circle_plot$data$centers$x,
  seg_count_y = circle_plot$data$centers$y
)

test_centers <- circle_plot$data$centers

test_input <- tibble::tibble(
  set = names(my_sets),
  fill = c("red", "green"),
  n = lengths(my_sets),
  count_x = list(circle_plot$data$centers$x),
  count_y = list(circle_plot$data$centers$y),
  count = list(circle_plot$data$centers$quantities)

) %>%
  cbind(circle_coords$ellipses)

test_input %>%
  ggplot() +
  geom_ellipse(aes(x0 = h, y0 = k, a = a, b = b, angle = phi, fill = fill),
               alpha = 0.2) +
  # I am unsure where the x y coords for A B C are stored
  geom_text(aes(label = quantities, x = x, y = y), data = test_centers, size = 4) +
  geom_text(aes(label = labels, x = x, y = y + 0.15), data = test_centers, size = 4.5, fontface = "bold") +
  coord_fixed()

## test plot_euler()
source("dev/plot-euler.R")
source("dev/setup-geometry.R")
debug(setup_geometry)
aaa <- plot_euler(circle_coords, quantities = TRUE, labels = FALSE)

## essential minimal input should probably be
df <- tibble::tibble(
  set = names(my_sets),
  n = lengths(my_sets),
  count = list(c(2, 3, 1))
)

## complete input (maybe?)

# 2 rows
df_complete <- tibble::tibble(
  set = names(my_sets),
  set_text_x = c(-0.54, 0.54),
  set_text_y = c(1.1, 1.1),
  set_circle_fill = c("red", "green"),
  set_circle_colour = c("blue", "brown"),
  set_fontface = c("plain", "bold"),
  set_size = c(4, 2),
  set_text_colour = c("black", "grey"),
  # set_circle_linewidth,
  set_n = lengths(my_sets),
  h = c(-0.54, 0.54),
  k = c(0, 0),
  a = c(0.97, 0.97),
  b = c(0.97, 0.97),
  phi = c(-1.39, -1.39),
  seg = list(c("A", "AB", "B")),
  seg_id = list(1:3),
  seg_text = list(c("A", NA_character_, "B")), # same as set??? redundant??
  seg_count = list(c(2, 1, 2)), # or just count?
  seg_count_x = list(circle_plot$data$centers$x),
  seg_count_y = list(circle_plot$data$centers$y),
  seg_count_colour = list(c("red", "blue", "green"))
)

## OR!

# 3 rows
df_complete2 <- data.frame(
  set = c("Some name of the set", "Another name of the set", NA_character_),
  set_n = c(3, 3, NA_real_),
  set_text_x = c(-0.54, 0.54, NA_real_),
  set_text_y = c(1.1, 1.1, NA_real_),
  # etc etc..
  seg = c("A", "B", "AB"), # standardise these to A, B, C, AB, AC, etc. etc.
  seg_id = 1:3,
  seg_count = c(2, 1, 2),
  seg_percent = c("40%", "20%", "40%"),
  seg_count_x = circle_plot$data$centers$x,
  seg_count_y = circle_plot$data$centers$y
)

# I think this will work best as there is precedence e.g.

zzz <- data.frame(
  x = c(1, 2, NA_real_),
  y = c(4, 4, NA_real_),
  colour = c("red", "blue", NA_character_),
  text = c("These", "are", "labels"),
  text_x = c(1, 1.5, 2),
  text_y = rep(4.025, 3),
  text_colour = c("purple", "orange", "pink")
)

ggplot(zzz, aes(x, y, colour = colour)) +
  geom_point(na.rm = TRUE) + # na.rm = TRUE stops the removed rows warning
  geom_text(aes(label = text, colour = text_colour, x = text_x, y = text_y), na.rm = TRUE) +
  scale_color_identity() # Colour mapping will work without, but this should usually be used

ggplot(zzz, aes(x, y, colour = colour)) +
  geom_point(na.rm = TRUE) + # na.rm = TRUE stops the removed rows warning
  geom_text(aes(label = text, x = text_x, y = text_y), colour = "brown", na.rm = TRUE) +
  scale_color_identity() # Colour mapping will work without, but this should usually be used

## OR!

# 2 sets, 3 segments = 6 rows?
df_complete3 <- data.frame(
  set = rep(c("A", "B", NA_character_), each = 2),
  set_text_x = rep(c(-0.54, 0.54, NA_real_), each = 2)
)
