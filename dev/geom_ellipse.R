install.packages("ggforce")

library(ggplot2)
library(dplyr)

ggplot() +
  ggforce::geom_ellipse(aes(x0 = 0, y0 = 0, a = 10, b = 3, angle = 0)) +
  coord_fixed()


data.frame(
  h = c(0, 0),
  k = c(0, 4),
  a = c(10, 10),
  b = c(3, 3),
  phi = c(0, 0),
  fill = c("red", "blue")
) %>%
  ggplot() +
  ggforce::geom_ellipse(aes(x0 = h, y0 = k, a = a, b = b, angle = phi, fill = fill), alpha = 0.2) +
  coord_fixed() +
  scale_fill_identity()

fit1$ellipses
