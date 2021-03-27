library(ggvd)

# Lets make some data
circles2 <- data.frame(
  x0 = c(1, 1.5),
  y0 = c(1, 1),
  a = c(0.5, 0.5),
  b = c(0.5, 0.5),
  angle = c(0, 0),
  count = c(100, 100)
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count), data = circles2, alpha = 0.5, colour = NA) +
  coord_fixed() +
  scale_fill_gradient()

# Lets make some data
circles3 <- data.frame(
  x0 = c(1, 1.5, 1.25),
  y0 = c(1, 1, 1.5),
  a = c(0.5, 0.5, 0.5),
  b = c(0.5, 0.5, 0.5),
  angle = c(0, 0, 0),
  count = c(100, 100, 100)
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count), data = circles3, alpha = 0.5, colour = NA) +
  coord_fixed() +
  scale_fill_gradient()

circles4 <- data.frame(
  x0 = c(-0.72 + 2/3, 2/3, 2/3, 0.72 + 2/3),
  y0 = c(-1/6, 0.2, 0.2, -1/6),
  a = c(0.75, 0.75, 0.75, 0.75),
  b = c(1.5, 1.5, 1.5, 1.5),
  angle = c(pi/4, pi/4, -pi/4, -pi/4),
  count = c(100, 100, 100, 100)
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count), data = circles4, alpha = 0.5, colour = NA) +
  coord_fixed() +
  scale_fill_gradient()
