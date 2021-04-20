library(ggvd)

# Lets make some data
circles2 <- data.frame(
  x0 = c(1, 1.5),
  y0 = c(1, 1),
  a = c(0.5, 0.5),
  b = c(0.5, 0.5),
  angle = c(0, 0),
  count = c(100, 100),
  colour = factor(c("red", "blue"))
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count, colour = colour), data = circles2, type = "continuous", alpha = 0.5) +
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
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count), data = circles3, type = "continuous", alpha = 0.5, colour = NA) +
  coord_fixed() +
  scale_fill_gradient()

circles4 <- data.frame(
  x0 = c(-0.72 + 2/3, 2/3, 2/3, 0.72 + 2/3),
  y0 = c(-1/6, 0.2, 0.2, -1/6),
  a = c(0.75, 0.75, 0.75, 0.75),
  b = c(1.5, 1.5, 1.5, 1.5),
  angle = c(pi/4, pi/4, -pi/4, -pi/4),
  count = c(100, 200, 300, 400)
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = count), data = circles4,  type = "continuous", alpha = 0.5, colour = NA) +
  coord_fixed() +
  scale_fill_gradient()

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b=b, angle = angle, fill = as.factor(count)), data = circles4,  type = "discrete", alpha = 0.5, colour = "black") +
  coord_fixed()

