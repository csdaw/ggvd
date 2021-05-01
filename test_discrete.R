# 2 way venn discrete
circles2 <- tibble::tibble(
  x0 = c(0.45, 1.05),
  y0 = c(0, 0),
  a = c(0.5, 0.5),
  b = c(0.5, 0.5),
  angle = c(0, 0),
  sets = factor(c("Set 1", "Set 2")),
  fill = factor(c("blue", "yellow"))
)

ggplot() +
  geom_venn(aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle,
                fill = fill, set_names = sets),
            data = circles2, type = "discrete", alpha = 0.5, colour = "black") +
  coord_fixed() +
  scale_fill_identity()
