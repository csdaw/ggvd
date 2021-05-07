library(ggvd)

# 2 way venn discrete
circles2 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2")),
  fill = factor(c("blue", "yellow"))
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets),
            data = circles2, type = "discrete", alpha = 0.5, colour = "black") +
  coord_fixed() +
  scale_fill_identity()

# Lets make some data
circles3 <- data.frame(
  sets = factor(c("Set 1", "Set 2", "Set 3")),
  fill = factor(c("blue", "yellow", "green"))
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets), data = circles3, type = "discrete", alpha = 0.5, colour = "black") +
  coord_fixed() +
  scale_fill_identity()

circles4 <- data.frame(
  sets = factor(c("Set 1", "Set 2", "Set 3", "Set 4")),
  fill = factor(c("blue", "yellow", "green", "red"))
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets), data = circles4, type = "discrete", alpha = 0.5, colour = "black") +
  coord_fixed() +
  scale_fill_identity()


