library(ggvd)

# 2 way venn discrete
circles2 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2")),
  fill = factor(c("blue", "yellow")),
  elements = list(c("A", "B", "C"), c("C", "D", "E")),
  counts = lengths(elements)
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles2, type = "discrete", alpha = 0.5, colour = "black", set_name_colour = "red") +
  scale_fill_identity()

# Lets make some data
circles3 <- tibble::tibble(
  sets = factor(c("BY4741", "ATCC90028", "ATCC10231")),
  fill = factor(c("blue", "yellow", "green")),
  elements = list(c(1,2,3), c(3,4,5), c(5,6,7)),
  counts = lengths(elements)
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles3, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()

circles4 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2", "Set 3", "Set 4")),
  fill = factor(c("blue", "yellow", "green", "red")),
  elements = list(c(1,2,3), c(4,5,6), c(7,8,9), c(10, 11, 12)),
  counts = lengths(elements)
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles4, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()
