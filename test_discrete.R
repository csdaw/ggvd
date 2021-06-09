library(ggvd)

# 2 way venn discrete
circles2 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2")),
  fill = factor(c("blue", "yellow")),
  elements = list(c(1, 2:3), c(2:3, 4:6)),
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
  elements = list(c(1,2:3,7:10,11:15), c(2:3,4:6,11:15,16:21), c(7:10,11:15,16:21,22:28)),
  counts = lengths(elements)
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles3, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()

circles4 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2", "Set 3", "Set 4")),
  fill = factor(c("blue", "yellow", "green", "red")),
  elements = list(c(1,11:15,29:36,46:55,56:66,67:78,79:91,106:120),
                  c(2:3,11:15,16:21,29:36,37:45,46:55,79:91,92:105),
                  c(4:6,16:21,22:28,29:36,37:45,46:55,56:66,67:78),
                  c(7:10,22:28,37:45,46:55,67:78,79:91,92:105,106:120)),
  counts = lengths(elements)
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles4, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()
