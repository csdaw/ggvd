library(ggvd)

# 2 way venn discrete
circles2 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2")),
  elements = list(c(1, 2:3), c(2:3, 4:6)),
  fill = c("red", "blue"),
  counts = c(1, 3)
)

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = fill),
            data = circles2, type = "discrete") +
  scale_fill_identity() + theme_void()

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = counts),
            data = circles2, type = "continuous") +
  scale_fill_gradient() + theme_void()

# Lets make some data
circles3 <- tibble::tibble(
  sets = factor(c("BY4741", "ATCC90028", "ATCC10231")),
  fill = factor(c("blue", "yellow", "green")),
  elements = list(c(1,2:3,7:10,11:15), c(2:3,4:6,11:15,16:21), c(7:10,11:15,16:21,22:28)),
  counts = c(1, 7, 1)
)

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = fill),
            data = circles3, type = "discrete") +
  scale_fill_identity() + theme_void()

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = counts),
            data = circles3, type = "continuous") +
  scale_fill_gradient() + theme_void()

circles4 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2", "Set 3", "Set 4")),
  fill = factor(c("blue", "yellow", "green", "red")),
  elements = list(c(1,11:15,29:36,46:55,56:66,67:78,79:91,106:120),
                  c(2:3,11:15,16:21,29:36,37:45,46:55,79:91,92:105),
                  c(4:6,16:21,22:28,29:36,37:45,46:55,56:66,67:78),
                  c(7:10,22:28,37:45,46:55,67:78,79:91,92:105,106:120)),
  counts = c(1, 15, 1, 15)
)

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = fill),
            data = circles4, type = "discrete") +
  scale_fill_identity() + theme_void()

ggplot() +
  geom_venn(aes(set_names = sets, elements = elements, fill = counts),
            data = circles4, type = "continuous") +
  scale_fill_gradient(limits = c(0, 15)) + theme_void()

