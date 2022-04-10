# install VennDiagram from my fork
remotes::install_github("csdaw/VennDiagram")
library(VennDiagram)
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
library(ggplot2)
library(magrittr)
library(ggvd)
source("R/generate_count.R")
source("dev/geom_euler.R")

# create some data
set.seed(1234)
set1 <- sample(1:2000, 1000)
set2 <- sample(1:2000, 342)
set3 <- sample(1:2000, 694)
set4 <- sample(1:2000, 343)

# pairwise venn - option 1
v1 <- tibble::lst(set1, set2)
p1 <- venn.diagram(v1, filename = NULL)
grid.draw(p1)

v1_2 <- prepare_venn(v1)
ggplot(v1_2) +
  geom_euler(aes(set_name = set_name, elements = elements))

xxx <- lapply(tibble::lst(circle1 = poly1, circle2 = poly2), data.frame)
yyy <- do.call(rbind, xxx) %>%
  tibble::rownames_to_column(var = "circle")
yyy$circle <- gsub("\\.[0-9]+$", "", yyy$circle)

yyy %>%
  ggplot(aes(x = x, y = y, fill = circle)) +
  geom_polygon(colour = "black") +
  coord_fixed()

# pairwise venn - option 2
v2 <- tibble::lst(set1 = c(1,2,3),
                  set2 = c(2,3))

p2 <- venn.diagram(v2, filename = NULL)
grid.draw(p2)

v2_2 <- prepare_venn(v2)
ggplot(v2_2) +
  geom_euler(aes(set_name = set_name, elements = elements))

xxx <- lapply(tibble::lst(circle1 = poly1, circle2 = poly2), data.frame)
yyy <- do.call(rbind, xxx) %>%
  tibble::rownames_to_column(var = "circle")
yyy$circle <- gsub("\\.[0-9]+$", "", yyy$circle)

yyy %>%
  ggplot(aes(x = x, y = y, fill = circle)) +
  geom_polygon(colour = "black") +
  coord_fixed()
