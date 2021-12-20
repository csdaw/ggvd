# install VennDiagram from my fork
library(VennDiagram)
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
library(ggplot2)
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
