
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggvd

<!-- badges: start -->
<!-- badges: end -->

Another implementation of Venn diagrams in
[ggplot2](https://github.com/tidyverse/ggplot2/).

Combines the good parts of
[ggvenn](https://github.com/yanlinlin82/ggvenn) and
[ggVennDiagram](https://github.com/gaospecial/ggVennDiagram).

``` r
library(ggvd)
## Loading required package: ggplot2
```

``` r
# 2 way Venn discrete
circles2 <- tibble::tibble(
  sets = factor(c("Set 1", "Set 2")),
  fill = factor(c("blue", "yellow")),
  elements = list(c(1, 2:3), c(2:3, 4:6))
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles2, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# 3 way Venn discrete
circles3 <- tibble::tibble(
  sets = factor(c("Set1", "Set2", "Set3")),
  fill = factor(c("blue", "yellow", "green")),
  elements = list(c(1,2:3,7:10,11:15), c(2:3,4:6,11:15,16:21), c(7:10,11:15,16:21,22:28))
)

ggplot() +
  geom_venn(aes(fill = fill, set_names = sets, elements = elements),
            data = circles3, type = "discrete", alpha = 0.5, colour = "black") +
  scale_fill_identity()
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# 4 way Venn discrete
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
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
