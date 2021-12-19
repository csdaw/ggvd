#### Description ####
# This script generates the x, y coordinates used to draw the ellipses in
# geom_venn(). I've abstracted this step outside of the package to avoid
# having the `sf` package as a dependency.

#### Setup ####
library(sf)
source("./data-raw/generate_ellipses.R")
source("./data-raw/generate_segments.R")
source("./R/st_multi.R")

venn_sets <- 2:4
venn_types <- c("discrete", "continuous")
ggvd_data <- vector(mode = "list", length = length(venn_sets))

#### Generate ellipses for geom_venn()
for (i in seq_along(venn_sets)) {
  ggvd_data[[i]] <- lapply(venn_types, function(t) {
    generate_ellipses(n_sets = venn_sets[i], type = t)
  })
  names(ggvd_data[[i]]) <- venn_types
}
names(ggvd_data) <- venn_sets

#### Save ggvd_data as internal data ####
usethis::use_data(ggvd_data, internal = TRUE, overwrite = TRUE)
