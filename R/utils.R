check_n_sets <- function(n_sets) {
  if (!n_sets %in% c(2, 3, 4)) {
    stop("geom_venn can only generate 2-4 way Venn diagrams.")
  }
}
