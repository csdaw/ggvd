generate_set_pos <- function(coord, panel_params, munched, n_sets) {
  if (!is.null(munched$set_pos)) {
    # todo: implement manual specification of set_name position
  } else {
    if (n_sets == 2) {
      set_pos <- gen_2_setpos(set_names = unique(munched$set_names), n_sets = n_sets)
    } else if (n_sets == 3) {
      set_pos <- gen_3_setpos(set_names = unique(munched$set_names), n_sets = n_sets)
    } else if (n_sets == 4) {
      set_pos <- gen_4_setpos(set_names = unique(munched$set_names), n_sets = n_sets)
    } else {
      stop("geom_venn can only generate 2-4 way Venn diagrams.")
    }

    set_munched <- ggplot2::coord_munch(coord, set_pos, panel_params)
    set_munched
  }
}

gen_2_setpos <- function(set_names, n_sets) {
  data.frame(
    # left, right
    set_names = set_names,
    x = c(-0.8, 0.8),
    y = c(1.15, 1.15),
    group = as.character(seq(1:n_sets))
  )
}

gen_3_setpos <- function(set_names, n_sets) {
  # special number to define circle overlap, must be between 0.5 and 1
  n <- 0.6
  # special number to shift circles up a bit
  shift <- 0.2
  # special number to define hypotenuse length
  hypo <- 1.3

  out <- data.frame(
    # top left, top right, bottom
    set_names = set_names,
    x = c(-n - cos(45)*hypo, n + cos(45)*hypo, 0),
    y = c(n/sqrt(3) + shift + sin(45)*hypo,
          n/sqrt(3) + shift + sin(45)*hypo,
          -2*n/sqrt(3) + shift - hypo + 0.15),
    group = as.character(seq(1:n_sets))
  )
}

gen_4_setpos <- function(set_names, n_sets) {
  data.frame(
    # left, center left, center right, right
    set_names = set_names,
    x = c(-1.58, -0.8, 0.8, 1.58),
    y = c(1.05, 1.05 + 11/30, 1.05 + 11/30, 1.05),
    group = as.character(seq(1:n_sets))
  )
}


