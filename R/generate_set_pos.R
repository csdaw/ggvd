generate_set_pos <- function(coord, panel_params, munched, n_ellipses) {
  if (!is.null(munched$set_pos)) {
    # todo: implement manual specification of set_name position
  } else {
    if (n_ellipses == 2) {
      set_pos <- gen_2_setpos(set_names = levels(munched$set_names), n_ellipses = n_ellipses)
    } else if (n_ellipses == 3) {
      set_pos <- gen_3_setpos(set_names = levels(munched$set_names), n_ellipses = n_ellipses)
    } else if (n_ellipses == 4) {
      set_pos <- gen_4_setpos(set_names = levels(munched$set_names), n_ellipses = n_ellipses)
    } else {
      stop("geom_venn can only generate 2-4 way Venn diagrams.")
    }

    set_munched <- ggplot2::coord_munch(coord, set_pos, panel_params)
    set_munched
  }
}

gen_2_setpos <- function(set_names, n_ellipses) {
  data.frame(
    set_names = set_names,
    x = c(0, 0.5),
    y = c(0, 0.5),
    group = as.character(seq(1:n_ellipses))
  )
}

gen_3_setpos <- function(set_names, n_ellipses) {
  data.frame(
    set_names = set_names,
    x = c(0, 0.5, 1),
    y = c(0, 0.5, 1),
    group = as.character(seq(1:n_ellipses))
  )
}

gen_4_setpos <- function(set_names, n_ellipses) {
  data.frame(
    set_names = set_names,
    x = c(-1.52, -0.8, 0.8, 1.52),
    y = c(1.5, 1.5 + 11/30, 1.5 + 11/30, 1.5),
    group = as.character(seq(1:n_ellipses))
  )
}


