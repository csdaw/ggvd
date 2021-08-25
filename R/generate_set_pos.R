generate_set_pos <- function(coord, panel_params, munched, n_sets, pos = NULL) {
  check_n_sets(n_sets)

  gen_set_pos <- match.fun(paste("gen", n_sets, "set_pos", sep = "_"))
  set_pos <- gen_set_pos(set_name = unique(munched$set_name), n_sets = n_sets)

  if (!is.null(pos)) {
    stopifnot(is.list(pos) && names(pos) %in% c("x", "y"))
    stopifnot(length(pos) == 2)
    stopifnot(lengths(pos) == n_sets)

    set_pos[, c("x", "y")] = pos
  }
  set_pos
}

gen_2_set_pos <- function(set_name, n_sets) {
  data.frame(
    # left, right
    set_name = set_name,
    x = c(-1.30, 1.30),
    y = c(1.1, 1.1),
    group = as.character(seq(1:n_sets))
  )
}

gen_3_set_pos <- function(set_name, n_sets) {
  # special number to define circle overlap, must be between 0.5 and 1
  n <- 0.6
  # special number to shift circles up a bit
  shift <- 0.2
  # special number to define hypotenuse length
  hypo <- 1.3

  out <- data.frame(
    # top left, top right, bottom
    set_name = set_name,
    x = c(-n - cos(45)*hypo, n + cos(45)*hypo, 0),
    y = c(n/sqrt(3) + shift + sin(45)*hypo,
          n/sqrt(3) + shift + sin(45)*hypo,
          -2*n/sqrt(3) + shift - hypo + 0.15),
    group = as.character(seq(1:n_sets))
  )
}

gen_4_set_pos <- function(set_name, n_sets) {
  data.frame(
    # left, center left, center right, right
    set_name = set_name,
    x = c(-1.85, -0.8, 0.8, 1.85),
    y = c(1.07, 1.14 + 11/30, 1.14 + 11/30, 1.07),
    group = as.character(seq(1:n_sets))
  )
}


