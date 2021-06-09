generate_ellipses <- function(data, n_sets, n) {
  check_n_sets(n_sets)

  data$group <- make.unique(as.character(data$group))

  data <- data[rep(seq_len(n_sets), each = n), ]
  points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)], n_sets)
  cos_p <- cos(points)
  sin_p <- sin(points)

  gen_ellipse <- match.fun(paste("gen", n_sets, "ellipses", sep = "_"))

  ellipses <- gen_ellipse()

  ellipses <- ellipses[rep(seq_len(n_sets), each = n), ]

  x_tmp <- abs(cos_p) * ellipses$a * sign(cos_p)
  y_tmp <- abs(sin_p) * ellipses$b * sign(sin_p)
  data$x <- ellipses$x0 + x_tmp * cos(ellipses$angle) - y_tmp * sin(ellipses$angle)
  data$y <- ellipses$y0 + x_tmp * sin(ellipses$angle) + y_tmp * cos(ellipses$angle)

  data
}

gen_2_ellipses <- function() {
  # special number to define circle overlap, must be between 0.5 and 1
  n <- 0.6

  tibble::tibble(
    # left, right
    x0 = c(-n, n),
    y0 = c(0, 0),
    a = c(1, 1),
    b = c(1, 1),
    angle = c(0, 0)
  )
}

gen_3_ellipses <- function() {
  # special number to define circle overlap, must be between 0.5 and 1
  n <- 0.6
  # special number to shift circles up a bit
  shift <- 0.2

  tibble::tibble(
    # top left, top right, bottom
    x0 = c(-n, n, 0),
    y0 = c(n/sqrt(3) + shift, n/sqrt(3) + shift, -2*n/sqrt(3) + shift),
    a = c(1, 1, 1),
    b = c(1, 1, 1),
    angle = c(0, 0, 0)
  )
}

gen_4_ellipses <- function() {
  tibble::tibble(
    # left, center left, center right, right
    x0 = c(-0.72, 0, 0, 0.72),
    y0 = c(-9/30, 2/30, 2/30, -9/30),
    a = c(0.75, 0.75, 0.75, 0.75),
    b = c(1.5, 1.5, 1.5, 1.5),
    angle = c(pi/4, pi/4, -pi/4, -pi/4)
  )
}
