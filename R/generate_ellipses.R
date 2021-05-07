generate_ellipses <- function(data, n_ellipses, n) {
  data$group <- make.unique(as.character(data$group))

  data <- data[rep(seq_len(n_ellipses), each = n), ]
  points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)], n_ellipses)
  cos_p <- cos(points)
  sin_p <- sin(points)

  if (n_ellipses == 2) {
    print("generate 2 ellipses!")
    ellipses <- gen_2_ellipses()
  } else if (n_ellipses == 3) {
    ellipses <- gen_3_ellipses()
  } else if (n_ellipses == 4) {
    ellipses <- gen_4_ellipses()
  } else {
    stop("geom_venn can only generate 2-4 way Venn diagrams.")
  }

  ellipses <- ellipses[rep(seq_len(n_ellipses), each = n), ]

  x_tmp <- abs(cos_p) * ellipses$a * sign(cos_p)
  y_tmp <- abs(sin_p) * ellipses$b * sign(sin_p)
  data$x <- ellipses$x0 + x_tmp * cos(ellipses$angle) - y_tmp * sin(ellipses$angle)
  data$y <- ellipses$y0 + x_tmp * sin(ellipses$angle) + y_tmp * cos(ellipses$angle)

  data
}

gen_2_ellipses <- function() {
  tibble::tibble(
    # left, right
    x0 = c(-0.6, 0.6),
    y0 = c(0, 0),
    a = c(1, 1),
    b = c(1, 1),
    angle = c(0, 0)
  )
}

gen_3_ellipses <- function() {
  tibble::tibble(
    # top left, top right, bottom
    x0 = c(-2/3, 2/3, 0),
    y0 = c(2/3, 2/3, -sqrt(2)/3),
    a = c(1, 1, 1),
    b = c(1, 1, 1),
    angle = c(0, 0, 0)
  )
}

gen_4_ellipses <- function() {
  tibble::tibble(
    x0 = c(-0.72 + 2/3, 2/3, 2/3, 0.72 + 2/3),
    y0 = c(-1/6, 0.2, 0.2, -1/6),
    a = c(0.75, 0.75, 0.75, 0.75),
    b = c(1.5, 1.5, 1.5, 1.5),
    angle = c(pi/4, pi/4, -pi/4, -pi/4)
  )
}
