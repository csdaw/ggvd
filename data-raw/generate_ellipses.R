generate_ellipses <- function(n_sets, n = 360, type = "discrete") {
  stopifnot(type %in% c("discrete", "continuous"))
  stopifnot(n_sets %in% 2:4)
  stopifnot(is.numeric(n))

  points <- rep(seq(0, 2 * pi, length.out = n + 1)[seq_len(n)], n_sets)
  cos_p <- cos(points)
  sin_p <- sin(points)

  # special number to define circle overlap, must be between 0.5 and 1
  overlap <- 0.6

  if (n_sets == 2) {
    ellipses <- tibble::tibble(
      # left, right
      group = as.character(1:n_sets),
      x0 = c(-overlap, overlap),
      y0 = c(0, 0),
      a = c(1, 1),
      b = c(1, 1),
      angle = c(0, 0)
    )
  } else if (n_sets == 3) {
    # special number to shift circles up a bit
    shift <- 0.2

    ellipses <- tibble::tibble(
      # top left, top right, bottom
      group = as.character(1:n_sets),
      x0 = c(-overlap, overlap, 0),
      y0 = c(n/sqrt(3) + shift, n/sqrt(3) + shift, -2*n/sqrt(3) + shift),
      a = c(1, 1, 1),
      b = c(1, 1, 1),
      angle = c(0, 0, 0)
    )
  } else if (n_sets == 4) {
    ellipses <- tibble::tibble(
      # left, center left, center right, right
      group = as.character(1:n_sets),
      x0 = c(-0.72, 0, 0, 0.72),
      y0 = c(-9/30, 2/30, 2/30, -9/30),
      a = c(0.75, 0.75, 0.75, 0.75),
      b = c(1.5, 1.5, 1.5, 1.5),
      angle = c(pi/4, pi/4, -pi/4, -pi/4)
    )
  }

  ellipses <- ellipses[rep(seq_len(n_sets), each = n), ]

  x_tmp <- abs(cos_p) * ellipses$a * sign(cos_p)
  y_tmp <- abs(sin_p) * ellipses$b * sign(sin_p)
  ellipses$x <- ellipses$x0 + x_tmp * cos(ellipses$angle) - y_tmp * sin(ellipses$angle)
  ellipses$y <- ellipses$y0 + x_tmp * sin(ellipses$angle) + y_tmp * cos(ellipses$angle)

  # get rid of now unnecessary columns
  ellipses <- ellipses[, c("x", "y", "group")]

  if (type == "discrete") {
    return(ellipses)

  } else if (type == "continuous") {
    # need to add segment column for recombining the `ellipses` data frame with
    # the segments data frame we create below
    ellipses$segment <- NA_character_

    # split data according to group (i.e. set number)
    ellipses_list <- split(ellipses, f = ellipses$group)

    ellipses_list <- lapply(ellipses_list, function(x) {
      # repeat first polygon point to close polygon
      x[nrow(x) + 1, ] <- x[1, ]

      # just output x and y coordinates
      list(as.matrix(x[c("x", "y")]))
    })

    # convert list of data frames to list of polygons
    polygons_list <- lapply(ellipses_list, function(x) sf::st_polygon(x))

    # perform segmentation of overlapping ellipse polygons using `sf`
    segments_list <- generate_segments(n_sets, polygons_list)

    # convert list of segment polygons into list of segment data frames
    segments_list <- lapply(seq_along(segments_list), function(i) {
      df <- as.data.frame(matrix(unlist(segments_list[[i]]), ncol = 2))
      colnames(df) <- c("x", "y")
      df$group <- as.character(n_sets + i)
      df$segment <- names(segments_list)[[i]]
      df
    })


    out <- rbind(ellipses, do.call(rbind, segments_list))
    out
  }
}

library(sf)
source("data-raw/generate_segments.R")
source("R/st_multi.R")
xxx <- generate_ellipses(n_sets = 2, type = "discrete")
debugonce(generate_ellipses)
yyy <- generate_ellipses(n_sets = 2, type = "continuous")
