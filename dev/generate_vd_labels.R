library(ggplot2)
library(ggvd)
library(dplyr)

find.cat.pos <- function(x, y, pos, dist, r = NULL) {

  if (is.null(r)) {
    cat.x <- dist * sin(pos * pi / 180) + x
    cat.y <- dist * cos(pos * pi / 180) + y
  }
  else {
    cat.x <- (r + dist) * sin(pos * pi / 180) + x
    cat.y <- (r + dist) * cos(pos * pi / 180) + y
  }

  return(
    list(
      x = cat.x,
      y = cat.y
    )
  )
}

find.dist <- function(area1, area2, cross.area, inverted = FALSE) {

  if (inverted) {
    r1 <- sqrt(area2 / pi)
    r2 <- sqrt(area1 / pi)
  }
  else {
    r1 <- sqrt(area1 / pi)
    r2 <- sqrt(area2 / pi)
  }

  # set up a sequence of distances corresponding to full intersection to 0 intersection with set resolution (step)
  d <- r1 + r2
  resolution <- 0.001
  d.list <- seq(r1 - r2 + resolution, d, resolution)
  int.list <- sapply(d.list, find.intersect, r1, r2)
  match.list <- (int.list >= cross.area)
  index.true <- length(match.list[match.list])
  index.false <- index.true + 1

  if (0 == index.true) {
    return(d.list[index.false])
  }
  else {
    return(mean(d.list[index.true], d.list[index.false]))
  }
}

# find the intersection of two circles
find.intersect <- function(d, r1, r2) {

  beta  <- (r1^2 + d^2 - r2^2) / (2 * r1 * d)
  gamma <- (r2^2 + d^2 - r1^2) / (2 * r2 * d)

  area <- r1^2 * (acos(beta) - 0.5 * sin(2 * acos(beta))) + r2^2 * (acos(gamma) - 0.5 * sin(2 * acos(gamma)))
  return(area)
}

generate_vd_labels <- function(area1, area2, cross.area, max.circle.size = 0.2,
                               scaled = TRUE, euler.d = TRUE,
                               offset = 0, sep.dist = 0.05, ext.percent = rep(0.05, 3),
                               ext.pos = rep(0, 2), ext.dist = rep(0, 2)) {
  # initialize logical variables to hold special conditions
  special.coincidental <- FALSE
  special.inclusion <- FALSE
  special.exclusion <- FALSE
  list.switch <- FALSE

  # which area is largest?
  max_area <- max(area1, area2)

  # determine radius of both circles and 'shrink' factor to give circles
  # a reasonable size (not too huge)
  r1 <- sqrt(area1 / pi)
  r2 <- sqrt(area2 / pi)
  shrink.factor <- max.circle.size / sqrt(max_area / pi)


  if (max_area != area1) inverted <- TRUE else inverted <- FALSE

  if (!inverted) {
    if (r2 == 0) r2 <- 0.5 * r1
  } else {
    if (r1 == 0) r1 <- 0.5 * r2
  }

  # convert radii to Grid dimensions
  r1 <- r1 * shrink.factor
  r2 <- r2 * shrink.factor

  # check special conditions
  if (area1 == area2 & area2 == cross.area) special.coincidental <- TRUE
  if (cross.area != 0 & (cross.area == area2 | cross.area == area1)) special.inclusion <- TRUE
  if (0 == cross.area) special.exclusion <- TRUE

  ## NORMAL EULER DIAGRAM ##
  if (scaled & !special.inclusion & !special.exclusion & !special.coincidental) {
    # calculate centres of circles
    d <- find.dist(area1, area2, cross.area, inverted = inverted)
    d <- d * shrink.factor

    if (!inverted) {
      x.centre.1 <- (1 + r1 - r2 - d) / 2
      x.centre.2 <- x.centre.1 + d
    } else {
      x.centre.2 <- (1 + r1 - r2 - d) / 2
      x.centre.1 <- x.centre.2 + d
    }

    ellipse1 <- ellipse(
      x0 = x.centre.1,
      y0 = 0,
      a = ifelse(!inverted, r1, r2),
      b = ifelse(!inverted, r1, r2)
    )

    ellipse2 <- ellipse(
      x0 = x.centre.2,
      y0 = 0,
      a = ifelse(inverted, r1, r2),
      b = ifelse(inverted, r1, r2)
    )

    area.1.pos <- x.centre.1 + ifelse(!inverted, -r1 + ( (2 * r1 - (r1 + r2 - d)) / 2), -r2 + ( (2 * r2 - (r2 + r1 - d)) / 2))
    area.2.pos <- x.centre.2 + ifelse(!inverted, r2 - ( (2 * r2 - (r1 + r2 - d)) / 2), r1 - ( (2 * r1 - (r2 + r1 - d)) / 2))

    # left circle label
    if ( (area1 - cross.area) / area1 > ext.percent[1] & (area1 - cross.area) / area2 > ext.percent[1] ) {
      label1 <- data.frame(
        label = area1 - cross.area,
        x = area.1.pos,
        y = 0
      )
    } else {
      label.pos <- find.cat.pos(area.1.pos, 0, ext.pos[1], ext.dist[1], r1)
      label1 <- data.frame(
        label = area1 - cross.area,
        x = label.pos$x,
        y = label.pos$y
      )
    }

    # right circle label
    if ((area2 - cross.area) / area2 > ext.percent[2] & (area2 - cross.area) / area1 > ext.percent[2] ) {
      label2 <- data.frame(
        label = area2 - cross.area,
        x = area.2.pos,
        y = 0
      )
    } else {
      label.pos <- find.cat.pos(area.2.pos, 0, ext.pos[2], ext.dist[2], r2)
      label2 <- data.frame(
        label = area2 - cross.area,
        x = label.pos$x,
        y = label.pos$y
      )
    }

    # intersection label
    if (cross.area / area2 > ext.percent[3] & cross.area / area1 > ext.percent[3] ) {
      label3 <- data.frame(
        label = cross.area,
        x = x.centre.1 + (d - ifelse(!inverted, r2, r1)) + (r1 + r2 - d) / 2,
        y = 0
      )
    } else {
      cross.area.pos <- x.centre.1 + (d - r2) + (r1 + r2 - d) / 2
      cross.pos <- find.cat.pos(cross.area.pos, 0, ext.pos[1], ext.dist[1], r1 + r2)
      label3 <- data.frame(
        label = cross.area,
        x = cross.pos$x,
        y = cross.pos$y
      )
    }

  }
  ## EULER DIAGRAM WHEN ONE SET IS COMPLETELY IN THE OTHER ##
  if (euler.d & special.inclusion & !special.coincidental) {
    ellipse1 <- ellipse(
      x0 = 0.5,
      y0 = 0,
      a = r1,
      b = r1
    )

    ellipse2 <- ellipse(
      x0 = 0.5 - offset * (r1 - r2),
      y0 = 0,
      a = r2,
      b = r2
    )
  }
  ## EULER DIAGRAM WITH COINCIDENTAL SETS ##
  if (euler.d & special.coincidental) {
    ellipse1 <- ellipse(
      x0 = 0.5,
      y0 = 0,
      a = max.circle.size,
      b = max.circle.size
    )

    ellipse2 <- ellipse(
      x0 = 0.5,
      y0 = 0,
      a = max.circle.size,
      b = max.circle.size
    )
  }

  ## EULER DIAGRAM WITH MUTUALLY EXCLUSIVE SETS ##
  if (euler.d & special.exclusion) {
    x.centre.1 <- 0 - r1 - (sep.dist / 2)
    x.centre.2 <- 0 + (sep.dist / 2) + r2

    ellipse1 <- ellipse(
      x0 = x.centre.1,
      y0 = 0,
      a = r1,
      b = r1
    )

    ellipse2 <- ellipse(
      x0 = x.centre.2,
      y0 = 0,
      a = r2,
      b = r2
    )
  }

  do.call(rbind, list(label1, label2, label3))
}

## left circle bigger than right ##
generate_vd_euler(70, 5, 3) %>%
  do.call(rbind, .) %>%
  ggplot(aes(x, y)) +
  geom_polygon(fill = NA, colour = "black") +
  geom_text(aes(x, y, label = label), data = generate_vd_labels(70, 5, 3)) +
  coord_fixed()


