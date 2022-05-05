#' @noRd
#' @export
generate_vd_euler <- function(area1, area2, cross.area, max.circle.size = 0.2,
                              inverted = FALSE, scaled = TRUE, euler.d = TRUE,
                              offset = 0, sep.dist = 0.05) {
  # initialize logical variables to hold special conditions
  special.coincidental <- FALSE
  special.inclusion <- FALSE
  special.exclusion <- FALSE
  list.switch <- FALSE

  if (!inverted) {
    tmp1 <- max(area1, area2)
    tmp2 <- min(area1, area2)
    if (tmp1 != area1) list.switch <- TRUE
    area1 <- tmp1
    area2 <- tmp2
    r1 <- sqrt(area1 / pi)
    r2 <- sqrt(area2 / pi)
    if (r2 == 0) r2 <- 0.5*r1
    shrink.factor <- max.circle.size / r1
  } else {
    tmp1 <- max(area1, area2)
    tmp2 <- min(area1, area2)
    if (tmp1 != area1) list.switch <- TRUE
    area1 <- tmp1
    area2 <- tmp2
    r1 <- sqrt(area1 / pi)
    r2 <- sqrt(area2 / pi)
    if (r1 == 0) r1 <- 0.5*r2
    shrink.factor <- max.circle.size / r2
  }

  # convert radii to Grid dimensions
  r1 <- r1 * shrink.factor;
  r2 <- r2 * shrink.factor;

  # check special conditions
  if (area1 == area2 & area2 == cross.area) special.coincidental <- TRUE
  if (cross.area != 0 & (cross.area == area2 | cross.area == area1)) special.inclusion <- TRUE
  if (0 == cross.area) special.exclusion <- TRUE

  denom <- area1 + area2 - cross.area

  ## NORMAL EULER DIAGRAM ##
  if (scaled & !special.inclusion & !special.exclusion & !special.coincidental) {
    # calculate centres of circles
    d <- find.dist(area1, area2, cross.area, inverted = inverted)
    d <- d * shrink.factor
    x.centre.1 <- (1 + r1 - r2 - d) / 2
    x.centre.2 <- x.centre.1 + d

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
  }
  ## EULER DIAGRAM WHEN ONE SET IS COMPLETELY IN THE OTHER ##
  if (euler.d & special.inclusion & !special.coincidental) {
    if (inverted) {
      tmp1 <- area1
      tmp2 <- area2
      area1 <- tmp2
      area2 <- tmp1
    }

    if (!scaled & !inverted) {
      r1 <- 0.4
      r2 <- 0.2
    }

    if (!scaled & inverted) {
      r1 <- 0.2
      r2 <- 0.4
    }

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

  ## EULER DIAGRAM WITH MUTUALLY EXCLUSIVE SETS
  if (euler.d & special.exclusion) {
    # determine centres of exclusive circles and draw them
    x.centre.1 <- (1 - 2 * (r1 + r2)) / 2 + r1 - sep.dist / 2
    x.centre.2 <- 1 - (1 - 2 * (r1 + r2)) / 2 - r2 + sep.dist / 2

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

  list(ellipse1, ellipse2)
}

find.dist <- function(area1, area2, cross.area, inverted = FALSE) {

  if (inverted) {
    r2 <- sqrt(area1 / pi);
    r1 <- sqrt(area2 / pi);
  }
  else {
    r1 <- sqrt(area1 / pi);
    r2 <- sqrt(area2 / pi);
  }

  # set up a sequence of distances corresponding to full intersection to 0 intersection with set resolution (step)
  d <- r1 + r2;
  resolution <- 0.001;
  d.list <- seq(r1 - r2 + resolution, d, resolution);
  int.list <- sapply(d.list, find.intersect, r1, r2);
  match.list <- (int.list >= cross.area);
  index.true <- length(match.list[match.list]);
  index.false <- index.true + 1;

  if (0 == index.true) {
    return(d.list[index.false]);
  }
  else {
    return(mean(d.list[index.true], d.list[index.false]));
  }
}

# find the intersection of two circles
find.intersect <- function(d, r1, r2) {

  beta  <- (r1^2 + d^2 - r2^2) / (2 * r1 * d);
  gamma <- (r2^2 + d^2 - r1^2) / (2 * r2 * d);

  area <- r1^2 * (acos(beta) - 0.5 * sin(2 * acos(beta))) + r2^2 * (acos(gamma) - 0.5 * sin(2 * acos(gamma)));
  return(area);
}
