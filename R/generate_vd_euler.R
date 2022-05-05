generate_vd_euler <- function(area1, area2, cross.area, max.circle.size, inverted) {
  # initialize logical variables to hold special conditions
  special.coincidental <- FALSE
  special.inclusion <- FALSE
  special.exclusion <- FALSE
  list.switch <- FALSE

  if (!inverted) {
    tmp1 <- max(area1, area2);
    tmp2 <- min(area1, area2);
    if (tmp1 != area1) { list.switch <- TRUE; }
    area1 <- tmp1;
    area2 <- tmp2;
    r1 <- sqrt(area1 / pi);
    r2 <- sqrt(area2 / pi);
    if (r2 == 0) {r2 <- 0.5*r1 }
    shrink.factor <- max.circle.size / r1;
  }
  else {
    tmp1 <- max(area1, area2);
    tmp2 <- min(area1, area2);
    if (tmp1 != area1) { list.switch <- TRUE; }
    area1 <- tmp1;
    area2 <- tmp2;
    r1 <- sqrt(area1 / pi);
    r2 <- sqrt(area2 / pi);
    if (r1 == 0) {r1 <- 0.5*r2 }
    shrink.factor <- max.circle.size / r2;
  }

  # convert radii to Grid dimensions
  r1 <- r1 * shrink.factor;
  r2 <- r2 * shrink.factor;

  # check special conditions
  if (area1 == area2 & area2 == cross.area) { special.coincidental <- TRUE; }
  if (cross.area != 0 & (cross.area == area2 | cross.area == area1)) { special.inclusion <- TRUE; }
  if (0 == cross.area) { special.exclusion <- TRUE; }

  denom <- area1+area2-cross.area;
  return("worked!")
}
