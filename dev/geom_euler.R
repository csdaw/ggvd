GeomEuler <- ggproto("GeomEuler", GeomPolygon,

                     required_aes = c("set_name", "elements"),
                     default_aes = aes(colour = "black", fill = NA, alpha = 0.5,
                                       size = 0.5, linetype = 1, fontface = "plain", family = ""),

                     extra_params = c('type', 'n', 'na.rm', 'scaled'),
                     setup_params = function(data, params) {
                       params$n_sets <- nrow(data)
                       params$count_matrix <- ggvd:::generate_count(data$elements)
                       params$set_totals <- paste0("(", lengths(data$elements), ")")

                       # for testing purposes
                       params$inverted <- FALSE

                       params
                     },
                     setup_data = function(data, params, n = 360) {
                       browser()
                       if (is.null(data)) return(data)

                       # drop list-column as we don't need it anymore
                       data <- data[, !names(data) %in% "elements"]

                       area1 <- params$count_matrix$count[2]
                       area2 <- params$count_matrix$count[4]
                       cross.area <- params$count_matrix$count[3]

                       max.circle.size <- 0.2

                       # initialize logical variables to hold special conditions
                       special.coincidental <- FALSE
                       special.inclusion <- FALSE
                       special.exclusion <- FALSE
                       list.switch <- FALSE

                       if (!params$inverted) {
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

                       # draw.pairwise.venn logic is quite complicated but overall
                       # the function has the following sections:

                       if (params$scaled & !special.inclusion & !special.exclusion & !special.coincidental) {
                         # Option (1): plot scaled, generic pairwise Venn diagram with or without external texts

                         # calculate centres of circles
                         d <- find.dist(area1, area2, cross.area, inverted = params$inverted);
                         d <- d * shrink.factor;
                         x.centre.1 <- (1 + r1 - r2 - d) / 2;
                         x.centre.2 <- x.centre.1 + d;

                         # draw both circles
                         poly1 <- VennDiagram::ell2poly(x = x.centre.1,
                                                        y = 0.5,
                                                        a = ifelse(!params$inverted, r1, r2),
                                                        b = ifelse(!params$inverted, r1, r2),
                                                        rotation = 0,
                                                        n.sides = 3000)

                         poly2 <- VennDiagram::ell2poly(x = x.centre.2,
                                                        y = 0.5,
                                                        a = ifelse(params$inverted, r1, r2),
                                                        b = ifelse(params$inverted, r1, r2),
                                                        rotation = 0,
                                                        n.sides = 3000)
                       }

                       # else if (euler.d & special.inclusion & !special.coincidental) {
                       # Option (2): plot scaled Venn diagram when one set is completely included
                       # in (but not exactly coincidental with) the other set
                       # with or without external texts
                       # }

                       # else if (euler.d & special.coincidental) {
                       # Option (3): plot scaled Venn diagrams when the two sets are coincidental
                       # }

                       # else if (euler.d & special.exclusion) {
                       # Option (4): plot scaled Venn diagrams when the two sets are mutually exclusive
                       # }



                       # draw.triple.venn logic is even more complicated wow!

                       data
                     },
                     draw_panel = function(data, panel_params, coord, count_matrix,
                                           n_sets = 1, set_totals = NULL,
                                           type = "discrete") {
                       if (nrow(data) == 1) return(ggplot2::zeroGrob())

                       munched <- ggplot2::coord_munch(coord, data[is.na(data$segment), ], panel_params)

                       if (!is.integer(munched$group)) {
                         munched$group <- match(munched$group, unique(munched$group))
                       }

                       # For gpar(), there is one entry per polygon (not one entry per point).
                       # We'll pull the first value from each group, and assume all these values
                       # are the same within each group.
                       first_idx <- !duplicated(munched$group)
                       first_rows <- munched[first_idx, ]

                       circle_outline <- grid::polygonGrob(
                         x = munched$x, y = munched$y,
                         id = munched$group, default.units = 'native',
                         gp = grid::gpar(
                           col = first_rows$colour,
                           fill = NA,
                           lwd = first_rows$size * ggplot2::.pt,
                           lty = first_rows$linetype
                         ))

                       ggplot2:::ggname("geom_euler",
                                        grid::grobTree(circle_outline))
                     }
)

geom_euler <- function(mapping = NULL, data = NULL,
                       stat = "identity",
                       position = "identity", ...,
                       type = "discrete",
                       scaled = TRUE,
                       inverted = FALSE,
                       offset = 0,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {

  list(
    layer(
      stat = stat, geom = GeomEuler, data = data, mapping = mapping,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(
        type = type,
        scaled = scaled,
        inverted = inverted,
        offset = offset,
        na.rm = na.rm,
        ...
      )
    ),
    coord_fixed()
  )

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

ell2poly <- function(x, y, a, b, rotation, n.sides) {
  # draw an n-sided polygon that resembles an ellipse

  rotation <- rotation * pi / 180;
  # calculate the angle corresponding to each "section" of the polygon
  # (there are as many sections as there are sides in the polygon)
  theta <- 2 * pi / n.sides;
  angles <- seq(0, 2 * pi, theta);

  # initialize vectors to hold the x and y coordinates of each vertex of the polygon
  x.coord <- vector(length = n.sides + 1, mode = 'numeric');
  x.coord[1] <- x + a * cos(rotation);
  y.coord <- vector(length = n.sides + 1, mode = 'numeric');
  y.coord[1] <- y + a * sin(rotation);

  # starting from the initial point, sequentially obtain the coordinates of each vertex of the polygon and store them
  for (i in 1:n.sides) {
    x.coord[i + 1] <- x + a * cos(angles[i + 1]) * cos(rotation) - b * sin(angles[i + 1]) * sin(rotation);
    y.coord[i + 1] <- y + a * cos(angles[i + 1]) * sin(rotation) + b * sin(angles[i + 1]) * cos(rotation);
  }

  return(
    list(
      x = x.coord,
      y = y.coord
    )
  );
}
