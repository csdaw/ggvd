StatEllipse2 <- ggproto(
  "StatEllipse2", Stat,
  required_aes = c("x0", "y0", "a", "b", "angle"),
  default_aes = aes(m1 = NA, m2 = NA),
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    # From ggforce::StatEllip
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2
    data
  },
  compute_panel = function(data, scales, n = 360) {
    if (nrow(data) == 0) return(data)
    print(paste("N IS", n))
    # CSD note: assumes 1 data row per circle
    n_ellipses <- nrow(data)
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))

    ellipses <- lapply(
      seq_len(n_ellipses),
      function(i) {
        ellipse_path <- ellipse(
          x0 = data$x0[i],
          y0 = data$y0[i],
          a = data$a[i],
          b = data$b[i],
          angle = data$angle[i],
          n = n,
          m1 = data$m1[i],
          m2 = data$m2[i]
        )

        cbind(ellipse_path, unclass(data[i, cols_to_keep]))
      }
    )

    do.call(rbind, ellipses)
  }
)

#' Draw (super)ellipses
#'
#' @description A short description...
#'
#' @inheritParams ggplot2::geom_polygon
#' @param n `integer`, number of points to sample along the ellipse.
#'
#' @return Returns something.
#' @export
#'
#' @examples
#' # An example
geom_ellipse <- function(mapping = NULL, data = NULL,
                         stat = "ellipse2", position = "identity", n = 360,
                         ...,
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}







library(ggplot2)
library(magrittr)


# Circles are drawn in order of
df <- data.frame(
  set = c(1, 2, NA),
  banana = c("red", "blue", NA),
  x0 = c(-0.5, 0.5, NA),
  y0 = c(0, 0, NA),
  a = c(1, 1, NA),
  b = c(1, 1, NA),
  angle = c(0, 0, NA)
)

StatEllipse2 <- ggproto(
  "StatEllipse2", Stat,
  required_aes = c("x0", "y0", "a", "b", "angle"),
  default_aes = aes(m1 = NA, m2 = NA),
  extra_params = c("na.rm"),
  setup_data = function(data, params) {
    print("ooo")

    # From ggforce::StatEllip
    data$m1 <- if (is.null(data$m1)) 2 else data$m1
    data$m2 <- if (is.null(data$m2)) data$m1 else data$m2

    # Remove NA rows to avoid warning when
    # NAs are automatically removed somewhere between here
    # and compute_panel
    # (otherwise must use na.rm = TRUE)
    data <- data[!is.na(data$x0), ]
    data
  },
  compute_panel = function(data, scales) {
    cols_to_keep <- setdiff(names(data), c("x0", "y0", "a", "b", "angle", "m1", "m2"))

    ellipses <- lapply(seq_len(nrow(data)), function(i) {
      ellipse_path <- ggvd:::ellipse(
        x0 = data$x0[i],
        y0 = data$y0[i],
        a = data$a[i],
        b = data$b[i],
        angle = data$angle[i],
        n = 360,
        m1 = data$m1[i],
        m2 = data$m2[i]
      )

      cbind(ellipse_path, unclass(data[i, cols_to_keep]))
    })

    print("aaa")
    out <- do.call(rbind, ellipses)
    print(out)
    out
  }
)

geom_ellipse <- function(mapping = NULL, data = NULL,
                         stat = "ellipse2", position = "identity",
                         ...,
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

# In this case, there is no grouping variable so the two circles are considered
# the same shape
df %>%
ggplot() +
  aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle) +
  geom_ellipse(colour = "black", alpha = 0.5) +
  scale_fill_identity() +
  coord_fixed()

# In this case, the circles are grouped by fill, and the circles are drawn in
# alphabetical order of the fill column i.e. blue circle = group 1 and
# red circle = group 2
df %>%
  ggplot() +
  aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, fill = banana) +
  geom_ellipse(colour = "black", alpha = 0.5, n = 360) +
  scale_fill_identity() +
  coord_fixed()

# In this case, the circles are grouped by set, and the circles are drawn in
# sort order of the set column i.e. red circle = group 1 and blue circle = group 2
df %>%
  ggplot() +
  aes(x0 = x0, y0 = y0, a = a, b = b, angle = angle, fill = banana, group = set) +
  geom_ellipse(colour = "black", alpha = 0.5) +
  scale_fill_identity() +
  coord_fixed()

