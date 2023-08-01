#' Polygon segmentation
#'
#' @description This function will attempt to segment a list of polygons, if
#'   any of them overlap.
#'
#' @param polys `list of lists(x,y)` giving the coordinates of several polygons,
#'   where `x` and `y` are numeric vectors giving the coordinates of the
#'   vertices of a single polygon. The last vertex **should not** repeat the
#'   first vertex.
#' @param tt `boolean matrix`, a truth-table describing which polygons are
#'   involved in each potential segment. This matrix should have the same
#'   number of columns as `length(polys)` and the same number of rows as
#'   `2L^length(polys) - 1L`, i.e. 3 rows for 2 polygons or 7 rows for 3 polygons.
#'
#' @return Returns a `list of lists(x,y)` with the coordinates of the resulting
#'   segmented polygons.
#' @source Slightly modified from `eulerr::setup_geometry()` which is distributed
#'   under the GPLv3 license.
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' e1 <- ellipse()
#' e2 <- ellipse(x0 = 0.5)
#'
#' # Visualise before segmentation
#' ggplot(mapping = aes(x, y)) +
#'   geom_polygon(data = e1, fill = "red") +
#'   geom_polygon(data = e2, fill = "blue")
#'
#' # Perform segmentation
#' truth_table <- matrix(
#'   c(TRUE, FALSE, TRUE,
#'     FALSE, TRUE, TRUE),
#'   ncol = 2,
#'   nrow = 3
#' )
#'
#' segments <- poly_segment(list(as.list(e1), as.list(e2)), truth_table)
#'
#' # Visualise after segmentation
#' segments_df <- do.call(rbind.data.frame, segments)
#' segments_df$segment_id <- sub("\\..*$", "", rownames(segments_df))
#' rownames(segments_df) <- NULL
#'
#' ggplot(segments_df, aes(x, y, fill = segment_id)) +
#'   geom_polygon()
#'
poly_segment <- function(polys, tt) {
  stopifnot(is.list(polys) & !is.data.frame(polys))
  stopifnot(is.matrix(tt))

  n_polys <- length(polys)
  n_segments <- 2L^n_polys - 1L

  stopifnot(n_segments == nrow(tt))

  segments <- vector(mode = "list", length = n_segments)
  names(segments) <- seq_len(n_segments)

  for (i in rev(seq_len(n_segments))) {
    tt_row <- which(tt[i, ])
    # for a given segment, how many polygons are intersecting?
    n_polys_intersect <- length(tt_row)

    if (n_polys_intersect == 1L) {
      segments[[i]] <- list(polys[[tt_row[1]]])
    } else {
      segments[[i]] <- poly_clip(polys[[tt_row[1]]], polys[[tt_row[2]]], "intersection")

      if (n_polys_intersect > 2L) {
        for (j in 3L:n_polys_intersect) {
          segments[[i]] <- poly_clip(segments[[i]], polys[[tt_row[j]]], "intersection")
        }
      }
    }

    for (k in which(!tt[i, ])) {
      segments[[i]] <- poly_clip(segments[[i]], polys[[k]], "minus")
    }
  }

  return(unlist(segments, recursive = FALSE))
}
