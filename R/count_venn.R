#' Compute counts for a list of sets
#'
#' @description Compute the overlaps of up to 26 sets in a list.
#' Similar to `vennCounts` from the `limma` package.
#'
#' @param l `list` of up to 26 numeric or character vectors (i.e. sets) to
#' compare. Within each set there must not be any duplicated elements. If the
#' list is not named then capital letters starting from A to Z will be
#' substituted as names.
#'
#' @return Returns a `numeric matrix` with `2^length(l)` rows and
#' `length(l) + 2` columns. Each row corresponds to the overlap of a particular
#' combination of sets. The first `length(l)` columns correspond to each set and
#' contain 1 or 0 indicating membership or not in each set. The second last
#' column called `count` gives the sum of each row and the last column `percent`
#' is the the value of `count` for each row, divided by sum of the `count`
#' column multiplied by 100.
#' @export
#'
#' @examples
#'
#' lst <- list(
#'   set1 = c(1:3),
#'   set2 = c(3:20),
#'   set3 = c(1, 20:66)
#' )
#'
#' count_venn(lst)
#'
count_venn <- function(l) {
  stopifnot(length(l) %in% c(1L:26L))

  n_sets <- length(l)

  # give arbitrary names to sets if they are not named already
  if(is.null(names(l))) names(l) <- LETTERS[1:n_sets]

  # convert l into matrix with one row per unique value
  l_mat <- table(utils::stack(l))

  # set up empty segment matrix, 1 row per segment, 1 column per set
  n_segments <- 2^n_sets
  segments_mat <- matrix(0, n_segments, n_sets)

  colnames(segments_mat) <- names(l)
  for (j in 1:n_sets) segments_mat[, j] <- rep(0:1, times = 2^(j - 1), each = 2^(n_sets - j))

  # perform counting and edit segment matrix
  count_ls <- list()
  for (i in 1:n_sets) count_ls[[i]] <- factor(l_mat[, n_sets - i + 1], levels = c(0, 1))

  count <- as.vector(table(count_ls))
  percentage <- count / max(lengths(count_ls)) * 100

  # bind segment matrix, counts, and percentages
  cbind(segments_mat, count, percentage)
}
