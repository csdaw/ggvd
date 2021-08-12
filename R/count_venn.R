#' @export
count_venn <- function(l) {
  n_sets <- length(l)

  # give arbitrary names to sets if they are not named already
  if(is.null(names(l))) names(l) <- LETTERS[1:n_sets]

  # convert l into matrix with one row per unique value
  l_mat <- table(stack(l))

  # set up empty segment matrix, 1 row per segment, 1 column per set
  n_segments <- 2^n_sets
  segments_mat <- matrix(0, n_segments, n_sets)

  colnames(segments_mat) <- rev(names(l))
  for (j in 1:n_sets) segments_mat[, j] <- rep(0:1, times = 2^(j - 1), each = 2^(n_sets - j))

  # perform counting and edit segment matrix
  count_ls <- list()
  for (i in 1:n_sets) count_ls[[i]] <- factor(l_mat[, n_sets - i + 1], levels = c(0, 1))

  count <- as.vector(table(count_ls))
  percentage <- count / max(lengths(count_ls)) * 100

  # bind segment matrix, counts, and percentages
  cbind(segments_mat[, ncol(segments_mat):1], count, percentage)
}
