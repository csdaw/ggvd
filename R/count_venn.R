#' @export
count_venn <- function(l) {
  n_sets <- length(l)

  # give arbitrary names to sets if they are not named already
  if(is.null(names(l))) names(l) <- LETTERS[1:n_sets]

  l_boolean <- lapply(l, function(i) as.numeric(unique(unlist(l)) %in% i))

  count <- as.vector(table(l_boolean))
  percentage <- count / max(lengths(l_boolean)) * 100

  n_segments <- 2^n_sets
  segments <- matrix(0, n_segments, n_sets)

  colnames(segments) <- rev(names(l))
  for (j in 1:n_sets) segments[,j] <- rep(0:1,times=2^(j-1),each=2^(n_sets-j))

  cbind(segments[, ncol(segments):1], count, percentage)
}
