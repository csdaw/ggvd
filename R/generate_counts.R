generate_counts <- function(data) {
  l <- data$elements
  n_sets <- length(l)
  names(l) <- LETTERS[1:n_sets]

  l_boolean <- lapply(l, function(i) as.numeric(unique(unlist(l)) %in% i))

  counts <- as.vector(table(l_boolean))

  n_segments <- 2^n_sets

  segments <- matrix(0, n_segments, n_sets)

  colnames(segments) <- names(l)
  for (j in 1:n_sets) segments[,j] <- rep(0:1,times=2^(j-1),each=2^(n_sets-j))
  cbind(segments, counts)
}
