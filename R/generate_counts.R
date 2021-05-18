generate_counts <- function(data) {
  venn_counts(data$elements)
}

venn_counts <- function(l) {
  n_sets <- length(l)

  # give arbitrary names to sets if they are not named already
  if(is.null(names(l))) names(l) <- LETTERS[1:n_sets]

  l_boolean <- lapply(l, function(i) as.numeric(unique(unlist(l)) %in% i))

  counts <- as.vector(table(l_boolean))

  n_segments <- 2^n_sets

  segments <- matrix(0, n_segments, n_sets)
  colnames(segments) <- names(l)
  for (j in 1:n_sets) segments[,j] <- rep(0:1,times=2^(j-1),each=2^(n_sets-j))
  cbind(segments, counts)
}
