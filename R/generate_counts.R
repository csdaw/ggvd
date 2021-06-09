generate_count <- function(elements) {
  n_sets <- length(elements)

  count <- count_venn(elements)

  #gen_count_pos <- match.fun(paste("gen", n_sets, "count_pos", sep = "_"))
  #count_pos <- gen_count_pos()

  count
}

count_venn <- function(l) {
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

gen_2_count_pos <- function() {
  data.frame(
    x = c(NA, 1, -1, 0),
    y = c(NA, 0, 0, 0)
  )
}

gen_3_count_pos <- function() {
  data.frame(
    #x = c(NA, 0, 1, 0.5, -1, 0, -0.5, 0),
    #y = c(NA, -1, 0.75, -0.15, 0.75, 0, 1, 0)
    x = c(NA, rep(0, 7)),
    y = c(NA, rep(0, 7))
  )
}

gen_4_count_pos <- function() {
  data.frame(
    x = c(NA, rep(0, 15)),
    y = c(NA, rep(0, 15))
  )
}

