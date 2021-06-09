generate_count <- function(elements) {
  n_sets <- length(elements)

  count <- count_venn(elements)

  gen_count_pos <- match.fun(paste("gen", n_sets, "count_pos", sep = "_"))
  count_pos <- gen_count_pos()

  cbind(count, count_pos)
}

count_venn <- function(l) {
  n_sets <- length(l)

  # give arbitrary names to sets if they are not named already
  if(is.null(names(l))) names(l) <- LETTERS[1:n_sets]

  l_boolean <- lapply(l, function(i) as.numeric(unique(unlist(l)) %in% i))

  count <- as.vector(table(l_boolean))

  n_segments <- 2^n_sets

  segments <- matrix(0, n_segments, n_sets)
  colnames(segments) <- names(l)
  for (j in 1:n_sets) segments[,j] <- rep(0:1,times=2^(j-1),each=2^(n_sets-j))
  cbind(segments, count)
}

gen_2_count_pos <- function() {
  tibble::tribble(
    ~seg, ~x, ~y,
    "",   NA, NA,
    "B",   1,  0,
    "A",  -1,  0,
    "AB",  0,  0
  )
}

gen_3_count_pos <- function() {
  tibble::tribble(
    ~seg,   ~x, ~y,
    "",     NA, NA,
    "C",     0, -1,
    "B",     1,  1,
    "BC",  0.5,  0,
    "A",    -1,  1,
    "AC", -0.5,  0,
    "AB",    0,  1,
    "ABC",   0,  0
  )
}

gen_4_count_pos <- function() {
  tibble::tribble(
    ~seg, ~x, ~y,
    "", NA, NA,
    "D", 1.5, 0.5,
    "C", 0.75, 1,
    "CD", 0.9, 0.5,
    "B", -0.75, 1,
    "BD", 0.75, -0.8,
    "BC", 0, 0.5,
    "BCD", 0.5, 0,
    "A", -1.5, 0.5,
    "AD", 0, -1.25,
    "AC", -0.75, -0.8,
    "ACD", -0.25, -0.8,
    "AB", -0.9, 0.5,
    "ABD", 0.25, -0.8,
    "ABC", -0.5, 0,
    "ABCD", 0, -0.5
  )
}

