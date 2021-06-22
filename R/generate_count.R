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

  colnames(segments) <- rev(names(l))
  for (j in 1:n_sets) segments[,j] <- rep(0:1,times=2^(j-1),each=2^(n_sets-j))

  cbind(segments[, ncol(segments):1], count)
}

gen_2_count_pos <- function() {
  tibble::tribble(
    ~seg, ~x, ~y,
    "",   NA, NA,
    "A",  -1,  0,
    "B",   1,  0,
    "AB",  0,  0
  )
}

gen_3_count_pos <- function() {
  # special number to define circle overlap, must be between 0.5 and 1
  n <- 1.2
  n2 <- 0.95
  # special number to shift circles up a bit
  shift <- 0.2

  tibble::tribble(
    ~seg,   ~x, ~y,
    "",     NA, NA,
    "A",    -n2,  n2/sqrt(3) + shift,
    "B",     n2,  n2/sqrt(3) + shift,
    "AB",    0,  n/sqrt(3) + shift,
    "C",     0, -2*n2/sqrt(3) + shift,
    "AC", -n/2,  (-2*n/sqrt(3) + n/sqrt(3))/2 + shift,
    "BC",  n/2,  (-2*n/sqrt(3) + n/sqrt(3))/2 + shift,
    "ABC",   0,  0 + shift
  )
}

gen_4_count_pos <- function() {
  tibble::tribble(
    ~seg,     ~x,    ~y,
    "",       NA,    NA,
    "A",    -1.5,   0.5,
    "B",   -0.75,     1,
    "AB",   -0.9,   0.5,
    "C",    0.75,     1,
    "AC",  -0.75,  -0.8,
    "BC",      0,   0.5,
    "ABC",  -0.5,     0,
    "D",     1.5,   0.5,
    "AD",      0, -1.25,
    "BD",   0.75,  -0.8,
    "ABD",  0.25,  -0.8,
    "CD",    0.9,   0.5,
    "ACD", -0.25,  -0.8,
    "BCD",   0.5,     0,
    "ABCD",    0,  -0.5
  )
}

