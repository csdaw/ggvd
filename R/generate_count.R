generate_count <- function(elements) {
  n_sets <- length(elements)

  # generate x/y positions for count labels
  gen_count_pos <- match.fun(paste("gen", n_sets, "count_pos", sep = "_"))
  count_pos <- gen_count_pos()

  # perform counting
  count <- count_venn(elements)

  # make column in count for merging with count x/y positions
  segment <- vector(mode = "character", length = 2^n_sets)
  for (j in 1:n_sets) {
    segment[count[, j] == 1] <- paste0(segment[count[, j] == 1], colnames(count)[j])
  }

  merge(cbind(data.frame(count), segment), count_pos, by = "segment")
}

gen_2_count_pos <- function() {
  tibble::tribble(
    ~segment, ~x, ~y,
    "",       NA, NA,
    "A",      -1,  0,
    "B",       1,  0,
    "AB",      0,  0
  )
}

gen_3_count_pos <- function() {
  # special number to define circle overlap
  n <- 1.2
  n2 <- 0.95
  # special number to shift circles up a bit
  shift <- 0.2

  tibble::tribble(
    ~segment,   ~x,                                   ~y,
    "",         NA,                                   NA,
    "A",       -n2,                   n2/sqrt(3) + shift,
    "B",        n2,                   n2/sqrt(3) + shift,
    "AB",        0,                    n/sqrt(3) + shift,
    "C",         0,                -2*n2/sqrt(3) + shift,
    "AC",     -n/2, (-2*n/sqrt(3) + n/sqrt(3))/2 + shift,
    "BC",      n/2, (-2*n/sqrt(3) + n/sqrt(3))/2 + shift,
    "ABC",       0,                            0 + shift
  )
}

gen_4_count_pos <- function() {
  tibble::tribble(
    ~segment,      ~x,    ~y,
    "",            NA,    NA,
    "A",         -1.5,   0.3,
    "B",        -0.75,     1,
    "AB",      -0.925,   0.5,
    "C",         0.75,     1,
    "AC",        -0.8, -0.65,
    "BC",           0,  0.55,
    "ABC",       -0.5,     0,
    "D",          1.5,   0.3,
    "AD",           0,  -1.2,
    "BD",         0.8, -0.65,
    "ABD",      0.290, -0.85,
    "CD",       0.925,   0.5,
    "ACD",     -0.290, -0.85,
    "BCD",        0.5,     0,
    "ABCD",         0,  -0.5
  )
}

