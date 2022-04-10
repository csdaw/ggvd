library(ggvd)

vec_l <- list(
  A = 1:5,
  B = 3:6
)
eulerr:::parse_list(vec_l)
vec <- c("A" = 2, "A&B" = 3, "B" = 1)
vec2 <- c(2, 3, 1)

names(vec)
names(vec2)
compare_sets(s = vec)
compare_sets(s = vec2)


combo_names <- strsplit(names(vec), split = "&", fixed = TRUE)
setnames <- unique(unlist(combo_names, use.names = FALSE))

n <- length(setnames)
id <- eulerr:::bit_indexr(n)
N <- NROW(id)

areas <- double(N)
for (i in 1L:N) {
  s <- setnames[id[i, ]]
  for (j in seq_along(combo_names)) {
    if (setequal(s, combo_names[[j]])) {
      areas[i] <- combinations[j]
    }
  }
}

input <- "disjoint"

# Decompose or collect set volumes depending on input
if (input == "disjoint") {
  areas_disjoint <- areas
  areas[] <- 0
  for (i in rev(seq_along(areas))) {
    prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
    areas[i] <- sum(areas_disjoint[prev_areas])
  }
} else if (input == "union") {
  areas_disjoint <- double(length(areas))
  for (i in rev(seq_along(areas))) {
    prev_areas <- rowSums(id[, id[i, ], drop = FALSE]) == sum(id[i, ])
    areas_disjoint[i] <- areas[i] - sum(areas_disjoint[prev_areas])
  }
  if (any(areas_disjoint < 0))
    stop("Check your set configuration. Some disjoint areas are negative.")
}

# setup return values
orig <- rep.int(0, N)
fit <- rep.int(0, N)
names(orig) <- names(fit) <-
  apply(id, 1L, function(x) paste0(setnames[x], collapse = "&"))

type <- "venn"

# return venn diagram early if requested
if (type == "venn") {
  fpar <- eulerr:::venn_spec[[n]]
  rownames(fpar) <- setnames

  orig[] <- areas_disjoint

  out <- structure(list(ellipses = fpar,
                        original.values = orig,
                        fitted.values = rep(1, length(orig))),
                   class = c("venn", "euler", "list"))
  return(out)
}
