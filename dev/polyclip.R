library(ggvd)
library(ggplot2)
library(polyclip)

## polyclip has 4 options (intersection, union, minus, xor)

shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(-1, 1, 1, -1),
       y = c(-1, -1, 1, 1)) # square
)

shapes_df <- do.call(rbind, lapply(shapes, data.frame))
shapes_df$id <- rep(c("triangle", "square"), times = c(3, 4))

ggplot(shapes_df, aes(x, y, group = id, colour = id)) +
  geom_polygon() +
  scale_x_continuous(limits = c(-1, 2)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(legend.position = "none")

shapes_int <- polyclip(shapes[[1]], shapes[[2]], op = "intersection")
shapes_int_df <- do.call(rbind, lapply(shapes_int, data.frame))

ggplot(shapes_int_df, aes(x, y)) +
  geom_polygon() +
  scale_x_continuous(limits = c(-1, 2)) +
  scale_y_continuous(limits = c(-1, 1))

# what does polyclip do in different situations?
empty_list <- vector(mode = "list", length = 0)

# returns empty list with warnings, regardless of op
polyclip(empty_list, shapes[[1]], op = "minus")


library(eulerr)

f1 <- venn(5, names = letters[1:5])
debug(eulerr:::setup_geometry)
plot(f1)


# From eulerr/R/geometry.R
poly_clip <- function(a, b, op = c("intersection", "union", "minus", "xor")) {
  op <- match.arg(op)
  a0 <- identical(length(a), 0L)
  b0 <- identical(length(b), 0L)

  if (op == "intersection") {
    if (a0 || b0)
      return(list())
  } else if (op == "union") {
    if (a0 && !b0)
      return(b)
    else if (!a0 && b0)
      return(a)
    else if (!a0 && !b0) { # maybe get rid of this check, I don't think it is necessary
      if (all(unlist(a) == unlist(b))) # this line is producing warnings sometimes,
                           # and polyclip::polyclip already seems to be able to deal with coincident shapes
        return(a)
    } else
      return(list())
  } else if (op == "minus") {
    if (!a0 && b0)
      return(a)
    else if (a0)
      return(list())
  }
  polyclip::polyclip(a, b, op = op)
}

# slightly overlapping different shapes
shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(-1, 1, 1, -1),
       y = c(-1, -1, 1, 1)) # square
)

polyclip::pointinpolygon(shapes[[1]], shapes[[2]]) # mix of 0 and -1 and 1

polyclip(shapes[[1]], shapes[[2]], op = "intersection")
poly_clip(shapes[[1]], shapes[[2]], op = "intersection")

polyclip(shapes[[1]], shapes[[2]], op = "union")
poly_clip(shapes[[1]], shapes[[2]], op = "union") # produces warning

# coincident identical shapes
shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)) # the same triangle
)

polyclip::pointinpolygon(shapes[[1]], shapes[[2]]) # all -1

polyclip(shapes[[1]], shapes[[2]], op = "intersection")
poly_clip(shapes[[1]], shapes[[2]], op = "intersection")

polyclip(shapes[[1]], shapes[[2]], op = "union")
poly_clip(shapes[[1]], shapes[[2]], op = "union")

# coincident non-identical shapes
shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(-1, 3, 3, -1),
       y = c(-1, -1, 3, 3)) # big square
)

polyclip::pointinpolygon(shapes[[1]], shapes[[2]]) # all 1

polyclip(shapes[[1]], shapes[[2]], op = "intersection")
poly_clip(shapes[[1]], shapes[[2]], op = "intersection")

polyclip(shapes[[1]], shapes[[2]], op = "union")
poly_clip(shapes[[1]], shapes[[2]], op = "union") # produces a warning

# non-overlapping shapes
shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(10, 11, 11, 10),
       y = c(20, 20, 21, 21)) # square
)

polyclip::pointinpolygon(shapes[[1]], shapes[[2]]) # all 0

polyclip(shapes[[1]], shapes[[2]], op = "intersection")
poly_clip(shapes[[1]], shapes[[2]], op = "intersection")

polyclip(shapes[[1]], shapes[[2]], op = "union")
poly_clip(shapes[[1]], shapes[[2]], op = "union") # produces warning



# which is faster
zeros <- rep(0, 600)

microbenchmark::microbenchmark(
  sum(zeros) == 0,
  all(as.logical(zeros)),
  all(zeros == 0) # this is flexible to -1 and 1 as well, use this
)


# SO... try changing all(unlist(a) == unlist(b))
# to use polyclip::pointinpolygon instead? (speed concerns?)
# or to all(unlist(a) %in% unlist(b)) (also speed concerns?)
# or just get rid of the check altogether!

# which is faster
shapes <- list(
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)), # triangle
  list(x = c(0, 2, 1),
       y = c(0, 0, 1)) # the same triangle
)

microbenchmark::microbenchmark(
  all(unlist(shapes[[1]]) == unlist(shapes[[2]])), # 2) warning occurs when op = "union"
  all(unlist(shapes[[1]]) %in% unlist(shapes[[2]])), # 1) this is fastest.. but perhaps not the most robust?
  all(pointinpolygon(shapes[[1]], shapes[[2]]) != 0), # 3) this is not thaaat much slower than 2
)









# segments is a list of lists (list length varies)
# the inner lists contain vectors x and y coords (vector length varies)
# hence df nrow varies on outer list length * vector length

e1 <- ellipse()
e2 <- ellipse(x0 = 0.5)

segments <- polyclip(e1, e2, op = "intersection")
length(segments)

df <- do.call(rbind, lapply(segments, data.frame))
nrow(df)

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

segments <- polyclip(e1, e2, op = "union")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

segments <- polyclip(e1, e2, op = "minus")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

segments <- polyclip(e1, e2, op = "xor")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

## Edge cases:

## Mutually exclusive
e1 <- ellipse()
e2 <- ellipse(x0 = 7)

# intersection = segments becomes list of length 0
segments <- polyclip(e1, e2, op = "intersection")

# union = segments behave as normal
segments <- polyclip(e1, e2, op = "union")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

# minus = segments behave as normal
segments <- polyclip(e1, e2, op = "minus")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

# xor = segments behave as normal
segments <- polyclip(e1, e2, op = "xor")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")


## Coincident
e2 <- e1

# intersection = segments behave as normal
segments <- polyclip(e1, e2, op = "intersection")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

# union = segments behave as normal
segments <- polyclip(e1, e2, op = "union")

df <- do.call(rbind, lapply(segments, data.frame))

ggplot(df, aes(x, y)) +
  geom_polygon(fill = "pink", colour = NA) +
  geom_polygon(data = e1, fill = NA, colour = "black") +
  geom_polygon(data = e2, fill = NA, colour = "black")

# minus = segments become list of 0
segments <- polyclip(e1, e2, op = "minus")

# xor = segments become list of 0
segments <- polyclip(e1, e2, op = "xor")

