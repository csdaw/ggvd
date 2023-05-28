library(ggvd)
library(ggplot2)
library(polyclip)

## polyclip has 4 options (intersection, union, minus, xor)

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

