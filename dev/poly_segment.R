library(ggvd)
library(ggplot2)

# eulerr/R/setup_geometry.R
# https://github.com/jolars/eulerr/blob/30fe2dd6213cb76cdd94a12d625b028819e80078/R/setup_geometry.R#L49

# e is a list of lists of x/y vectors
# each entry of e is 1 ellipse

# pieces is a list of length n_segments

# n_id = n_segments

# id is a truth-table? matrix with no rownames, no colnames,
# with n_row = n_segments (non-empty segments)
# and n_col = n_sets (non-empty sets)

# so idx is the i'th row of the id matrix
# and is a numeric corresponding to which sets
# are overlapped for the segment i
# e.g.
# Segment ABC for a 3 set venn would have the idx
# which(TRUE TRUE TRUE)
# which would be be the numeric vector
# 1 2 3

# n_idx = length(idx) # how many of the n_sets are
# included in this segment


### OUTER LOOP (for i in rev(seq_len(n_segments)))
# Deal with each segment (each i) at a time

# IF

# if n_idx == 1, i.e. if the segment describes just 1 set
# Then to the pieces list[[i]]
# assign a list, which contains a list, which contains x and y vectors
# of the first ellipse in e

# ELSE

# piece[[i]] is the intersection of the first ellipse (list of vectors) and the
# second ellipse (list of vectors)
# if n_idx > 2
## INNER LOOP (for j in 3:n_idx, i.e. 3:n_sets in this segment)
# overwrise pieces[[i]] with the intersection of pieces[[i]] and
# each ellipse more than the second ellipse i.e. e[[3]], e[[4]] etc. etc.

# THEN

# for the sets NOT described by the segment
# overwrite pieces[[i]] with the 'minus' of pieces[[i]]
# and the ellipses for the sets NOT described by the
# segment


### END OUTER LOOP

# Input e: list of lists of length=2 with xy vectors
# Input tt: truth table with nrow the same as length(e)

# output: named list of lists of length=2 with xy vectors

poly_segment <- function(e, tt) {
  n_e <- length(e)
  n_segments <- 2L^n_e - 1L

  segments <- vector(mode = "list", length = n_segments)
  names(segments) <- seq_len(n_segments)


  for (i in rev(seq_len(n_segments))) {
    tt_row <- which(tt[i, ])
    n_intersects <- length(tt_row)

    if (n_intersects == 1L) {
      segments[[i]] <- list(e[[tt_row[1]]])
    } else {
      segments[[i]] <- poly_clip(e[[tt_row[1]]], e[[tt_row[2]]], "intersection")

      if (n_intersects > 2L) {
        for (j in 3L:n_segments) {
          segments[[i]] <- poly_clip(segments[[i]], e[[tt_row[j]]], "intersection")
        }
      }
    }

    for (k in which(!tt[i, ])) {
      segments[[i]] <- poly_clip(segments[[i]], e[[k]], "minus")
    }
  }

  return(unlist(segments, recursive = FALSE))
}

e1 <- ellipse()
e2 <- ellipse(x0 = 0.5)

elist <- list(as.list(e1), as.list(e2))

ggplot(mapping = aes(x, y)) +
  geom_polygon(data = e1, colour = "red") +
  geom_polygon(data = e2, colour = "blue")

truthtable <- matrix(
  c(T, F, T, F, T, T),
  ncol = 2
)

#debugonce(poly_segment)
test <- poly_segment(elist, truthtable)
test
names(test)
length(test)


## To do: write function to convert poly_segment output to a dataframe for
## plotting

test2 <- do.call(rbind.data.frame, test)
head(test2)
rownames(test2)

test3 <- test2
test3$segment_id <- sub("\\..*$", "", rownames(test3))
rownames(test3) <- NULL

ggplot(data = test3, aes(x, y, fill = segment_id)) +
  geom_polygon()


## EDGE CASES TESTS

# coincident circles
e1 <- ellipse()
e2 <- e1
elist <- list(as.list(e1), as.list(e2))

truthtable <- matrix(
  c(T, F, T, F, T, T),
  ncol = 2
)

debugonce(poly_segment)
test <- poly_segment(elist, truthtable)
test
names(test)
length(test) # length = 1!

# list names doesn't work if there is only 1 non-zero list
test2 <- do.call(rbind.data.frame, test)
head(test2)
rownames(test2)

test3 <- test2
test3$segment_id <- sub("\\..*$", "", rownames(test3))
rownames(test3) <- NULL

ggplot(data = test3, aes(x, y, fill = segment_id)) +
  geom_polygon()

# mutually exclusive circles (This seems to be fine)
e1 <- ellipse()
e2 <- ellipse(x0 = 10)
elist <- list(as.list(e1), as.list(e2))

truthtable <- matrix(
  c(T, F, T, F, T, T),
  ncol = 2
)

#debugonce(poly_segment)
test <- poly_segment(elist, truthtable)
test
names(test)
length(test) # length = 1!

# list names doesn't work if there is only 1 non-zero list
test2 <- do.call(rbind.data.frame, test)
head(test2)
rownames(test2)

test3 <- test2
test3$segment_id <- sub("\\..*$", "", rownames(test3))
rownames(test3) <- NULL

ggplot(data = test3, aes(x, y, fill = segment_id)) +
  geom_polygon()



