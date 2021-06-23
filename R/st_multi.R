#' Perform geometric set intersection, difference, and union with more than two
#' simple feature geometry collections
#'
#' @param ... at least three items are needed if this parameter is used.
#' @param l a `list` of polygons
#'
#' @return Returns the intersection/union/difference of `...`
#' @name st_multi
st_multi_intersection <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- sf::st_intersection(v,l[[i]])
    }
    return(v)
  }
}

#' @name st_multi
st_multi_difference <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- sf::st_difference(v,l[[i]])
    }
    return(v)
  }
}


#' @name st_multi
st_multi_union <- function ( ...,l = NULL){
  if (is.null(l)) l <- list(...)
  n <- length(l)
  if (n < 3){
    stop("At least three parameters.")
  }
  else {
    v <- l[[1]]
    for (i in 2:n){
      v <- sf::st_union(v,l[[i]])
    }
    return(v)
  }
}
