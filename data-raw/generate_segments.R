generate_segments <- function(n_sets, polygons) {
  stopifnot(n_sets %in% 2:4)

  if (n_sets == 2) {
    # regions
    A <- st_difference(polygons[[1]], polygons[[2]])
    B <- st_difference(polygons[[2]], polygons[[1]])
    AB <- st_intersection(polygons[[1]], polygons[[2]])

    out <- list(A = A, AB = AB, B = B)
    out
  } else if (n_sets == 3) {
    # regions
    A <- st_multi_difference(l = polygons)
    B <- st_multi_difference(l = polygons[c(2, 1, 3)])
    C <- st_multi_difference(l = polygons[c(3, 1, 2)])
    AB <- st_difference(st_intersection(polygons[[1]], polygons[[2]]), polygons[[3]])
    AC <- st_difference(st_intersection(polygons[[1]], polygons[[3]]), polygons[[2]])
    BC <- st_difference(st_intersection(polygons[[3]], polygons[[2]]), polygons[[1]])
    ABC <- st_multi_intersection(l = polygons)

    out <- list(A = A, AB = AB, ABC = ABC, AC = AC,
                B = B, BC = BC, C = C)
    out
  } else if (n_sets == 4) {
    # regions
    A <- st_multi_difference(l = polygons)
    B <- st_multi_difference(l = polygons[c(2, 1, 3, 4)])
    C <- st_multi_difference(l = polygons[c(3, 1, 2, 4)])
    D <- st_multi_difference(l = polygons[c(4, 1:3)])
    AB <- st_difference(st_intersection(polygons[[1]], polygons[[2]]), st_union(polygons[[3]], polygons[[4]]))
    AC <- st_difference(st_intersection(polygons[[1]], polygons[[3]]), st_union(polygons[[2]], polygons[[4]]))
    AD <- st_difference(st_intersection(polygons[[1]], polygons[[4]]), st_union(polygons[[3]], polygons[[2]]))
    BC <- st_difference(st_intersection(polygons[[3]], polygons[[2]]), st_union(polygons[[1]], polygons[[4]]))
    BD <- st_difference(st_intersection(polygons[[4]], polygons[[2]]), st_union(polygons[[3]], polygons[[1]]))
    CD <- st_difference(st_intersection(polygons[[3]], polygons[[4]]), st_union(polygons[[1]], polygons[[2]]))
    ABC <- st_difference(st_multi_intersection(l = polygons[1:3]), polygons[[4]])
    ABD <- st_difference(st_multi_intersection(l = polygons[c(1, 2, 4)]), polygons[[3]])
    ACD <- st_difference(st_multi_intersection(l = polygons[c(1, 3, 4)]), polygons[[2]])
    BCD <- st_difference(st_multi_intersection(l = polygons[c(4, 2, 3)]), polygons[[1]])
    ABCD <- st_multi_intersection(l = polygons)

    out <- list(A = A, AB = AB, ABC = ABC, ABCD = ABCD,
                AC = AC, ACD = ACD, AD = AD,
                B = B, BC = BC, BCD = BCD, BD = BD,
                C = C, CD = CD, D = D)
    out
  }
}
