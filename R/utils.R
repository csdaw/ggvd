# from https://stackoverflow.com/questions/40049313
lcombn <- function(v) {
  do.call("c", lapply(seq_along(v), function(i) combn(v, i, FUN = list)))
}
