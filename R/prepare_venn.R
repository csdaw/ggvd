#' @export
prepare_venn <- function(ls, ...) {
  count_matrix <- count_venn(ls)

  out <- tibble::tibble(
    set_name = if (!is.null(names(ls))) names(ls) else LETTERS[1:length(ls)],
    elements = ls,
    count = rep_len(c(min(count_matrix[, "count"]),
                      max(count_matrix[, "count"])), length(ls))
  )

  out$set_name <- factor(out$set_name, as.character(unique(out$set_name)))

  dots <- list(...)
  if (length(dots) > 0) cbind(out, dots) else out
}
