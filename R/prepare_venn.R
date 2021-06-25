#' @export
prepare_venn <- function(ls) {
  count_matrix <- ggvd:::count_venn(ls)

  tibble::tibble(
    sets = if (!is.null(names(ls))) names(ls) else LETTERS[1:length(ls)],
    elements = ls,
    count_range = rep_len(c(min(count_matrix[, "count"]),
                            max(count_matrix[, "count"])), length(ls))
  )
}
