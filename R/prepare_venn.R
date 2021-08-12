#' @export
prepare_venn <- function(ls, fill = NULL, colour = NULL, color = NULL) {
  count_matrix <- count_venn(ls)

  tibble::tibble(
    set_name = if (!is.null(names(ls))) names(ls) else LETTERS[1:length(ls)],
    elements = ls,
    count = rep_len(c(min(count_matrix[, "count"]),
                      max(count_matrix[, "count"])), length(ls)),
    fill = fill,
    colour = colour,
    color = color
  )
}
