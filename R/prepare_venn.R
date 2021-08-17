#' Convert list to geom_venn compatible data.frame
#'
#' @description Description.
#'
#' @param ls Description.
#' @param ... Description.
#'
#' @return Returns ...
#' @export
#'
#' @examples
#'
#' lst <- list(
#'   set1 = c(1:3),
#'   set2 = c(3:20),
#'   set3 = c(1, 20:66)
#' )
#'
#' prepare_venn(lst, fill = c("blue", "red", "green"))
#'
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
