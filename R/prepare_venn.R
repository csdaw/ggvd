#' Convert list to `geom_venn` compatible data.frame
#'
#' @description Using a list of sets, construct a data.frame with the correct
#' format for \code{\link{geom_venn}}.
#'
#' @param l `list` of up to 26 numeric or character vectors (i.e. sets) to
#' compare. Within each set there must not be any duplicated elements. If the
#' list is not named then capital letters starting from A to Z will be
#' substituted as names.
#' @param ... Name-value pairs and other arguments to be passed to
#' \code{\link[tibble]{add_column}}. See **Examples** section for an example.
#'
#' @return Returns a `data.frame` with `length(l)` rows and at least three
#' essential columns: column 1 is `set_name` with the names of each set,
#' column 2 is `elements` contains the elements in each set, column 3 is
#' `count` which defines the smallest and the largest set overlap which is
#' used for the legend limits in `geom_venn(type = 'continuous')`. Any additional
#' columns are added afterward.
#' @export
#'
#' @examples
#' lst <- list(
#'   set1 = c(1:3),
#'   set2 = c(3:20),
#'   set3 = c(1, 20:66)
#' )
#'
#' # Add an additional column called 'fill'
#' vd <- prepare_venn(lst, fill = c("blue", "red", "green"))
#'
#' # Plot a Venn diagram
#' ggplot() +
#'   geom_venn(aes(set_name = set_name, elements = elements, fill = fill), data = vd) +
#'   scale_fill_identity()
#'
prepare_venn <- function(l, ...) {
  if (any(sapply(l, function(x) any(duplicated(x))))) {
    stop("The vectors in the input list contain duplicates which must be removed.")
  }
  count_matrix <- count_venn(l)

  out <- tibble::tibble(
    set_name = if (!is.null(names(l))) names(l) else LETTERS[1:length(l)],
    elements = l,
    count = rep_len(c(min(count_matrix[, "count"]),
                      max(count_matrix[, "count"])), length(l))
  )

  out$set_name <- factor(out$set_name, as.character(unique(out$set_name)))

  tibble::add_column(out, ...)
}
