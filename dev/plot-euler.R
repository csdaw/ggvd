plot_euler <- function(x,
                       fills = TRUE,
                       edges = TRUE,
                       legend = FALSE,
                       labels = identical(legend, FALSE),
                       quantities = FALSE,
                       strips = NULL,
                       main = NULL,
                       n = 200L,
                       adjust_labels = TRUE,
                       ...) {

  stopifnot(is.logical(adjust_labels))

  # retrieve default options
  opar <- eulerr_options()

  groups <- attr(x, "groups")
  dots <- list(...)

  do_custom_legend <- grid::is.grob(legend)

  do_fills <- !eulerr:::is_false(fills) && !is.null(fills)
  do_edges <- !eulerr:::is_false(edges) && !is.null(edges)
  do_labels <- !eulerr:::is_false(labels) && !is.null(labels)
  do_quantities <- !eulerr:::is_false(quantities) && !is.null(quantities)
  do_legend <- !eulerr:::is_false(legend) && !is.null(legend)
  do_groups <- !is.null(groups)
  do_strips <- !eulerr:::is_false(strips) && do_groups
  do_main <- is.character(main) || is.expression(main) || is.list(main)

  # x$ellipses defines the ellipse geometries
  ellipses <- if (do_groups) x[[1L]]$ellipses else x$ellipses

  n_e <- NROW(ellipses) # number of ellipse
  n_id <- 2^n_e - 1 # numer of segments
  id <- eulerr:::bit_indexr(n_e) # which part of the data do segments include

  setnames <- rownames(ellipses) # set names

  if (do_groups) {
    # delete this bit
    res <- lapply(x, function(xi) is.na(xi$ellipses)[, 1L])
    empty_sets <- apply(do.call(rbind, res), 2, all)

    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

    res <- lapply(x, function(xi) {
      fitted <- xi$fitted.values[!empty_subsets]
      nonzero <- eulerr:::nonzero_fit(fitted)
      ifelse(is.na(nonzero), FALSE, nonzero)
    })

    nonzero <- apply(do.call(rbind, res), 2, any)

  } else {
    # don't delete
    empty_sets <- is.na(x$ellipses[, 1L])
    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0
    fitted <- x$fitted.values[!empty_subsets]
    nonzero <- eulerr:::nonzero_fit(fitted)
    nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)
  }

  merged_sets <- rep(FALSE, length(setnames))

  if (!do_groups && any(nonzero)) { # keep if any nonzero
    n_overlaps <- integer(n_id)
    single_mass <- logical(n_e)

    for (i in seq_len(n_e)) {
      nz <- eulerr:::nonzero_fit(x$fitted.values)
      nzi <- nz & id[, i]

      if (sum(nzi) == 1) {
        n_overlaps[which(nzi)] <- n_overlaps[which(nzi)] + 1
      }

      single_mass[i] <- sum(nzi) == 1
    }

    complete_overlaps <- n_overlaps > 1

    merge_sets <- id[complete_overlaps, ] & single_mass

    if (any(merge_sets)) {
      setnames[merge_sets] <- paste(setnames[merge_sets],
                                    collapse = ",")
      merged_sets[which(merge_sets)[length(which(merge_sets))]] <- TRUE
    }
  }

  stopifnot(n > 0, is.numeric(n) && length(n) == 1)

  # setup fills (I think I can delete this?)
  if (do_fills) {
    fills_out <- eulerr:::replace_list(
      list(fill = opar$fills$fill,
           alpha = opar$fills$alpha),
      if (is.list(fills))
        fills
      else if (isTRUE(fills))
        list()
      else
        list(fill = fills)
    )
    fills_out <- eulerr:::replace_list(fills_out, dots)
    fills_out$col <- "transparent"

    if (is.function(fills_out$fill))
      fills_out$fill <- fills_out$fill(n_e)

    n_fills <- length(fills_out$fill)
    if (n_fills < n_id) {
      for (i in (n_fills + 1L):n_id) {
        fills_out$fill[i] <- eulerr:::mix_colors(fills_out$fill[which(id[i, ])])
      }
    }
    fills <- list()
    fills$gp <- eulerr:::setup_gpar(fills_out, list(), n_id)
  } else {
    fills <- NULL
  }

  # setup edges (I think I can delete this?)
  if (do_edges) {
    if (isTRUE(edges))
      edges_out <- list()
    else if (!is.list(edges))
      edges_out <- list(col = edges)
    else
      edges_out <- edges

    edges <- list()
    edges$gp <- eulerr:::setup_gpar(list(col = opar$edges$col,
                                         alpha = opar$edges$alpha,
                                         lex = opar$edges$lex,
                                         lwd = opar$edges$lwd,
                                         lty = opar$edges$lty),
                                    eulerr:::update_list(edges_out, dots),
                                    n_e)
  } else {
    edges <- NULL
  }

  # setup strips (I think I can delete this?)
  if (do_groups) {
    group_names <- lapply(groups, levels)
    n_levels <- sum(lengths(group_names))
  }

  if (do_strips) {
    strips <- list(gp = eulerr:::setup_gpar(opar$strips, strips, n_levels),
                   groups = groups)
  } else {
    strips <- NULL
  }

  # setup labels (I think I can delete this?)
  if (do_labels) {
    if (is.list(labels)) {
      labels <- eulerr:::update_list(list(labels = setnames,
                                          rot = opar$labels$rot),
                                     labels)
    } else if (isTRUE(labels)) {
      labels <- list(labels = setnames,
                     rot = opar$labels$rot)
    } else {
      labels <- list(labels = labels,
                     rot = opar$labels$rot)
    }

    labels$rot <- rep_len(labels$rot, n_e)
    labels$gp <- eulerr:::setup_gpar(list(col = opar$labels$col,
                                          alpha = opar$labels$alpha,
                                          fontsize = opar$labels$fontsize,
                                          cex = opar$labels$cex,
                                          fontfamily = opar$labels$fontfamily,
                                          lineheight = opar$labels$lineheight,
                                          font = opar$labels$font),
                                     labels,
                                     n_e)
  } else {
    labels <- NULL
  }

  # setup quantities (I think I can delete this?)
  if (do_quantities) {
    if (is.list(quantities)) {
      if (!is.null(quantities$type)) {
        if (!all(quantities$type %in% c("counts", "percent")))
          stop("'type' must be one or both of 'counts' and 'percent")

        quantities_type <- match.arg(quantities$type,
                                     c("counts", "percent"),
                                     several.ok = TRUE)
      } else {
        quantities_type <- opar$quantities$type
      }

      quantities <- eulerr:::update_list(list(labels = NULL,
                                              type = quantities_type,
                                              rot = opar$quantities$rot),
                                         quantities)

    } else if (isTRUE(quantities)) {
      quantities <- list(labels = NULL, rot = opar$quantities$rot)
    } else {
      quantities <- list(labels = quantities, rot = opar$quantities$rot)
    }
    quantities$rot <- rep_len(quantities$rot, n_id)

    quantities$gp <- eulerr:::setup_gpar(list(col = opar$quantities$col,
                                              alpha = opar$quantities$alpha,
                                              fontsize = opar$quantities$fontsize,
                                              cex = opar$quantities$cex,
                                              fontfamily = opar$quantities$fontfamily,
                                              lineheight = opar$quantities$lineheight,
                                              font = opar$quantities$font),
                                         quantities,
                                         n_id)
  } else {
    quantities <- NULL
  }

  # setup legend (I think I can delete this?)
  if (do_custom_legend) {
    legend <- legend
  } else if (do_legend) {
    # TODO: create a better, custom legend

    legend <- eulerr:::update_list(
      list(labels = setnames[!empty_sets & !merged_sets],
           side = opar$legend$side,
           nrow = sum(!empty_sets),
           ncol = 1L,
           byrow = opar$legend$byrow,
           do.lines = opar$legend$do.lines,
           lines.first = opar$legend$lines.first,
           hgap = opar$legend$hgap,
           vgap = opar$legend$vgap,
           default.units = opar$legend$default.units,
           pch = opar$legend$pch),
      legend
    )

    legend$gp <- eulerr:::setup_gpar(
      list(
        fill = if (do_fills) fills$gp$fill[!empty_sets & !merged_sets] else "transparent",
        alpha = if (do_fills)
          fills$gp$alpha[!empty_sets & !merged_sets]
        else if (do_edges)
          edges$gp$alpha[!empty_sets & !merged_sets]
        else
          0,
        cex = opar$legend$cex,
        fontsize =
          opar$legend$fontsize/opar$legend$cex,
        font = opar$legend$font,
        fontfamily = opar$legend$fontfamily,
        lwd = if (do_edges) edges$gp$lwd[!empty_sets & !merged_sets] else 0,
        lex = if (do_edges) edges$gp$lex[!empty_sets & !merged_sets] else 0,
        col = if (do_edges)
          edges$gp$col[!empty_sets & !merged_sets]
        else "transparent"),
      legend,
      sum(!empty_sets)
    )
  } else {
    legend <- NULL
  }

  # I think I can delete this?
  if (do_main) {
    if (is.list(main)) {
      if (length(main) == 1 && is.null(names(main)))
        label <- main[[1]]
      else
        label <- main$label

      if (is.null(label))
        stop("you need to provide a 'label' item if 'main' is a list.")

    } else {
      label <- main
    }

    main <- eulerr:::update_list(
      list(label = label,
           x = opar$main$x,
           y = opar$main$y,
           just = opar$main$just,
           hjust = opar$main$hjust,
           vjust = opar$main$vjust,
           rot = opar$main$rot,
           check.overlap = opar$main$check.overlap,
           default.units = opar$main$default.units),
      main
    )

    main$gp <- eulerr:::setup_gpar(
      list(
        cex = opar$main$cex,
        fontsize = opar$main$fontsize,
        font = opar$main$font,
        fontfamily = opar$main$fontfamily,
        col = opar$main$col,
        lineheight = opar$main$lineheight,
        alpha = opar$main$alpha
      ),
      main,
      1
    )
  } else {
    main <- NULL
  }

  # set up geometry for diagrams
  if (do_groups) {
    data <- lapply(x,
                   setup_geometry,
                   fills = fills,
                   edges = edges,
                   labels = labels,
                   quantities = quantities,
                   n = n,
                   id = id,
                   merged_sets = merged_sets)
  } else {
    # important: x (input list), n (integer), id? (matrix), merged_sets (logical)
    # maybe important: labels, quantities
    # likely not important: fills, edges
    data <- setup_geometry(x,
                           fills = fills,
                           edges = edges,
                           labels = labels,
                           quantities = quantities,
                           n = n,
                           id = id,
                           merged_sets = merged_sets)
  }

  data
}
