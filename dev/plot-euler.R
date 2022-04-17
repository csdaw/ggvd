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

  do_fills <- !is_false(fills) && !is.null(fills)
  do_edges <- !is_false(edges) && !is.null(edges)
  do_labels <- !is_false(labels) && !is.null(labels)
  do_quantities <- !is_false(quantities) && !is.null(quantities)
  do_legend <- !is_false(legend) && !is.null(legend)
  do_groups <- !is.null(groups)
  do_strips <- !is_false(strips) && do_groups
  do_main <- is.character(main) || is.expression(main) || is.list(main)

  ellipses <- if (do_groups) x[[1L]]$ellipses else x$ellipses

  n_e <- NROW(ellipses)
  n_id <- 2^n_e - 1
  id <- bit_indexr(n_e)

  setnames <- rownames(ellipses)

  if (do_groups) {
    res <- lapply(x, function(xi) is.na(xi$ellipses)[, 1L])
    empty_sets <- apply(do.call(rbind, res), 2, all)

    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0

    res <- lapply(x, function(xi) {
      fitted <- xi$fitted.values[!empty_subsets]
      nonzero <- nonzero_fit(fitted)
      ifelse(is.na(nonzero), FALSE, nonzero)
    })

    nonzero <- apply(do.call(rbind, res), 2, any)

  } else {
    empty_sets <- is.na(x$ellipses[, 1L])
    empty_subsets <- rowSums(id[, empty_sets, drop = FALSE]) > 0
    fitted <- x$fitted.values[!empty_subsets]
    nonzero <- nonzero_fit(fitted)
    nonzero <- ifelse(is.na(nonzero), FALSE, nonzero)
  }

  merged_sets <- rep(FALSE, length(setnames))

  if (!do_groups && any(nonzero)) {
    n_overlaps <- integer(n_id)
    single_mass <- logical(n_e)

    for (i in seq_len(n_e)) {
      nz <- nonzero_fit(x$fitted.values)
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

  # setup fills
  if (do_fills) {
    fills_out <- replace_list(
      list(fill = opar$fills$fill,
           alpha = opar$fills$alpha),
      if (is.list(fills))
        fills
      else if (isTRUE(fills))
        list()
      else
        list(fill = fills)
    )
    fills_out <- replace_list(fills_out, dots)
    fills_out$col <- "transparent"

    if (is.function(fills_out$fill))
      fills_out$fill <- fills_out$fill(n_e)

    n_fills <- length(fills_out$fill)
    if (n_fills < n_id) {
      for (i in (n_fills + 1L):n_id) {
        fills_out$fill[i] <- mix_colors(fills_out$fill[which(id[i, ])])
      }
    }
    fills <- list()
    fills$gp <- setup_gpar(fills_out, list(), n_id)
  } else {
    fills <- NULL
  }

  # setup edges
  if (do_edges) {
    if (isTRUE(edges))
      edges_out <- list()
    else if (!is.list(edges))
      edges_out <- list(col = edges)
    else
      edges_out <- edges

    edges <- list()
    edges$gp <- setup_gpar(list(col = opar$edges$col,
                                alpha = opar$edges$alpha,
                                lex = opar$edges$lex,
                                lwd = opar$edges$lwd,
                                lty = opar$edges$lty),
                           update_list(edges_out, dots),
                           n_e)
  } else {
    edges <- NULL
  }

  # setup strips
  if (do_groups) {
    group_names <- lapply(groups, levels)
    n_levels <- sum(lengths(group_names))
  }

  if (do_strips) {
    strips <- list(gp = setup_gpar(opar$strips, strips, n_levels),
                   groups = groups)
  } else {
    strips <- NULL
  }

  # setup labels
  if (do_labels) {
    if (is.list(labels)) {
      labels <- update_list(list(labels = setnames,
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
    labels$gp <- setup_gpar(list(col = opar$labels$col,
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

  # setup quantities
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

      quantities <- update_list(list(labels = NULL,
                                     type = quantities_type,
                                     rot = opar$quantities$rot),
                                quantities)

    } else if (isTRUE(quantities)) {
      quantities <- list(labels = NULL, rot = opar$quantities$rot)
    } else {
      quantities <- list(labels = quantities, rot = opar$quantities$rot)
    }
    quantities$rot <- rep_len(quantities$rot, n_id)

    quantities$gp <- setup_gpar(list(col = opar$quantities$col,
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

  # setup legend
  if (do_custom_legend) {
    legend <- legend
  } else if (do_legend) {
    # TODO: create a better, custom legend

    legend <- update_list(
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

    legend$gp <- setup_gpar(
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

    main <- update_list(
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

    main$gp <- setup_gpar(
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
    data <- setup_geometry(x,
                           fills = fills,
                           edges = edges,
                           labels = labels,
                           quantities = quantities,
                           n = n,
                           id = id,
                           merged_sets = merged_sets)
  }

  # start setting up grobs

  if (do_groups) {
    n_groups <- length(data)
    euler_grob_children <- grid::gList()
    for (i in seq_len(n_groups)) {
      euler_grob_children[[i]] <- setup_grobs(data[[i]],
                                              fills = fills,
                                              edges = edges,
                                              labels = labels,
                                              quantities = quantities,
                                              number = i,
                                              adjust_labels = adjust_labels,
                                              merged_sets = merged_sets)
    }
    euler_grob <- grid::gTree(grid::nullGrob(),
                              name = "canvas.grob",
                              children = euler_grob_children)
    pos <- vapply(groups, as.numeric, numeric(NROW(groups)), USE.NAMES = FALSE)
    layout <- lengths(lapply(groups, unique))
    if (length(layout) == 1L)
      layout <- c(1L, layout)
    xlim <- range(unlist(lapply(data, "[[", "xlim")))
    ylim <- range(unlist(lapply(data, "[[", "ylim")))
  } else {
    euler_grob <- setup_grobs(data,
                              fills = fills,
                              edges = edges,
                              labels = labels,
                              quantities = quantities,
                              number = 1,
                              adjust_labels = adjust_labels,
                              merged_sets = merged_sets)
    euler_grob <- grid::grobTree(euler_grob,
                                 name = "canvas.grob")

    xlim <- data$xlim
    ylim <- data$ylim
    pos <- c(1L, 1L)
    layout <- c(1L, 1L)
  }

  xlim <- grDevices::extendrange(xlim, f = 0.01)
  ylim <- grDevices::extendrange(ylim, f = 0.01)
  xrng <- abs(xlim[1L] - xlim[2L])
  yrng <- abs(ylim[1L] - ylim[2L])

  if (xrng == 0 || yrng == 0) {
    xrng <- yrng <- 1
    ylim <- xlim <- c(0, 1)
  }

  ar <- xrng/yrng
  # adjust <- layout[1L]/layout[2]

  do_strip_left <- layout[1L] > 1L && do_strips
  do_strip_top <- layout[2L] > 1L && do_strips

  strip_top_row <- strip_top_col <- strip_left_row <- strip_left_col <- 1

  nrow <- ncol <- 1
  heights <- grid::unit(1, "null")
  widths <- grid::unit(1*ar*layout[2]/layout[1], "null")
  diagram_col <- 1
  diagram_row <- 1

  if (do_main) {
    diagram_row <- diagram_row + 2
    nrow <- nrow + 2
    strip_left_row <- strip_left_row + 2
    strip_top_row <- strip_top_row + 2
  }

  if (do_strip_left) {
    diagram_col <- diagram_col + 1
    ncol <- ncol + 1
  }
  if (do_strip_top) {
    diagram_row <- diagram_row + 1
    nrow <- nrow + 1
  }

  if (do_strip_left && do_strip_top) {
    strip_top_col <- strip_top_col + 1
    strip_left_row <- strip_left_row + 1
  }

  # draw strips
  if (do_strip_top) {
    strip_top_vp <-
      grid::viewport(layout.pos.row = strip_top_row,
                     layout.pos.col = strip_top_col,
                     name = "strip.top.vp",
                     layout = grid::grid.layout(nrow = 1, ncol = layout[2]))

    lvls <- levels(strips$groups[[1]])
    n_lvls <- length(lvls)
    step <- 1/n_lvls

    strip_top_grob <- grid::textGrob(lvls,
                                     x = step/2 + (seq(0, n_lvls - 1)*step),
                                     name = "strip.top.grob",
                                     gp = do.call(grid::gpar, strips$gp),
                                     vp = strip_top_vp)

    heights <- grid::unit.c(grid::unit(2, "grobheight", list(strip_top_grob)),
                            heights)
  }

  if (do_strip_left) {
    strip_left_vp <-
      grid::viewport(layout.pos.row = strip_left_row,
                     layout.pos.col = strip_left_col,
                     name = "strip.left.vp",
                     layout = grid::grid.layout(nrow = layout[1], ncol = 1))

    lvls <- levels(strips$groups[[2]])
    n_lvls <- length(lvls)
    step <- 1/n_lvls

    strip_left_grob <- grid::textGrob(lvls,
                                      y = step/2 + (seq(0, n_lvls - 1)*step),
                                      name = "strip.left.grob",
                                      rot = 90,
                                      gp = do.call(grid::gpar, strips$gp),
                                      vp = strip_left_vp)

    widths <- grid::unit.c(grid::unit(2, "grobwidth", list(strip_left_grob)),
                           widths)
  }

  if (do_legend) {
    if (do_custom_legend) {
      legend_grob <- legend
      legend <- list(side = "right")
    } else {
      legend_grob <- grid::legendGrob(
        labels = legend$labels,
        do.lines = legend$do.lines,
        ncol = legend$ncol,
        nrow = legend$nrow,
        hgap = legend$hgap,
        vgap = legend$vgap,
        default.units = legend$default.units,
        pch = legend$pch,
        gp = legend$gp
      )
    }

    legend_grob$name <- "legend.grob"
    if (do_strip_top)
      legend_row <- 2 + 2*do_main

    if (legend$side == "right") {
      # legend on right (default)
      ncol <- ncol + 2L
      legend_row <- nrow
      legend_col <- ncol
      widths <- grid::unit.c(widths, grid::unit(c(1, 1),
                                                c("lines", "grobwidth"),
                                                list(NULL, legend_grob)))
    } else if (legend$side == "left") {
      # legend on left
      ncol <- ncol + 2L
      legend_row <- if (do_strip_top) 2L + 2*do_main else 1L + 2*do_main
      legend_col <- 1
      diagram_col <- diagram_col + 2L
      if (do_strip_left)
        strip_left_col <- strip_left_col + 2L
      if (do_strip_top)
        strip_top_col <- strip_top_col + 2L
      widths <- grid::unit.c(grid::unit(c(1, 1),
                                        c("grobwidth", "lines"),
                                        list(legend_grob, NULL)), widths)
    } else if (legend$side == "top") {
      # legend on top
      nrow <- nrow + 2L
      legend_row <- 1L + do_main*2
      legend_col <- if (do_strip_left) 2L else 1L
      diagram_row <- diagram_row + 2L
      if (do_strip_top)
        strip_top_row <- strip_top_row + 2L
      if (do_strip_left)
        strip_left_row <- strip_left_row + 2L
      heights <- grid::unit.c(grid::unit(c(1, 1),
                                         c("grobheight", "lines"),
                                         list(legend_grob, NULL)),
                              heights)
    } else {
      # legend on bottom
      nrow <- nrow + 2L
      legend_row <- nrow
      legend_col <- if (do_strip_left) 2L else 1L
      heights <- grid::unit.c(heights,
                              grid::unit(c(1, 1),
                                         c("lines", "grobheight"),
                                         list(NULL, legend_grob)))
    }
    legend_grob$vp <- grid::viewport(layout.pos.row = legend_row,
                                     layout.pos.col = legend_col,
                                     name = "legend.vp")
  }

  if (do_main) {
    main_grob <- grid::textGrob(label = main$label,
                                x = main$x,
                                y = main$y,
                                just = main$just,
                                hjust = main$hjust,
                                vjust = main$vjust,
                                rot = main$rot,
                                check.overlap = FALSE,
                                default.units = main$default.units,
                                gp = main$gp,
                                name = "main.grob")
    heights <- grid::unit.c(grid::unit(c(1, 1),
                                       c("lines", "grobheight"),
                                       list(NULL, main_grob)),
                            heights)
    main_grob$vp <- grid::viewport(layout.pos.row = 1,
                                   layout.pos.col = diagram_col,
                                   name = "main.vp")
  }

  canvas_vp <- grid::viewport(
    layout.pos.row = diagram_row,
    layout.pos.col = diagram_col,
    name = "canvas.vp",
    layout = grid::grid.layout(nrow = layout[1L],
                               ncol = layout[2L],
                               widths = rep(1/ar, layout[1L]),
                               heights = rep(1, layout[2L]))
  )

  for (i in seq_along(euler_grob$children)) {
    if (NCOL(pos) == 2L) {
      j <- pos[i, 1L]
      k <- pos[i, 2L]
    } else {
      j <- 1L
      k <- pos[i]
    }
    euler_grob$children[[i]]$vp <- grid::viewport(
      layout.pos.row = j,
      layout.pos.col = k,
      xscale = if (xlim[1] == -Inf) c(-1, 1) else xlim,
      yscale = if (ylim[1] == -Inf) c(-1, 1) else ylim,
      name = paste0("panel.vp.", j, ".", k)
    )
  }

  euler_grob$vp <- canvas_vp

  # return a gTree object
  children <- gList(
    if (do_main) main_grob = main_grob,
    if (do_strip_top) strip_top_grob = strip_top_grob,
    if (do_strip_left) strip_left_grob = strip_left_grob,
    if (do_legend) legend_grob = legend_grob,
    euler_grob = euler_grob
  )

  grid::gTree(
    data = data,
    children = children,
    vp = grid::viewport(layout = grid::grid.layout(nrow = nrow,
                                                   ncol = ncol,
                                                   widths = widths,
                                                   heights = heights,
                                                   respect = TRUE),
                        name = "euler.vp"),
    cl = "eulergram",
    name = "euler.diagram"
  )
}
