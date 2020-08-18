function (object, show = c("estimate", "se"), contour = TRUE, 
          contour_col = "black", n_contour = NULL, xlab, ylab, 
          title = NULL, subtitle = NULL, caption = NULL, response_range = NULL, 
          ...) 
{
  smooth_vars <- names(object)[3:4]
  show <- match.arg(show)
  if (isTRUE(identical(show, "estimate"))) {
    guide_title <- "Effect"
    plot_var <- "est"
    guide_limits <- if (is.null(response_range)) {
      c(-1, 1) * max(abs(object[[plot_var]]))
    }
    else {
      response_range
    }
  }
  else {
    guide_title <- "Std. err."
    plot_var <- "se"
    guide_limits <- range(object[["se"]])
  }
  plt <- ggplot(object, aes_string(x = smooth_vars[1], y = smooth_vars[2])) + 
    geom_raster(mapping = aes_string(fill = plot_var))
  if (isTRUE(contour)) {
    plt <- plt + geom_contour(mapping = aes_string(z = plot_var), 
                              colour = contour_col, bins = n_contour)
  }
  if (missing(xlab)) {
    xlab <- smooth_vars[1L]
  }
  if (missing(xlab)) {
    xlab <- smooth_vars[1L]
  }
  if (missing(ylab)) {
    ylab <- smooth_vars[2L]
  }
  if (is.null(title)) {
    title <- unique(object[["smooth"]])
  }
  if (all(!is.na(object[["by_variable"]]))) {
    spl <- strsplit(title, split = ":")
    title <- spl[[1L]][[1L]]
    if (is.null(subtitle)) {
      by_var <- as.character(unique(object[["by_variable"]]))
      subtitle <- paste0("By: ", by_var, "; ", 
                         unique(object[[by_var]]))
    }
  }
  plt <- plt + labs(x = xlab, y = ylab, title = title, subtitle = subtitle, 
                    caption = caption)
  plt <- plt + scale_fill_distiller(palette = "RdBu", 
                                    type = "div", limits = guide_limits)
  plt <- plt + guides(fill = guide_colourbar(title = guide_title, 
                                             direction = "vertical", barheight = grid::unit(0.25, 
                                                                                            "npc")))
  plt <- plt + theme(legend.position = "right")
  plt
}