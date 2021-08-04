#' plot_g_extract_legend
#'
#' @description Extract a legend from a ggplot.
#'
#' @param a.gplot A ggplot object.
#' @details #https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @export
plot_g_extract_legend <- function(a.gplot) {

  plot = ggplot2::ggplot_build(a.gplot)
  tmp <- ggplot2::ggplot_gtable(plot)
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")

  returnGrob = NULL
  if(any(leg)) {
    returnGrob = tmp$grobs[[leg]]
  }
  #else stop("attempt to extract legend for empty plot")

  return(returnGrob)
}
