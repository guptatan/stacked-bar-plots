#' plot_g_draw_grid
#' @description  Draw a vertically and horizontally aligned plot and return the
#'   resulting grob. Not currently used.
#'
#' @param plots list of plots.
#' @param ncol number of columns. Default 1.
#' @param nrow number of rows. Default is \code{length(plots)}.
#' @param heights heights for each pane. Default is NULL.
#' @param widths widths for each pane. Default is NULL.
# #' @param title Unused
#' @param draw draw on current device? Default is TRUE.
#'
#' @details This is based on
#'   http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot.
#'   It works by creating a grob for each plot with a specified width, thereby
#'   aligning the axes.
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @export
plot_g_draw_grid = function(plots,
                            ncol = 1,
                            nrow = length(plots),
                            heights = NULL, # for each pane
                            widths = NULL,
                            # title,   Not an option actually
                            draw = TRUE  # draw on current device?
)
{

  if(!is.list(plots))
    stop("plots must be a list (i.e. created w/ list())")

  grobs <- list()
  widths <- list()

  for (i in 1:length(plots)){
    grobs[[i]] <- ggplotGrob(plots[[i]])
    widths[[i]] <- grobs[[i]]$widths[2:5]
  }

  maxwidth <- do.call(grid::unit.pmax, widths)

  for (i in 1:length(grobs)){
    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
  }

  # create the grob without drawing it (yet)
  plotArgs = list(grobs = grobs,
                  #heights = heights,
                  ncol = ncol,
                  nrow = nrow,
                  top = title)

  if(draw)
    # use grid.arrange to plot to the current device
    # FIXME:  this doesn't account for use case where
    # a legend is added later
    do.call(gridExtra::grid.arrange, plotArgs)

  # use arrangeGrob to create a grob that can be
  # used by the caller (typically for printing to a file)
  # marrangeGrob supports printing across multiple pages.
  # Note this REQUIRES both a col and row value (i.e. not NULL)
  plot = do.call(gridExtra::arrangeGrob, plotArgs)

  return(plot)

}

