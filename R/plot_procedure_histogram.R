#' plot_procedure_histogram
#'
#' @details Plot a histogram of the frequencies of procedures
#' @param evals  dataframe
#' @param filename If saving, the file name; will go to Plot directory (see \code{plot_output_dir})
#' @param width  Width in units. Default 10.
#' @param height Height in units. Default 5.
#' @param units Default is inches 'in'. See \code{unit} in \code{ggplot2}
#' @param title.axis.x Guess.
#' @param plot.to.device Default TRUE. Show plot in graphics device as normally done.
#' @param save.as.file Default FALSE. Save plot as file.
#'
#' @importFrom forcats fct_reorder
#' @return A ggplot object
#' @export
#'
plot_procedure_histogram = function(evals = evals,
                                    filename = "proc.freq.histogram.jpg",
                                    width = 10,
                                    height = 5,
                                    units="in",
                                    title.axis.x = "Procedures",
                                    plot.to.device = TRUE,
                                    save.as.file = FALSE) {

  plot = evals %>%
    dplyr::group_by(procName) %>%
    dplyr::tally() %>%
    arrange(desc(n)) %>%
    ggplot2::ggplot(ggplot2::aes(x=forcats::fct_reorder(procName, -n), y=n)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = title.axis.x, y = "# Ratings") +
    ggplot2::theme_bw() +
    #ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -55, hjust = 0)) +
    ggplot2::theme(plot.margin = ggplot2::unit(c(10, 120, 10, 10), "points")) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())

  # TODO: add some color?  Add numbers above each one (optionally)?

  if(plot.to.device) {
    plot
  }

  if(save.as.file) {
    ggsave(filename=file.path(plot_output_dir(), filename),
           width = width,
           height = height,
           units=units)
  }

  return(plot)

}

