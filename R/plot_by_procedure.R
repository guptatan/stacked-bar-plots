#' plot_by_procedure
#'
#' @description Plot likert scales with procedure on the Y axis. This is simply a convenience wrapper.
#'
#' @param evals The evaluation data to plot
#' @param procedures.to.include The procedures to include in the plot
#' @param savefile savefile? Default to TRUE.
#' @param file.output.base The base name of the file to which the plot is saved
#' @param title The title of the plot
#' @param ordered.by.observations If TRUE, order the procedures by the number of observations
#' @param ind.var.name The name of the independent variable
#' @param ind.var.label The label for the independent variable
#' @param dep.var.name  The name of the dependent variable
#' @param dep.var.label The label for the dependent variable
#' @param dep.var.level.labels The labels for each level of the dependent variable
#' @param include.legend If TRUE, includes a legend as part of the plot.  Otherwise saves it as a separate image file.
#' @param use.gray.scale If TRUE, use black and white version of plot. Default is false.
#'
#' @return A list with n.evals (number of total evaluations included in this plot) and stats (the likert data)
#'
#'
#' @export
#'
plot_by_procedure = function(evals,
                             # This must be a data.frame of the form $name, $procID
                             # where name may be repeated
                             procedures.to.include,
                             ind.var.name = "procName",
                             ind.var.label = "",
                             dep.var.name = "performance",
                             dep.var.label = "Performance",
                             dep.var.level.labels = simpl.constants$levels.performance,
                             include.legend = FALSE,
                             savefile = TRUE,
                             file.output.base = 'test',
                             title = 'zuh',
                             # file.output.base = paste0(dep.var.name, ".vs.top",
                             #                           length(unique(procedures)), "procedures"),
                             # title = paste0("Performance distribution for PGY5 residents\nfor ",
                             #                length(unique(procedures)),
                             #                " most frequent Core procedures"),
                             ordered.by.observations = TRUE,
                             use.gray.scale = FALSE)
{
  rename_procedures = function(evals = evals,
                               procIDs = lap.chole,
                               newProcName = "Laparoscopic cholecystectomy +/- IOC") {
    subset = evals %>%
      filter(procID %in% procIDs)

    if(nrow(subset) == 0)
      warning(paste0("rename_procedures called with unknown procIDs: ", procIDs))
    else
      subset$procName = newProcName

    return(subset)
  }

  # filter (and group) the evaluations to include only listed procedures
  evals.filtered = procedures.to.include %>%
    group_by(name) %>%
    do(rename_procedures(evals = evals,
                         procIDs = .$procID,
                         newProcName = unique(.$name))) %>%
    data.frame()

  n.evals = nrow(evals.filtered)



  # only do B&W if they want it.
  if(use.gray.scale) {
    plot_diverging_stacked_bar_chart(evals = evals.filtered,
                                     savefile = savefile,
                                     file.output.base = paste0(file.output.base, "-bw"),
                                     include.title = TRUE,
                                     ind.var.name = ind.var.name,
                                     ind.var.label = ind.var.label,
                                     dep.var.name = dep.var.name,
                                     dep.var.label = dep.var.label,
                                     dep.var.level.labels = dep.var.level.labels,
                                     include.legend = FALSE,
                                     title = title,
                                     ordered.by = "rank", #ordered.by.observations,
                                     flip.coord = TRUE,
                                     use.gray.scale = TRUE)
  }

  # either way, create color
  stats =
    plot_diverging_stacked_bar_chart(evals = evals.filtered,
                                     savefile = savefile,
                                     file.output.base = paste0(file.output.base, "-color"),
                                     include.title = TRUE,
                                     ind.var.name = ind.var.name,
                                     ind.var.label = ind.var.label,
                                     dep.var.name = dep.var.name,
                                     dep.var.label = dep.var.label,
                                     dep.var.level.labels = dep.var.level.labels,
                                     include.legend = FALSE,
                                     title = title,
                                     ordered.by = "rank", #ordered.by.observations,
                                     flip.coord = TRUE,
                                     use.gray.scale = FALSE)

  return(list(n.evals = n.evals,
              stats = stats))


}
