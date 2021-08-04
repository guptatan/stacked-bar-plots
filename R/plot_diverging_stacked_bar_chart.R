library(purrr)
library(forcats) 
library(gridExtra)
library(likert)
library(lubridate)
library(dplyr)
library(tibble)
library(rlang)
library(tidyselect)
library(magrittr)
library(memisc)
library(lazyeval)
library(digest)

source("R/plot_g_draw_grid.R")
source("R/plot_g_extract_legend.R")
source("R/window_evals.R")
source("R/clean_evals.R")

#read raw data (list of column names: caseID, evaluationID, programID, program, creatorID, raterID, raterFirstName, raterLastName, raterConsented, raterArchived, subjectID, subjectFirstName, subjectLastName, subjectConsented, subjectArchived, subjectRoleName, traineePGY, traineeType, supervision, performance, complexity, hasDictation, procID, procName, procStartTime, procStopTime, evalCompleted, status, statusDetails, raterLastLogin, subjectLastLogin )
evals = read.csv("simpl_pull.csv")

#clean data
evals = evals %>% clean_evals()

#' plot_diverging_stacked_bar_chart
#'
#' @description This defines a function that will create a  plot of one ordinal categorical variable against another.  It will save the plot in the given file.
#'
#' @param evals  data frame
#' @param savefile savefile? Default to TRUE.
#' @param file.output.base name of the file. Defaults to "plot_diverging_stacked_bar_chart-test" .
#' @param dir.output output directory. Default is working directory.
#' @param include.title guess
#' @param date.start part of data processing. See \code{window_evals}.
#' @param date.stop  part of data processing. See \code{window_evals}.
#' @param ind.var.name  'independent' variable name. The default for this is "traineePGY", and will likely work with little else.
#' @param ind.var.label Capitalize first lettters of ind.var.label
#' @param ind.var.limits numeric values to serve as upper and lower bounds of ind.var.name
#' @param ignore.na.ind Drop NAs in independent. Default TRUE.
# #' @param ind.var.max.length
#' @param ind.var.angle element_text argument 'angle'
#' @param dep.var.name dependent variable name. Default is performance.
#' @param dep.var.label Capitalize first lettters of dep.var.label
#' @param dep.var.level.labels numeric values to serve as upper and lower bounds of dep.var.name
#' @param ignore.na.dep Drop NAs in dependent. Default TRUE.
# #' @param stratifier.var.name
# #' @param stratifier.var.label
#' @param title plot title. Default is paste(dep.var.label, "distribution by", ind.var.label).
#' @param center.level.index  Something to do with the \code{likert} package function. Default is length(dep.var.level.labels) - 2, but .5 is added to it.
#' @param include.histogram  likert option. See \code{likert.bar.plot} Default TRUE.
#' @param include.legend Add legend to plot. Default TRUE.
#' @param ordered.by.observations order as is (TRUE) or otherwise? Default is FALSE.
#' @param ordered.by if ordered.by.observations FALSE, order by observations or factor. Other values will presumably ignore ordered.by.observations.
#' @param order.decreasing default FALSE.
#' @param text.size likert plot function option. Default 3.
#' @param flip.coord likert plot function option. Will also change the margins. Default FALSE.
#' @param bar.width likert.bar.plot function option. Default .9.
#' @param use.gray.scale Because it's not 2017 where you live. Default FALSE.
#' @param low.color color for low levels. Default is "#1e90ff"
#' @param high.color color for high levels. Default is #399F96
#' @param label.y likert.bar.plot function option. Default is 'Percentage'.
#' @param label.y.histogram likert.histogram.plot function option. Default is '# Ratings'.
#' @param include.y.lab include label.y? Default TRUE.
# #' @param include.y.ticks Not used anywhere
#' @param include.x.lab show.lab.x.ticks in likert.bar.plot. Default is \code{ifelse(include.histogram & !flip.coord, FALSE, TRUE)}.
#' @param width.plot.saved.inches grob widths. Default is \code{ifelse(!flip.coord | !include.histogram,4,7)}.
#' @param height.saved.inches grob heights. Default is 4.
#' @param width.legend.saved.inches width in ggsave and possibly elsewhere with something added. You'll just have to find out! Default is 2.5.
#' @param padding.y.low For likert.bar.plot. Default 40.
#' @param padding.y.high For likert.bar.plot. Default 40.
#'
#' @importFrom gridExtra arrangeGrob grid.arrange
#' @importFrom likert likert likert.bar.plot likert.histogram.plot
#' @importFrom graphics title
#' @importFrom stats filter
#' @export
#'
plot_diverging_stacked_bar_chart =
  function(evals = evals[evals$subjectRole == "Trainee",],
           savefile = TRUE,
           file.output.base = "plot_diverging_stacked_bar_chart-test",
           dir.output = getwd(),
           include.title = TRUE,
           date.start = min(as.Date(evals$procStartTime)),
           date.stop = Sys.Date(),
           ind.var.name = "traineePGY",
           ind.var.label = stringr::str_to_title(ind.var.name),
           ind.var.limits = NULL,
           # ind.var.max.length = 45,  MC Note: This isn't used anywhere in the function
           ind.var.angle = 0,
           ignore.na.ind = TRUE,
           dep.var.name = "performance",
           dep.var.label = stringr::str_to_title(dep.var.name),
           dep.var.level.labels = levels(evals[,dep.var.name]),
           ignore.na.dep = TRUE,
           # FIXME:  create a new facet for each stratifier
           # stratifier.var.name = "program",  # a variable name, e.g c("program")  MC Note: This isn't used anywhere in the function
           # stratifier.var.label = "Program",
           title = paste(dep.var.label, "distribution by", ind.var.label),
           center.level.index = length(dep.var.level.labels) - 2,
           include.histogram = TRUE,
           include.legend = TRUE,
           ordered.by.observations = FALSE,
           ordered.by = ifelse(ordered.by.observations, "observations", "factor"), # factor, observations, or rank
           order.decreasing = FALSE,
           text.size = 3,
           flip.coord = FALSE,
           bar.width = 0.9,
           use.gray.scale = FALSE,
           low.color = "#1e90ff",
           high.color = "#399F96",
           label.y = "Percentage",
           label.y.histogram = "# Ratings",
           include.y.lab = TRUE,
           # include.y.ticks = TRUE,  Not used anywhere
           include.x.lab = ifelse(include.histogram & !flip.coord, FALSE, TRUE),
           width.plot.saved.inches = ifelse(!flip.coord | !include.histogram,4,7),
           height.saved.inches = 4,
           width.legend.saved.inches = 2.5,
           padding.y.low = 40,
           padding.y.high = 40
  ){
    if(ordered.by.observations & ordered.by != "observations")
      warning("plot_diverging_stacked_bar_chart(): Conflicting ordering specification, will default to observations")

    # Figure out how to order.  Default to factor.
    bOrderedByObservations = ordered.by == "observations"
    bOrderedByRank = ordered.by == "rank" # i.e. likert proportions, high to low
    bOrderedByFactor = ordered.by == "factor" | !bOrderedByObservations | !bOrderedByRank



    # TODO: delete after testing
    # width.saved.inches = 10
    # if(!flip.coord | !include.histogram) {
    #   width.saved.inches = 7
    #   height.saved.inches = 4
    # }

    # TODO: parameterize plotting of comparison.  This will affect the
    # calculation of the bounds for the box.

    evals.filtered = suppressWarnings(window_evals(evals, window.date.start = date.start, window.date.stop = date.stop))

    # Optionally ignore NA values

    # TODO: may need to re-factor (w/ exclude=NULL) to make NA a level
    # which can then be changed to "Unrated" and included in plots


    if(ignore.na.dep)
      evals.filtered = dplyr::filter_(evals.filtered,
                               lazyeval::interp(~ !is.na(var),
                                                var = as.name(dep.var.name)))
    if(ignore.na.ind)
      evals.filtered = dplyr::filter_(evals.filtered,
                               lazyeval::interp(~ !is.na(var),
                                                var = as.name(ind.var.name)))


    # default ordering, and re-factor to get rid of empty levels?
    # May not want to do this, as sometimes want to plot "empty" levels?
    indLevels = levels(factor(evals.filtered[,ind.var.name], ordered = TRUE))
    evals.filtered[,ind.var.name] = factor(evals.filtered[,ind.var.name],
                                           levels = indLevels,
                                           ordered = TRUE)

    # reorder the levels to sort by frequency of observation, if indicated
    # if not ordered by frequency of observations, they will be
    # ordered based on the default ordering of the grouping or, if set, by rank.
    if(bOrderedByObservations) {
      tb <- table(factor(evals.filtered[,ind.var.name]))
      evals.filtered[,ind.var.name] = factor(evals.filtered[,ind.var.name],
                                             levels = names(tb[order(tb)]),
                                             ordered = TRUE)
      indLevels = levels(factor(evals.filtered[,ind.var.name], ordered = TRUE))
    }

    if(nrow(evals.filtered) == 0) {
      message("plot_diverging_stacked_bar_chart called with data frame that, after filtering, is empty.  SKIPPING.")
      return(NULL)
    }


    # FIXME:  filter the evals by the limits, or possible
    # just pass the limits to the methods below.
    if(!is.null(ind.var.limits)) {
      indLevels = indLevels[indLevels >= ind.var.limits[1] &
                              indLevels <= ind.var.limits[2]]
    }

    # rename the levels according to the parameter
    levels(evals.filtered[,dep.var.name]) = dep.var.level.labels

    # figure out if we want to use grey scale or not
    colors = NULL
    if(use.gray.scale) {
      plot.colors = grDevices::colorRampPalette(c("grey0", "grey80"))
      n.levels = levels(evals.filtered[,dep.var.name]) %>% length()
      colors = plot.colors(n.levels)
    }




    # NOTE: if things are acting strangely, be absolutely sure
    # that the HH package isn't loaded--it will collide and
    # mess up everything.  There is a way to "unload" packages
    # but I couldn't figure out how to get it to work

    # require(likert)
    #
    # require(grid)

    title = paste0(title, "\n(", prettyNum(length(unique(evals.filtered$evaluationID)), big.mark = ","), " ratings by ",
                   prettyNum(length(unique(evals.filtered$raterID)), big.mark = ","), " raters of ",
                   prettyNum(length(unique(evals.filtered$subjectID)), big.mark = ","), " subjects)")


    # calculate the proportions for each category
    likert.result =
      likert::likert(dplyr::select(evals.filtered, as.name(dep.var.name)),
                     grouping = evals.filtered[,ind.var.name])


    p.lbp = likert::likert.bar.plot(l = likert.result,
                                    # if not ordering by observations, then let the likert
                                    # package use whatever it wants.  If grouped, then it
                                    # it will use the natural order of the group factor, otherwise
                                    # it will order from high to low.
                                    ordered = bOrderedByRank,
                                    order.decreasing = order.decreasing,
                                    include.histogram = FALSE,
                                    center = center.level.index + 0.5,
                                    # colorRampPalette(colors = c(simpl.constants$color.grey.dark,
                                    #                             simpl.constants$color.grey.light))(center.level.index)
                                    # colorRampPalette(colors = c(simpl.constants$color.dark.2,
                                    #                             simpl.constants$color.grey.light))(center.level.index)
                                    # (length(dep.var.level.labels))
                                    #colors = colorRampPalette(colors = c(color.accent.1,color.light.1))(length(dep.var.level.labels)),
                                    colors = colors, # NULL OK
                                    low.color = low.color,
                                    high.color = high.color,
                                    legend = paste0(dep.var.label, " rating"),
                                    flip.coord = flip.coord,
                                    plot.percent.neutral = FALSE,
                                    plot.percent.low = FALSE,
                                    plot.percent.high = TRUE,
                                    text.size = text.size,
                                    bar.width = bar.width,
                                    label.y = ifelse(include.y.lab, label.y, ''),
                                    show.lab.x.ticks = include.x.lab,
                                    padding.y.low = padding.y.low,
                                    padding.y.high = padding.y.high
    )

    # TODO: use the call below to create the array of colors, interpolated
    # between SIMPL green and PLSC red.  Then use the add.alpha method (defined
    # below) to add alpha to all but the ends of the array.

    p.lbp = ggplot2::last_plot() +
      # use theme to modify the look of the legend.  legend.title etc
      # also available.
      ggplot2::theme(legend.direction='vertical',legend.box='horizontal') +
      # use a guide w/ guide_legend to modify settings
      ggplot2::guides(fill = ggplot2::guide_legend(reverse=TRUE))
    if(include.x.lab) {
      p.lbp = p.lbp + xlab(ind.var.label)
    }




    # extract the legend so we can add the histogram separately.
    # This has to be done here, before removing the legend below.
    # Note that we do it whether or not the plot will include a legend,
    # since we otherwise save it independently
    mylegend = NULL
    mylegend = suppressWarnings(plot_g_extract_legend(a.gplot = p.lbp))

    p.lbp = p.lbp +
      ggplot2::theme_bw()  +
      ggplot2::theme(legend.position="none") +
      ggplot2::theme(strip.background = ggplot2::element_blank(),
            strip.text = ggplot2::element_blank()) +
      # note: the following MUST go here or it will be overwritten the next time
      # a theme() call is made.  Not sure why but oh well.
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ind.var.angle,
                                       hjust = ifelse(ind.var.angle==0, 0.5, 1)))

    # save it for future usage
    likert.result$plot.lbp = p.lbp

    if(include.histogram) {
      # create the histogram plot
      p.lhp = NULL

      # figure out the order of the groups for this histogram by simply
      # leveraging the group factor levels already set in the barplot method.
      levels.ordered = rev(levels(p.lbp$data$Group))
      p.lhp = likert::likert.histogram.plot(likert.result,
                                            group.order = levels.ordered,
                                            xlab = ifelse(flip.coord, label.y.histogram, ind.var.label),
                                            ylab = ifelse(flip.coord, '', label.y.histogram),
                                            bar.color = "#1e90ff",
                                            plot.missing = FALSE,
                                            missing.bar.color = "#399F96",
                                            legend.position = "none",
                                            flip.coord = flip.coord,
                                            text.size = text.size)
      # }
      # else {
      #   p.lhp = likert.histogram.plot(likert.result,
      #                                 xlab = "n",
      #                                 bar.color = c$color.light.2,
      #                                 plot.missing = FALSE,
      #                                 missing.bar.color = c$color.accent.1)
      # }


      # If flipped, nest the bar and histogram plots
      if(flip.coord) {
        p.lhp = ggplot2::last_plot() +
          #   scale_y_continuous(breaks = scales::pretty_breaks(n = 3)) +
          #   theme_bw() + theme(legend.position="none") +
          #   theme(strip.background = element_blank(), strip.text = element_blank()) +
          #   theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
          ggplot2::theme(plot.margin=ggplot2::unit(c(1,1,0.5,-0.5), "cm"))

        p.lbp = p.lbp +
          ggplot2::theme(plot.margin=ggplot2::unit(c(1,0,0.5,1), "cm"))
      }else {
        if(!include.x.lab) {

          p.lhp = ggplot2::last_plot() +
            ggplot2::theme(plot.margin=ggplot2::unit(c(-0.5,0.5,0.5,0.5), "cm")) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ind.var.angle,
                                                               hjust = ifelse(ind.var.angle==0, 0.5, 1)))


          p.lbp = p.lbp +
            ggplot2::theme(plot.margin=ggplot2::unit(c(0.5,0.5,0,0.5), "cm")) +
            ggplot2::theme(axis.text.x=ggplot2::element_blank(),
                  axis.ticks.x=ggplot2::element_blank())


        }
      }

      likert.result$barplot = p.lhp


    }


    p = NULL



    # if this plot includes "extra" elements, then we'll need
    # to create grobs and combine them.
    if(include.histogram || (include.legend & !is.null(mylegend))) {
      # create the grob without drawing it (yet)

      # TODO: use plot_g_draw_grid?  currently that doesn't
      # work because it is designed for vertical grids

      # convert to gtable in order to manipulate size better
      # this suppresses one of the "Stacking not well defined when ymin != 0" warnings
      p.lbp.grob = suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p.lbp)))
      #p.lbp.grob = suppressWarnings(ggplotGrob(p.lbp))


      # if(!include.legend)
      #   width.legend.saved.inches = 0

      # basic plot params
      grobs = list(p.lbp.grob)
      grob.widths = c(width.plot.saved.inches)
      grob.heights = c(height.saved.inches)
      layout = matrix(c(1))


      # now add addition info as needed
      # TODO: calculate these based on width/height parameters
      # TODO: make the multiplier factors (below) a parameter too?
      # Note, only the relative size of the width and height parameters matters here.
      if(include.histogram) {

        # again, convert to gtable so we can easily manipulate size
        p.lhp.grob = suppressWarnings(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p.lhp)))


        # FIXME:  need to combine into one list--use an indexl
        grobs = list(p.lbp.grob, p.lhp.grob)

        if(!flip.coord) {

          # if not flipped then resize to be the same width
          # based (sort-of) on
          p.lhp.grob$widths[2:3] = ggplot2::unit(1.1, "cm")
          p.lbp.grob$widths[2:3] = ggplot2::unit(1.1, "cm")
          grobs = list(p.lbp.grob, p.lhp.grob)

          grob.widths = c(width.plot.saved.inches,
                          width.plot.saved.inches)
          if(ind.var.angle == 0) {
            grob.heights = c(height.saved.inches * 3/4,
                             height.saved.inches * 1/4)
          } else {
            # TODO: calculate the amount of room needed by the labels and adjust accordingly
            grob.heights = c(height.saved.inches * 1/2,
                             height.saved.inches * 1/2)

          }

          layout = rbind(c(1,1),
                         c(2,2))
        }
        else {
          grob.widths = c((width.plot.saved.inches) * .8,
                          (width.plot.saved.inches) * .2) # horizontal stacking
          grob.heights = c(height.saved.inches)
          layout = t(matrix(c(1,2)))
        }
      }

      if(include.legend & !is.null(mylegend)) {
        grobs = c(grobs, list(mylegend)) # append the legend
        #grobs[[length(grobs)+1]] = mylegend

        grob.widths = c(grob.widths,
                        width.legend.saved.inches + 2)  # Note: this "+2" is a total hack for legends that have lengthly labels
        # add a new layout column for the legend, sized & numbered appropriately
        layout = cbind(layout, c(rep(ncol(layout) + 1, nrow(layout))))
      }

      plotArgs = list(grobs = grobs,
                      layout_matrix = layout,
                      heights = grob.heights,
                      nrow = ifelse(flip.coord || !include.histogram, 1, 2),
                      widths=grob.widths,
                      top = ifelse(include.title, title, waiver()))



      # use arrangeGrob to create a grob that can be
      # used by the caller (typically for printing to a file)
      p = do.call(gridExtra::arrangeGrob, plotArgs)

      # use grid.arrange to plot to the current device
      gridExtra::grid.arrange(p)

    }else {
      p = p.lbp

      # use plot to plot to the current device
      plot(p.lbp)
    }


    ### Save to filesystem ###

    if (savefile) {
      # create output directory if it doesn't already exist
      dir.create(file.path(dir.output), showWarnings = FALSE)

      # calculate the width of the images
      width.saved.inches = width.plot.saved.inches

      # if a legend isn't included in the plot, then
      # save an independent image
      if(!include.legend) {
        ggsave(filename = file.path(dir.output,
                                    paste(file.output.base, "-legend.jpg", sep="")),
               plot = mylegend,
               width = width.legend.saved.inches,
               height = height.saved.inches,
               units = "in")
      }
      else {
        width.saved.inches = width.saved.inches + width.legend.saved.inches
      }

      file.name.pdf = file.path(dir.output, paste(file.output.base, ".pdf", sep=""))
      file.name.jpg = file.path(dir.output, paste(file.output.base, ".jpg", sep=""))

      # if(DEBUG_plot_diverging_stacked_bar_chart)
      #   message("Saving ", file.name.pdf)


      ggsave(file.name.pdf,
             plot=p,
             width=width.saved.inches,
             height=height.saved.inches,
             units = "in")
      ggsave(file.name.jpg,
             plot=p,
             width=width.saved.inches,
             height=height.saved.inches,
             units = "in")
    }


    # now add CIs to the likert results


    return(likert.result)
  }

#generate plot
plot_diverging_stacked_bar_chart(evals = evals[evals$subjectRole == "Trainee",])
