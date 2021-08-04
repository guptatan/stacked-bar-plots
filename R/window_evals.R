#' window_evals
#'
#' @description Window the evals based on fuzzy date boundaries, to account for the fact that
#' evaluations may be completed by one rater during the window but by the other
#' rater(s) outside the window.
#'
#'
#' @param evals The raw (un-windowed) evals
#' @param window.date.start The starting data of the window
#' @param window.date.stop The stopping data of the window (inclusive)
#' @param window.dates.file allows per-program dates to be used.  Must contain column progID and window.date.start and/or window.date.stop.
#' @param by.completed.after.start.date If TRUE, only include those procedures that started after the start date.  If FALSE, then include anything that has an evaluation after the start date, even if the procedure began earlier.
#' @param return.paired If TRUE, return a "paired" version of the data.frame where each procedure has both attending and trainee data on the same line
#'
#' @details #' If by.completed.after.start.date is TRUE, then this function
#' will window evals based on the date that the first evaluation for any given PE
#' was completed.  This has the benefit of making counts "stable" between
#' windows, i.e. it won't double count evaluations in multiple reports.  It also
#' more closely reflects the date the *evaluation* is done, and not the case.
#' This is most useful when tabulating descriptive statistics about how many
#' evaluations are done in a given time period.

#' However, there are some use cases where windowing should be done based on the
#' PE date--for example if doing a time series analysis looking at interactions
#' between observations. Then you would want the date to correlate with the
#' actual performance and not just the date of the rating.
#'
#'
#' @importFrom utils read.csv
#' @importFrom rlang .data
#' @return The windowed evals.
window_evals = function(evals = evals, # all evals
                        window.date.start = as.Date("2015-09-01"),
                        window.date.stop = Sys.Date(),
                        window.dates.file = NULL,
                        by.completed.after.start.date = TRUE,
                        return.paired = FALSE
                        ) {

  if(is.null(window.date.stop))   # MC Note: the default is not NULL, and this is only useful if it is.
    window.date.stop = Sys.Date()

  # check to see if this window has already been completed by creating a unique key
  # FIXME 2016.08.02:  not quite right...could be a different subset of evals that is being
  # passed...but wait, that shouldn't matter right?
  # window.key = paste0(itostr(as.integer(as.POSIXct(window.date.start)), base=36),
  #                     itostr(as.integer(as.POSIXct(window.date.stop)), base=36))
  # if(window.key %in% colnames(evals)) {
  #   return(evals) # already windowed for this window
  # }
  # else {
  #   evals[window.key] = NA
  # }



  # save the names for later (but in an order that puts the caseID and procID first)
  original.col.names = names(dplyr::select(evals, .data$caseID, .data$procID, dplyr::everything()))

  # TODO: maybe "roughly" window them before doing anything else, say w/ a 7 day buffer on each
  # side?

  # TODO: maybe break out all the "pairing" code into a separate function
  # and then call it from here iff there isn't already a 'creatorID' column?
  # If there is a creatorID column, then we can avoid the join in doing our
  # windowing, and instead just filter based on the eval that was first created.
  # 2016.08.02:  the creatorID is now always there since it comes from the SQL.
  # That means below could probably be optimized...?



  # NOTE:  this is expensive since it does a join--should probably be done for subsets
  #        of data.  Or maybe just do it with a few columns and then merge it all back
  #        together after the join and rbind are done?
  # FIXME: this assumes that raters can only rate those with a different
  #        role (i.e. Attendings rate Trainees and vice-versa).  Currently true!
  evals.paired <-
    dplyr::select(evals, tidyselect::everything()) %>%    # MC Note: por que?
    dplyr::filter(.data$raterRole == "Trainee") %>%  # left side is just trainees
    dplyr::full_join( dplyr::select(dplyr::filter(evals, .data$raterRole == "Attending"),
                                    tidyselect::everything()),
              by = c("caseID", "procID",
                     "raterID" = "subjectID") ) # join on "paired" evals

  # add a new column that pulls the status from the paired eval, which is
  # used in some analyses to decide whether or not to include the evaluation
  if(!("pairedStatus" %in% colnames(evals))) {
    evals.paired = evals.paired %>% dplyr::mutate(pairedStatus.x = .data$status.y,
                                                  pairedStatus.y = .data$status.x)
    original.col.names = c(original.col.names, "pairedStatus")
  }


  # in order to really determine the window in which a given eval is counted, we use the first
  # evaluation submitted for a PE.  This "fuzzy" boundary accounts for those evaluations that
  # are submitted after a date threshold for procedures that actually happened within a given
  # time period.  This ensures consistent counting between various methods.  To do this
  # create a "window time" depending on the criteria used
  if(by.completed.after.start.date) {
    evals.paired$window.time = apply(evals.paired, 1,
                                     FUN = function(x) { sort(x[c("evalCompleted.x",
                                                                  "evalCompleted.y")])[1] })
  }else { # use procStopTime
    evals.paired$window.time = apply(evals.paired, 1,
                                     FUN = function(x) { sort(x[c("procStopTime.x",
                                                                  "procStopTime.y")])[1] })
  }

  # if the delay isn't there, add it
  if(!("delay" %in% colnames(evals)))
  {
    # Before we do the final filter, try and figure out
    # a) which eval was done first
    # b) what the time difference is between the CREATION of the eval
    #    and the COMPLETION of the eval
    evals.paired$delay.x = apply(evals.paired, 1,
                                 FUN = function(x) {
                                   if(is.na(x[c("evalCompleted.x")]))
                                     return(NA)
                                   else
                                     return(difftime(x[c("evalCompleted.x")],
                                                     x[c("procStopTime.x")],
                                                     units = "secs"))
                                 })
    evals.paired$delay.y = apply(evals.paired, 1,
                                 FUN = function(x) {
                                   if(is.na(x[c("evalCompleted.y")]))
                                     return(NA)
                                   else
                                     return(difftime(x[c("evalCompleted.y")],
                                                     x[c("procStopTime.y")],
                                                     units = "secs"))
                                 })

    original.col.names = c(original.col.names, "delay")
  }

  # Use only the earliest evaluation to "window" the evaluation.  This ensure
  # consistent counting between periods.  Note that "sort" conveniently ignores
  # NA
  # first figure out if this is a per-program window or a global window
  if(!is.null(window.dates.file)) {
    stop("Not yet implemented for per-program filtering")
    # now filter for each program's switch date
    switch.dates = utils::read.csv(file = file.path(window.dates.file),
                            head=TRUE,
                            na.strings=c("NA", "NULL"))
    switch.dates$window.date.stop = as.Date(switch.dates$window.date.stop, format = "%m/%d/%Y")

    # FIXME:  this won't work because there is a programID.x and programID.y column yet
    # we want to iterate over each programID as a group.  May need to instead
    # iterate over the switch.dates and use those as inputs to some calls to "do" and/or filter.

    # first join the dates to the evals--will need to be smart since have .x and .y.

    # vestigal below:
    evals.paired = evals.paired %>%
      dplyr::group_by(.data$programID) %>%
      dplyr::do(dplyr::filter(.data$window.time >=
                         (as.Date(switch.dates[switch.dates$progID==
                                                                .data$programID[1],
                                               "window.date.start"])) ) %>%
                  dplyr::filter(.data$window.time <
                                  (as.Date(switch.dates[switch.dates$progID==
                                                          .$programID[1],"window.date.stop"]) + 1) )) %>%
      data.frame()

  }else {# global
    evals.paired = evals.paired %>%
      dplyr::filter(.data$window.time >= (as.Date(window.date.start)) ) %>%
      dplyr::filter(.data$window.time < (as.Date(window.date.stop) + 1) ) # accounts for entire stop day
  }




  # add back the subjectID.y column which was dropped by the full_join, then rename
  # the raterID and subjectID to include the ".x" suffix.  This is all needed
  # to split the two frames apart (below)
  raterArchivedYIndex = grep("^raterArchived.y$", colnames(evals.paired))
  evals.paired = data.frame(evals.paired[,1:(raterArchivedYIndex)],
                            subjectID.y = evals.paired$raterID,
                            evals.paired[,(raterArchivedYIndex+1):length(evals.paired[1,])])

  raterIDIndex = grep("^raterID$", colnames(evals.paired))
  subjectIDIndex = grep("^subjectID$", colnames(evals.paired))
  colnames.new = colnames(evals.paired)
  colnames.new[raterIDIndex] = "raterID.x"
  colnames.new[subjectIDIndex] = "subjectID.x"
  colnames(evals.paired) = colnames.new

  subset.x = NULL
  subset.y = NULL

  # if the paired isn't there, add it
  if(!("paired" %in% colnames(evals))) {
    # create a new column (paired) to indicate if both trainee and attending did the rating.
    evals.paired$paired = !is.na(evals.paired$evaluationID.x) & !is.na(evals.paired$evaluationID.y)
    subset.x = dplyr::select(evals.paired, .data$caseID, .data$procID, dplyr::ends_with(".x"), .data$paired) %>%
      dplyr::filter(!is.na(.data$evaluationID.x))
    subset.y = dplyr::select(evals.paired, .data$caseID, .data$procID, dplyr::ends_with(".y"), .data$paired) %>%
      dplyr::filter(!is.na(.data$evaluationID.y))
    original.col.names = c(original.col.names, "paired")
  }else {
    subset.x = dplyr::select(evals.paired, .data$caseID, .data$procID, dplyr::ends_with(".x")) %>%
      dplyr::filter(!is.na(.data$evaluationID.x))
    subset.y = dplyr::select(evals.paired, .data$caseID, .data$procID, dplyr::ends_with(".y")) %>%
      dplyr::filter(!is.na(.data$evaluationID.y))
  }


  names(subset.x) = original.col.names
  names(subset.y) = names(subset.x)
  evals.unpaired = rbind(subset.x, subset.y) %>%
    dplyr::arrange(.data$caseID, .data$procID)


  # View(evals.unpaired %>% select(caseID, procID, paired,
  #                              evaluationID,
  #                              raterLastName, subjectLastName,
  #                              evalCompleted))
  #
  # View(evals.paired %>% select(caseID, procID, paired,
  #                         evaluationID.x, evaluationID.y,
  #                         raterLastName.x, subjectLastName.x,
  #                         raterLastName.y, subjectLastName.y,
  #                         evalCompleted.x, evalCompleted.y))


  # TODO: cache these results in memory or to disk somewhere?  need to have a unique
  # signature for each set of parameters, not trivial.

  if(return.paired)
    return(evals.paired)
  else
    return(evals.unpaired)

}

