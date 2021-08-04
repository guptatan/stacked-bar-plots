
#' SIMPL evals function
#'
#' @description Fetches all evaluations from the simpl production read replica
#' and cleans them. This is an expensive operation for the database
#' so use sparingly.
#'
#' TODO: date ranges.
#' @param .sample Return previously generated sample data. Not to be used for real analysis.
#' @return all cleaned evals from the SIMPL database, or from sample data based on params.
#' @export


#' Remove duplicate data from evaluations.
#' @param evals SIMPL evaluations returned from get_evals()
#' @importFrom rlang .data
#' 
clean_evals = function(evals){
  evals %>% dplyr::distinct(.data$caseID, .data$procID, .data$raterID, .data$subjectID, .keep_all = TRUE)
  
  
  #' Add a status column to evaluations that can either be "IGNORE", "ERROR", or "DECLINED" based
  #' on the evaluation's statusDetails field
  #' @param evals SIMPL evaluations returned from get_evals()
  evals$status = factor(ifelse(evals$status == "IGNORE" & evals$statusDetails == "Insufficient interaction",
                               "DECLINED",
                               as.character(evals$status)))
  evals$status = factor(ifelse(evals$status == "IGNORE" & evals$statusDetails == "Incorrect case details",
                               "ERROR",
                               as.character(evals$status)))
  evals$status = factor(ifelse(evals$status == "IGNORE" & evals$statusDetails == "Other",
                               "DECLINED",
                               as.character(evals$status)))
  
  #' Formats all time fields represented by a string as POSIXct type.
  #' @param evals SIMPL evaluations returned from get_evals()
  
  evals$procStartTime <- as.POSIXct(evals$procStartTime, tz="UTC")
  evals$procStopTime <- as.POSIXct(evals$procStopTime, tz="UTC")
  evals$evalCompleted <- as.POSIXct(evals$evalCompleted, tz="UTC")
  
  
  #' Translates the subjectRoleName field into raterRole and subjectRole
  #' Possible values are "Trainee" and "Attending"
  #' @param evals SIMPL evaluations returned from get_evals()
  # add some new columns defining the roles and training levels
  # FIXME: a hack to work around SL-757
  evals = dplyr::filter(evals, evals$subjectRoleName != "OR_ROLE_OTHER")
  subjectRoleNameIndex = grep("^subjectRoleName$", colnames(evals))
  evals = data.frame(evals[,1:(subjectRoleNameIndex-1)],
                     raterRole = ifelse(evals$subjectRoleName == "OR_ROLE_SUPER", "Trainee", "Attending"),
                     subjectRole = ifelse(evals$subjectRoleName == "OR_ROLE_SUPER", "Attending", "Trainee"),
                     evals[,(subjectRoleNameIndex+1):length(evals[1,])])
  
  
  #' Translates all 1 or 0 boolean fields to R logical.
  #' @param evals SIMPL evaluations returned from get_evals()
  
  # change some int columns to logical
  evals$raterArchived = evals$raterArchived > 0
  evals$subjectArchived = evals$subjectArchived > 0
  evals$hasDictation = evals$hasDictation > 0
  
  
  #' Factorize PGY values
  #' @param evals SIMPL evaluations returned from get_evals()
  
  evals$traineePGY = factor(evals$traineePGY, ordered=TRUE)
  
  #' rename and order the levels for supervision and performance.  For performance,
  #' change performance level 6 to NA (it is not scored because of supervision being S&T)
  #' and simplify the levels.  Note that NA is not considered a level.
  #' @param evals SIMPL evaluations returned from get_evals()
  
  evals[!is.na(evals$performance) &
          evals$performance == 6,]$performance = NA
  # remove all the unused levels
  evals$performance = factor(evals$performance, ordered = TRUE)
  # reorder the levels
  evals$performance = factor(evals$performance, levels=rev(levels(evals$performance)), ordered=TRUE)
  evals$supervision = factor(evals$supervision, ordered=TRUE)
  
  # refactor complexity to get rid of observations with null level factor.
  evals$complexity = factor(evals$complexity)
  
  # refactor complexity to get rid of observations with null level factor.
  evals$complexity = factor(evals$complexity)
  
  # factor the IDs
  evals$procID = factor(evals$procID)
  
  # order them by caseID
  evals = evals[order(evals$caseID),]
  
  
  
  #' Create a new column w/ dates and the week (relative to the start date for the study
  #' and for that specific program).
  #' NOTE: using procStartTimes and not evalComplete times since this data
  #' is most likely to be used for time-series analysis of performance
  #' trends (which depend on the actual performance and not the time of evaluation)
  #' @param evals SIMPL evaluations returned from get_evals()
  
  evals$day.study = as.numeric(as.Date(evals$procStartTime)
                               - min(as.Date(evals$procStartTime)))
  evals$week.study = evals$day.study %/% 7
  
  # include a date column that is a Date but w/ appropriate levels
  evals$date = factor(lubridate::floor_date(evals$evalCompleted, "day"))
  
  # figure out the last day of the week for the given evaluation
  evals$week.ending = factor(lubridate::ceiling_date(evals$evalCompleted, "week"))
  
  # include a week column that numbers the weeks of the year, e.g. "2015-25"
  # (replace "00" with "52" and adjust corresponding year)
  evals$week = factor(paste(lubridate::year(evals$evalCompleted),
                            lubridate::week(lubridate::ceiling_date(evals$evalCompleted, "week")) - 1, sep = "-"))
  
  # include a month column that numbers the months of the year, e.g. "2016-Jan"
  evals$month = factor(paste(lubridate::year(evals$evalCompleted),
                             lubridate::month(evals$evalCompleted,
                                              label = TRUE,
                                              abbr = TRUE),
                             sep = "-"))
  return(evals)
}
  


