#' Formats the data set for hRELSA calculation
#'
#' The \code{hrelsa_format} renames and rearranges the columns to
#' proper work with
#'
#' @param dat data set
#' @param id id variable
#' @param treatment treatment variable
#' @param condition condition variable
#' @param day day variable
#' @param vars variables to work with
#'
#' @return \code{dat} data set rearranged,
#' just with needed and with properly named columns
#'
#' @export
#'

hrelsa_format <-
  function (dat,
            id = NULL,
            treatment = NULL,
            condition = NULL,
            day = NULL,
            vars = NULL) {
    # Searching for errors ----------------------------------------------------
    
    abort <- FALSE
    if (is.null(dat)) {
      warning("There was no data set found. The data set was not formatted.")
      abort <- TRUE
    }
    if (is.null(id) || is.null(day)) {
      warning("There was no id or day column found.
              The data set was not formatted.")
      abort <- TRUE
    }
    cols <- c(id, treatment, condition, day, vars)
    if (FALSE %in% (cols %in% names(dat))) {
      warning("One or more of the columns was not found in the data set.
              The data set was not formatted.")
      abort <- TRUE
    }
    
    # Function code -----------------------------------------------------------
    
    if (abort) {
      
    } else {
      col_id <- which(names(dat) == id)
      colnames(dat)[col_id] <- "id"
      
      col_day <- which(names(dat) == day)
      colnames(dat)[col_day] <- "day"
      
      cols <- c("id")
      if (!(is.null(treatment))) {
        col_treatment <- which(names(dat) == treatment)
        colnames(dat)[col_treatment] <- "treatment"
        cols <- append(cols, "treatment")
      }
      
      if (!(is.null(condition))) {
        col_condition <- which(names(dat) == condition)
        colnames(dat)[col_condition] <- "condition"
        cols <- append(cols, "condition")
      }
      cols <- append(cols, "day")
      
      dat <-
        select(dat, all_of(cols), all_of(vars))
    }
    return(dat)
  }