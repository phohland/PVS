#' PVS
#'
#' The \code{pvs_format} formats the data
#'
#' @param dat data set
#' @param id id variable
#' @param treatment treatment variable
#' @param condition condition variable
#' @param time time variable
#' @param vars variables
#' @param included_realtime variable of real time variable when needed
#'
#' @return \code{dat} formatted data set
#'
#' @export
#'

pvs_format <-
  function (dat,
            id = NULL,
            treatment = NULL,
            condition = NULL,
            time = NULL,
            vars = NULL,
            included_realtime = NULL) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(dat)) {
      warning("There was no data set found. The data set was not formatted.")
      abort <- TRUE
    }
    if (is.null(id) || is.null(time)) {
      warning("There was no id or time column found.
              The data set was not formatted.")
      abort <- TRUE
    }
    cols <- c(id, treatment, condition, time, vars)
    if (FALSE %in% (cols %in% names(dat))) {
      warning("One or more of the columns was not found in the data set.
              The data set was not formatted.")
      abort <- TRUE
    }
    if (!(is.null(included_realtime)) && !(included_realtime %in% names(dat))) {
      warning("The realtime column was not found in the data set.
              The data set was not formatted.")
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    if (abort) {

    } else {
      col_id <- which(names(dat) == id)
      colnames(dat)[col_id] <- "id"
      dat[col_id] <- as.factor(unlist(dat[col_id], use.names = FALSE))


      col_time <- which(names(dat) == time)
        colnames(dat)[col_time] <- "time"
        time <- "time"

      cols <- c("id")
      if (!(is.null(treatment))) {
        col_treatment <- which(names(dat) == treatment)
        colnames(dat)[col_treatment] <- "treatment"
        cols <- append(cols, "treatment")
        dat[col_treatment] <- as.factor(unlist(dat[col_treatment], use.names = FALSE))
      } else {
        dat$treatment <- NA
        cols <- append(cols, "treatment")
      }

      if (!(is.null(condition))) {
        col_condition <- which(names(dat) == condition)
        colnames(dat)[col_condition] <- "condition"
        cols <- append(cols, "condition")
        dat[col_condition] <- as.factor(unlist(dat[col_condition], use.names = FALSE))
      } else {
        dat$condition <- NA
        cols <- append(cols, "condition")
      }

      cols <- append(cols, time)

        if (!(is.null(included_realtime))) {
          dat <- select(dat, all_of(cols), all_of(vars), included_realtime)
      } else {
        dat <-
          select(dat, all_of(cols), all_of(vars))
      }


    }
    return(dat)
  }
