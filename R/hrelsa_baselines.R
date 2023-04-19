#' Defines baselines and maximum severity values for hRELSA calculation
#'
#' The \code{hrelsa_baselines} function calculates baselines, maximum severity,
#' maximum delta and some characteristics
#'
#' @param pre normalized data set
#' @param bslday time point in the data set used for baseline calculation
#' (i.e. day=-1) (not needed with just z variables)
#' @param vars variables to use as references
#' @param turnvars variables with "turned" direction
#' (e.g. c("hr","temp")) (not needed with just z variables)
#' @param zvars z variables (SDS variables)
#' @param ambivars ambivalent variables
#'
#' @return \code{ret} list with reference variables set to 100 %,
#' maximum reached values and some characteristics
#'
#' @export
#'

hrelsa_baselines <-
  function(pre,
           bslday = NULL,
           vars = NULL,
           zvars = NULL,
           turnvars = NULL,
           ambivars = NULL
           ) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(pre)) {
      warning("There was no data set found. The baseline was not calculated.")
      abort <- TRUE
    }
    if (is.null(vars)) {
      warning("There was no variable for the baseline calculation.
              The baseline was not calculated.")
      abort <- TRUE
    }
    if (length(turnvars) > 0) {
      warn <- FALSE
      for (i in 1:length(turnvars %in% vars)) {
        if ((turnvars %in% vars)[i]) {

        } else {
          abort <- TRUE
          warn <- TRUE
        }
      }
      if (warn) {
        warning(
          "The given turnvars are not stated in the vars array.
          The baseline was not calculated."
        )
      }
    }
    if (length(zvars) > 0) {
      warn <- FALSE
      for (i in 1:length(zvars %in% vars)) {
        if ((zvars %in% vars)[i]) {

        } else {
          abort <- TRUE
          warn <- TRUE
        }
      }
      if (warn) {
        warning("The given zvars are not stated in the vars array.
                The baseline was not calculated.")
      }
    }
    if (length(vars) > length(zvars) && is.null(bslday)) {
      warning(
        "When calculating with normal variables, a baseline day (bslday) is
        needed. The baseline was not calculated."
      )
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    z <- FALSE
    if (length(zvars) == length(vars)) {
      z <- TRUE
    }

    if (abort) {

    } else {
      # Fetch normalized data of reference group on baseline day
      # (typically everywhere 100%)
      # The baseline of normalized z variables is always 100
      # (without a specific baseline day)
      if (z) {

      } else {
        bl                <- pre[pre$time == bslday, vars]
        baseline          <- bl
        if (length(zvars) > 0) {
          for (i in 1:length(zvars)) {
            baseline[, which(vars == zvars[i])] <- 100
          }
        }
      }

      # Fetch values at highest severity depending on how they turn
      # The highest severity of normalized z variables is
      # he highest delta from 100 no matter which direction
      if (z) {
        maxsev <- NULL
      } else {
        maxsev <- apply(pre[, vars], 2, min, na.rm = TRUE)
      }

      if (length(turnvars) > 0) {
        maxsev[turnvars] <- apply(pre[, turnvars], 2, max, na.rm = TRUE)
      }

      if (length(zvars) > 0) {
        maxsev[zvars] <- apply(abs(pre[, zvars]), 2, max, na.rm = TRUE)
      }

      if (length(ambivars) > 0) {
        maxsev[ambivars] <- apply(pre[, ambivars], 2, max, na.rm = TRUE)
      }

      # Fetch maximum delta between 100% and highest severity
      maxdelta            <- abs(100 - maxsev)

      # Model characteristics
      ristics           <-
        data.frame(
          n          = length(unique(pre$id)),
          treatments = length(unique(pre$treatment)),
          conditions = length(unique(pre$condition)),
          variables  = dim(pre[, vars])[2]
        )

      # Giving the baseline table, max severity, max delta and
      # the characteristics out
      # For just z variables without baseline table
      if (z) {
        ret <- list(maxsev = maxsev,
                    maxdelta = maxdelta,
                    ristics = ristics)
      } else {
        ret <-
          list(
            baseline = baseline,
            maxsev = maxsev,
            maxdelta = maxdelta,
            ristics = ristics
          )
      }

      cat(
        "The baseline has been calculated with",
        length(vars),
        "variables.",
        length(turnvars),
        "of them are turned variables,",
        length(zvars),
        "of them are z variables and",
        length(ambivars),
        "of them are ambivalent variables."
      )
      return(ret)

    }
  }
