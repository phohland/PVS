#' PVS
#'
#' The \code{pvs} function calculates a root mean square as PVS value
#' using the prior calculated baseline for a specific patient
#'
#' @param pre normalized data set
#' @param bsl baseline
#' @param a unique patient id
#' @param drop variables which shall not used
#' @param turnvars variables with "turned" direction
#' @param ambivars ambivalent variables
#' @param zvars z variables
#' @param pvsNA code how to handle NaN values during calculations
#' (default is NA)
#'
#' @return \code{pvs} list with PVS results for specific patient
#' (differences and weights plus PVS score)
#'
#' @export

pvs <-
  function(pre = pre,
           bsl,
           a = 1,
           drop = NULL,
           turnvars = NULL,
           ambivars = NULL,
           zvars = NULL,
           pvsNA = NA) {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(pre)) {
      warning("There was no data set found. The PVS was not calculated.")
      abort <- TRUE
    }
    if (is.null(bsl)) {
      warning("There was no baseline found. The PVS was not calculated.")
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    ret <- NULL
    if (abort) {

    } else {
      # Build a data subset defined by the chosen patient
      whichhuman  <- unique(pre$id)
      subdata     <- pre[pre$id == whichhuman[a], which(names(pre) == "time"):dim(pre)[2]]
      time         <- subdata$time

      # Drop variables
      if (length(turnvars[turnvars %in% names(subdata)]) == 0) {
        turnvars <- NULL
      }

      if (length(ambivars[ambivars %in% names(subdata)]) == 0) {
        ambivars <- NULL
      }

      if (is.null(drop)) {

      } else {
        subdata[names(subdata) %in% drop] <- NULL
        turnvars                          <-
          turnvars[!(turnvars %in% drop)]
        ambivars                          <-
          ambivars[!(ambivars %in% drop)]
        zvars                             <-
          zvars[!(zvars %in% drop)]
      }

      # Calculate difference matrix for the patient
      delta            <- 100 - subdata[-1]
      bsdelta          <- 100 - bsl$maxsev

      if (length(turnvars) > 0) {
        delta[, turnvars] <- delta[, turnvars] * -1
        bsdelta[turnvars] <- bsdelta[turnvars] * -1
      }

      if (length(ambivars) > 0) {
        delta[, ambivars] <- delta[, ambivars] * -1
        bsdelta[ambivars] <- bsdelta[ambivars] * -1
      }

      if (length(zvars) > 0) {
        delta[, zvars] <- delta[, zvars] * -1
        bsdelta[zvars] <- bsdelta[zvars] * -1
      }

      # Set all negative values to NA
      # (otherwise zero would allow usage in the mean)
      delta[delta < 0]     <- NA

      # Round delta
      delta              <- round(delta, 2)

      # Get the order of names in delta
      namen              <- names(delta)


      # PVS wf calculation --------------------------------------------------

      # Weighting the PVS score by knowing the extreme values
      # from the baseline function
      wfactor   <- NULL
      for (r in 1:dim(delta)[1]) {
        wfactor          <-
          as.data.frame(round(rbind(wfactor, delta[r,] / bsdelta[namen]), 2))
      }

      # Set all negative values to NA
      wfactor[wfactor < 0] <- NA

      # Set all NaN values to 0 (new)
      #wfactor <- as.data.frame(lapply(wfactor, function(x) ifelse(is.nan(x), 0, x)))

      # wf calculation works the following way:
      # form the average of the wfactors of each day
      if (is.null(dim(wfactor))) {
        wf                <- wfactor
      } else{
        wf_sum            <-
          round(apply(wfactor, 1, sum, na.rm = T), 2) # Summarize
        wf                <-
          wf_sum / (dim(wfactor)[2] - rowSums(is.na(wfactor)))
      }
      wf <- as.numeric(wf)
      wf <- round(wf, 2)

      # Kill NaN weights
      if (length(wf) > 0) {
        wf[is.nan(wf)] <- pvsNA
      } else{

      }

      # Kill negative weights
      wf[wf < 0]        <- pvsNA

      # Get NA positions for rms correction
      na.idx           <- is.na(wf)


      # PVS rms calculcation -------------------------------------------------
      # rms calculation works the following way:
      # calculate each wfactor value like this: square root of (x^2)
      # form the average of this each day

      if (is.null(dim(wfactor))) {
        rms          <- sqrt(wfactor ^ 2 / (1 - is.na(wfactor[l])))
      } else{
        rms     <- NULL
        for (l in 1:dim(wfactor)[1]) {
          RMS                <-
            sqrt(sum(wfactor[l, ] ^ 2 , na.rm = TRUE) / (length(wfactor[l, ]) - sum(is.na(wfactor[l, ]))))

          if (sum(is.na(wfactor[l, ])) == length(names(wfactor))) {
            RMS <- NA
          } else {

          }
          rms[l]             <- RMS
        }
        rms[na.idx]          <- NA
        rms     <- as.data.frame(rms)

      }

      # finalizing PVS results -----------------------------------------------

      pvs_3            <- NULL
      pvs_3            <- round(wfactor , 2)
      pvs_3            <- as.data.frame(pvs_3)
      pvs_3            <- cbind(pvs_3, wf)
      pvs_3            <- cbind(time, pvs_3)

      if (all(is.na(rms)) == TRUE) {
        pvs_3$rms      <- as.numeric(unlist(rms))
      } else{
        pvs_3$rms      <- round(rms, 2)
      }

      vars  <- names(subdata)[!(names(subdata) %in% "time")]

      ret <- list(delta = delta,
                  pvs = pvs_3,
                  testVars = vars)
    }
    return(ret)

  }
