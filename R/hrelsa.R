#' Relative Severity Assessment Score for Humans
#' = hRELSA
#'
#' The \code{hrelsa} function calculates a composite relative severity score
#' based on normalized differences. Further, the values are regularized with a
#' reference set to estimate relative severity.
#' This is the main function of hRELSA and is normally used in
#' the hrelsa_final function.
#'
#' @param pre normalized data set
#' @param bsl data set baseline
#' @param a unique patient index in data set
#' @param drop variables which shall not used for hRELSA
#' @param turnvars variables with "turned" direction
#' @param zvars z variables
#' @param relsaNA code how to handle NaN values during calculations
#' (default is NA)
#'
#' @return \code{hrelsa} list with hRELSA results for specific patient
#' (differences and weights plus hRELSA score)
#'
#' @export

hrelsa <-
  function(pre = pre,
           bsl,
           a = 1,
           drop = NULL,
           turnvars = NULL,
           zvars = NULL,
           relsaNA = NA) {
    # Searching for errors ----------------------------------------------------
    
    abort <- FALSE
    if (is.null(pre)) {
      warning("There was no data set found. The baseline was not calculated.")
      abort <- TRUE
    }
    if (is.null(bsl)) {
      warning("There was no baseline found. The baseline was not calculated.")
      abort <- TRUE
    }
    
    # Function code -----------------------------------------------------------
    
    ret <- NULL
    if (abort) {
      
    } else {
      # Build a data subset defined by the chosen patient
      whichhuman  <- unique(pre$id)
      subdata     <- pre[pre$id == whichhuman[a], which(names(pre) == "day"):dim(pre)[2]]
      day         <- subdata$day
      
      # Drop variables
      if (length(turnvars[turnvars %in% names(subdata)]) == 0) {
        turnvars <- NULL
      }
      
      if (is.null(drop)) {
        
      } else {
        subdata[names(subdata) %in% drop] <- NULL
        turnvars                          <-
          turnvars[!(turnvars %in% drop)]
        zvars                             <-
          zvars[!(zvars %in% drop)]
      }
      
      # Calculate difference matrix for the patient & turn "positive" variables
      delta            <- 100 - subdata[-1]
      bsdelta          <- 100 - bsl$maxsev
      
      if (length(turnvars) > 0) {
        delta[, turnvars] <- delta[, turnvars] * -1
        bsdelta[turnvars] <- bsdelta[turnvars] * -1
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
      
      
      # hRELSA wf calculcation --------------------------------------------------
      
      # Weighting the relsa score by knowing the extreme values
      # from the baseline function
      wfactor   <- NULL
      for (r in 1:dim(delta)[1]) {
        wfactor          <-
          as.data.frame(round(rbind(wfactor, delta[r,] / bsdelta[namen]), 2))
      }
      
      # Set all negative values to NA
      wfactor[wfactor < 0] <- NA
      
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
        wf[is.nan(wf)] <- relsaNA
      } else{
        
      }
      
      # Kill neagative weights
      wf[wf < 0]        <- relsaNA
      
      # Get NA positions for rms correction
      na.idx           <- is.na(wf)
      
      
      # hRELSA rms calculcation -------------------------------------------------
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
      
      # finalizing hRELSA results -----------------------------------------------
      
      relsa_3            <- NULL
      relsa_3            <- round(wfactor , 2)
      relsa_3            <- as.data.frame(relsa_3)
      relsa_3            <- cbind(relsa_3, wf)
      relsa_3            <- cbind(day, relsa_3)
      
      if (all(is.na(rms)) == TRUE) {
        relsa_3$rms      <- as.numeric(unlist(rms))
      } else{
        relsa_3$rms      <- round(rms, 2)
      }
      
      vars  <- names(subdata)[!(names(subdata) %in% "day")]
      
      ret <- list(delta = delta,
                  relsa = relsa_3,
                  testVars = vars)
    }
    return(ret)
    
  }
