#' PVS
#'
#' The \code{pvs_final} applies the PVS code on the whole data set
#'
#' @param pre normalized data set
#' @param bsl baseline
#' @param drop variables which shall not used for PVS
#' @param turnvars variables with "turned" direction
#' @param zvars z variables
#' @param ambivars ambivalent variables
#' @param pvsNA code how to handle NaN values during calculations
#' (default is NA)
#'
#' @return \code{final} list with PVS results for all patients
#' (differences and weights plus PVS score)
#'
#' @export
#'

pvs_final <-
  function(pre,
           bsl,
           drop = NULL,
           turnvars = NULL,
           ambivars = NULL,
           zvars = NULL,
           pvsNA = NA) {
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

    final <- NULL
    if (abort) {

    } else {
      for (i in 1:length(unique(pre$id))) {
        PVS        <- pvs(
          pre = pre,
          bsl,
          a = i,
          drop = NULL,
          turnvars = turnvars,
          ambivars = ambivars,
          zvars = zvars,
          pvsNA = pvsNA
        )$pvs

        d1 <- (unique(pre$id))[i]
        d2 <- pre %>% filter(id == d1)

        PVS <- data.frame(
          id = d2$id,
          id2 = i,
          treatment = d2$treatment,
          condition = d2$condition,
          PVS
        )
        final <- rbind(final, PVS)
        final <- as_tibble(final)
      }
    }

    return(final)
  }
