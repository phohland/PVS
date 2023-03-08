#' Relative Severity Assessment Score for Humans
#' = hRELSA
#'
#' The \code{hrelsa_final} function calculates a composite relative severity score
#' based on normalized differences. Further, the values are regularized with a
#' reference set to estimate relative severity.
#' This is just the function to perform hRELSA on all patients.
#' The main function is hRELSA.
#'
#' @param pre normalized data set
#' @param bsl data set baseline
#' @param drop variables which shall not used for hRELSA
#' @param turnvars variables with "turned" direction
#' @param zvars z variables
#' @param relsaNA code how to handle NaN values during calculations
#' (default is NA)
#'
#' @return \code{final} list with hRELSA results for all patients
#' (differences and weights plus hRELSA score)
#'
#' @export

hrelsa_final <-
  function(pre,
           bsl,
           drop = NULL,
           turnvars = turnvars,
           zvars = zvars,
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

    final <- NULL
    if (abort) {

    } else {
      for (i in 1:length(unique(pre$id))) {
        RELSA        <- hrelsa(
          pre = pre,
          bsl,
          a = i,
          drop = NULL,
          turnvars = turnvars,
          zvars = zvars,
          relsaNA = relsaNA
        )$relsa

        d1 <- (unique(pre$id))[i]
        d2 <- pre %>% filter(id == d1)

        RELSA <- data.frame(
          id = d2$id,
          id2 = i,
          treatment = d2$treatment,
          condition = d2$condition,
          RELSA
        )
        final <- rbind(final, RELSA)
        final <- as_tibble(final)
      }
    }

    return(final)
  }
