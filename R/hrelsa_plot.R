#' Plot hRELSA score and variables together
#'
#' The \code{hrelsa_plot} function visualizes the hRELSA score alone or in
#' combination with one of the variables.
#'
#' @param set data set
#' @param RELSA hRELSA object
#' @param levels k+1 levels from kmeans calculations (hrelsa_levels function)
#' @param a which unique patient in the set?
#' @param plotvar which variable shall be plotted (does not scale with hRELSA score)
#' @param plothRELSA plot the hRELSA score?
#' @param myylim custom y-axis limits (left axis)
#' @param myYlim custom y-axis limits (right axis)
#' @param mypch custom dot marker
#' @param mycol custom plot color
#' @param myXlab custom x-axis label
#'
#' @return \code{plotset} data.frame with not normalized values of unique patient
#'
#' @importFrom graphics plot abline mtext
#'
#' @export
#'


hrelsa_plot <-
  function(set,
           hRELSA,
           levels = NULL,
           a = 1,
           plotvar = NULL,
           plothRELSA = TRUE,
           myylim = c(-4, 4),
           myYlim = c(0, 2),
           mypch = 1,
           mycol = "red",
           myXlab = "time point") {
    # Searching for errors ----------------------------------------------------

    abort <- FALSE
    if (is.null(set)) {
      warning("There was no data set found. The plot was not generated.")
      abort <- TRUE
    }
    if (is.null(hRELSA)) {
      warning("There was no hRELSA found. The plot was not generated.")
      abort <- TRUE
    }
    if (a < 1 || a > (length(unique(set$id)))) {
      warning("The unique patient id was invalid. The plot was not generated.")
      abort <- TRUE
    }

    # Function code -----------------------------------------------------------

    plotset    <- set[set$id == unique(set$id)[a],]

    if (abort) {

    } else {
      hrelsatype  <- as.numeric(unlist(hRELSA$relsa$rms))

      if (length(plotvar) != 0) {
        plot(
          plotset$time,
          unlist(plotset[, which(names(plotset) == plotvar)]),
          pch = mypch,
          cex.axis = 1.2,
          cex.lab = 1.2,
          type = "b",
          ylab = plotvar,
          xlab = myXlab,
          ylim = myylim
        )

        if (plothRELSA == TRUE) {
          par(new = TRUE)
          plot(
            plotset$time,
            hrelsatype,
            type = "b",
            lwd = 2,
            xaxt = "n",
            yaxt = "n",
            ylab = "",
            xlab = "",
            col = mycol,
            ylim = myYlim,
            pch = mypch
          )
          axis(side = 4)
          mtext("hRELSA Score", side = 4, line = 3)

          # write level lines to plot
          if (length(levels) == 0) {

          } else{
            for (l in 1:length(levels)) {
              abline(h = levels[l],
                     lwd = 1,
                     lty = 2)
            }
          }

        } else{

        }

      } else{
        plot(
          hRELSA$relsa$time ,
          hrelsatype,
          type = "b",
          lwd = 2,
          ylab = "hRELSA score",
          cex.axis = 1.2,
          cex.lab = 1.2,
          xlab = myXlab,
          col = mycol,
          ylim = myYlim,
          pch = mypch
        )

        # write level lines to plot
        if (length(levels) == 0) {

        } else{
          for (l in 1:length(levels)) {
            abline(h = levels[l],
                   lwd = 1,
                   lty = 2)
          }
        }

      }
    }

    #return(hRELSA_plot)
  }
