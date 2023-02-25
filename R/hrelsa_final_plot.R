#' hRELSA final plot
#'
#' The \code{hrelsa_final_plot} 
#'
#' @param x 
#'
#' @return \code{robj}
#'
#' @import dplyr
#' @export
#'

relsa_final_plot <- function(
  hRELSA = NULL,
  which_patient = 1,
  
)
{
  
  plot <- hrelsa_plot(dat, hRELSA, levels = levels, a = which_patient, plotvar = which_plotvar, plotRELSA = TRUE,
                      myylim = c(-4, 4),  myYlim = c(0, 1), mypch = 1, mycol = "red", myXlab="days")
  
  
}