#' Calculation of normal blood pressure values for children in germany
#'
#' The \code{hrelsa_kiggs} 
#'
#' @param 
#' @param 
#' @param 
#'
#' @return \code{set} 
#' 
#' @export
#'
#'

## Diastole boys
## z-Score z = (x : M – 1) : S
## M = 54,22924 + 0,90280 • Alter + 0,02825 • (Alter – 10)2 + 0,52654 • zGröße

hrelsa_kiggs <- function(systole = TRUE, sex, age, height){
  
  if (sex %in% c(1,2) && age >= 3 && height > 0) {
    
    ##
  } else {
    warning("Please check the input variables. The blood pressure was not calculated.")
  }
  
  return(rr)
}