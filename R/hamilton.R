#' hamilton
#' Hamilton: Two-Point correlation Function Estimator (1993)

#' @param DD,DR,RR: numeric arrays of DATA-DATA and DATA-RANDOM counts
#' @examples
#'   RESULTS <- hamilton(DD=DD.counts,RR=RR.counts,DR=DR.counts) 

hamilton  <- function(DD,RR,DR)
{
  E <- ((DD*RR)/(DR^2))-1 
  return(E)
}
