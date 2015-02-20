#' szalay
#' Landy & Szalay: Two-Point correlation Function Estimator (1993)
#' @param DD,DR,RR: numeric arrays of DATA-DATA and DATA-RANDOM counts
#' ND,NR: integer values of the size of the DATA and RANDOM sets respectively
#' @examples
#'   RESULTS <- szalay(DD=DD.counts,RR=RR.counts,DR=DR.counts,ND=100,NR=1000) 

szalay    <- function(DD,RR,DR,ND,NR)
{
  E <- 1 + ((NR/ND)^2)*(DD/RR)-2*(NR/ND)*(DR/RR) 
  return(E)
}