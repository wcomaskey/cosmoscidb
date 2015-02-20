#' peebles
#' Davis & Peebles: Two-Point correlation Function Estimator (1983)

#' @param DD,DR: numeric arrays of DATA-DATA and DATA-RANDOM counts
#' ND,NR: integer values of the size of the DATA and RANDOM sets respectively
#' @examples
#'   A     <- scidb("ARRAY",data.frame=FALSE) 
#'   RESULTS <- peebles(DD=DD.counts,DR=DR.counts,ND=100,NR=1000) 


#ESTIMATOR
peebles   <- function(DD,DR,ND,NR)
{
  E <- (NR/ND)*(DD/DR)-1
  return(E)
}
