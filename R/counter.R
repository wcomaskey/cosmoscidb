#' counter
#' Returns the count of an Array filtered at a given radius
#' *Does not count Zero values
#' @param sdbARRAY: SciDB Array
#'   r: distance value with which the array will be filtered and counted
#' @examples
#'   A    <- scidb("DATA",data.frame=FALSE) 
#'   N    <- counter(sdbARRAY=A, r=50.5)


# Returns the Result of the filtering of a SciDB matrix and counting the result
counter   <- function(sdbARRAY,r)
{
  FILT<-paste("d_Mpc>0 and d_Mpc<",r,"")
  B <- Filter(FILT,sdbARRAY)
  c <-count(B)
  return(c)
}