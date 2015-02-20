#' tpcf
#' Given three input SciDB Arrays and an array of filtering Radii
#' Creates a dataframe of counts over each radius value
#' *suggested input of DATA-DATA,RANDOM-RANDOM,and DATA-RANDOM covariance
#' distance matrices
#' @param DD: SciDB Array
#'   RR: SciDB Array
#'   DR: SciDB Array
#'   R: numeric array of Radii values
#'   status: Print completion status DEFAULTS to TRUE
#' @examples
#'   DD    <- scidb("ARRAY1",data.frame=FALSE) 
#'   RR    <- scidb("ARRAY2",data.frame=FALSE)  
#'   DR    <- scidb("ARRAY3",data.frame=FALSE)  
#'   R     <- numeric(1.0,5.0,10.0,50.0)
#'   DATA  <- tpcf(DD=DD, RR=RR, DR=DR, R=R, status=TRUE)

#Correlation Function
tpcf      <- function(DD,RR,DR,R,status=TRUE)
{
  #Initiate Arrays
  DD.counts <- numeric()
  RR.counts <- numeric()
  DR.counts <- numeric()
  
  
  for(i in (1:length(R)))  
  {  
    r<-R[i]
    #Get the counts for each of the Covariance Matrcices
    dd <- counter(sdbARRAY=DD,r=r)
    rr <- counter(sdbARRAY=RR,r=r)
    dr <- counter(sdbARRAY=DR,r=r)
    
    DD.counts <- c(DD.counts,dd)
    RR.counts <- c(RR.counts,rr)
    DR.counts <- c(DR.counts,dr)
    
    if(status){print(paste((100*i/length(R)),"% Complete"))}
  }
  Data <- data.frame(R,DD.counts,RR.counts,DR.counts)
  c("r.Mpc","DD","RR","DR") -> colnames(Data)
  
  return(Data)
}