#' bernoulli
#' Randomly Samples a fraction of a SciDB Array and returns a new SciDB Array 
#' *cartesian x,y,z formatting
#' @param sdbARRAY: SciDB type array
#'   name: string -  name of new scidb array
#' @examples
#'   A     <- scidb("ARRAY",data.frame=FALSE) 
#'   B     <- bernoulli(sdbARRAY=A, name="sub-DATA", fraction=0.1)

# Create a subarray using a random fraction of the SciDB Array
bernoulli <- function(sdbARRAY,name,fraction)
{
  #sdbSCHEMA <- sdbARRAY@schema
  #TARGET =  paste("create array ",name,"",sdbSCHEMA,"")
  ifdel(name)
  sdbNAME   <- sdbARRAY@name
  sample    <- paste("bernoulli(",sdbNAME,",",fraction,")",sep="")
  RDM       =  paste("store(redimension(",sample,",",name,"),",name,")",sep="")
  TARGET    =  paste("create array ",name,"<x:double,y:double,z:double>[i=0:*,10000,0]",sep="")
  
  iquery(TARGET)
  iquery(RDM)
  
  BERNOULLI<-scidb(name,data.frame=FALSE)
  return(BERNOULLI)
}