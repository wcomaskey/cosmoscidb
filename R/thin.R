#' thin
#' Progressively Samples a fraction of a SciDB Array and returns a new SciDB Array 
#' Chooses every Nth element of the SciDB Array
#' *cartesian x,y,z formatting
#' @param sdbARRAY: SciDB type array
#'   name: string -  name of new scidb array
#' @examples
#'   A     <- scidb("ARRAY",data.frame=FALSE) 
#'   B     <- thin(sdbARRAY=A, name="sub-DATA", factor=100)


# Create a subarray using every Nth element of the SciDB Array
thin      <- function(sdbARRAY,name,factor)
{  
  ifdel(name)
  sdbNAME   <- sdbARRAY@name
  sample    <- paste("thin(",sdbNAME,",0,",factor,")",sep="")
  TARGET    =  paste("create array ",name,"<x:double,y:double,z:double>[i=0:*,10000,0]",sep="")
  RDM       =  paste("store(redimension(",sample,",",name,"),",name,")",sep="")
  
  iquery(TARGET)
  iquery(RDM)
  
  THIN<-scidb(name,data.frame=FALSE)
  return(THIN)
}