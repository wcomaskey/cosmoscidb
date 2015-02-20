#' target.region
#' Given an input SciDB Array and the minimum and maximum values for x,y,and z coordinates
#' Allows for efficient analyses of target region
#' @param sdbARRAY: SciDB Array
#'   name: Name of New SciDB Array
#'   xmin,ymin,zmin,xmax,ymax,zmax: double precision coordinate values to bound region
#' @examples
#'   A    <- scidb("ARRAY1",data.frame=FALSE) 
#'   REG  <- target.region(sdbARRAY=A,name="SubRegion",xmin  = 0.0, ymin  = 0.0,zmin  = 0.0, xmax  = 50.0,ymax  = 50.0, zmax  = 50.0)

# Create a 1D array from a region in a 3D Array (SciDB)
target.region        <- function(sdbARRAY,name,xmin,ymin,zmin,xmax,ymax,zmax)
{
  sdbNAME   <- sdbARRAY@name
  xmin<-as.integer(1000*xmin)
  xmax<-as.integer(1000*xmax)
  ymin<-as.integer(1000*ymin)
  ymax<-as.integer(1000*ymax)
  zmin<-as.integer(1000*zmin)
  zmax<-as.integer(1000*zmax)
  N   <-as.integer(count(sdbARRAY)-1)
  TARGET =  paste("create array ",name,"<x:double,y:double,z:double>[i=0:*,1000,0]",sep="")
  BTW    =  paste("subarray(",sdbNAME,",0,",xmin,",",ymin,",",zmin,",",N,",",xmax,",",ymax,",",zmax,")",sep="")
  RDM    =  paste("store(redimension(",BTW,",",name,"),",name,")",sep="")
  
  ifdel(name)
  iquery(TARGET)
  iquery(RDM)
  
  ONE.D<-scidb(name, data.frame=FALSE)
  return(ONE.D)
}