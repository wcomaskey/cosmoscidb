#' tpcf
#' Given an input 1D SciDB Array with Cartesian coordinates
#' Returns a 3D SciDB Array with dimensions of 1000 times the values
#' to maintain uniqueness but allow for fast regional querying
#' @param sdbARRAY: Input 1D scidb array with cartesian coordinates x,y,z
#'   name: Name of Resulting SciDB Array
#' @examples
#'   A     <- scidb("ARRAY",data.frame=FALSE) 
#'   A.3D  <- make.3D(sdbARRAY=A,name="Array3D")


# Creates a 3D array from a 1D Array (SciDB)
make.3D        <- function(sdbARRAY,name)
{
  sdbNAME   <- sdbARRAY@name
  
  # Get the range for the Indices
  MX <- as.integer(1000*max(sdbARRAY)[1:4])    # Convert Ranges to Kilo-parsecs
  MN <- as.integer(1000*min(sdbARRAY)[1:4])    #
  
  # Optimal Chunk size for a 3D array
  cz = as.integer(5E4)                
  
  # Create Target Array
  TARGET =  paste("create array ",name,"<x:double,y:double,z:double,m:int64,vx:double,vy:double,vz:double>[i=0:*,",cz,",0,X=",MN[2],":",MX[2],",",cz,",0,Y=",MN[3],":",MX[3],",",cz,",0,Z=",MN[4],":",MX[4],",",cz,",0]",sep="")
  
  # Convert to target
  Q1 = paste("project(apply(",sdbNAME,",X,int64(x*1000),Y,int64(y*1000),Z,int64(z*1000)),X,Y,Z,x,y,z,m,vx,vy,vz)")
  Q2 = paste("store(redimension(",Q1,",",name,"),",name,")")
  
  ifdel(name)
  iquery(TARGET)
  iquery(Q2)
  
  Three.D<-scidb(name, data.frame=FALSE)
  return(Three.D)
}