#' poisson.scidb
#' This function allows you to create a random poisson sampling of data points in cartesian space,
#' given an input Array from SciDB
#' @param sdbARRAY: SciDB Array
#'   name: string - name of new SciDB array
#'   factor: integer - How much larger than the base Array - DEFAULTS to 1
#' @examples
#'   A <- scidb('DATA', data.frame=FALSE)
#'   R <- poisson.scidb(sdbARRAY=A,factor=10)

# Create a random 3 attribute SciDB Array between Minimum 'Min' and Maximum 'Max'values of Size 'size'
poisson.scidb   <- function(sdbARRAY,name,factor=1)
{
  # Takes 3Dim Min and Max Arrays and the Size N
  # Create a Large Sample of Poisson Particles within the same volume as Data-Set
  # Delete Pre-existing Arrays
  ifdel('X')
  ifdel('Y')
  ifdel('Z')
  # Get size of base array
  size<-count(sdbARRAY)
  
  # GET the Max Values for each Dimension
  xMax <- max(sdbARRAY)[2]
  yMax <- max(sdbARRAY)[3]
  zMax <- max(sdbARRAY)[4]
  
  # GET the Min Values for each Dimension
  xMin <- min(sdbARRAY)[2]
  yMin <- min(sdbARRAY)[3]
  zMin <- min(sdbARRAY)[4]
  
  # Random Number Generator Creates a number from 0 to 2147483647
  Qx  <- 2147483647.0/(xMax-xMin)
  Qy  <- 2147483647.0/(yMax-yMin)
  Qz  <- 2147483647.0/(zMax-zMin)
  
  # Initiate Random SciDB arrays along each spatial dimension
  N  <- factor*size-1
  X  = paste("create array X<x:double>[i=0:",N,",1000,0]")
  Y  = paste("create array Y<y:double>[i=0:",N,",1000,0]")
  Z  = paste("create array Z<z:double>[i=0:",N,",1000,0]")
  
  iquery(X)
  iquery(Y)
  iquery(Z)  
  
  # Build Random SciDB arrays for each spatial dimension
  RX = paste("store(build(X,random()/",Qx," +",xMin,"),X)")
  RY = paste("store(build(Y,random()/",Qy," +",yMin,"),Y)")
  RZ = paste("store(build(Z,random()/",Qz," +",zMin,"),Z)")
  
  
  iquery(RX)
  iquery(RY)
  iquery(RZ)
  
  # Glue them all together (SciDB's Fault)
  C1 = paste("cross_join(X as A,Y as B,A.i,B.i)")
  C2 = paste("cross_join(",C1,",Z as C,A.i,C.i)")
  
  SC = paste("store(",C2,",",name,")")
  
  ifdel(name)
  iquery(SC)
  
  return( scidb(name,data.frame=FALSE) )
}
