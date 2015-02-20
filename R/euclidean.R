#' euclidean
#' Creates a covariance Matrix utilizing the Euclidean distance metric
#' Given two input SciDB Arrays
#' *Preferably put the Larger Array first as per Paradigm4 suggestion
#' @param sdbARRAY1: SciDB Array
#'   sdbARRAY2: SciDB Array
#' @examples
#'   A    <- scidb("ARRAY1",data.frame=FALSE) 
#'   B    <- scidb("ARRAY2",data.frame=FALSE)  
#'   C    <- euclidean(sdbARRAY1=A, sdbARRAY2=B, name="Euclidean")

# Creates a Covariance Distance matrix using two SciDB arrays with a specified name
euclidean <- function(sdbARRAY1,sdbARRAY2,name)
{
  L  <- sdbARRAY1@name
  R  <- sdbARRAY2@name
  ifdel(name)
  
  #Create Queries
  Q1  = paste("project(cross_join(",L," as A,",R," as B),A.x,A.y,A.z,B.x,B.y,B.z)")
  Q2  = paste("store(project(apply(",Q1,", d_Mpc, dist(A.x,A.y,A.z,B.x,B.y,B.z)),d_Mpc),",name,")")
  
  #Evaluate Queries
  iquery("load_module('macros.txt')")
  iquery(Q2)
  return(scidb(name,data.frame=FALSE))
}