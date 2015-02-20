#' ifdel
#' Deletes a given SciDB array if it exists within the database
#' given an input Array-name from SciDB
#' @param name: Name of SciDB Array
#' @examples
#'   name<-"data"
#'   ifdel(name)
#'   OUTPUT: "Array Deleted"


# Delete an existing SciDB array if it Exists
ifdel     <- function(name)
{
  #If 'name' exists delete array
  NAMES <-scidblist()
  if (name %in% NAMES){scidbremove(name,force=TRUE)}
  return("Array Deleted")
}