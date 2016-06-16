#' Organize rows and columns of a matrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @param method The method to be used to reorganize the matrix.
#' \code{method="alpha"} is the default and will organize rows/columns based
#' on alphanumeric order of rownames/colnames. \code{method="wins"} will
#' return a matrix orderd in descending order of summed rows (i.e. total
#' competitive interactions won). If rows have tied number of total wins,
#' they will be returned in the order of the inputted matrix.
#' @return The same matrix \code{m} with reordered rows/columns
#' @examples
#' org_matrix(bonobos)
#' org_matrix(mouse, method="wins")
#' @export


#Organizing a matrix by highest to lowest wins
org_matrix <- function(m, method="alpha"){

if (method=="alpha"){
  m <- as.matrix(m)
  xr<- rownames(m)
  m <- m[order(xr),order(xr)]
  return(m)
}

if (method=="wins"){
  m <- as.matrix(m)
  xr<-rowSums(m, na.rm=T)
  m <- m[order(xr, decreasing=T),order(xr, decreasing=T)]
  return(m)
}

}
