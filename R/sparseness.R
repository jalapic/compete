#' Calculate the sparseness of relationships in a sociomatrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The sparseness of \code{m}
#' @examples
#' sparseness(mouse)
#' sparseness(caribou)
#' @section Further details:
#' The sparseness of a matrix is the proportion of null dyads
#' @export


sparseness<-function(m){
  m<-as.matrix(m)
  r<-rshps(m)
  sparse=r[[2]]/r[[1]]
  return(sparse)
}

