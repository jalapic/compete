#' Calculate the number of unknown relationships in a sociomatrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The number of unknown relationships in \code{m}
#' @examples
#' unknowns(mouse)
#' @section Further details:
#' An unknown relationship is defined as one whereby M(i,j)==M(j,i)==0 .
#' The zeros in each cell of dyads that have this property may be
#' referred to as structural zeros.
#' @export


unknowns<-function(m){
  m<-as.matrix(m)
  unk<-(length(m[m==0 & t(m)==0])-length(diag(m)))/2
  return(unk)
}


