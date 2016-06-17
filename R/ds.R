#' Get David's Scores of Individuals
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @param norm whether to normalize scores
#' @param type either \code{method="D"} for Dij or \code{method="P"} for Pij.
#' @return a vector of David scores in same order as names of \code{m}.
#' @examples
#' m <- matrix(c(NA,2,30,6,19,122,0,NA,18,
#' 0,19,85,0,1,NA,3,8,84,0,0,0,NA,267,50,0,
#' 0,0,5,NA,10,1,0,4,4,1,NA), ncol=6)  #table 2, Vervaecke et al. 2000  - fleeing in bonobos
#' ds(m)
#' ds(m,type="P")
#' ds(m,norm=TRUE)
#' @section References:
#' Gammell et al, 2003, David's score: a more appropriate dominance ranking method
#' than Clutton-Brock et al.'s index, Animal Behaviour.
#' @export



ds = function(m, norm=FALSE, type="D"){
  M = as.matrix(m)
  diag(M)=0
  M1 = M + t(M)
  if(type=="D") { M2 = M/M1 - (((M/M1) - 0.5)/(M1 + 1)) }
  if(type=="P") { M2 = M/M1 }
  M2[is.na(M2)]=0
  ds = as.vector(rowSums(M2) + ( M2 %*% rowSums(M2)) - colSums(M2) - t(t(colSums(M2)) %*% M2))
  if(norm==TRUE){ ds = (ds + compete::rshps(m)[[1]])/nrow(M)}
  names(ds)=colnames(m)
  return(ds)
}
