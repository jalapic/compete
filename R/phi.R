#' Get the phi skew-symmetry of a sociomatrix.
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The phi skew-symmetry index of \code{m}.
#' @examples
#' m <- matrix(c(NA,2,30,6,19,122,0,NA,18,
#' 0,19,85,0,1,NA,3,8,84,0,0,0,NA,267,50,0,
#' 0,0,5,NA,10,1,0,4,4,1,NA), ncol=6)  #table 2, Vervaecke et al. 2000  - fleeing in bonobos
#' phi(m)
#' @section References:
#' Leiva D et al, 2008, Testing reciprocity in social interactions: A comparison between the
#' directional consistency and skew-symmetry statistics, Behav Res Methods.
#' @section Further details:
#' Phi is the skew-symmetry index (0 means completely symmetric, 0.5 means completely not symmetric)
#' @export

phi=function(m){
  m=as.matrix(m)
  diag(m)=0
  K=(m-t(m))/2
  phi=sum(diag(t(K)%*%K))/sum(diag(t(m)%*%m))
  return(phi)
}
