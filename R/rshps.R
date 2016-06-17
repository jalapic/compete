#' Get Relaiontship Descriptives of Sociomatrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return a list of total, unknown, tied, two-way and one-way relationships
#' @examples
#' rshps(people)
#' @export


rshps=function(m){
  M = as.matrix(m)
  diag(M)=0
  totalr = (nrow(M)*(nrow(M)-1))/2
  unk = compete::unknowns(M)
  v=M[M==t(M)]
  ties=length(v[v>0])/2
  twoway= (length( M[M>0 & t(M)>0] )/2) - ties
  oneway = totalr-unk-ties-twoway
  return(list('total'=totalr, 'unknowns'=unk, 'ties'=ties, 'twoways'=twoway, 'oneways'=oneway))
}
