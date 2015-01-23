#' Calculate the number of unknown relationships in a sociomatrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The number of unknown relationships in \code{m}
#' @examples
#' unknowns(m)
#' @section References:
#' .
#' @section Further details:
#' An unknown relationship is defined as one whereby M(i,j)==M(j,i)==0 .
#' The zeros in each cell of dyads that have this property may be
#' referred to as structural zeros.
#' @export


unknowns<-function(m){
  m<-as.matrix(m)
  diag(m)<-NA #diagonal of matrix has to be NA for this function to work
  a<-as.data.frame(reshape2::melt(m)) #melt matrix
  a$id <- apply(a[1:2], 1, function(x) paste(sort(x), collapse=",")) #id relationships/pairs
  a<-a[complete.cases(a),]  #remove NAs - i.e. rows that are 1,1 2,2 3,3 etc.
  a$id<-as.factor(a$id) #make sure id is a factor
  a<-a[with(a, order(id)), ] #order df by id
  a$pairid<-c("a", "b")  #identify each row for each pair as 'a' and 'b'
  a<-a[c(3:5)] #remove first two variables, var1 and var2
  a<-reshape2::dcast(a, id ~ pairid) #make new df by id
  a$sum<-a[,2]+a[,3] #make sum variable
  a<-a[c(4)] #just take sum variable
  a<-as.numeric(colSums(a==0)) #count zeros in sum column
  unk<-as.numeric(a) #calculate directional consistency
  return(unk)
}
