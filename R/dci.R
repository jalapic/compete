#' Get the directional consistency index (DCI) of a sociomatrix.
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The directional consistency of \code{m}.
#' @examples
#' #table 2, Vervaecke et al. 2000  - fleeing in bonobos
#' m <- matrix(c(NA,2,30,6,19,122,0,NA,18,
#' 0,19,85,0,1,NA,3,8,84,0,0,0,NA,267,50,0,
#' 0,0,5,NA,10,1,0,4,4,1,NA), ncol=6)
#' dci(m)  #0.96
#' @section References:
#' Van Hooff JARAM, Wensing JAB. 1987.
#' Dominance and its behavioural measures in a captive wolf pack.
#' In: Frank HW, editor. Man and Wolf.
#' Dordrecht, Olanda (Netherlands): Junk Publishers
#' pp.219-252.
#' @section Further details:
#' The DCI represents the proportion of occurrences of a behavior that occurs across all dyads
#' in a group from the individual within each dyad performing the behavior with a higher frequency (H)
#' to the individual within each dyad performing the behavior with a lower frequency (L).
#' It is calculated by averaging the following formula across all dyads: DCI = (H - L)/(H + L).
#' The DCI ranges from 0 (no directional asymmetry) to 1 (completely unidirectional).
#' @export




dci<-function(m){
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
  a$absdif<-abs(a[,2]-a[,3]) #gives absolute positive difference between two ids of each pair
  a<-a[c(4:5)] #just take sum variable and absdif variable
  a<-colSums (a, na.rm = FALSE, dims = 1)
  dc<-as.numeric(a[2]/a[1]) #calculate directional consistency
  return(dc)
}
