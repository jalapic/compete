#' Transforms a frequency interaction sociomatrix (valued data) into a dichotomized 1/0 matrix
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @param type Determines the type of dichotomized matrix to be returned.
#'  \strong{\code{type}="wl"} is the default which returns a win-loss matrix
#'  with a '1' representing a consistent winner and a '0' representing a
#'  consistent loser for each dyad of the matrix. A consistent winner is
#'  defined as being the individual in each dyad that has absolutely more
#'  wins than defeats.   In the default condition if competitors have the
#'  same number of wins each, they both receive a 0.
#'  If \strong{\code{type}="wlties"} the default dichotomized win-loss
#'  matrix will be returned but it will also return 0.5 into cells for tied
#'  relationships.
#'  If \strong{\code{type}="wlties0"} the default dichotomized win-loss
#'  matrix will be returned but it will also return 0.5 into cells for tied
#'  relationships. Additionally, if two competitors never interacted both
#'  cells for that relationship will be returned with a 0.
#'  If \strong{\code{type}="wlbinom"} every relationship within the win-loss
#'  matrix is assessed for whether one competitor significantly wins more
#'  competitive interactions than the other competitor.  Significance is
#'  calculated using a binomial test with probability of p=0.05. A '1' is
#'  given to significant winners within a relationship and a '0' is given
#'  to significant losers or if neither individual is a winner.
#'  If \strong{\code{type}="wlbinomties"} The same procedure is done as for
#'  \strong{\code{type}="wlbinom"}, but if no signficiant winner/loser can
#'  be determined then a 0.5 is returned rather than a 0.
#'  If \strong{\code{type}="pa"} the inputted matrix will be turned into a
#'  dichotomized presence-absence matrix, with a '1' indicating that the
#'  competitor in a the row of the matrix beat the competitor in the column
#'  at least once. A '0' indicates that that competitor never beat the
#'  other competitor.
#'  If \strong{\code{type}="dom"} the inputted matrix will be turned into a
#'  dominance score matrix, with a '1' indicating that the
#'  competitor in a the row of the matrix dominates the competitor in the
#'  column. A '-1' indicates that that competitor in a row is subordinate
#'  to the competitor in the column. A '0.5' indicates a tie.  A '0'
#'  indicates an observational or structural zero.
#' @return A dichotomized win/loss or presence/absence matrix.
#' @examples
#' get_di_matrix(bonobos)
#' get_di_matrix(mouse)
#' @section References:
#' Appleby, M. C. 1983. The probability of linearity in hierarchies.
#' Animal Behaviour, 31, 600-608.
#' @export

get_di_matrix <- function(m, type="wl"){

  mtxbinom<-mx<-m1<-m2<-m3<-NULL

if(type=="wl") {
m <- as.matrix(m)
m <- (m > t(m)) + 0
return(m)
}

if(type=="wlties") {
m <- as.matrix(m)
m <-((m > t(m)) + 0)  + ((m == t(m)) + 0)/2
return(m)
}

if(type=="wlties0") {
  m <- as.matrix(m)
  mx <-((m > t(m)) + 0)  + ((m == t(m)) + 0)/2
  mx[(m==0 & t(m)==0)==T] <-0
return(mx)
}


if(type=="wlbinom") {
m <- as.matrix(m)
mx <- mtxbinom(m)
return(mx)
}


if(type=="wlbinomties") {
m <- as.matrix(m)
mx <- mtxbinom(m)
mx <- mx + ((m == t(m)) + 0)/2
return(mx)
}


if(type=="pa") {
m <- as.matrix((m > 0) + 0)
return(m)
}

if(type=="dom") {
  m <- as.matrix(m)

  m1 <- (m > t(m)) + 0     #put in +1s
  m2 <- -((m < t(m)) - 0)  #put in -1s

  m3 <- ((m == t(m)) + 0)/2   #put in 0.5s
  m3[(m==0 & t(m)==0)==T] <-0  #put in 0s to structural zeros

  mx <- m1 + m2 + m3

  return(mx)
}

}
