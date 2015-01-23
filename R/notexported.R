#' Extra, not-exported, functions
#'
#' Function required for transforming to 1/0 win/loss matrix based on binomial distribution
#'
#' @param m A matrix with individuals ordered identically in rows and columns.


# define the function
mtxbinom <- function(mtx, p=0.5, alpha=0.05) {
  # preallocate the matrix in memory
  m2 <- mtx
  for (rr in 2:nrow(mtx)) {
    for (cc in 1:(rr-1)) {
      # these two `for` loops work on the non-diag lower triangle
      x <- mtx[rr,cc]
      y <- mtx[cc,rr]

{if (x+y!=0) {sig <- (binom.test(x, x+y, p)$p.value <= alpha)} else {
  sig<-0}}

# lower-triangle entry
m2[rr,cc] <- 1*((x>y) & sig)
# opposing element in the upper-triangle
m2[cc,rr] <- 1*((y>x) & sig)
    }
  }
m2
}
