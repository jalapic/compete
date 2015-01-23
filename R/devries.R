#' Calculate the linearity of a dominance hierarchy - De Vries' method
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @return The modified Landau's h' value of \code{m}, associated p-value and
#' plot of randomization test distribution.
#' @examples
#' devries(bonobos)
#' @section References:
#' Han de Vries (1995) An improved test of linearity in dominance hierarchies
#' containing unknown or tied relationships. Animal Behaviour 50 pp. 1375-1389.
#' @section Further details:
#' This code is an edited and improved version of code originally written by Dai Shizuka.
#' @section Acknowledgements:
#' Jochen Weber
#' @export

devries <- function(m) {

  # ensure matrix form
  m <- as.matrix(m)

  # replace NA values in diagonal with 0

  # compute total "encounters" (as sum of matrix with transpose)
  total <- m + t(m)

  # requires igraph
  g <- igraph::graph.adjacency(m, mode="directed", weighted=TRUE, diag=FALSE)

  # get number of rows (and columns)
  N <- nrow(m)
  NN <- N * N

  # store adjacency degree in d
  d <- igraph::degree(g, mode="out")

  # compute point-estimate of h
  hF1 <- (12 / ((N ^ 3) - N))
  hF2 <- (N - 1) / 2
  h <- hF1 * sum((d - hF2) ^ 2)

  # prepare arrays
  h0 <- vector(length=10000)
  hr <- vector(length=10000)

  # pre-compute matrices with fixed and random elements
  fixedels <- ((total > 0) + 0.0)
  randomels <- 1 - fixedels
  diag(randomels) <- 0

  # for fixed elements, the result is
  fixedvals <- fixedels * (0.5 * (((m > t(m)) + 0.0) - ((t(m) > m) + 0.0) + 1))

  # iterate over cases
  for (k in 1:10000){

    # compute new matrix
    randmat <- matrix(runif(NN), ncol=N)
    newmat <- fixedvals + randomels * ((randmat > t(randmat)) + 0.0)

    # compute sum over rows
    V <- rowSums(newmat)

    # apply formula
    h0[k] <- hF1 * sum((V - hF2) ^ 2)

    # compare with fully random matrix
    randmat <- matrix(runif(NN), ncol=N)
    nm <- (randmat > t(randmat)) + 0.0
    #diag(nm) = 0
    Vr <- rowSums(nm)
    hr[k] <- hF1 * sum((Vr - hF2) ^ 2)
  }

  # final computation
  t <- sum((hr >= h0) + 0.0)
  hmod <- mean(h0)
  p <- t / 10000

  cat(" Landau's h= ",h,"\n","modified Landau's h= ",hmod,"\n","p-value from simulations= ",p)
  hist(hr,xlim=c(0,1),xlab="Landau h values from simulation")
  abline(v=hmod,lty=3,lwd=1.5)

}
