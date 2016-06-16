#' Calculate the linearity of a dominance hierarchy - De Vries' method
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @param Nperms Number of randomizations
#' @param history Whether to store results of randomization
#' @param plot Whether to plot results of randomization
#' @return The modified Landau's h' value of \code{m}, the associated p-value
#' @examples
#' devries(bonobos)
#' devries(mouse,plot=TRUE)
#' devries(people,history=TRUE)
#' @section References:
#' Han de Vries (1995) An improved test of linearity in dominance hierarchies
#' containing unknown or tied relationships. Animal Behaviour 50 pp. 1375-1389.
#' @section Further details:
#' This code is an edited and faster version of code originally written by Dai Shizuka.
#' http://biosci.unl.edu/daizaburo-shizuka
#' Note that plot will only be shown if \strong{\code{history}=F}
#' @importFrom igraph "graph.adjacency"
#' @importFrom igraph "degree"
#' @export

devries <- function(m, Nperms=10000, history=FALSE, plot=FALSE) {
  diag(m)<-0
  m <- as.matrix(m)
  total <- m + t(m)
  g <- igraph::graph.adjacency(m, mode="directed", weighted=TRUE, diag=FALSE)
  N <- nrow(m)
  NN <- N * N
  d <- igraph::degree(g, mode="out")
  hF1 <- (12 / ((N ^ 3) - N))
  hF2 <- (N - 1) / 2
  h <- hF1 * sum((d - hF2) ^ 2)
  h0 <- vector(length=Nperms)
  hr <- vector(length=Nperms)
  fixedels <- ((total > 0) + 0.0)
  randomels <- 1 - fixedels
  diag(randomels) <- 0
  fixedvals <- fixedels * (0.5 * (((m > t(m)) + 0.0) - ((t(m) > m) + 0.0) + 1))

  for (k in 1:Nperms){
    randmat <- matrix(runif(NN), ncol=N)
    newmat <- fixedvals + randomels * ((randmat > t(randmat)) + 0.0)
    V <- rowSums(newmat)
    h0[k] <- hF1 * sum((V - hF2) ^ 2)
    randmat <- matrix(runif(NN), ncol=N)
    nm <- (randmat > t(randmat)) + 0.0
    Vr <- rowSums(nm)
    hr[k] <- hF1 * sum((Vr - hF2) ^ 2)
  }
  t <- sum((hr >= h0) + 0.0)
  hmod <- mean(h0)
  p <- t / Nperms

  if(history==F & plot==F) {return(list('h-modified' = hmod, 'p-value'=p))}

  if(history==T & plot==F) {return(list('h-modified' = hmod, 'p-value'=p, 'h-rand'=hr))}

  if(history==F & plot==T) {
    cat('h-modified =', hmod,"\n", 'p-value=', p)
    hist(hr,xlim=c(0,1),xlab="Landau h values from simulation")
    abline(v=hmod,lty=3,lwd=1.5,col="red")
  }

  if(history==T & plot==T) {
    warning("Plot only shown when history=F")
    cat('h-modified =', hmod,"\n", 'p-value=', p)
    hist(hr,xlim=c(0,1),xlab="Landau h values from simulation")
    abline(v=hmod,lty=3,lwd=1.5,col="red")
  }
}


