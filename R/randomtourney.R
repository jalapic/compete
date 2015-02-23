#' Generates a randomized tournament with random outcomes
#'
#' @param n Number of individuals in tournament
#' @param matchups Number of times individuals compete in tournament. Can
#' be a numeric input, or, if \strong{\code{matchups}="random"} interactions
#' are random
#' @param pties Probability of each individual matchup ending in a tie.
#' Default is 0, i.e. no ties.  Needs to be a number between 0 and 1.
#' @param type Whether to return results as W/L characters or 1/2 numbers.
#' \strong{\code{type}="char"} is the default, \strong{\code{type}="nums"}
#'  returns 1/2 numbers referring to winner as id1 or id2
#' @return A competition results dataframe
#' @examples
#' randomtourney(20,2) #20 individuals interact twice with each other
#' randomtourney(5,6) #5 individuals interact six times with each other
#' randomtourney(8) #8 individuals interact twice with each other
#' @section Further details:
#' Specify number of individuals to compete in a tournament and
#' the number of times they compete with each other.  Winners
#' and losers are determined at random. The resulting dataframe
#' will have variables: \code{id1}, \code{id2}, \code{result}.
#' Result refers to the outcome from \code{id1}'s perspective, i.e. a "W"
#' refers to \code{id1} beating \code{id2}, and a "L" refers to
#' \code{id2} beating \code{id1}. Individuals are referred to by a
#' random assignment of two conjoined letters.
#' @export


randomtourney <- function(n,matchups=2, pties=0, type="char"){


  if (n>325) { stop("randomtourney allows a maximum of 325 individuals")}

  indivs <- apply(combn(LETTERS[1:26],2), 2, function(x) paste(x[1],x[2],sep=""))
  N <- sample(indivs, n)

  m<-combn(N,2)
  is.even <- function(x) x %% 2 == 0

  if (is.even(matchups)==TRUE & is.numeric(matchups)==TRUE) {

    m1 <- matrix(rep(m, matchups/2) , nrow=nrow(m))
    m2 <- rbind(m1[2,],m1[1,])
    mk <- cbind(m1,m2)
    df <- data.frame(t(mk))
    p3<-pties
    p1<-p2<-(1-pties)/2
    df[,3]<-sample(c("W","L", "T"), prob=c(p1,p2,p3), nrow(df), replace=T)
    colnames(df)<-c("id1", "id2", "result")

    if (type=="nums") {
      df[,3] <- gsub("W", 1, df[,3])
      df[,3] <- gsub("L", 2, df[,3])
      df[,3] <- gsub("T", 0, df[,3])
      df[,3] <- as.numeric(df[,3])
    }

    return(df)



  }


  else

    if (is.even(matchups)==FALSE & is.numeric(matchups)==TRUE) {

      matchups1 <- matchups-1
      m1 <- matrix(rep(m, matchups1/2) , nrow=nrow(m))
      m2 <- rbind(m1[2,],m1[1,])
      mk <- cbind(m1,m2,m)
      df <- data.frame(t(mk))
      p3<-pties
      p1<-p2<-(1-pties)/2
      df[,3]<-sample(c("W","L", "T"), prob=c(p1,p2,p3), nrow(df), replace=T)
      colnames(df)<-c("id1", "id2", "result")

      if (type=="nums") {
        df[,3] <- gsub("W", 1, df[,3])
        df[,3] <- gsub("L", 2, df[,3])
        df[,3] <- gsub("T", 0, df[,3])
        df[,3] <- as.numeric(df[,3])
      }


      return(df)
    }


  else

    if (matchups=="random" & is.numeric(matchups)==FALSE) {

      df <- data.frame(matrix(replicate(n, sample(N,2)), ncol=2))
      p3<-pties
      p1<-p2<-(1-pties)/2
      df[,3]<-sample(c("W","L", "T"), prob=c(p1,p2,p3), nrow(df), replace=T)
      colnames(df)<-c("id1", "id2", "result")


      if (type=="nums") {
        df[,3] <- gsub("W", 1, df[,3])
        df[,3] <- gsub("L", 2, df[,3])
        df[,3] <- gsub("T", 0, df[,3])
        df[,3] <- as.numeric(df[,3])
      }

      return(df)



    }

}
