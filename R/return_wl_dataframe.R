#' Converts win-loss matrix of valued data to win-loss dataframe
#'
#' @param mat A win-loss matrix of valued data
#' @return A win-loss dataframe
#' @examples
#' return_wl_dataframe(bonobos)
#' @section Further details:
#' The inputted matrix must be valued data with no ties.
#' The diagonal may be zeros or NA. The returned win-loss
#' dataframe will have three variables: winner, loser, result
#'  (which will all contain "W").
#' @export



return_wl_dataframe   <- function(mat){

    df <-reshape2::melt(as.matrix(mat))
    df <- df[df[,3]!=0,]
    df <- df[complete.cases(df),]
    df.expanded <- df[rep(row.names(df), df$value), 1:2]
    dfx <- data.frame(winner = df.expanded[,1], loser = df.expanded[,2], result = "W")
    return(dfx)
  }


