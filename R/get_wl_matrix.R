#' Converts win-loss dataframe to win-loss matrix
#'
#' @param df A results or win-loss dataframe
#' @param ties How to handle ties, default is \code{ties="remove"}
#' Alternative is \code{ties="keep"}
#' @return A win-loss matrix
#' @examples
#' get_wl_matrix(randomtourney(8))
#' get_wl_matrix(randomtourney(15,pties=.15))
#' get_wl_matrix(randomtourney(15,pties=.15),ties="keep")
#' get_wl_matrix(el)
#' @section Further details:
#' Input dataframes or matrices with only 2 columns are
#' considered to be winners in column 1 and losers in column 2.
#' If input dataframe has three columns, the third column
#' will be the result of the interaction between column 1
#' subject and column 2 subject. The result can be in
#' the "W/L/T" format or "1/0/0.5" format.
#' See \code{\link{get_wl_df}}: for further info.
#' @importFrom stats "xtabs"
#' @export

get_wl_matrix <- function(df, ties="remove"){
  mylevs = unique(c(as.character(df[,1]),as.character(df[,2])))

  if (ncol(df)==2){
    df <- as.data.frame(df)
    df[,1] <- factor(df[,1], levels=mylevs)
    df[,2] <- factor(df[,2], levels=mylevs)
    df$result<-1
    m1 = stats::xtabs(result ~ ., data = df)
    m1 <- m1[order(rownames(m1)), order(colnames(m1))]
    return(m1)

  }
  else
    if (ncol(df)>2 & ties=="remove"){
      df <- get_wl_df(df,ties="remove")
      df[,1] <- factor(df[,1], levels=mylevs)
      df[,2] <- factor(df[,2], levels=mylevs)
      m1 = stats::xtabs(result ~ ., data = df)
      m1 <- m1[order(rownames(m1)), order(colnames(m1))]
      return(m1)
    }

  else
    if (ncol(df)>2 & ties=="keep"){

      df <- get_wl_df(df,ties="keep")
      dfT <-df[(df[,3]==.5),]
      dfWL <-df[(df[,3]==1),]
      dfWL[,1] <- factor(dfWL[,1], levels=mylevs)
      dfWL[,2] <- factor(dfWL[,2], levels=mylevs)
      m1 = stats::xtabs(result ~ ., data = dfWL)
      m1 <- m1[order(rownames(m1)), order(colnames(m1))]
      #giving 0.5 for a win for ties
      dfT[ , c("winner", "loser")] <- lapply(dfT[ , c("winner", "loser")], function(x) factor(x, levels = mylevs))
      m2=stats::xtabs(result ~ ., data = dfT)
      m2 <- m2[order(rownames(m2)), order(colnames(m2))]
      mm <- m1 + m2 + t(m2)

      return(mm)

    }
}
