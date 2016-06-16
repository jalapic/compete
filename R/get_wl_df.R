#' Converts results dataframe to win-loss dataframe
#'
#' @param df A results dataframe
#' @param ties How to handle ties, default is \code{ties="remove"}
#' Alternative is \code{ties="keep"}
#' @return A win-loss dataframe
#' @examples
#' get_wl_df(randomtourney(8))
#' @section Further details:
#' A results dataframe first 3 variables are id1, id2, result.
#' Results can be "W", "L", or "T" or "1", "0", "0.5".
#' The output will be a win-loss dataframe that will
#' reorganize the first 3 variables into winner, loser
#' and result (=1 for Win or =0.5 for ties).
#' @export



get_wl_df <- function(df, ties="remove"){

  colnames(df)[1:3] <- c("id1", "id2", "result")

  df[,3] <- gsub("W", 1, df[,3])
  df[,3] <- gsub("L", 2, df[,3])

  if (ties=="remove") {df[,3] <- gsub("T", NA, df[,3])}
  if (ties=="keep") {df[,3] <- gsub("T", 0, df[,3])}

  df[,3] <- as.numeric(df[,3])

  #turn into winner-loser dataframe
  df <- df[!is.na(df[,3]), ]  #get rid of NAs ties if exist

  a1 <- df[df[,3]==1,1:2]
  a2 <- df[df[,3]==2,2:1]
  a1$result<-a2$result<-1
  colnames(a2)[1:3] <- colnames(a1)[1:3] <- c("winner", "loser", "result")
  dfx <-  rbind(a1,a2)

  a3 <- df[df[,3]==0,1:2]

  if(nrow(a3)>0){
  a3$result<-0.5
  colnames(a3)<-c("winner","loser","result")
  dfx <- rbind(dfx,a3)
    }

  return(dfx)
}
