#' Converts results or win-loss dataframe to win-loss matrix
#'
#' @param df A competition results or win-loss dataframe
#' @param ties How to handle ties, default is \code{ties="remove"}
#' Alternative is \code{ties="keep"}
#' @return A win-loss matrix
#' @examples
#' get_wl_matrix(randomtourney(8))
#' @section Further details:
#' Add more detailed description.
#' See \code{\link{get_wl_dataframe}}: for further info.
#' @export



get_wl_matrix <- function(df, ties="remove"){

if (ties=="remove"){
  df <- get_wl_dataframe(df,ties="remove")
  df <- as.matrix(df[,1:2])
  g<-igraph::graph.edgelist(df)
  m1 <- igraph::get.adjacency(g, sparse=FALSE)
  m1 <- m1[order(rownames(m1)), order(colnames(m1))]
  return(m1)
}

if (ties=="keep"){
  df <- get_wl_dataframe(df,ties="keep")
  dfT <-df[(df[,3]=="T"),]
  dfWL <-df[(df[,3]!="T"),]

  dfWL <- as.matrix(dfWL[,1:2])
  g<-igraph::graph.edgelist(dfWL)
  m1 <- igraph::get.adjacency(g, sparse=FALSE)
  m1 <- m1[order(rownames(m1)), order(colnames(m1))]


  #giving 0.5 for a win for ties
  dfTx <- dfT %>%
    dplyr::group_by(winner,loser) %>%
    dplyr::summarize(n = n(), val=0.5*n ) %>%
    dplyr::select(-n) %>%
    dplyr::ungroup()

  dfTx <- as.data.frame(dfTx)


  #quick way of adding to m1 from dfTx
 dfTx[ , c("winner", "loser")] <- lapply(dfTx[ , c("winner", "loser")], function(x) factor(x, levels = rownames(m1)))
 m2 <- xtabs(val ~ ., data = dfTx)
 mm <- m1 + m2 + t(m2)

 return(mm)

}


}
