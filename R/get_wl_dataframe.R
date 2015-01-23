#' Converts results dataframe to win-loss dataframe
#'
#' @param df A results dataframe
#' @param ties How to handle ties, default is \code{ties="remove"}
#' Alternative is \code{ties="keep"}
#' @return A win-loss dataframe
#' @examples
#' get_wl_dataframe(randomtourney(8))
#' @section Further details:
#' Add more detailed description.
#' Ensure that first 3 columns are id1, id2, result
#' don't have to be named that but must refer to those
#' Will keep all remaining columns intact
#' @export



get_wl_dataframe <- function(df, ties="remove"){

  library(dplyr)
  #ensure that first 3 columns are id1, id2, result
  #though don't have to be named that

  #make sure results df is in number format

  colnames(df)[1] <- "id1"
  colnames(df)[2] <- "id2"
  colnames(df)[3] <- "result"

  df[,3] <- gsub("W", 1, df[,3])
  df[,3] <- gsub("L", 2, df[,3])

  if (ties=="remove") {df[,3] <- gsub("T", NA, df[,3])}
  if (ties=="keep") {df[,3] <- gsub("T", 0, df[,3])}

  df[,3] <- as.numeric(df[,3])

  #turn into winner-loser dataframe
  df <- df[!is.na(df[,3]), ]  #get rid of NAs ties if exist

  a1 <- df %>%
    dplyr::filter(result==1) %>%
    dplyr::rename(winner=id1, loser=id2) %>%
    dplyr::mutate(result="W")

  a2 <- df %>%
    dplyr::filter(result==2) %>%
    dplyr::rename(winner=id2, loser=id1) %>%
    dplyr::mutate(result="W")

  a3 <- df %>%
    dplyr::filter(result==0) %>%
    dplyr::rename(winner=id1, loser=id2) %>%
    dplyr::mutate(result="T")

  df <-  rbind(a1,a2,a3)
  return(df)
}
