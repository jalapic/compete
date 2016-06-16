#' Compute best ranked matrixed based on original I&SI method
#'
#' @param M A win-loss matrix
#' @param nTries Number of tries to find best order
#' @return A computed ranked matrix best_matrix best_ranking I and SI
#' @examples
#' isi98(mouse,nTries=100)
#' @section Further details:
#'
#' Code based on algorithm described by de Vries, H. 1998. Finding a
#' dominance order most consistent with a linear hierarchy:
#' a new procedure and review. Animal Behaviour, 55, 827-843.
#' The number of iterations should be very high and/or the function
#' should be run several times to detect the optimal matrix or matrices.
#'  It may take several runs to find the matrix with the lowest SI,
#'  especially for very large matrices.
#' See \code{\link{isi13}}: for further info.
#' @export

isi98<-function(M,nTries=1000){
  n<-ncol(M)
  l=funisi(M)
  Imin=l[1];SImin=l[2]
  best=c(1:n)
  nTries=nTries%/%n
  for (jjj in 1:n){
    stopIteration1=FALSE;stopIteration2=FALSE
    index=sample(n,n);t=0
    matrix0=matrix_change(M,index); matrix1 = (matrix0-t(matrix0))/2
    while (stopIteration1==FALSE){
      while (stopIteration2==FALSE){
        stopIteration2<-TRUE

        for (i in 1:(n-1)){
          for (j in (i+1):n){
            if (matrix1[i,j]<0){
              sum1=sum(sign(matrix1[j,i:(j-1)]))
              if (sum1>0){
                matrix1=swap(matrix1,i,j)
                change=index[i];index[i]=index[j];index[j]=change
                stopIteration2=FALSE
              }
            }
          }
        }
      }
      l<-funisi(matrix1)
      I=l[1];SI=l[2]
      if ((I<Imin)|((I==Imin)&(SI<SImin))){
        best=list()
        best[[1]]=index
        Imin=I
        SImin=SI
        stopIteration1=FALSE
      }
      else if((I==Imin)&(SI==SImin)){
        t=t+1
        best[[length(best)+1]]=index
      }
      else{
        t=t+1
      }
      if ((SImin>0)&(t<nTries)){
        for(j in 2:n){
          kk=matrix1[j,1:(j-1)]
          if (max(kk>0)){
            rand=sample((j-1),1)
            matrix1=swap(matrix1,rand,j)
            change=index[rand];index[rand]=index[j];index[j]=change
            stopIteration1=FALSE
          }
        }
      }
      else{
        stopIteration1=TRUE
      }
    }
  }
  best=unique(best)
  correlation=rep(0,length(best))
  for (k in 1:length(best)){
    correlation[k]=cor.test(nature(M),best[[k]])$p.value
  }
  num<-which(correlation==min(correlation))
  result_1=list()
  result_2=list()
  for (i in 1:length(num)){
    result_1[[i]]=matrix_change(M,best[[num[i]]])
    result_2[[i]]=best[[num[i]]]
  }

  result_1x=as.data.frame.matrix(result_1[[1]])

  answer=list("best_matrix"=result_1x,"best_order"=colnames(result_1x),"I"=Imin,"SI"=SImin)
  return(answer)
}
