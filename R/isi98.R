#' Calculate the rank order of a dominance matrix using De Vries' 1998 I&SI method
#'
#' @param M A matrix with individuals ordered identically in rows and columns.
#' @param nTries Number of permutation tries.
#' @return A list containing the most optimal matrix or matrices found, the
#'  rank order of individuals, the numbrer of inconsistencies \code{I} and the
#'  strength of inconsistencies \code{SI}
#' @examples
#' isi98(bonobos)
#' isi98(people)
#' @section References:
#' Han de Vries (1998) de Vries H. 1998. Finding a dominance order most consistent
#'  with a linear hierarchy: a new procedure and review. Animal Behaviour 55:827-843
#' @section Further details:
#' none
#' @export


isi98<-function(M,nTries){
  diag(M)<-0
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
  answer=list(best_matrix=result_1,best_order=result_2,I=Imin,SI=SImin)
  return(answer)
}
