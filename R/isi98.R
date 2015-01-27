#' Calculate the rank order of a matrix according to the original I&SI method
#'
#' @param Mk A matrix with individuals ordered identically in rows and columns.
#' @param nTries The number of iterations
#' @return The best ranked matrix \code{Mk}, the rank order of dominant individuals,
#' Imin - the minimum number of inconsistencies found in the best matrix,
#' SImin - the strength of inconsistencies found in the best matrix
#' @examples
#' isi98(mouse)
#' @section References:
#' Han de Vries (1998)
#' @section Further details:
#' Forthcoming.
#' @export


isi98<-function(Mk,nTries){
  n<-ncol(Mk)
  index=nature(Mk)
  M=matrix_change(Mk,index)
  initial=M
  index_initial=index
  l=fun(M)
  Imin=l[1];SImin=l[2];t=0
  matrix1 = M-t(M)
  stopIteration1=FALSE;stopIteration2=FALSE
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
    l<-fun(matrix1)
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
      if ('best' %in% ls()){
        best = list()
      }
      best[[length(best)+1]]=index
    }
    else{
      t=t+1
    }
    if ((SImin>0)&(t<nTries)){
      kk=matrix1
      kk[upper.tri(kk)]=0
      if (length(which(kk>0))>0){
        rand=sample((j-1),1)
        matrix1=swap(matrix1,rand,j)
        change=index[rand];index[rand]=index[j];index[j]=change
        stopIteration1=FALSE
      }
    }
    else{
      stopIteration1=TRUE
    }
  }
  best=unique(best)
  correlation=rep(0,length(best))
  for (k in 1:length(best)){
    correlation[k]=cor.test(c(1:n),best[[k]])$p.value
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

####
#THE FOLLOWING ARE ALL REQUIRED FUNCTION FOR 'fun_1998',please load them first
###

##
#This function is used to calculate the I and SI given the dominance matrix
##
fun<-function(M){
  n=ncol(M)
  result=rep(0,2)
  k = M
  k[upper.tri(k)]=0
  result[1] = sum(k>0)
  a=length(which(k>0))
  y=which(k>0)%%n
  x=(which(k>0)-1)%/%n+1
  y[y==0]=y[y==0]+n
  if (a>0){
    result[1]=a
    result[2]=sum(y-x)
  }
  return(result)
}

fun2<-function(M){
  n=ncol(M)
  result=rep(0,2)
  k=M-t(M)
  k[lower.tri(k)]=0
  temp = matrix(rep(c(1:n),n), nrow = n, ncol = n, byrow = T) - rep(n)
  result = sum((k<0)*temp)
  return(result)
}





###
#This function is to modify the dominance matrix when ith individual and
#jth individual exchange their positions.
###
swap<-function(M,i,j){
  result=M
  k=result[i,];result[i,]=result[j,];result[j,]=k
  k=result[,i];result[,i]=result[,j];result[,j]=k
  return(result)
}

###
#'nature' is used to offer the initial sequence depends on the porpotion of dominance
###
nature<-function(M){
  n=ncol(M)
  index=c(1:n)
  D<-rep(0,n)
  S<-rep(0,n)
  for (i in 1:n){
    fast<-M-t(M)
    D[i]=D[i]+length(which(sign(fast[i,])==1))
    S[i]=S[i]+length(which(sign(fast[i,])==-1))
  }
  result=D/(D+S)
  save=sort(result,decreasing=TRUE,index.return=TRUE)
  sequence=save$ix
  result=save$x
  Dom_sub=D-S
  for (i in 1:(n-1)){
    if (result[i]==result[i+1]){
      if (Dom_sub[i]<Dom_sub[i+1]){
        mid<-sequence[i];sequence[i]=sequence[i+1];sequence[i+1]=mid
      }
    }
  }
  return(sequence)
}

####
#'matrix_change' is used to modify the dominance matrix according to the give sequence
###
matrix_change<-function(M,sequence){
  n=ncol(M)
  new_matrix<-NULL
  for(i in 1:n){
    temp<-M[sequence[i],]
    temp<-temp[sequence]
    new_matrix<-rbind(new_matrix,temp)
  }
  return(new_matrix)
}






