#' Compute best ranked matrixed besed on I&SI method 1998
#'
#' @param M A competition results or win-loss matrix
#' @param nTries Number of iterations
#' @return A computed ranked matrix best_matrix best_ranking I and SI
#' @examples
#' isi98(mouse)
#' @section Further details:
#' Add more detailed description.
#' See \code{\link{isi98}}: for further info.
#' @export

isi98<-function(M,nTries){
    n<-ncol(M)
    l=fun(M)
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



