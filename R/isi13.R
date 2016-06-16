#' Compute best ranked matrixed besed on I&SI method 2013
#'
#' @param Mk A competition results or win-loss matrix
#' @param p A vector of probabilities for each of 4 methods
#' @param a_max Number of tries
#' @param nTries Number of iterations
#' @param p2 probability for last method
#' @return A computed ranked matrix best_matrix best_ranking I and SI
#' @examples
#' isi98(mouse)
#' @section Further details:
#' Add more detailed description.
#' See \code{\link{isi98}}: for further info.
#' @export
#'
isi13<-function(Mk,p=c(1,0,0,0),a_max=50,nTries=30,p2){
    original1=Mk
    Mk=transform(Mk)
    attempt=1
    n<-ncol(Mk)
    index=nature(Mk)
    M=matrix_change(Mk,index)
    initial=M
    index_initial=index
    l=fun(M)
    best=list(index);
    Imin=l[1];SImin=l[2]
    matrix1 = (M-t(M))/2
    while (attempt<a_max){
        t=1
        if (attempt%%2==0){
        random_p = rmultinom(1,1,p)
        p1=which(random_p==1)}else{
        p1=0}
        if (runif(1)<p2) p1=p1+5
        stopIteration1=FALSE;stopIteration2=FALSE
        while (stopIteration1==FALSE){
             count_1=0;count_2=0
            while ((stopIteration2==FALSE)&(count_1<7)){
                stopIteration2<-TRUE
                ####strategy 1
                if (p1==0){
                    count_1=count_1+.5
                    for (i in 1:(n-1)){
                        for (j in (i+1):n){
                             if (matrix1[i,j]<0){
                             sum1=sum(matrix1[j,i:(j-1)])
                                if (sum1>0){
                                    matrix1=swap(matrix1,i,j)
                                    change=index[i];index[i]=index[j];index[j]=change
                                    stopIteration2=FALSE
                                }
                            sum1=0
                             }
                        }
                    }
                }

                #####strategy2
                else if (p1==1){
                       count_1=count_1+.5
                    for (i in 1:(n-1)){
                        for (j in (i+1):n){
                             sum1=sum(matrix1[j,i:(j-1)])
                             #  if (j-i==1) {sum1=matrix1[j,i]}
                             #  if (j-i>1)  {sum1=matrix1[j,i]-sum(matrix1[i,(i+1):(j-1)])+sum(matrix1[j,(i+1):(j-1)])}

                               if (sum1>0){
                                matrix1=swap(matrix1,i,j)
                                change=index[i];index[i]=index[j];index[j]=change
                                stopIteration2=FALSE
                            }
                       sum1=0
                        }
                    }
                }
                #####strategy4
                else if(p1==2){
              count_1=count_1+1
                    for (i in 1:n){
                        mini=0
                        for (j in 1:n){
                                aaa=sum(matrix1[i,j:i])*sign(j-i)
                                if (aaa<mini){
                            mini=aaa;rand=j        }
                                       }
                        if (mini<0){
                            matrix1<-shift(matrix1,i,rand)
                            index<-shift_index(index,i,rand)
                            stopIteration2=FALSE
                        }
                    }
                    if ((stopIteration2==TRUE)&(count_2<3)){
                    record=matrix(0,2,n);count_2=count_2+1
                    for (i in 1:n){
                        for (j in 1:n){
                            if((sum(matrix1[i,j:i])*sign(j-i)==0)&(i!=j)) {
                            record[2,j]=delta_si(matrix1,i,j)}
                            else{record[2,j]=100}
                        }
                        a2=record[2,]
                            if (min(a2)<0){
                                final=sample(which(a2==min(a2)),1)
                                matrix1<-shift(matrix1,i,final)
                                index<-shift_index(index,i,final)


}
                        }

                    }
                }





                #####strategy5
                else if (p1==3){
                     count_1=count_1+1
                    count=0;pair=NULL
                    for (i in 1:n){
                        for (j in 1:n){
                                if (sum(matrix1[i,i:j])*sign(j-i)<0) {
                                    pair=rbind(pair,c(i,j));count=count+1

                            }
                        }
                    }
                    if (count>0){
                        rand<-sample(count,1)
                        matrix1<-shift(matrix1,pair[rand,1],pair[rand,2])
                        index<-shift_index(index,pair[rand,1],pair[rand,2])
                        stopIteration2=FALSE
                    }
                                    }

                #####strategy3
                else if (p1==4){
                     count_1=count_1+2
                    for (i in 1:n){
                        for (j in 1:n){
                               if(sum(matrix1[i,i:j])*sign(j-i)<0){
                                    matrix1<-shift(matrix1,i,j)
                                    index<-shift_index(index,i,j)
                                    stopIteration2=FALSE
                                }
                                                   }
                    }
                if (stopIteration2==TRUE){
                                                for (i in 1:n){
                            for (j in 1:n){
                                     delta_i=sum(matrix1[i,i:j])*sign(j-i)
                                if((delta_i==0)&(i!=j)){
                                     if(delta_si(matrix1,i,j)<0){
                                    matrix1<-shift(matrix1,i,j)
                                    index<-shift_index(index,i,j)

                                                 }
							}
                                       }
                              }
                        }

                }

              ####strategy *6
              else  if (p1==5){
                    count_1=count_1+.5
                    for (i in 1:(n-1)){
                        for (j in (i+1):n){
                             if (matrix1[i,j]<0){
                             sum1=sum(matrix1[j,i:(j-1)])
                                if (sum1>=0){
                                    matrix1=swap(matrix1,i,j)
                                    change=index[i];index[i]=index[j];index[j]=change
                                    stopIteration2=FALSE
                                }
                            sum1=0
                             }
                        }
                    }
                }


                else if (p1==6){
                       count_1=count_1+.5
                    for (i in 1:(n-1)){
                        for (j in (i+1):n){

                               if (j-i==1) {sum1=matrix1[j,i]}
                               if (j-i>1)  {sum1=matrix1[j,i]-sum(matrix1[i,(i+1):(j-1)])+sum(matrix1[j,(i+1):(j-1)])}

                               if (sum1>=0){
                                matrix1=swap(matrix1,i,j)
                                change=index[i];index[i]=index[j];index[j]=change
                                stopIteration2=FALSE
                            }
                       sum1=0
                        }
                    }
                }

                else if(p1==7){
                    count_1=count_1+1
                    for (i in 1:n){
                        mini=0
                        for (j in 1:n){
                                aaa=sum(matrix1[i,j:i])*sign(j-i)
                                if (aaa<mini){
                            mini=aaa;rand=j        }
                                       }
                        if ((mini<=0)&(rand!=i)){
                            matrix1<-shift(matrix1,i,rand)
                            index<-shift_index(index,i,rand)
                            stopIteration2=FALSE
                        }
                    }
                    if ((stopIteration2==TRUE)&(count_2<3)){
                    record=matrix(0,2,n);count_2=count_2+1
                    for (i in 1:n){
                        for (j in 1:n){
                            if((sum(matrix1[i,j:i])*sign(j-i)==0)&(i!=j)) {
                            record[2,j]=delta_si(matrix1,i,j)}
                            else{record[2,j]=100}
                        }
                        a2=record[2,]
                            if (min(a2)<0){
                                final=sample(which(a2==min(a2)),1)
                                matrix1<-shift(matrix1,i,final)
                                index<-shift_index(index,i,final)


                            }
                        }

                    }
                }

                else if (p1==8){
                     count_1=count_1+1
                    count=0;pair=NULL
                    for (i in 1:n){
                        for (j in 1:n){
                                if ((sum(matrix1[i,i:j])*sign(j-i)<=0)&(i!=j)) {
                                    pair=rbind(pair,c(i,j));count=count+1

                            }
                        }
                    }
                    if (count>0){
                        rand<-sample(count,1)
                        matrix1<-shift(matrix1,pair[rand,1],pair[rand,2])
                        index<-shift_index(index,pair[rand,1],pair[rand,2])
                        stopIteration2=FALSE
                    }
                                    }

                else if (p1==9){
                     count_1=count_1+2
                    for (i in 1:n){
                        for (j in 1:n){
                               if((sum(matrix1[i,i:j])*sign(j-i)<=0)*(i!=j)){
                                    matrix1<-shift(matrix1,i,j)
                                    index<-shift_index(index,i,j)
                                    stopIteration2=FALSE
                                }
                                                   }
                    }
                if (stopIteration2==TRUE){
                                                for (i in 1:n){
                            for (j in 1:n){
                                     delta_i=sum(matrix1[i,i:j])*sign(j-i)
                                if((delta_i==0)&(i!=j)){
                                     if(delta_si(matrix1,i,j)<0){
                                    matrix1<-shift(matrix1,i,j)
                                    index<-shift_index(index,i,j)

                                                 }
							}
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
                stopIteration1=FALSE;t=t+1
            }
            else if((I==Imin)&(SI==SImin)){
                t=t+1
                best[[length(best)+1]]=index
            }
            else{
                t=t+1
            }
            if ((SImin>0)&(t<nTries)){
                for (j in (n-1):n){
                kk=matrix1[j,1:(j-1)]
                if (max(kk)>0){
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
        attempt=attempt+1
    }
    best=unique(best)
    correlation=rep(0,length(best))
    for (k in 1:length(best)){
        correlation[k]=cor.test(nature(original1),best[[k]])$p.value
    }
        num<-which(correlation==min(correlation))
        result_1=list()
        result_2=list()
        for (i in 1:length(num)){
            result_1[[i]]=matrix_change(original1,best[[num[i]]])
            result_2[[i]]=best[[num[i]]]
        }
    answer=list(best_matrix=result_1,best_order=result_2,I=Imin,SI=SImin)
    return(answer)
}

####
#THE FOLLOWING ARE ALL REQUIRED FUNCTION FOR 'fun_2013',please load them first
###


##
#This function is used to calculate the I and SI given the dominance matrix
##
fun<-function(M){
    n=ncol(M)
    result=rep(0,2)
    k=(M-t(M))/2
    k[upper.tri(k)]=0
    result[1] = 0
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

####
#'transform' function is the function to transform the original matrix to the fictitious matrix
#M is the original matrix
####
transform<-function(M){
    n=ncol(M)
    for (i in 1:(n-1)){
        for (j in (i+1):n){
            if (M[j,i]>M[i,j]){
                M[j,i]=1;M[i,j]=-1
            }
            else if((M[j,i]>0)&(M[i,j]==M[j,i])){
                M[i,j]=M[j,i]=.5
            }
            else if(M[j,i]<M[i,j]){
            M[j,i]=-1;M[i,j]=1
            }
        }
    }
return(M)
}

####
#'shift' function is used to put the rank ith individual at the jth position
#M is the matrix
####
shift<-function(M,i,j){
    n=ncol(M)
    kk=M[,-i]
    if (j==1){
        left<-rep(0,n)
    }
    else{
        left<-cbind(rep(0,n),kk[,1:(j-1)])
    }
    if (j==n){
        right<-rep(0,n)}else{
        right<-cbind(kk[,j:(n-1)],rep(0,n))
    }
    matrix1=cbind(left,M[,i],right)
    matrix1=matrix1[,-c(1,n+2)]
    kk=matrix1[-i,]
    if (j==1){
        top<-rep(0,n)}else{
        top<-rbind(rep(0,n),kk[1:(j-1),])
    }
    if (j==n){
        down<-rep(0,n)
    }
    else{
        down<-rbind(kk[j:(n-1),],rep(0,n))
    }
    matrix2<-rbind(top,matrix1[i,],down)
    matrix2<-matrix2[-c(1,n+2),]
    return(matrix2)
}

####
#'shift_index' is a function change the rank
####
shift_index<-function(index,i,j){
    n<-length(index)
    memory<-index[-i]
    if (j==1){
        left=NULL
    }
    else{
        left<-memory[1:(j-1)]
    }
    if(j==n){
        right=NULL}
    else{
        right<-memory[j:(n-1)]
    }
    new<-c(left,index[i],right)
    return(new)
}


delta_si=function(Matrix,i,j){
    n=ncol(Matrix)

    if (i==j){
        result=0
    }
    else if (i<j){
if (i==1){
        u=x=0}
         else{
        u=sum((Matrix[1:(i-1),(i+1):j]-1)/-2)
        x=sum((Matrix[1:(i-1),i]-1)/-2)
             }
if (j==n){
w=v=0}
else{
 w=sum(floor((Matrix[(i+1):j,(j+1):n]-1)/-2))
 v=sum(floor((Matrix[i,(j+1):n]-1)/-2))
}
   z=j-i
        zhishu<-Matrix[i,(i+1):j]
        a=which(zhishu==1)
        yi=sum(j-i-a+1)
        b=which(zhishu==-1)
        ye=sum(b)
        result=yi-ye+w-u+z*(x-v)
    }
    else{
        if (i==n){
        u=x=0}
        else{
        u=sum(floor((Matrix[j:(i-1),(i+1):n]-1)/-2))
        x=sum(floor((Matrix[i,(i+1):n]-1)/-2))
        }
     if (j==1){
     w=v=0}
     else{
     w=sum(floor((Matrix[1:(j-1),j:(i-1)]-1)/-2) )
     v=sum(floor((Matrix[1:(j-1),i]-1)/-2))
     }
        z=i-j
        zhishu<-Matrix[j:(i-1),i]
        a=which(zhishu==1)
        yi=sum(a)
        b=which(zhishu==-1)
        ye=sum(i-j-b+1)
        result=yi-ye+w-u+z*(x-v)
    }
return(result)
}

