#' Compute best ranked matrixed based on new I&SI method
#'
#' @param m A win-loss matrix
#' @param p A vector of probabilities for each of 4 methods
#' @param a_max Number of tries
#' @param nTries Number of iterations
#' @param p2 probability for last method
#' @return A computed ranked matrix best_matrix best_ranking I and SI
#' @examples
#' isi13(people)
#' @section Further details:
#' Code based on algorithm described by Schmid & de Vries 2013,
#' Finding a dominance order most consistent with a linear hierarchy:
#' An improved algorithm for the I&SI method, Animal Behaviour
#' 86:1097-1105. This first implementation of this algorithm is not
#' very fast. The number of tries should be very high and/or the function
#' should be run several times to detect the optimal matrix or matrices.
#' It may take several runs to find the matrix with the lowest SI,
#' especially for very large matrices. For small matrices it may be more
#' efficient to use the older algorithm. See \code{\link{isi98}}:
#' for further info.
#' @export

isi13<-function(m,p=c(1,0,0,0),a_max=50,nTries=30,p2=0.5){

    morig=org_matrix(get_di_matrix(as.matrix(m)),method="wins")
    Mk=m[colnames(morig),colnames(morig)]
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

result_1x=as.data.frame.matrix(result_1[[1]])
rownames(result_1x)<-colnames(result_1x)
answer=list("best_matrix"=result_1x,"best_order"=colnames(result_1x),"I"=Imin,"SI"=SImin)
return(answer)
}

