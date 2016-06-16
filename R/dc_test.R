#' Randomization Test of DC and Skew-Symmetry of a Sociomatrix.
#'
#' @param m A matrix with individuals ordered identically in rows and columns.
#' @param N The number of behaviors for each dyad
#' @param ntimes Number of simulations
#' @return A list containing p-value of each test, observed values and descriptive
#'      measures of randomized data.
#' @examples
#' m <- matrix(c(NA,2,30,6,19,122,0,NA,18,
#' 0,19,85,0,1,NA,3,8,84,0,0,0,NA,267,50,0,
#' 0,0,5,NA,10,1,0,4,4,1,NA), ncol=6)  #table 2, Vervaecke et al. 2000  - fleeing in bonobos
#' dc_test(m)
#'
#' mm = matrix(c(0,9,3,6,12,18,13,0,7,17,45,6,7,2,0,5,8,0,11,26,
#' 12,0,8,6,4,3,0,1,0,2,2,5,0,1,0,0),6,6,TRUE)
#' dc_test(mm,N=16)
#' @section References:
#' Leiva D et al, 2008, Testing reciprocity in social interactions: A comparison between the
#' directional consistency and skew-symmetry statistics, Behav Res Methods.
#' @section Further details:
#' This is a one-sided significance test i.e. that observed values are higher than expected
#' @export


dc_test=function(m,N=20,ntimes=10000){



  dc_compute=function(Matrix){
    Matrix=as.matrix(Matrix)
    diag(Matrix)=0
    N=sum(Matrix)/2
    DC=sum(abs(Matrix-t(Matrix)))/2/sum(Matrix)
    S=(Matrix+t(Matrix))/2
    K=(Matrix-t(Matrix))/2
    phi=sum(diag(t(K)%*%K))/sum(diag(t(Matrix)%*%Matrix))
    #phi is the skew-symmetrical index
    si=1-phi
    #si is the symmetrical index
    result=list(DC=DC,S=S,K=K,phi=phi,si=si)
    return(result)
  }


  m=as.matrix(m)
  n=nrow(m)
  phi=matrix(0,ntimes)
  DC=matrix(0,ntimes)
  result=rep(0,n^2*ntimes)
  dim(result)=c(n,n,ntimes)

  p=matrix(.5,nrow(m),ncol(m))

  if (length(unique(colSums(p)))==1){
    number=rbinom(n*(n-1)/2*ntimes,N,p[1,2])
    dim(number)=c(ntimes,n*(n-1)/2)
    for (i in 1:ntimes){
      result[,,i][upper.tri(result[,,i])]=number[i,]
      result[,,i][lower.tri(result[,,i])]=N-number[i,]
    }
      }else{
      for (i in 1:(n-1)){
        for (j in (i+1):n){
          for (k in 1:ntimes){
            number=rbinom(ntimes,N,p[i,j])
            result[i,j,k]=number[k]
            result[j,i,k]=N-number[k]
          }
        }
      }
    }
    a=apply(result,3,dc_compute)
    for (i in 1:ntimes){
      phi[i]=a[[i]]$phi;DC[i]=a[[i]]$DC
    }

    a=dc_compute(m);phi_0=a$phi;DC_0<-a$DC
    phi1=sort(c(phi,phi_0));DC1=sort(c(DC,DC_0))
    phi.pvalue=min(1-which(phi1==phi_0)/(ntimes+1),1-which(phi1==phi_0)/(ntimes+1))
    DC.pvalue=min(1-which(DC1==DC_0)/(ntimes+1),1-which(DC1==DC_0)/(ntimes+1))
    if (phi.pvalue==0) phi.pvalue=1/ntimes
    if (DC.pvalue==0) DC.pvalue=1/ntimes
    mean_phi=mean(phi);mean_DC=mean(DC)
    variance_phi=var(phi);variance_DC=var(DC)
    p.value=list(DC.pvalue=DC.pvalue,phi.pvalue=phi.pvalue,mean_phi=mean_phi,mean_DC=mean_DC
                 , variance_phi=variance_phi,variance_DC=variance_DC,DC=DC_0,phi=phi_0)
    return(p.value)
}
