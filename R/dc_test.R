#This function is used to compute the DC by formula (H-L)/(H+L)
#input is a matrix
#output is a list, DC is the DC
#phi is the skew-symmetric index(0 means completely symmetric, 0.5 means completely not symmetric)
#si is the symmetric index
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

#This function is used to compute the p value of the symmetric test for a give matrix
#input is a relation matrix, p is the probability matrix used to do simulation
#N is the total results between each dyad
#ntimes is how many time we should simulate to get the empirical p.value
#output is the p value of the skew-symmetric index and DC in their
#empirical quantile
#PS  this is the one side test, that means is DC and the skew-symmetric index are too large
#the p value will be small and rejects the null hypothesis(This matrix is symmetric)
dc_test=function(matrix,p,N=20,ntimes=10000)
{
  matrix=as.matrix(matrix)
  n=nrow(matrix)
  phi=matrix(0,ntimes)
  DC=matrix(0,ntimes)
  result=rep(0,n^2*ntimes)
  dim(result)=c(n,n,ntimes)
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

a=dc_compute(matrix);phi_0=a$phi;DC_0<-a$DC
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

##example
data<-rbind(c(0 ,9 ,3, 6, 12, 18),c(13,0,7,17,45,6),c(7,2,0,5,8,0),c(11,26,12,0,8,6)
,c(4,3,0,1,0,2),c(2,5,0,1,0,0))
p=matrix(.5,6,6)
diag(p)=0
dc_test(data,p,20,10000)
