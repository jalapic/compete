#' Extra, not-exported, functions


#' Function required for transforming to 1/0 win/loss matrix based on binomial distribution
# define the function
mtxbinom <- function(mtx, p=0.5, alpha=0.05) {
  # preallocate the matrix in memory
  m2 <- mtx
  for (rr in 2:nrow(mtx)) {
    for (cc in 1:(rr-1)) {
      # these two `for` loops work on the non-diag lower triangle
      x <- mtx[rr,cc]
      y <- mtx[cc,rr]

{if (x+y!=0) {sig <- (binom.test(x, x+y, p)$p.value <= alpha)} else {
  sig<-0}}

# lower-triangle entry
m2[rr,cc] <- 1*((x>y) & sig)
# opposing element in the upper-triangle
m2[cc,rr] <- 1*((y>x) & sig)
    }
  }
m2
}




####
#THE FOLLOWING ARE ALL REQUIRED FUNCTION FOR 'fun_1998',please load them first
###


##
#This function is used to calculate the I and SI given the dominance matrix
##
funisi<-function(M){
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
