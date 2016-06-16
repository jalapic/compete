
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


swap<-function(M,i,j){
  result=M
  k=result[i,];result[i,]=result[j,];result[j,]=k
  k=result[,i];result[,i]=result[,j];result[,j]=k
  return(result)
}



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

