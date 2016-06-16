#' Compute best ranked matrixed based on original I&SI method
#'
#' @param m A  win-loss matrix
#' @param nTries Number of tries to find best order
#' @return A computed ranked matrix best_matrix best_ranking I and SI
#' @examples
#' isi98b(mouse,nTries=50)
#' @section Further details:
#' Code based on algorithm described by de Vries, H. 1998. Finding a
#' dominance order most consistent with a linear hierarchy:
#' a new procedure and review. Animal Behaviour, 55, 827-843.
#' The number of iterations should be very high and/or the function
#' should be run several times to detect the optimal matrix or matrices.
#'  It may take several runs to find the matrix with the lowest SI,
#'  especially for very large matrices.
#' See \code{\link{isi13}}: for further info.
#' @export

isi98b=function(m,nTries=100)
{

   data <- org_matrix(as.matrix(m),method='wins')

  cat("\nI&SI IMPROVED ALGORITHM FOR DOMINANCE ORDER\n")

  # INITIAL VALUES
  n=as.numeric(dim(data)[1])
  m=matrix(0,n,n)
  siind=matrix(0,n,n)
  I=0
  SI=0
  for (j in 2:n)
  {
    for (i in 1:(j-1))
    {
      siind[j,i]=j-i
      siind[i,j]=j-i
      if (data[j,i]==0&data[i,j]==0)
      {
        m[j,i]=0
        m[i,j]=0
      }else{
        if (data[j,i]>data[i,j])
        {
          m[j,i]=1
          m[i,j]=-1
        }
        if (data[j,i]<data[i,j])
        {
          m[j,i]=-1
          m[i,j]=1
        }
        if (data[j,i]==data[i,j])
        {
          m[j,i]=0.5
          m[i,j]=0.5
        }
      }
    }
  }
  I=sum(lower.tri(m)*m==1)
  SI=sum((lower.tri(m)*m==1)*siind)
  Imin=I
  SImin=SI
  rank=colnames(data)
  best=rank
  t=0
  cat("\nINITIAL RANK: \n")
  print(rank)
  cat(paste("I = ",I,"\n",sep=""))
  cat(paste("SI = ",SI,"\n",sep=""))


  # OUTER LOOP STARTS
  Stop1=F
  while (!Stop1)
  {

    # INNER LOOP STARTS
    stop2=F
    while (!stop2)
    {

      # I AND SI CALCULATION WITHOUT SWAP STARTS
      dI=matrix(0,n,n)
      dSI=matrix(0,n,n)
      for (i in 1:n)
      {
        for (j in 1:n)
        {
          if (j!=i)
          {
            # DELTA VALUES OF SWAPING FROM I TO J
            small=(1:min(i,j))[-min(i,j)]
            large=(max(i,j):n)[-1]
            z=abs(i-j)
            if(i!=1) x=sum(m[i,small]==1) else x=0
            if(j!=n) v=sum(m[large,i]==1) else v=0
            if(i!=1) u=sum(m[i:j,small]==1)-x else u=0
            if(j!=n) w=sum(m[large,i:j]==1)-v else w=0
            y=sum(m[i,i:j])-sum(m[i:j,i])
            i.down=((m==1)*(z+1-siind))[i,i:j]
            c.down=((m==-1)*siind)[i,i:j]
            i.up=((m==1)*siind)[i,i:j]
            c.up=((m==-1)*(z+1-siind))[i,i:j]
            yi=(i<j)*sum(i.down)+(i>j)*sum(i.up)
            ye=(i<j)*sum(c.down)+(i>j)*sum(c.up)
            dI[i,j]=sign(j-i)*y/2
            dSI[i,j]=sign(j-i)*(w-u+z*(x-v)+(yi-ye))
          }
        }
      }
      # I AND SI CALCULATION WITHOUT SWAP ENDS

      stop2=(sum(dI<0)==0&sum((dI==0)*dSI<0)==0)
      if (!stop2)
      {
        # BEST SWAP STARTS
        I.SI=(dI==min(dI))*(dSI-max(dSI)-1)
        all.swap=which(I.SI==min(I.SI),arr.ind =TRUE)
        swap.ind=sample(1:dim(all.swap)[1],1)
        swap.from=all.swap[swap.ind,1]
        swap.to=all.swap[swap.ind,2]
        I=I+dI[swap.from,swap.to]
        SI=SI+dI[swap.from,swap.to]
        # ROW
        temp=m[swap.from,]
        m=m[-swap.from,]
        if(swap.to==1) m=rbind(temp,m)
        if(swap.to==n) m=rbind(m,temp)
        if(swap.to!=1&swap.to!=n) m=rbind(m[1:(swap.to-1),],temp,m[swap.to:(n-1),])
        # COLUMN
        temp=m[,swap.from]
        m=m[,-swap.from]
        if(swap.to==1) m=cbind(temp,m)
        if(swap.to==n) m=cbind(m,temp)
        if(swap.to!=1&swap.to!=n) m=cbind(m[,1:(swap.to-1)],temp,m[,swap.to:(n-1)])
        # RANK
        temp=rank[swap.from]
        rank=rank[-swap.from]
        if(swap.to==1) rank=c(temp,rank)
        if(swap.to==n) rank=c(rank,temp)
        if(swap.to!=1&swap.to!=n) rank=c(rank[1:(swap.to-1)],temp,rank[swap.to:(n-1)])
        m=matrix(m,n,n)
        # BEST SWAP ENDS
      }

    }
    # INNER LOOP ENDS


    # VALUES AFTER SWAP
    I=sum(lower.tri(m)*m==1)
    SI=sum((lower.tri(m)*m==1)*siind)


    # CHECKING OPTIMAL VALUES STARTS
    if ((I<Imin)|(I==Imin&SI<SImin))
    {
      best=rank
      Imin=I
      SImin=SI
    }else{
      t=t+1
      if (SImin>0&t<nTries)
      {
        # RANDOM SWAP STARTS
        for (j in 2:n)
        {
          some=0
          for (i in 1:(j-1))
          {
            some=some+(m[j,i]>m[i,j])
          }
          if (some>0)
          {
            ind=sample(1:(j-1),1)
            sup=rank[j]
            inf=rank[ind]
            rank[ind]=sup
            rank[j]=inf
            row.sup=m[j,]
            row.inf=m[ind,]
            m[ind,]=row.sup
            m[j,]=row.inf
            col.sup=m[,j]
            col.inf=m[,ind]
            m[,ind]=col.sup
            m[,j]=col.inf
          }
        }
        # RANDOM SWAP ENDS
      }else{
        cat("\noptimal rank is found!\n")
        Stop1=T
      }
    }
    # CHECKING OPTIMAL VALUES ENDS


  }
  # OUTER LOOP ENDS


  # RESULTS
  cat("\nBEST RANK: \n")
  print(best)
  cat(paste("I = ",Imin,"\n",sep=""))
  cat(paste("SI = ",SImin,"\n",sep=""))
  cat("\n")

}

