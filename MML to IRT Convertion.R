aa<-matrix(c(2.730,.999, .240, 1.306, 2.099,2.730,.999, .240, 1.306, 2.099,2.730,.999, .240, 1.306, 2.099),ncol=5,nrow=3,byrow=TRUE)

 irtcoefs(aa,5)
[1] -0.8655 -0.2260  2.1745 -0.6865 -0.3965


diagArr <-
  function (dim) 
  {
    n <- dim[2]
    if(dim[1] != n) stop("expecting first two dimensions to be equal")
    d <- seq(1, n*n, by=n+1)
    as.vector(outer(d, seq(0, by=n*n, length=prod(dim[-1:-2])), "+"))
  }




n<-5
reps<-3


b=matrix(NA,nrow=reps,ncol=n)

beta=aa

#pop=seq(-2,2,length=n)
#This program does a 1PL MML to IRT Param Conversion
res=matrix(NA,nrow=reps,ncol=n)

j<-1
for(j in 1:reps){
  i<-1
  for( i in 1:n){
    notbeta=do.call("rbind", replicate(n, beta[j,], simplify = FALSE))
    notbeta[diagArr(dim(notbeta))] <- NA
    b[j,i]=(-beta[j,i])-((1/(n-1))*sum(-notbeta[i,],na.rm=TRUE))#mean(-notbeta[,i],na.rm=TRUE)   
    print(b)
  }
}







