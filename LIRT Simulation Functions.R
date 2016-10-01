#Load these packages
library(psych)
library(mirt)
library(metafor)
library(lme4)
library(reshape2)

#Function to generate IRT data
generate<-function(n,p,a,b,theta){
  responses = matrix(0, nrow=n, ncol=p) 
  for(i in 1:n) {
    for(j in 1:p) {
      prob =((exp(a[j]*(theta[i]-b[j])))/(1+exp(a[j]*(theta[i]-b[j]))))
      responses[i,j] = (runif(1) < prob)
    }
  }
  return(responses)
}

#This function generates IRT data given n and p
#Data is then transformed to UNIVARIATE from MULTIVARIATE
#p is items, n is persons
genitems<-function(n,p,type){
  p=p; n=n; x=rnorm(n)#sample n number of times from normal distribution 
  mu=0; s=1
  type=type
  #Generates correlated thetas (Set at .5)
  theta=mu+s*x
  #First Set of Items
  
  #sequence of difficulty from -2 to 2
  b=seq(-2.00,2.00,length=p)
  
  #slope parameters set to 1
  a=c(rep(1,p))
  
  responses=matrix(0,ncol=p,nrow=n)
  
  if (type=='normal'){
    i=1
    for(i in 1:p){
      theta=theta*1.00 #this creats normal thetas
      responses[1:n,i]=generate(n,1,a,b,theta) 
    }
  }
  if (type == 'divergent'){
    for(i in 1:p){
      theta=theta*1.05 #this creats divergent thetas
      responses[1:n,i]=generate(n,1,a,b,theta) 
    }  
  }
  
  if (type == 'correlated'){
    for(i in 1:p){
      theta=theta*0.5 #this creats correlated thetas
      responses[1:n,i]=generate(n,1,a,b,theta) 
    }  
  }
  
  one=rep('i',p)
  two=rep(1:p)
  
  id=rep(1:n)
  final=cbind(id,responses)
  #print(c("sloptes=", a))
  #print(c("intercept = ", b))
  
  colnames(final)=c("id",paste(one,two,sep=""))
  final=data.frame(final) 
  return(final)  
}


temp<-genitems(1000,6,type='normal')
temp<-genitems(1000,6,type='divergent')
temp<-genitems(1000,6,type='correlated')




#Simulation program, include n, p, reps and type of theta 
simulation<-function(n,p,reps,type){
  
  res=matrix(0,ncol=(6+p+p+p),nrow=reps)
  
  for(i in 1:reps){
    print(c("Rep number = ", i), quote=FALSE)
    a=genitems(n,p,type=type)
    a_long=melt(a, id.vars=c("id"))
    ir=a_long$value
    results=glmer(ir ~ -1+ variable +(1|id), data=a_long, family=binomial,verbose=FALSE) 
    stuff=summary(results)
   
    res[i,1]=stuff$AICtab[1] #AIC
    res[i,2]=stuff$AICtab[2] #BIC
    res[i,3]=stuff$AICtab[3] #logLik
    res[i,4]=stuff$AICtab[4] #deviance
    res[i,5]=stuff$AICtab[5] #df.resid
    res[i,6]=stuff$varcor$id[1,1] #Ranom effects variance
    #res=stuff$varcor$id
    res[i,7:(6+p)]=results@beta[1:p]# FE results
    res[i,(7+p):(6+p+p)]=as.numeric(stuff$coefficients[,3]) #z values
    res[i,(7+p+p):(6+p+p+p)]=as.numeric(stuff$coefficients[,4]) #pvalues
    
  }
  return(res)
}

simulation(100,10,2,type='normal')






