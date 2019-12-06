logit<-function(x){log(x/(1-x))}
expit<-function(x){exp(x)/(1+exp(x))}

## computes the joint disribution
## computes the joint disribution
log_post<-function(Y,X,beta,n){
  #prob<-rep(0,n)
  #like<-rep(0,n)
  X1<-X[,1]
  X2<-X[,2]
  X3<-X[,3]
  X4<-X[,4]
  X5<-X[,5]
  X6<-X[,6]
  X7<-X[,7]
  X8<-X[,8]
  prob1<-expit(beta[1]*X1+beta[2]*X2+beta[3]*X3+beta[4]*X4+beta[5]*X5+beta[6]*X6+beta[7]*X7+beta[8]*X8)
  like<- sum(dbinom(Y,1,prob1,log=TRUE))
  prior<-sum(dnorm(beta,0,10,log=TRUE))
  val=like+prior
  return(val)
}
Bayes.logistic<-function(Y,X,n.samples,can.sd,n){
  #Initial values:
  beta <-c(-6,0,0,0,0,0,0,0)
  # Keep track of the samples   
  keep.beta<- matrix(0,n.samples,8)
  keep.beta[1,] <- beta
  acc <- att <- rep(0,8)
  curlp<-log_post(Y,X,beta,n) # log posterior at current beta
  for(i in 2:n.samples){
    #Update beta using MH sampling:
    for(j in 1:8){
      att[j] <- att[j] + 1
      # Draw candidate:
      canbeta    <- beta
      canbeta[j] <- rnorm(1,beta[j],can.sd)
      canlp      <- log_post(Y,X,canbeta,n)
      # Compute acceptance ratio:
      R <- exp(canlp-curlp)  
      U <- runif(1)                          
      if(U<R){       
        beta<-canbeta
        curlp<-canlp
        acc[j]<-acc[j]+1
      }
    }
    keep.beta[i,]<-beta
  }
  # Return the posterior samples of beta and
  # the Metropolis acceptance rates
  list(beta=keep.beta,acc.rate=acc/att)
}
burn<-200
n.samples<-1000
Pima.data<-read.csv("F:/Bayesian_project/Pima.csv",sep=",",header=TRUE)
data<-Pima.data[,2:9]
X<-cbind(1,data[,1:7])
Y<-data[,8]
n<-length(Y)
logit_fit<-Bayes.logistic(Y,X,n.samples=n.samples,can.sd=0.03,n)
apply(logit_fit$beta,2,mean)
apply(logit_fit$beta,2,sd)
# X1<-data[,1:7]
# T<-1000 ## set low for CRAN checks; increase to >= 1000 for better results
# out7<-reglogit(T,Y,X1, nu=6, nup=NULL, bstart=bstart, normalize=FALSE)
# out7
pi3<-logit_fit$beta%*%t(X)
p3<-exp(pi3)/(1+exp(pi3))
ypred.logit<-colMeans(p3)
table(ypred.logit,Y)
roc(Y,ypred.logit,plot=T)

Pima.train<-read.csv("F:/Bayesian_project/pima.train.csv",sep=",",header=TRUE)
Xtest=Pima.train[,2:8]
Ytest=Pima.train[,9]
Xtest<-cbind(1,Xtest)
pi31<-logit_fit$beta%*%t(Xtest)
p31<-exp(pi31)/(1+exp(pi31))
ypred.logit1<-colMeans(p31)
table(ypred.logit1,Ytest)
roc(Ytest,ypred.logit1,plot=T)







