library(LearnBayes)
Pima.data<-read.csv("F:/Bayesian_project/Pima.csv",sep=",",header=TRUE)
fit1=glm(type~npreg+glu+bp+skin+bmi+ped+age,family=binomial(link=probit),data=Pima.data)
summary(fit1)
m=10000
X=cbind(1,Pima.data$npreg,Pima.data$glu,Pima.data$bp,Pima.data$skin,Pima.data$bmi,Pima.data$ped,Pima.data$age)
type<-c(Pima.data$type)
#type
fit2=bayes.probit(type,X,m)
apply(fit2$beta,2,mean)
# beta22=c(.5,.5,.5,.5,.5,.5,.5,.5);P22=solve(diag(c(1/12,1/12,1/12,1/12,1/12,1/12,1/12,1/12)))
# fit22=bayes.probit(type,X,m,prior=list(beta=beta22,P=P22))
fit2$log.marg

beta0=c(0,0,0,0,0,0,0,0); c0=100
P0=t(X)%*%X/c0
fit3<-bayes.probit(Y,X,m,list(beta=beta0,P=P0))
fit3$log.marg

head(X)
head(X[,-4:-5])
fit4<-bayes.probit(Y,X[,-c(4,5)],m,list(beta=beta0[-c(4,5)],P=P0[-c(4,5),-c(4,5)]))
fit4$log.marg
BF<-exp(fit3$log.marg)/exp(fit4$log.marg)
BF1<-exp(fit4$log.marg)/exp(fit3$log.marg)
BF1
fit5<-bayes.probit(Y,X[,-4],m,list(beta=beta0[-4],P=P0[-4,-4]))
BF2<-exp(fit5$log.marg)/exp(fit3$log.marg)

fit6<-bayes.probit(Y,X[,-5],m,list(beta=beta0[-5],P=P0[-5,-5]))
BF3<-exp(fit6$log.marg)/exp(fit3$log.marg)
BF3

BF4<-exp(fit3$log.marg)/exp(fit5$log.marg)
BF4

BF5<-exp(fit3$log.marg)/exp(fit6$log.marg)
BF5

BF6<-exp(fit5$log.marg)/exp(fit6$log.marg)
BF6

BF7<-exp(fit5$log.marg)/exp(fit4$log.marg)
BF7

BF8<-exp(fit6$log.marg)/exp(fit4$log.marg)
BF8

BF9<-exp(fit6$log.marg)/exp(fit5$log.marg)
BF9

BF10<-exp(fit4$log.marg)/exp(fit5$log.marg)
BF10

BF11<-exp(fit4$log.marg)/exp(fit6$log.marg)
BF11



pi6<-fit2$beta%*%t(X)
p6<-exp(pi6)/(1+exp(pi6))
ypred.probit6<-colMeans(p6)
table(ypred.probit6,Y)
library(pROC)
roc(Y,ypred.probit6,plot=T)

Pima.train<-read.csv("F:/Bayesian_project/pima.train.csv",sep=",",header=TRUE)
Xtest=Pima.train[,2:8]
Ytest=Pima.train[,9]
Xtest<-cbind(1,Xtest)
pi61<-fit2$beta%*%t(Xtest)
p61<-exp(pi61)/(1+exp(pi61))
ypred.probit61<-colMeans(p61)
table(ypred.probit61,Ytest)
library(pROC)
roc(Ytest,ypred.probit61,plot=T)

Xtest1<-Xtest[,-c(4,5)]
pi_fit<-fit4$beta%*%t(Xtest1)
p_fit<-exp(pi_fit)/(1+exp(pi_fit))
ypred.fit4<-colMeans(p_fit)
table(ypred.fit4,Ytest)
roc(Ytest,ypred.fit4,plot=T)

glu<-seq(65,200)
nglu<-length(glu)
X1<-cbind(1,rep(mean(X[,2]),nglu),glu,rep(mean(X[,6]),nglu),rep(mean(X[,7]),nglu),rep(mean(X[,8]),nglu))
p.glu=bprobit.probs(X1,fit4$beta)
plot(glu,apply(p.glu,2,quantile,0.5),type="l",ylim=c(0,1),xlab="glu",ylab="probability of diabetic")
lines(glu,apply(p.glu,2,quantile,0.05),lty=2)
lines(glu,apply(p.glu,2,quantile,0.95),lty=2)


age<-seq(21,81)
nage<-length(age)
X2<-cbind(1,rep(mean(X[,2]),nage),rep(mean(X[,3]),nage),rep(mean(X[,6]),nage),rep(mean(X[,7]),nage),age)
p.age=bprobit.probs(X2,fit4$beta)
plot(age,apply(p.age,2,quantile,0.5),type="l",ylim=c(0,1),xlab="age",ylab="probability of diabetic")
lines(age,apply(p.age,2,quantile,0.05),lty=2)
lines(age,apply(p.age,2,quantile,0.95),lty=2)


bmi<-seq(19,68)
nbmi<-length(bmi)
X3<-cbind(1,rep(mean(X[,2]),nbmi),rep(mean(X[,3]),nbmi),bmi,rep(mean(X[,7]),nbmi),rep(mean(X[,8]),nbmi))
p.bmi=bprobit.probs(X3,fit4$beta)
plot(bmi,apply(p.bmi,2,quantile,0.5),type="l",ylim=c(0,1),xlab="bmi",ylab="probability of diabetic")
lines(bmi,apply(p.bmi,2,quantile,0.05),lty=2)
lines(bmi,apply(p.bmi,2,quantile,0.95),lty=2)


#X

#proper prior
Y<-Pima.data$type
beta0=c(0,0,0,0,0,0,0,0); c0=100
P0=t(X)%*%X/c0
fit3<-bayes.probit(Y,X,m,list(beta=beta0,P=P0))
apply(fit3$beta,2,mean)
apply(fit3$beta,2,sd)

pi5<-fit3$beta%*%t(X)
p5<-exp(pi5)/(1+exp(pi5))
ypred.probit5<-colMeans(p5)
table(ypred.probit5,Y)
librayr(pROC)
roc(Y,ypred.probit5,plot=T)

pi51<-fit3$beta%*%t(Xtest)
p51<-exp(pi51)/(1+exp(pi51))
ypred.probit51<-colMeans(p51)
table(ypred.probit51,Ytest)
library(pROC)
roc(Ytest,ypred.probit51,plot=T)


#BF
BF1<-exp(fit3$log.marg)/exp(fit2$log.marg)
bayes.probit(type,X,m)$log.marg

# logprobit<-function(theta,datapar){
#   y=datapar$data[,8]
#   x=as.matrix(cbind(1,datapar$data[,1:7]))
#   beta1=theta[1]
#   beta2=theta[2]
#   beta3=theta[3]
#   beta4=theta[4]
#   beta5=theta[5]
#   beta6=theta[6]
#   beta7=theta[7]
#   beta8=theta[8]
#   sigma=theta[9]
#   beta<-c(beta1,beta2,beta3,beta4,beta5,beta6,beta7,beta8)
#   n=length(y)
#   lp=x%*%beta
#   bb=pnorm(-lp)
#   tt=(bb*(1-y)+(1-bb)*y)*runif(n)+bb*y
#   Z=qnorm(tt)+lp
#   mn=(solve(t(x)%*%x))%*%t(x)%*%Z
#   v=solve(t(x)%*%x)
#   logbeta=dmnorm(beta,mean=mn,varcov=v,log=TRUE)
#   return(logbeta-(n/2)*log(sigma))
# }
# beta.start=as.vector(fit1$coefficients)
# datapar<-list(data=Pima.data[,2:9])
# fit11<-laplace(logprobit,c(beta.start,0),datapar)
# class(beta.start)
head(y)
# head(Pima.data[,9])
# head(Pima.data[,2:9])
cbind(1,datapar$data[,1:7])


