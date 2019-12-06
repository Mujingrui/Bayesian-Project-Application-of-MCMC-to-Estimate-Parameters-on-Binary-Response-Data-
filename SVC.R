Pima.data<-read.csv("F:/Bayesian_project/Pima.csv",sep=",",header=TRUE)
X_SVM=Pima.data[,2:8]
Y_SVM=Pima.data[,9]
dat=data.frame(x=X_SVM,y=as.factor(Y_SVM))
library(e1071)
svmfit=svm(y~., data=dat,kernel="linear",cost=10)
svmfit$index
summary(svmfit)
set.seed(1)
tune.out<-tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod=tune.out$best.model
Pima.train<-read.csv("F:/Bayesian_project/pima.train.csv",sep=",",header=TRUE)
Xtest=Pima.train[,2:8]
Ytest=Pima.train[,9]
testdat=data.frame(x=Xtest,y=as.factor(Ytest))
ypred=predict(bestmod,testdat)
table(predict=ypred,truth=testdat$y)
svmfit.opt<-svm(y~.,data=dat,kernel="linear",cost=0.01,decision.values=T)
fitted<-attributes(predict(svmfit.opt,testdat,decision.values=TRUE))$decision.values
library(ROCR)
rocplot(fitted,testdat["y"],main="Testing Data")
roc(Ytest,fitted,plot=T)
Ytest<-as.numeric(Ytest)
class(fitted)
fit_pred<-predict(svmfit.opt,newx=testdat[-y,])

roc(Ytest,fit_pred,plot=T)

