ccFraud<-read.table(header=TRUE,file="D:/Master Folder/Fall 2022 Classes/QMST 3339/REGRESSION/RDataFiles/creditcard.csv",sep=",")
attach(ccFraud)

install.packages("penalized")
install.packages("tidyverse")
install.packages("caret")
install.packages("leaps")


library(penalized)
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
library(dplyr)


maxa=max(Amount)
mina=min(Amount)
maxT=max(Time)
minT=min(Time)
table(Class)
fraudsub=ccFraud[which(Class==1) ,]
nonfraudsub=ccFraud[which(Class==0),]
mean(fraudsub$Amount)
median(fraudsub$Amount)
hist(fraudsub$Amount)
par(mfrow=c(1,2))
boxplot(fraudsub$Amount,ylim=c(mina,maxa),main="Fraud",xlab="")
boxplot(nonfraudsub$Amount,ylim=c(mina,maxa),main="NonFraud",xlab="")
boxplot(fraudsub$Time,ylim=c(minT,maxT),main="Fraud",xlab="")
boxplot(nonfraudsub$Time,ylim=c(minT,maxT),main="NonFraud",xlab="")
#AIC=2*k-2*LogLikelihood
library(dplyr)

names(ccFraud)
K=ncol(ccFraud)-1
AICF=matrix(nrow=K,ncol=1)
sAICF=matrix(nrow=K,ncol=1)
droppedc=matrix(nrow=K,ncol=1)
for(k in 1:K){
AICF[k]=summary(glm(Class~ccFraud[,k],family=binomial(link="logit")))$aic
}
ord=order(AICF)
space=ccFraud[,ord[1]]
sAICF[1]=AICF[ord[1]]
  
for(k in 2:K){
space=cbind(space,ccFraud[,ord[k]])
lcol=dim(space)[2]
sAICF[k]=summary(glm(Class~space,family=binomial(link="logit")))$aic
if (sAICF[k]>=sAICF[k-1]){
space=space[,-c(lcol)];droppedc[k]=ord[k]
} 
}
IVSpace=ccFraud[,c(setdiff(ord,droppedc))]
optima=glm(Class~. ,data=IVSpace,family=binomial(link="logit"))
optimapred=(predict(optima,type="response")>0.5)*1
#Sensitivity P(Yhat=1|Y=1)
sum((optimapred==1 & Class==1))/sum(Class==1)

#Specifity 
sum((optimapred==0 & Class==0))/sum(Class==0)
#Accuracy
(sum((optimapred==1 & Class==1))+sum((optimapred==0 & Class==0)))/length(Class)
sum(Class==optimapred)/length(Class) #same as above.
#P(Y=1|Yhat=1)
sum((optimapred==1 & Class==1))/sum(optimapred==1)

#P(Y=0|Yhat=0)
sum((optimapred==0 & Class==0))/sum(optimapred==0)

library(InformationValue)
# The columns are actuals, while rows are predicteds.
optCutOff <- optimalCutoff(Class, predict(optima,type="response"))
predicted=predict(optima,type="response")
plotROC(Class, predicted)
Concordance(Class,predicted)
#sum(Class==0)*sum(Class==1)

sensitivity(Class, predicted, threshold = 0.5)
sensitivity(Class, predicted, threshold =optCutOff )
specificity(Class, predicted, threshold = 0.5)
specificity(Class, predicted, threshold = optCutOff )
confusionMatrix(Class, predicted, threshold = 0.5)
confusionMatrix(Class, predicted, threshold =optCutOff )

