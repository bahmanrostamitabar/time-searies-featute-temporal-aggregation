library(nnet)

train2 <- train.data[,]
test2 <- test.data[,]

nn <- nnet(marks~.,data=train2, size=10, maxit=500, decay=0.001)

#Informacije o mrezi
nn

summary(nn)

summary(nn$residuals)

nn.pred=predict(nn,test2, type="class")

nn.pred
summary(as.factor(nn.pred))

tt <- table(nn.pred,test2[,"marks"]);print(tt)

(NNeval <- c(classificationMetrics(test2[,"marks"],nn.pred,posClass = "TA"),AUC=roc.curve(test2[,"marks"],nn.pred,FALSE)$auc,classificationMetrics(test2[,"marks"],nn.pred,posClass = "TA","totU",cbM)))

test.err=double(10)
AUC.err=double(10)
f.err=double(10)
for(size in 1:10){
  fit=nnet(marks~.,data=train2, size=size, maxit=500, decay=0.001)
  nn.pred=predict(fit,test.data, type="class")
  test.err[size]=(classificationMetrics(nn.pred,test.data[,"marks"])["err"]*100)
  AUC.err[size]=roc.curve(as.factor(test.data[,"marks"]),as.factor(nn.pred),FALSE)$auc
  f.err[size]=(classificationMetrics(nn.pred,test.data[,"marks"])["F"])
  cat(size," ")
}
matplot(1:size,test.err,pch=19,col=c("red"),type="b",ylab="Misclassification Error")
legend("topright",legend=c("Test"),pch=19,col=c("red"))#From the graph size=2


matplot(1:size,AUC.err,pch=19,col=c("red"),type="b",ylab="AUC")
legend("topright",legend=c("AUC"),pch=19,col=c("red"))#From the graph size=10

matplot(1:size,f.err,pch=19,col=c("red"),type="b",ylab="F Error")
legend("topright",legend=c("F"),pch=19,col=c("red"))#From the graph mtry=10

#Odluka je da se ide na set up 10 neurona
source("cutoff.R")

nn.probs=predict(nn,test2)
cut_nn <- cutoff_nn(nn.probs)


i<-cut_nn
nn.pred=ifelse(nn.probs>i,"TA","Direct")
t<-table(nn.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(lr.pred==ClassTable[-train,"marks"])

#Znaci error ce biti 0.43

#AUC 0.55
roc.curve(as.factor(test.data[,"marks"]),as.factor(nn.pred),FALSE)$auc


(classificationMetrics(nn.pred,test.data[,"marks"])["F"])
(classificationMetrics(nn.pred,test.data[,"marks"])["macroF"])
#Promjeni ono gore da ti ne daje klasu nego vjerovatnocu.

#Isporbaj jos sa onom drugim paketom nauralnet

#Classification example with Neuralnet in R

