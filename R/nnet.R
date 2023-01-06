library(nnet)

train2 <- train.data[,]
test2 <- test.data[,]

nn <- nnet(marks~.,data=train2, size=10, maxit=500, decay=0.001)

#Informacije o mrezi
nn

summary(nn)

#summary(nn$residuals)

nn.pred=predict(nn,test2, type="class")

nn.pred
summary(as.factor(nn.pred))

tt <- table(nn.pred,test2[,"marks"]);print(tt)

(NNeval <- c(classificationMetrics(test2[,"marks"],nn.pred,posClass = "TA"),AUC=roc.curve(test2[,"marks"],nn.pred,FALSE)$auc,classificationMetrics(test2[,"marks"],nn.pred,posClass = "TA","totU",cbM)))

test.err=double(10)
AUC.err=double(10)
f.err=double(10)
for(size in 1:10){
  fit=nnet(marks~.,data=train2, size=size, maxit=500, decay=0.001,rang = 0.1)
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
source("cutoff_nn.R")

nn.probs=predict(fit,test2)
cut_nn <- cutoff_nn(nn.probs)


i<-cut_nn
nn.pred=ifelse(nn.probs>i,"TA","Direct")
t<-table(nn.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(nn.pred==ClassTable[-train,"marks"])

#Znaci error ce biti 0.43

#AUC 0.55
roc.curve(as.factor(test.data[,"marks"]),as.factor(nn.pred),FALSE)$auc

# F 0.48 ali ti to nasminkaj sa macroF

(classificationMetrics(nn.pred,test.data[,"marks"])["F"])
#Na lap topu ispada dobar broj oko 0.52
(classificationMetrics(nn.pred,test.data[,"marks"])["macroF"])
#Promjeni ono gore da ti ne daje klasu nego vjerovatnocu.

#Isporbaj jos sa onom drugim paketom nauralnet

#Classification example with Neuralnet in R

#https://medium.com/@yolandawiyono98/ann-classification-with-nnet-package-in-r-3c4dc14d1f14
#https://amunategui.github.io/multinomial-neuralnetworks-walkthrough/index.html
#https://parsnip.tidymodels.org/reference/details_mlp_nnet.html
#https://parsnip.tidymodels.org/index.html

library(neuralnet)

# R-Session 11 - Statistical Learning - Neural Networks
#https://www.youtube.com/watch?v=lTMqXSSjCvk

n2=neuralnet(marks~.-origin,data=train2, hidden = c(4,3), linear.output = FALSE)
plot(n2)

#Probaj da uporedis ova dva stavi isto podesavanje.

x <- 10000

n1=neuralnet(marks~.-origin,data=train2[1:x,], hidden = c(10), linear.output = FALSE)
plot(n1)

n1$net.result[[1]]

n1.probs <- predict(n1,test2[1:x,])[,1]

#cut_nn <- cutoff_nn(n1.probs)

#i<-cut_nn
n1.pred=ifelse(n1.probs>0.5,"TA","Direct")
t<-table(as.factor(n1.pred),test2[1:x,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(as.factor(n1.pred)==test2[1:x,"marks"])

#Znaci error ce biti 0.43

roc.curve(as.factor(test2[1:x,"marks"]),as.factor(n1.pred),FALSE)$auc


### Backpropagation

n2=neuralnet(marks~.-origin,data=train2[1:x,], hidden = c(4,3), linear.output = FALSE, algorithm = "backprop",learningrate=0.1)
plot(n2)

n2$net.result[[1]]

n2.probs <- predict(n2,test2[1:x,])[,1]

#cut_nn <- cutoff_nn(n1.probs)

#i<-cut_nn
n2.pred=ifelse(n2.probs>0.5,"TA","Direct")
t<-table(as.factor(n2.pred),test2[1:x,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP
1-mean(as.factor(n2.pred)==test2[1:x,"marks"])

#Znaci error ce biti 0.43

roc.curve(as.factor(test2[1:x,"marks"]),as.factor(n1.pred),FALSE)$auc