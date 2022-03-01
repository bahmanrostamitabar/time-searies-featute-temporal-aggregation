library(fpp)
library(thief)
library(M4comp2018)
library(dplyr)

Monthly_M4 <- Filter(function(l) l$period == "Monthly", M4)
n<-length(Monthly_M4)

####NAPOMENA GESKA U KODU PROIZVEDI PROGNOZE NEZAVISNI, ALI PROGNOYE SA MJESECNOG NIVOA AGGREGIRAJ
### NA GODISNJI NIVO I TADA RACUNAJ PRECIZNOST

#Use thief - da probas da napravis rekonslijaciju izmedju nivoa. 

AggForcast<-function(data=Monthly_M4,n=length(Monthly_M4),...){
  
  fsteps<-length(Monthly_M4[[1]]$xx)+6#dodato plus 6 u zavisnosti dal prognoziras 18 ili 24 mjeseca unaprijed
  m<-c("Monthly","2-Monthly","Quarterly","4-Monthly","Biannual","Annual")
  
  TAForecasts<-array(dim=c(n,fsteps,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c(m)))
  TAErrors<-array(dim=c(n,8,length(m)),dimnames = list(c(),c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U"),c(m)))
  
  #browser()
  
  for(i in (1:n)){
    #SCENARIO 2 dodavanje veceg test skupa:
    train<-ts(Monthly_M4[[i]]$x[1:(length(Monthly_M4[[i]]$x)-6)],frequency = 12,start = tsp(Monthly_M4[[i]]$x)[1])
    test<-ts(c(Monthly_M4[[i]]$x[(length(Monthly_M4[[i]]$x)-5):length(Monthly_M4[[i]]$x)],Monthly_M4[[i]]$xx),frequency = 12, start=c(tsp(train)[2]+1/12))
    testagg<-tsaggregates(test,align = "start")
    h=24
    
    #Scenario 1 sa 12 perioda skracivanje test skupa
    #1 nacin sa 12 perioda
    #train<-Monthly_M4[[i]]$x
    #testagg<-tsaggregates(ts((Monthly_M4[[i]]$xx)[1:12],start = tsp(Monthly_M4[[i]]$xx)[1],frequency =tsp(Monthly_M4[[i]]$xx)[3]),align = "start")
    #h=12
    
    print(i)
    for(k in (1:length(m))){
      if(k==1){
        prediction<-forecast(train,h)
        originalLevel<-prediction
        TAForecasts[i,1:h,k]<-prediction$mean
      }else{
        prediction<-tsaggregates(originalLevel$mean,align = "start")[m[k]]
        TAForecasts[i,1:length(testagg[[m[k]]]),k]<-prediction[[1]]
      }
      
      if(m[[k]]=="Annual"){
        TAErrors[i,1:5,k]<-accuracy(prediction[[1]],testagg[[m[k]]])[,1:5]
      }else if(m[[k]]=="Monthly"){
        TAErrors[i,,k]<-accuracy(prediction,testagg[[m[k]]])[2,]
      }else{
        TAErrors[i,1:5,k]<-accuracy(prediction[[1]],testagg[[m[k]]])[,1:5]
      }
    }
  }
  return(list(TAForecasts=TAForecasts,TAErrors=TAErrors))
}

AggForcast(Monthly_M4,1)

estimations<-AggForcast(Monthly_M4)

plot(estimations$TAErrors[1,"MAPE",1:6],type="b",ylim=c(0,25),ylab = "MAPE error")
boje<-2:100
for(i in (2:100)){
  readline("Pres enter to add a plot")
  lines(estimations$TAErrors[i,"MAPE",1:6],type="b",ylim=c(0,25),col=boje[i])
}

#MAPE
boxplot(estimations$TAErrors[,"MAPE",1:6])
boxplot(estimations$TAErrors[,"MAPE",1:6],outline=FALSE)#without outliers
summary(estimations$TAErrors[,"MAPE",1:6])

bestperforming2<-function(dataset,agglevels=c("Monthly","2-Monthly","Quarterly","4-Monthly","Biannual","Annual")){
  Best2<-matrix(rep(NA,dim(dataset)[1]*dim(dataset)[2]),ncol=dim(dataset)[2])
  colnames(Best2)<-c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U")
  browser()
  
  for(i in 1: dim(dataset)[1]){
    for(j in 1:5){#dodati ovdje natpise svake greske "ME","RMSE","MAE","MPE","MAPE","MASE"
      minimum<-dataset[i,j,1]
      bestlevel<-agglevels[1]
      for(k in agglevels){
        print(i)
          if(abs(dataset[i,j,k])<abs(minimum)){
            minimum<-dataset[i,j,k]
            bestlevel<-k
        }
      }
      Best2[i,j]<-bestlevel
    }
  }
  return(Best2)
}
Best<-bestperforming2(estimations$TAErrors)#All Levels
head(Best)
summary(Best)

Best<-bestperforming2(estimations$TAErrors,agglevels=c("Monthly","Annual"))
head(Best)
summary(Best)

#uraditi cross validaciju podataka
#https://robjhyndman.com/hyndsight/tscv/
#dodati i u opis podataka iz koje oblasti dolaze Demographic, Industry, Finance...


#4870/48000#ranije sa nepovezanim nivoima
5798/48000#sa povezanim nivoima i h=24 periona unaprijed

library(dplyr)

Best2idx<-data.frame(cbind(1:48000,Best))

BestOriginal<-filter(Best2idx, MAPE=="Monthly")
BestOriginal<-edit(BestOriginal)#promjeniti kolonu u numeric kod V1

#Crtanje originalih serija na kojima je ets originalna frekvencija bila bolja
for (i in ((BestOriginal[,1]))){
  
  plot(tsaggregates(Monthly_M4[[i]]$x, align = "end"),main=(paste("Data on which original sampling was better",i)))
  
  readline("Pres enter to add a plot")
}

#slucajno izabranih 10 varijabli

dev.new();par(mfrow=c(3,2))

sample<-sample(BestOriginal$V1,6)
#for(i in sample){
#  plot(Monthly_M4[[i]]$x,xlab=NULL,ylab=paste(Monthly_M4[[i]]$type,i))
#}

for(i in sample){
  plot(Monthly_M4[[i]]$x,xlim=c(tsp(Monthly_M4[[i]]$x)[1],tsp(Monthly_M4[[i]]$x)[2]+18/12),ylab=paste(Monthly_M4[[i]]$type,i), ylim=c(min(Monthly_M4[[i]]$x),max(Monthly_M4[[i]]$x)+mean(Monthly_M4[[i]]$x)/10),lwd=1.5,cex=1.5)
  lines(Monthly_M4[[i]]$xx,col="blue")
}


#Summarising the data charactristics of the series were Original level was better

Monthly_M4[[BestOriginal$V1]]$x
hist(BestOriginal)

izdvojeneOsobine<-data.frame(cbind(1:48000,osobine))

#add characteristics of the data on which ets was best on original data

osobine<-filter(features,frequency==12)

#Ovo ispod je samo histogramisanje mjesecnih dobrih serija-popravi taj kod kasnije
izdvojeneOsobine<-data.frame(cbind(1:48000,osobine))
samomjesecne<-izdvojeneOsobine[BestOriginal$idx,]
samomjesecne<-samomjesecne[,-c(2,3,4,9,10,13,14,15,16,17)]

dev.new()
par(mfrow=c(2,5))
for(i in (2:10)){
  
  hist(samomjesecne[,i],xlab =NULL,main = names(samomjesecne)[i],col = "green")
}

par(mfrow=c(1,1))

summary(samomjesecne[,-1])


samogodisnje<-izdvojeneOsobine[-BestOriginal$idx,]
samogodisnje<-samogodisnje[,-c(2,3,4,9,10,13,14,15,16,17)]

par(mfrow=c(2,5))
for(i in (2:10)){
  
  hist(samogodisnje[,i],xlab =NULL,main = names(samogodisnje)[i],col = "green")
}

par(mfrow=c(1,1))

summary(samogodisnje[,-1])


ts.origin<-rep(NA,48000)
for(i in (1:48000)){
  ts.origin[i]<-as.character(Monthly_M4[[i]]$type)
}

metrics<-cbind(ts.origin,data.frame(osobine)); colnames(metrics)<-c("type",colnames(osobine))

metrics<-metrics[,-c(2,3,4)];head(metrics)
summary(metrics)
metrics[,2:18]<-scale(metrics[,2:18])#summary(metrics)
boxplot(metrics)
str(metrics)
hist(metrics[,2])
cor(metrics[,-1])#there is a lot of corelation, so petentional confounding
dev.new()
pairs(metrics[,-c(1,6,7,10,11,12,13,14,18)],diag.panel = panel.hist)


#Making classification data base

classdata<-Best# Monthly Vs. Annual

criteria<-"MAPE"
marks<-rep(NA,dim(classdata)[1])
for(i in (1:dim(classdata)[1])){
  if(classdata[i,criteria]=="Monthly"){
    marks[i]<-"Original"
  }else{marks[i]<-"Aggregate"}
}
ClassTable<-cbind(metrics,marks)

str(ClassTable)

attach(ClassTable)

dev.new()
#pairs(ClassTable[,2:18],col=ClassTable$marks)#nacrtati za prezentaciju samo onde koje su najznacajnije
pairs(ClassTable[,-c(1,6,7,10,11,12,13,14,18)],col=ClassTable$marks)

samocrvene<-filter(ClassTable[,-c(1,6,7,10,11,12,13,14,18)],marks=="Original")
pairs(samocrvene,col=samocrvene$marks)

lr<-glm(marks~.,family = binomial, data=ClassTable);summary(lr)
#dodati train data

lr<-glm(marks~.-e_acf1-e_acf10-x_acf10-diff1_acf1-diff1_acf10-diff2_acf1-diff2_acf10,family = binomial, data=ClassTable[,-1]);summary(lr)

set.seed(1)
train=sample(1:48000,48000*0.7)

train.data<-ClassTable[train,]
test.data<-ClassTable[-train,]

lr<-glm(marks~.-e_acf1-e_acf10-x_acf10-diff1_acf1-diff1_acf10-diff2_acf1-diff2_acf10,family = binomial, data=train.data[,-1]);summary(lr)

lr.probs=predict(lr,type="response",test.data)
lr.probs[1:10];summary(lr.probs)

contrasts(ClassTable[,"marks"])

cutoff<-0.18
lr.pred<-rep("Aggregate",48000-length(train))
lr.pred[lr.probs>cutoff]="Original"
t<-table(lr.pred,ClassTable[-train,"marks"]);print(t)
mean(lr.pred==ClassTable[-train,"marks"])
sensitivity<-t[2,2]/(t[2,2]+t[1,2]);sensitivity#procenat pravilno identifikovanih originalih serija koje su identifikovane
specificity<-(1-(t[2,1]/(t[1,1]+t[2,1])))*100;specificity#procenat pravilno identifikovanih Agreagiranih serija koje su identifikovane


summary(ClassTable[-train,"marks"])
2487/(11913+2487)

cutoff<-seq(from=0.05,to=0.4,by=0.01)

Misclas<-rep(NA,length(cutoff))
FN<-rep(NA,length(cutoff))
FP<-rep(NA,length(cutoff))
library(ROSE)#AUC
AUC<-rep(NA,length(cutoff))

j=1
for(i in cutoff){
  lr.pred=ifelse(lr.probs>i,"Original","Aggregate")
  
  t<-table(lr.pred,ClassTable[-train,"marks"]);print(t)
  
  print(c("cutoff is",i))
  print(c("the error is",mean(lr.pred==ClassTable[-train,"marks"])))
  #roc.curve(ClassTable[-train,"marks"],lr.pred)
  #print(roc.curve(ClassTable[-train,"marks"],lr.pred))
  
  Misclas[j]<-mean(lr.pred==ClassTable[-train,"marks"])
  
  sensitivity<-t[2,2]/(t[2,2]+t[1,2]);print(c("sensitivity",sensitivity))#procenat pravilno identifikovanih originalih serija koje su identifikovane
  specificity<-(1-(t[2,1]/(t[1,1]+t[2,1])))*100;print(c("specificity",specificity))#procenat pravilno identifikovanih Agreagiranih serija koje su identifikovane
  FN[j]<-(t[1,2]/(t[1,2]+t[2,2]));print(c("FN is",FN[j]))
  FP[j]<-(t[2,1]/(t[1,1]+t[2,1]));print(c("FP is",FP[j]))
  AUC[j]<-((roc.curve(ClassTable[-train,"marks"],lr.pred,FALSE))$auc)[1]
  j<-j+1
}
plot(cutoff,FN,type="l",col="blue")
points(cutoff,FP,type="l",col="orange")
points(cutoff,Misclas,type="l")
plot(cutoff,AUC,type="l")

cutoff[which.max(AUC)]

#cutoff treba da bude 0.18 ili 0.2-naknadno odluci
i<-0.18
lr.pred=ifelse(lr.probs>0.18,"Original","Aggregate")
t<-table(lr.pred,ClassTable[-train,"marks"]);print(t)
(t[1,2]/(t[1,2]+t[2,2]))#FN
(t[2,1]/(t[1,1]+t[2,1]))#FP

library(performanceEstimation)
classificationMetrics(ClassTable[-train,"marks"],lr.pred)


library(ROSE)
#test ROC curve
roc.curve(ClassTable[-train,"marks"],lr.pred)
#AUC 0.594

#train ROC curve
lr.probs=predict(lr,type="response",train.data)
lr.pred<-rep("Aggregate",length(train))
lr.pred[lr.probs>0.18]="Original"

roc.curve(ClassTable[train,"marks"],lr.pred)
#AUC 0.59


#dodati test skup
#uraditi cross validaciju  

#OVAKO MOZES MJENJATI SVAKU FUNKCIJU
edit(tsfeatures)

#bas lijepa vremenska serija
plot(forecast(Monthly_M4[[1000]]$x))
lines(Monthly_M4[[1000]]$xx,col="red")

---------
  #08.06.
  # Upoređivanje samo Mjesecni i godisnji nivo
  
  reducedTable<-estimations$TAErrors[,,c(1,6)]
Best3<-bestperforming2(reducedTable)

Best3<-bestperforming2(estimations$TAErrors,agglevels = c("Monthly","Annual"))