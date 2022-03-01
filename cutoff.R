
cutoff <- function(lr.probs){
  
  cutoff<-seq(from=quantile(lr.probs)[1],to=quantile(lr.probs)[5],by=0.01)
  #cutoff<-seq(from=0.5,to=0.7,by=0.01)
  
  Misclas<-rep(NA,length(cutoff))
  FN<-rep(NA,length(cutoff))
  FP<-rep(NA,length(cutoff))
  AUC<-rep(NA,length(cutoff))
  PurchaseAccuracy<-rep(NA,length(cutoff))
  
  j=1
  for(i in cutoff){
    lr.pred=ifelse(lr.probs>i,"TA","Direct")
    
    t<-table(lr.pred,ClassTable[-train,"marks"]);print(t)
    
    print(c("cutoff is",i))
    print(c("the error is",1-mean(lr.pred==ClassTable[-train,"marks"])))
    #roc.curve(ClassTable[-train,"marks"],lr.pred)
    #print(roc.curve(ClassTable[-train,"marks"],lr.pred))
    
    Misclas[j]<-1-mean(lr.pred==ClassTable[-train,"marks"])
    
    sensitivity<-t[2,2]/(t[2,2]+t[1,2]);print(c("sensitivity",sensitivity))#procenat pravilno identifikovanih originalih serija koje su identifikovane
    specificity<-(1-(t[2,1]/(t[1,1]+t[2,1])))*100;print(c("specificity",specificity))#procenat pravilno identifikovanih Agreagiranih serija koje su identifikovane
    FN[j]<-(t[1,2]/(t[1,2]+t[2,2]));print(c("FN is",FN[j]))
    FP[j]<-(t[2,1]/(t[1,1]+t[2,1]));print(c("FP is",FP[j]))
    AUC[j]<-((roc.curve(ClassTable[-train,"marks"],lr.pred,FALSE))$auc)[1]
    PurchaseAccuracy[j] <- (t[2,2]/(t[2,1]+t[2,2]));print(c("PurchaseAccuracy is (%)",100*PurchaseAccuracy[j]))
    j<-j+1
  }
  plot(cutoff,FN,type="l",col="blue")
  points(cutoff,FP,type="l",col="orange")
  points(cutoff,Misclas,type="l")
  plot(cutoff,AUC,type="l")
  plot(cutoff,PurchaseAccuracy,type="l")# OVO NIJE DOBRO JER PROCENTUALNO ON PREDVIDI NAJVISE TACNIH ALI IH PREDVIDI MALO, TAKO DA JE OVO DOBRO SAMO YA MALE SKUPOVE KADA SE POJAVLJUJE MALI BROJ OSTVARENJA NPR 6%. 
  
  #cutoff[which.max(PurchaseAccuracy)]
  
  cutoff[which.max(AUC)]
  #cutoff[which.min(Misclas)] #Ako hoces minimum misklasifikacije funkcije!
}
