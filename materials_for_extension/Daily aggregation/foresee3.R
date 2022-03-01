foresee3<-function(data=Daily_M4,n=length(Daily_M4),model,h=90,...){
  
  fsteps <- h
  #m<-c("Daily","Weekly","4-Weeks","8-Weeks","16-Weeks","26-Weeks","52-Weeks")#jer funcija tsaggreagts ne moze da agregira dalje od osnovnog i weekly nivoa
  #To napravi da uducuce moze, a za sad prognoziraj samo daily weekly nivoe
  
  m<-c("Daily","Weekly","Monthly","Quarterly")
  
  Daily_M4sch <- Filter(function(l) length(l$x) > 5*h, Daily_M4);cat("Number of the series = ", length(Daily_M4sch), "\n")
  
  n=length(Daily_M4sch)
  #browser()
  
  Forecasts<-array(dim=c(n,fsteps,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c(m)))
  #Kada budes osposobio predikcione intervale koristi ovu gresku ispod sa svim greskama
  #Errors<-array(dim=c(n,16,length(m)),dimnames = list(c(),c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1", "MIS","MSIS","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS","Theil's U"),c(m)))
  error <- c("ME","RMSE","MAE","MPE","MAPE","MASE","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS")
  Errors<-array(dim=c(n,12,length(m)),dimnames = list(c(),c(error),c(m)))
  
  for(i in (1:n)){
    
    train<-ts(Daily_M4sch[[i]]$x[1:(length(Daily_M4sch[[i]]$x)-c(h-14))],start = tsp(Daily_M4sch[[i]]$x)[1])
    train <- msts(train, seasonal.periods=c(7,365.25))
    test<-ts(c(Daily_M4sch[[i]]$x[(length(Daily_M4sch[[i]]$x)-c(h-15)):length(Daily_M4sch[[i]]$x)],Daily_M4sch[[i]]$xx), start=c(tsp(train)[2]+1/365))
    test <- msts(test, seasonal.periods=c(7,365.25), start=c(tsp(train)[2]+1/365))
    trainagg<-aggdaily(train,align = "start")
    testagg<-aggdaily(test,align = "start")
    
    
    print(i)
    for(k in (1:length(m))){
      if(k==1){
        prediction<-forecast(train,h)
        originalLevel<-prediction
        Forecasts[i,1:h,k]<-prediction$mean
      }else{
        #browser()
        switch(model,
               TA={prediction<-aggdaily(originalLevel$mean,align = "start")[m[k]]
               Forecasts[i,1:length(testagg[[m[k]]]),k]<-prediction[[1]]
               prediction<-prediction[[1]]
               rown <- 1},
               base={prediction<-forecast(trainagg[[m[k]]],h=length(testagg[[m[k]]]))
               Forecasts[i,1:length(testagg[[m[k]]]),k]<-prediction$mean
               rown <- 2}
        )
      }
      
      if(m[[k]]=="Annual"){
        Errors[i,,k]<-extended_accuracy(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[rown,c(error)]#ovo je stavljeno da lijepo poredja greske jer ih ne daje isto za ne forecast i forecsat objekte
      }else if(m[[k]]=="Daily"){
        Errors[i,,k]<-extended_accuracy(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[2,c(error)]#rown je dodat zbog test i trening greske
      }else{
        Errors[i,,k]<-extended_accuracy(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[rown,c(error)]
      }
    }
  }
  return(list(Forecasts=Forecasts,Errors=Errors))
}