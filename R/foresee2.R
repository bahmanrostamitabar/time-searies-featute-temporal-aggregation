foresee2<-function(data=Monthly_M4,n=length(Monthly_M4),model,h=24,...){
  
  fsteps <- h
  m<-c("Monthly","2-Monthly","Quarterly","4-Monthly","Biannual","Annual")
  
  #browser()
  
  Forecasts<-array(dim=c(n,fsteps,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c(m)))
  #Kada budes osposobio predikcione intervale koristi ovu gresku ispod sa svim greskama
  #Errors<-array(dim=c(n,16,length(m)),dimnames = list(c(),c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1", "MIS","MSIS","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS","Theil's U"),c(m)))
  error <- c("ME","RMSE","MAE","MPE","MAPE","MASE","RMSSE","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS")
  Errors<-array(dim=c(n,13,length(m)),dimnames = list(c(),c(error),c(m)))
  
  for(i in (1:n)){
    if(h==24){
      #SCENARIO 2 dodavanje veceg test skupa:
      train<-ts(Monthly_M4[[i]]$x[1:(length(Monthly_M4[[i]]$x)-6)],frequency=12,start=tsp(Monthly_M4[[i]]$x)[1])
      test<-ts(c(Monthly_M4[[i]]$x[(length(Monthly_M4[[i]]$x)-5):length(Monthly_M4[[i]]$x)],Monthly_M4[[i]]$xx),frequency=12,start=c(tsp(train)[2]+1/12))
      trainagg<-tsaggregates(train,align = "start")#it could be end for train data here, but it doesent matter since accuracy function is taking inputs as vectors and only concers are they equal length, it asumes tha they are the same future periods.
      testagg<-tsaggregates(test,align = "start")
      #h=24
      
    }else if (h==12){
      #Scenario 1 sa 12 perioda skracivanje test skupa
      #1 nacin sa 12 perioda
      train<-Monthly_M4[[i]]$x
      testagg<-tsaggregates(ts((Monthly_M4[[i]]$xx)[1:12],start=tsp(Monthly_M4[[i]]$xx)[1],frequency=tsp(Monthly_M4[[i]]$xx)[3]),align="start")
      trainagg<-tsaggregates(train,align = "start")
      #h=12
      
    }else{#podrazumjeva se da je h=60 ne neko drugo h
      #SCENARIO 3 dodavanje veceg test skupa:
      train<-ts(Monthly_M4[[i]]$x[1:(length(Monthly_M4[[i]]$x)-42)],frequency = 12,start = tsp(Monthly_M4[[i]]$x)[1])
      test<-ts(c(Monthly_M4[[i]]$x[(length(Monthly_M4[[i]]$x)-41):length(Monthly_M4[[i]]$x)],Monthly_M4[[i]]$xx),frequency = 12, start=c(tsp(train)[2]+1/12))
      trainagg<-tsaggregates(train,align = "start")
      testagg<-tsaggregates(test,align = "start")
      #h=60
    }
    print(i)
    for(k in (1:length(m))){
      if(k==1){
        prediction<-forecast(train,h)
        originalLevel<-prediction
        Forecasts[i,1:h,k]<-prediction$mean
      }else{
        #browser()
        switch(model,
               TA={prediction<-tsaggregates(originalLevel$mean,align = "start")[m[k]]
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
      }else if(m[[k]]=="Monthly"){
        Errors[i,,k]<-extended_accuracy(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[2,c(error)]#rown je dodat zbog test i trening greske
      }else{
        Errors[i,,k]<-extended_accuracy(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[rown,c(error)]
      }
    }
  }
  return(list(Forecasts=Forecasts,Errors=Errors))
}
