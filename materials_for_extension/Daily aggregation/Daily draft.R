
Daily_M4 <- Filter(function(l) l$period == "Daily", M4)
length(Daily_M4)

length(Daily_M4[[1]]$xx)
length(Daily_M4[[1]]$x)/365

plot(forecast(Daily_M4[[1]]$x,h=365))
b <- msts(Daily_M4[[1]]$x,seasonal.periods=c(7,365.25))
plot(forecast(b))

st <- rep(NA, length(Daily_M4))
for(i in 1:length(Daily_M4)){
  st[i] <- length(Daily_M4[[i]]$x)
}
hist(st)
st

###Daily to quarterly forecasts

Daily <- Filter(function(l) length(l$x) > 500, Daily_M4)

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

taa<-foresee3(Daily_M4,2,model="TA");taa$Forecasts[1,,]


dailyTA<-foresee3(Daily_M4,model="TA")
dailyBase<-foresee3(Daily_M4,model="base")

###New Features
library(feasts)

library(fpp3)

tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs="feasts"))



tourism_features
dim(tourism_features)

###sa malo vise featuresa
Mdata <- Monthly_M4

Mseries<-list(rep(NA,length(Mdata)))
series_names<-c(rep(NA,length(Mdata)))

for(k in(1:length(Mdata))){
  Mseries[[k]]<-Mdata[[k]]$x
  series_names[k]<-Mdata[[k]]$st
}
names(Mseries)<-series_names

#Probaj sad sa ovim drugim osobinama kakva ce biti regresija.

newfeat2 <- bind_cols(
  tsfeatures(Mseries,
             c("acf_features","entropy","lumpiness",
               "flat_spots","crossing_points","nonlinearity","stability","hurst","unitroot_kpss","unitroot_pp")),
  tsfeatures(Mseries,"stl_features", s.window='periodic', robust=TRUE),
  tsfeatures(Mseries, "max_kl_shift", width=48),
  tsfeatures(Mseries,
             c("mean","var"), scale=FALSE, na.rm=TRUE),
  tsfeatures(Mseries,
             c("max_level_shift","max_var_shift"), trim=TRUE)) %>%
  select(mean, var, x_acf1, trend, linearity, curvature,
         seasonal_strength, peak, trough,
         entropy, lumpiness, spike,nonlinearity,stability,hurst,
         unitroot_kpss,unitroot_pp,max_level_shift, max_var_shift, flat_spots,
         crossing_points)
###Ne dobija se Bog zna koliko preciznija prognoza.

mm <- matrix(rep(NA,16*length(Mseries)),ncol=16)
for(i in 1:length(Mseries)){
  mm[i,] <- compengine(Mseries[[i]])
}
#Izbacuje gresku na i = 2209
a <- compengine(Mseries[[1]])
colnames(mm) <- names(a)
View(mm)

datanew <- cbind(newfeat2,mm)#probaj sad sa ovim podacima da racunas

-h=60
-eksperimentisati sa new features
-daily data uraditi

#Stari foresee

foresee2<-function(data=Monthly_M4,n=length(Monthly_M4),model,h=24,...){
  
  #fsteps<-length(Monthly_M4[[1]]$xx)+6#dodato plus 6 u zavisnosti dal prognoziras 18 ili 24 mjeseca unaprijed
  fsteps <- h
  m<-c("Monthly","2-Monthly","Quarterly","4-Monthly","Biannual","Annual")
  
  browser()
  
  Forecasts<-array(dim=c(n,fsteps,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c(m)))
  #Kada budes osposobio predikcione intervale koristi ovu gresku ispod sa svim greskama
  #Errors<-array(dim=c(n,16,length(m)),dimnames = list(c(),c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1", "MIS","MSIS","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS","Theil's U"),c(m)))
  error <- c("ME","RMSE","MAE","MPE","MAPE","MASE","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS")
  Errors<-array(dim=c(n,12,length(m)),dimnames = list(c(),c(error),c(m)))
  
  for(i in (1:n)){
    
    if(h==24){
      #SCENARIO 2 dodavanje veceg test skupa:
      train<-ts(Monthly_M4[[i]]$x[1:(length(Monthly_M4[[i]]$x)-6)],frequency = 12,start = tsp(Monthly_M4[[i]]$x)[1])
      test<-ts(c(Monthly_M4[[i]]$x[(length(Monthly_M4[[i]]$x)-5):length(Monthly_M4[[i]]$x)],Monthly_M4[[i]]$xx),frequency = 12, start=c(tsp(train)[2]+1/12))
      trainagg<-tsaggregates(train,align = "start")
      testagg<-tsaggregates(test,align = "start")
      #h=24
    }else if (h==12){
      #Scenario 1 sa 12 perioda skracivanje test skupa
      #1 nacin sa 12 perioda
      train<-Monthly_M4[[i]]$x
      testagg<-tsaggregates(ts((Monthly_M4[[i]]$xx)[1:12],start = tsp(Monthly_M4[[i]]$xx)[1],frequency =tsp(Monthly_M4[[i]]$xx)[3]),align = "start")
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

ta<-foresee2(Monthly_M4,2,model="TA");ta$Forecasts[1,,]
#Predikcione intervale vidjeti kako ce funkcionisati. I provjeriti dal su tacne sve ove greske.


time_0<-Sys.time()

ta<-foresee2(Monthly_M4,100,h=60,model="TA")
direct<-foresee2(Monthly_M4,100,h=60,model="base")

time_1<-Sys.time()
time_1-time_0
#Oko 2.6 dana
#############
