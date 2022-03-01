#Accuracy for each h step period

forecasthorizont<-function(predictions,holdout,trainingset){
  
  if((class(predictions)!="forecast") && (is.null(trainingset))){
    stop("Provide train data in order to calculate MASE and sCFE")
  }
  
  if((class(predictions)!="forecast") && (class(holdout)!="ts")){
    stop("Ne moze se racunati greska za ulaze koji su vektori, moraju biti ili jedno forecast class ili da drugo bude ts")
  }
  
  if(class(holdout)=="ts"){
    k <- list(errors=16,label = c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1","Theil's U","MIS","MSIS","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS"))
  }else{
    k <- list(errors=14,label = c("ME","RMSE","MAE","MPE","MAPE","MASE","MIS","MSIS","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS"))
  } 
    
    if((class(predictions)=="forecast")){
      fh<-array(dim=c(1,length(holdout),k$errors),dimnames = list(c("model"),
                                                           c(paste(1:length(holdout),"steps")),
                                                           (k$label)))
      trainingset <- predictions$x # ovo je dodato da bi kod mogao da funkcione iako je to dupliranje jer kasnije u extended ig get response se dobije dx a to je train data
      
    }else{
      fh<-array(dim=c(1,length(holdout),k$errors-2),dimnames = list(c("model"),
                                                             c(paste(1:length(holdout),"steps")),
                                                             c("ME","RMSE","MAE","MPE","MAPE","MASE","ACF1", "Theil's U","CFE","sCFE","PIS","SL(%)","OOS","MaxOOS")))}
    
  for(i in 1:length(holdout)){
    fh[1,i,]<-extended_accuracy(predictions,holdout,trainingset=trainingset,test=i)
  }
  #nevalja za RMSE, ACF1, Utails,CFE,sCFE,PIS, osss, max osss i MIS i MSIS
  #Dok ne smislis nacin kako da i za te greske racunas multi step errors one nece biti prikazivane
  #Pa ih sad izbacujes -c("RMSE","CFE","PIS","SL(%)","OOS","MaxOOS")
  
  if(class(predictions)=="forecast"){
    fh<-fh[,,c("ME","MAE","MPE","MAPE","MASE"),drop=FALSE]
  }else{fh<-fh[,,c("ME","MAE","MPE","MAPE","MASE"),drop=FALSE]}
  
  return(fh)
}

#Average over 1-h periods
Avg_h<-function(hstepserrors){
  label<-rep(NA,dim(hstepserrors[1,,])[1])
  mean_h<-matrix(rep(NA,dim(hstepserrors[1,,])[1]*dim(hstepserrors[1,,])[2]), nrow=dim(hstepserrors[1,,])[1])
  for(i in 2:dim(hstepserrors[1,,])[1]){#Ova zadnja vrijednost je uvijek kao da si odmah racunao kumulativnu gresku
    mean_h[i,]<-colMeans(hstepserrors[1,,][1:i,])
    label[i]<-paste(c("1",i,"steps"),collapse="-")
  }
  colnames(mean_h)<-colnames(hstepserrors[1,,])
  rownames(mean_h)<-label
  return(mean_h)
}
#Avg_h(fh)

#####################################
#Postoje vise kombinacija ulaza u forecasthorizont funkciju i extended accuracy funkciju

#extended accuracy prima sve ulaye isto kao i accuracy i racuna sve isto sto i ona

#forecasthorizont tj multi step accuracy funkcija radi na 3 od cetri moguca slucaja ulaza argumenata u funkciju:

#1. forecast i ts
#2. forecast i non ts
#3, non forecast i ts
#4. non forecast i non ts - ZA ovu kombinaciju multi step nisam htjeo da pravim


#Prvi primjer
#1. Kombinacija forecast i ts SVE radi
library(fpp2)

beer3<-window(ausbeer,start=2006)
plot(beer3)
beer2<-window(ausbeer,start=1992,end=2006-.1)
beerfit1<-snaive(beer2,h=18)
plot(beerfit1)
accuracy(beerfit1,beer3)

fh<-forecasthorizont(beerfit1,beer3)
Avg_h(fh)
fh[1,,]
colMeans(fh[1,,])#ovakop uvijek mozes dobit accuracy funkciju

#2. Drugi primjer
# Kombinacija forecast i non ts
fit1 <- rwf(EuStockMarkets[1:199,1],h=101)
fit2 <- meanf(EuStockMarkets[1:199,1],h=101)

accuracy(fit1)
extended_accuracy(fit1)

accuracy(fit1,EuStockMarkets[200:300,1])
extended_accuracy(fit1,EuStockMarkets[200:300,1])
extended_accuracy(fit1,EuStockMarkets[200:300,1],benchmark=fit2)

#Kod ove kombinacije moras dodati i trening set
fh<-forecasthorizont(fit1,EuStockMarkets[200:300,1],trainingset = EuStockMarkets[1:199,1])#Da bi radilo u ovoj kombinaciji9 mora da bude dat i trainingskup
fh[1,,]
tail(Avg_h(fh))

#uporedjivanje da vidis da je tacno
accuracy(fit1,EuStockMarkets[201:300,1])

#Treci primjer
#3, non forecast i ts
#Mora isto trainigset

fh3<-forecasthorizont(beerfit1$mean,beer3,trainingset = beer2)
fh1<-forecasthorizont(beerfit1,beer3)

fh1[1,,]==fh3[1,,]

#4. Primjer iz Measuring accuracy od Hyndmana (non forecast i non ts kombinacija)
actual<-c(427,383,394,473,420,390,410,488,415,398,419,488,414,374)
prog<-c(423.69,386.88,404.71,483.59,423.81,385.42,403.25,482.13,422.35,383.96,401.79,480.67,420.89,382.5)

extended_accuracy(prog, actual)
accuracy(prog, actual)

fh<-forecasthorizont(prog,actual)#nece raditi jer su ulazi vektori

#############################
#Dodati na GitHub paket

#VISE SERIJA
#Procedura treba za vise serija racunati treba da ide ovako
#1. izracunas za vaku seribju psebno i sa rbind dodajes na fh
#2. nadjes za svaku gresku colMeans prosjek po hsyteps colMeans(fh[,,"ME"])
#3. To onda ubacis u fuknciju Avg da dobijes prosjeke po periodima.
#4. abind(fh,fh1,along = 1)# Ovako slazi arrejes jednu na drugu

foresee<-function(data=Monthly_M4,n=length(Monthly_M4),model,...){
  
  fsteps<-length(Monthly_M4[[1]]$xx)+6#dodato plus 6 u zavisnosti dal prognoziras 18 ili 24 mjeseca unaprijed
  m<-c("Monthly","2-Monthly","Quarterly","4-Monthly","Biannual","Annual")
  
  Forecasts<-array(dim=c(n,fsteps,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c(m)))
  Errors<-array(dim=c(n,fsteps,5,length(m)),dimnames = list(c(),c(paste(rep(c("t+"),fsteps),1:fsteps)),c("ME","MAE","MPE","MAPE","MASE"),c(m)))
  
  for(i in (1:n)){
    #SCENARIO 2 dodavanje veceg test skupa:
    train<-ts(Monthly_M4[[i]]$x[1:(length(Monthly_M4[[i]]$x)-6)],frequency = 12,start = tsp(Monthly_M4[[i]]$x)[1])
    test<-ts(c(Monthly_M4[[i]]$x[(length(Monthly_M4[[i]]$x)-5):length(Monthly_M4[[i]]$x)],Monthly_M4[[i]]$xx),frequency = 12, start=c(tsp(train)[2]+1/12))
    trainagg<-tsaggregates(train,align = "start")
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
        Forecasts[i,1:h,k]<-prediction$mean
      }else{
        #browser()
        switch(model,
               TA={prediction<-tsaggregates(originalLevel$mean,align = "start")[m[k]]
               Forecasts[i,1:length(testagg[[m[k]]]),k]<-prediction[[1]]
               prediction<-prediction[[1]]
               error<-4},
               base={prediction<-forecast(trainagg[[m[k]]],h=length(testagg[[m[k]]]))
               Forecasts[i,1:length(testagg[[m[k]]]),k]<-prediction$mean
               error<-5}
        )
      }
      
      if(m[[k]]=="Annual"){
        Errors[i,1:length(testagg[[m[k]]]),1:error,k]<-forecasthorizont(prediction,testagg[[m[k]]])[1,,]#1:4 (tj error) jer nemas MASE gresku za prognoye koje nisu iz forecast paketa a to su ove jer ih dobijas agregiranjem
      }else if(m[[k]]=="Monthly"){
        Errors[i,,,k]<-forecasthorizont(prediction,testagg[[m[k]]])[1,,]
      }else{
        Errors[i,1:length(testagg[[m[k]]]),1:error,k]<-forecasthorizont(prediction,testagg[[m[k]]])[1,,]
      }
    }
  }
  return(list(Forecasts=Forecasts,Errors=Errors))
}

#Provjera tacnosti za TA prognoze
a<-foresee(Monthly_M4,50,model="TA")
colMeans(a$Errors[1,,1:5,],na.rm = TRUE)
#Provjera sa aggregacionom funkcijom
proba<-AggForcast(Monthly_M4,50)
proba$TAErrors[1,,]

#Gledanje na svaki pojedinacni nivo
plot(colMeans(a$Errors[1,,"MAPE",],na.rm = TRUE),type = "b")#Po novom kodu
plot(proba$TAErrors[1,"MAPE",1:6],type="b",ylab = "MAPE error")# Po starom kodu

#Provjera tacnosti za base prognoze
b<-foresee(Monthly_M4,50,model="base")
colMeans(b$Errors[1,,1:5,],na.rm = TRUE)

proba2<-OriginalForcast(Monthly_M4,50)
proba2$TAErrors[1,,]

#Gledanje na svaki pojedinacni nivo
plot(colMeans(b$Errors[1,,"MAPE",],na.rm = TRUE),type = "b")#Po novom kodu
plot(proba2$TAErrors[1,"MAPE",1:6],type="b",ylab = "MAPE error")# Po starom kodu

  
#Racunanje prosjeka za svaki period i svaku gresku
averaging<-function(errors){
  dims<-dim(errors)
  nam<-dimnames(errors)
  browser()
  Table<-array(dim = c(1,dims[2],dims[3],dims[4]),dimnames = list(c(), nam[2][[1]],nam[3][[1]],nam[4][[1]]))
  
  for(k in 1:dims[4]){
    for(e in 1:dims[3][[1]]){
      Table[,,e,k]<-colMeans(errors[,,e,k])
    }
  }
  return(Table)
}
ap<-averaging(a$Errors)
bp<-averaging(b$Errors)

#Igrati se sad da nadjes odgovarajuci pogled i pustiti za sve M4 serije
ap[,,1,]
ap[,,"MAE",]

#Sve to sada provjeriti i sa base forecasts i sa TA forecaST
agregacija<-proba #proba ili proba2
prosjek<-ap # ap ili bp

colMeans(agregacija$TAErrors)
colMeans(prosjek[,,"MAE",],na.rm = TRUE)

colMeans(agregacija$TAErrors)["MAPE",]
colMeans(prosjek[,,"MAPE",],na.rm = TRUE)
#Isto se dobija sto je odlicno! Ako se gleda sumarum na svih 50 serija

plot(colMeans(prosjek[,,"MAPE",],na.rm = TRUE),type = "b")

#Kod base forecasts dodaj i MASE
plot(colMeans(prosjek[,,"MASE",],na.rm = TRUE),type = "b")

#Dalji koraci
#Izvrsena je procjena greske za Original i estimations za 48000 serija
#One su snimljene i do njih se moze doći preko 
Original<- readRDS("Original.rds")
estimations<- readRDS("estimations.rds")
features<-readRDS("features.rds")#Osobnine svih serija mjerenih preko Integrated aaproach-a
#Vidjeti kako od multi stepa forsee da napravis dimenziju manje, odnosno kako da forsee pretvoris u Original ili estimations format data frame kako bi koristi ostatak onog koda
#Poboljisati funckiju besperforming da to bude medjusobno poredjenje
#analizirati koje su po MAPE koje po MASE kriterijumu ispale slicne tacne na najnizem nivou.
#Rasplesti kod do kraja i napraviti regresjiju i klasifikaciju po vise kriterijuma
#Napraviti lijepu skriptu slozenu u RMarkdown dokument.