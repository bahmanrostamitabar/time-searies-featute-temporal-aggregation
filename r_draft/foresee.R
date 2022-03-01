#VISE SERIJA
#Procedura treba za vise serija racunati treba da ide ovako
#1. izracunas za vaku seribju psebno i sa rbind dodajes na fh
#2. nadjes za svaku gresku colMeans prosjek po hsyteps colMeans(fh[,,"ME"])
#3. To onda ubacis u fuknciju Avg da dobijes prosjeke po periodima.
#4. abind(fh,fh1,along = 1)# Ovako slazi arrejes jednu na drugu

#OVO OVDJE JE FORESEE SA MULTI-HORIZONT GRESKAMA

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
               error<-5},
               base={prediction<-forecast(trainagg[[m[k]]],h=length(testagg[[m[k]]]))
               Forecasts[i,1:length(testagg[[m[k]]]),k]<-prediction$mean
               error<-5}
        )
      }
      
      if(m[[k]]=="Annual"){
        Errors[i,1:length(testagg[[m[k]]]),1:error,k]<-forecasthorizont(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[1,,]#1:4 (tj error) jer nemas MASE gresku za prognoye koje nisu iz forecast paketa a to su ove jer ih dobijas agregiranjem
      }else if(m[[k]]=="Monthly"){
        Errors[i,,,k]<-forecasthorizont(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[1,,]
      }else{
        Errors[i,1:length(testagg[[m[k]]]),1:error,k]<-forecasthorizont(prediction,testagg[[m[k]]],trainingset = trainagg[[m[k]]])[1,,]
      }
    }
  }
  return(list(Forecasts=Forecasts,Errors=Errors))
}
#Provjeriti i da li za ostal PIS i ostale metrike vazi ovo za jedan korak prognoza da je tacna
#Provjeriti kako da dodas predikcione intervale
#vidida izvrtis za base forecast sve kako bi dobio i MASE gresku
#napravi uporednu funkciju tacnosti
#probaj sveto na M3 ako ti je puno

#Provjera tacnosti za TA prognoze
a<-foresee(Monthly_M4,200,model="TA")
#Ako zelis da vidis svaku pojedinacnu prognozu po svakom koraku onda upotrebi kod ispod. To ne moze sa starim kodom
a$Errors[1,,1:5,]
colMeans(a$Errors[1,,1:5,],na.rm = TRUE)
#Provjera sa aggregacionom funkcijom
proba<-AggForcast(Monthly_M4,10)
proba$TAErrors[1,,]

#Gledanje na svaki pojedinacni nivo
plot(colMeans(a$Errors[1,,"MAPE",],na.rm = TRUE),type = "b")#Po novom kodu
plot(proba$TAErrors[1,"MAPE",1:6],type="b",ylab = "MAPE error")# Po starom kodu za prvu seriju kroz nivoe

#Provjera tacnosti za base prognoze
b<-foresee(Monthly_M4,10,model="base")
colMeans(b$Errors[1,,1:5,],na.rm = TRUE)

proba2<-OriginalForcast(Monthly_M4,10)
proba2$TAErrors[1,,]

#b<-foresee(Monthly_M4,1000,model="base")

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

library(abind)#izbacivanje prve dimenzije iz array jer su sada ovo gore sve podaci sumirani za jednu seriju
prosjek <- adrop(prosjek[1,,,,drop=FALSE],drop=1)#Izbacis jednu dimenziju jer si gore uprosjecio rezultate za sve serije

colMeans(agregacija$TAErrors)
colMeans(prosjek[,"MAE",],na.rm = TRUE)#bez izbacivanja dimenzije ovako bi se pisalo# colMeans(prosjek[,,"MAE",],na.rm = TRUE)

colMeans(agregacija$TAErrors)["MAPE",]
colMeans(prosjek[,"MAPE",],na.rm = TRUE)
#Isto se dobija sto je odlicno! Ako se gleda sumarum na svih 50 serija


plot(colMeans(prosjek[,"MAPE",],na.rm = TRUE),type = "b")

#Kod base forecasts dodaj i MASE
plot(colMeans(prosjek[,"MASE",],na.rm = TRUE),type = "b")

#crtanje svih 48000 i cjelokupno upoređivanje procesa
#MAPE
plot(colMeans(estimations$TAErrors)["MAPE",],type = "b")
points(colMeans(Original$TAErrors)["MAPE",],type = "b",col="red")

colMeans(estimations$TAErrors)["MAPE",]
colMeans(Original$TAErrors)["MAPE",]

#MASE
plot(colMeans(estimations$TAErrors)["MASE",],type = "b")
points(colMeans(Original$TAErrors)["MASE",],type = "b",col="red")

colMeans(estimations$TAErrors)["MASE",]
colMeans(Original$TAErrors)["MASE",]#Nema MASE i zato ne možeš da nacrtaš moras preko forsee da napravis za MASE gresku

#Pojedinacno upoređivanje po seriji
plot(estimations$TAErrors[1,"MAPE",1:6],type="b",ylim=c(0,40),ylab = "MAPE error")
points((Original$TAErrors)[1,"MAPE",1:6],type = "b",col="red")

boje<-2:100
for(i in (2:100)){
  readline("Pres enter to add a plot")
  plot(estimations$TAErrors[i,"MAPE",1:6],type="b",ylim=c(0,40),col=boje[i])
  points(Original$TAErrors[i,"MAPE",1:6],type="b",col="red")
  
}

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