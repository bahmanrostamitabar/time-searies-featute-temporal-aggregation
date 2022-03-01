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