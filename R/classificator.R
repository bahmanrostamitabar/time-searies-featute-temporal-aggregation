classificator <- function(ideal_matrix,predX,Ideal=FALSE){
  modelX <- rep(NA,length(predX))
  for(i in 1:length(predX)){
    if(predX[i]=="TA"){
      modelX[i] <- ideal_matrix[-train,1:2][i,"TA"]
    }else{
      modelX[i] <- ideal_matrix[-train,1:2][i,"Direct"]
    }
  }
  if(Ideal==TRUE){
    table <- cbind(ideal_matrix[-train,],modelX) 
  }else{
    table <- cbind(ideal_matrix[-train,1:2],modelX) 
  }
}