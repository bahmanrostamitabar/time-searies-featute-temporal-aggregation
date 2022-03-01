## Use Below
library(M4comp2018)
library(fpp)
series_M4 <- Filter(function(l) l$period == "Quarterly", M4)
NN <- 100 # length(series_M4)
categor <- rep(NA,NN)
for (i in (1:NN)) {
  s1 <- series_M4[[i]]$x#extract traing set
  fts <- frequency(s1)
  s2 <- series_M4[[i]]$xx#extract test set
  tseries <- c(s1, s2) # concat traing and test to a vector
  fit <- ets(ts(tseries, frequency=4))
  fit$method
  second_element <- fit$components[2]
  third_element <- fit$components[3]
  if ((second_element == "N") & (third_element=="N")) 
  {
    categor[i] <- "N"
  }
  else if ((second_element != "N") & (third_element=="N"))
  {
    categor[i] <- "T"
  }
  else if ((second_element != "N") & (third_element!="N"))
  {
    categor[i] <- "TS"
  }
  else {
    categor[i] <- "S"
  }
}

## Seperate into 4 groups
unicategor <- c("N","T","TS","S")
ncategor <- length(unicategor)
container <- list()

for (i in 1:ncategor){
  icategor <- unicategor[i]
  idx <- which(categor %in% icategor)
  nidx <- length(idx)
  idata <- list()
  
  for (j in 1:nidx){
    idata[[j]] <- series_M4[[idx[j]]]
  }
  
  #container[[i]] <- idata
  container[[icategor]] <- idata
}


  


