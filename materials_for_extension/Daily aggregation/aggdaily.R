
aggdaily <- function (y, m = frequency(y), align = c("end", "start"), 
          aggregatelist = NULL) 
{
  #browser()
  align <- match.arg(align)
  n <- length(y)
  m <- as.integer(m)
  mout <- seq_len(m)
  mout <- mout[m%%mout == 0L]
  if(m==365){
    mout <- c(1,7,30,90,182,365)
  }
  mout <- mout[mout <= n]
  if (length(mout) == 0L) 
    stop("Series too short for aggregation")
  if (!is.null(aggregatelist)) 
    mout <- mout[mout %in% aggregatelist]
  if (length(mout) == 0L) 
    stop("No valid factors in aggregatelist argument")
  k <- length(mout)
  y.out <- vector("list", k)
  y.out[[1L]] <- y
  if (!is.ts(y)) 
    y <- ts(y, frequency = m)
  for (i in seq_len(k)[-1L]) {
    if (align == "end") 
      start <- n%%mout[i] + 1L
    else start <- 1L
    nk <- trunc(n/mout[i])
    tmp <- matrix(y[start - 1L + seq_len(mout[i] * nk)], 
                  ncol = nk)
    y.out[[i]] <- ts(colSums(tmp), frequency = round(m/mout[i]), 
                     start = tsp(y)[1] + (start - 1)/m)
  }
  names(y.out) <- paste("Period", m/mout)
  
  if (m == 4L) {
    names(y.out)[mout == 4L] <- "Annual"
    names(y.out)[mout == 2L] <- "Biannual"
    names(y.out)[mout == 1L] <- "Quarterly"
  }
  
  else if (m == 365) {
    names(y.out)[mout == 365L] <- "Annual"
    names(y.out)[mout == 182L] <- "Biannual"
    names(y.out)[mout == 90L] <- "Quarterly"
    names(y.out)[mout == 30L] <- "Monthly"
    names(y.out)[mout == 7L] <-"Weekly"
    names(y.out)[mout == 1L] <- "Daily"
  }
  else if (m == 12L) {
    names(y.out) <- paste(mout, "-Monthly", sep = "")
    names(y.out)[mout == 12L] <- "Annual"
    names(y.out)[mout == 6L] <- "Biannual"
    names(y.out)[mout == 3L] <- "Quarterly"
    names(y.out)[mout == 1L] <- "Monthly"
  }
  else if (m == 7L) {
    names(y.out)[mout == 7L] <- "Weekly"
    names(y.out)[mout == 1L] <- "Daily"
  }
  else if (m == 24L | m == 168L | m == 8760L) {
    names(y.out) <- paste(mout, "-Hourly", sep = "")
    j <- mout%%24L == 0L
    names(y.out)[j] <- paste(mout[j]/24L, "-Daily", 
                             sep = "")
    j <- mout%%168L == 0L
    names(y.out)[j] <- paste(mout[j]/168L, "-Weekly", 
                             sep = "")
    j <- mout%%8760L == 0L
    names(y.out)[j] <- paste(mout[j]/8760L, "-Yearly", 
                             sep = "")
    names(y.out)[mout == 8760L] <- "Annual"
    names(y.out)[mout == 2190L] <- "Quarterly"
    names(y.out)[mout == 168L] <- "Weekly"
    names(y.out)[mout == 24L] <- "Daily"
    names(y.out)[mout == 1L] <- "Hourly"
  }
  else if (m == 48L | m == 336L | m == 17520L) {
    j <- mout%%2L == 0L
    names(y.out)[j] <- paste(mout[j]/2L, "-Hourly", 
                             sep = "")
    j <- mout%%48L == 0L
    names(y.out)[j] <- paste(mout[j]/48L, "-Daily", 
                             sep = "")
    j <- mout%%336L == 0L
    names(y.out)[j] <- paste(mout[j]/336L, "-Weekly", 
                             sep = "")
    j <- mout%%17520L == 0L
    names(y.out)[j] <- paste(mout[j]/17520L, "-Yearly", 
                             sep = "")
    names(y.out)[mout == 17520L] <- "Annual"
    names(y.out)[mout == 4380L] <- "Quarterly"
    names(y.out)[mout == 336L] <- "Weekly"
    names(y.out)[mout == 48L] <- "Daily"
    names(y.out)[mout == 2L] <- "Hourly"
    names(y.out)[mout == 1L] <- "Half-hourly"
  }
  else if (m == 52L) {
    names(y.out) <- paste(mout, "-Weekly", sep = "")
    names(y.out)[mout == 52L] <- "Annual"
    names(y.out)[mout == 26L] <- "Biannual"
    names(y.out)[mout == 13L] <- "Quarterly"
    names(y.out)[mout == 1L] <- "Weekly"
  }
  return(structure(y.out, class = "tsaggregates"))
}

ab <- ccc(USAccDeaths)
dev.new()
plot(ab$Monthly)
plot(ab$`2-Monthly`)
plot(ab$`5-Monthly`)

aaa <- ts(Daily_M4[[1]]$x,frequency = 365)
ba <- ccc(aaa,align=c("start"))

vv <- rep(NA,length(Daily_M4))

for(i in 1:length(Daily_M4)){
  vv[i] <- length(Daily_M4[[i]]$x)/365
}
hist(vv)
summary(vv)
boxplot(vv)

which.min(vv)

aaa <- ts(Daily_M4[[which.min(vv)]]$x)
aaa <- msts(aaa, seasonal.periods=c(7,365.25))
ba <- aggdaily(aaa,align=c("start"))


#Sa freq=7 prognoze
a <- Daily_M4[[1]]$x
a<-ts(a,frequency = 7)
ff <- forecast(a,h=150)
plot(ff)

#sa freq 7 i 365
y <- msts(a, seasonal.periods=c(7,365.25))
fit <- tbats(y)
fc <- forecast(fit)
plot(fc)

y <- msts(a, seasonal.periods=c(7,365.25))
plot(forecast(y))
plot(forecast(y,h=150))
