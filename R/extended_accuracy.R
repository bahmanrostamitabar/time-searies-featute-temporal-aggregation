## Measures of forecast accuracy
## Forecasts in f. This may be a numerical vector or the output from arima or ets or derivatives.
## Actual values in x
# dx = response variable in historical data
## test enables a subset of x and f to be tested.
# MASE: d is the # of differencing
# MASE: D is the # of seasonal differencing

testaccuracy <- function(f, x, test, d, D, benchmark,trainingset) {
  #browser()#trainging set je dodat da bi racunao i MASE gresku za modele koji nisu forecast
  #dx se koristi za MASE gresku, to je train set ustvari
  dx <- getResponse(f)
  
  if(!is.null(trainingset)){
    dx<-trainingset
  }
  
  if (is.data.frame(x)) {
    responsevar <- as.character(formula(f$model))[2]
    if (is.element(responsevar, colnames(x))) {
      x <- x[, responsevar]
    } else {
      stop("I can't figure out what data to use.")
    }
  }
  
  predictionintervals<-NULL
  
  
  if (is.ts(x) && is.ts(f)) {
    tspf <- tsp(f)
    tspx <- tsp(x)
    start <- max(tspf[1], tspx[1])
    end <- min(tspf[2], tspx[2])
    # Adjustment to allow for floating point issues
    start <- min(start, end)
    end <- max(start, end)
    f <- window(f, start = start, end = end)
    x <- window(x, start = start, end = end)
  }
  
  
  if (is.list(f)) {
    if (is.element("mean", names(f))) {
      if(f$method=="Croston's method"){
        predictionintervals<-NULL
        f <- f$mean
      }else{# za sve ostale modele iz forecast paketa
        #Dodati i prag alphe
        upper<-f$upper[,2]
        lower<-f$lower[,2]
        predictionintervals<-TRUE
        f <- f$mean
      }
    } else {
      stop("Unknown list structure")
    }
  }
  
  n <- length(x)
  if (is.null(test)) {
    test <- 1:n
  } else if (min(test) < 1 || max(test) > n) {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }
  
  ff <- f
  xx <- x
  
  # Check length of f
  if (length(f) < n) {
    stop("Not enough forecasts. Check that forecasts and test data match.")
  }
  
  error <- (xx - ff[1:n])[test]
  pe <- error / xx[test] * 100
  
  me <- mean(error, na.rm = TRUE)
  mse <- mean(error ^ 2, na.rm = TRUE)
  mae <- mean(abs(error), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  mpe <- mean(pe, na.rm = TRUE)
  out <- c(me, sqrt(mse), mae, mpe, mape)
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  # Compute MASE if historical data available
  if (!is.null(dx)) {
    tspdx <- tsp(dx)
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx - mean(dx, na.rm = TRUE)), na.rm = TRUE)
    }
    mase <- mean(abs(error / scale), na.rm = TRUE)
    out <- c(out, mase)
    names(out)[length(out)] <- "MASE"
  }
  
  # Additional time series measures
  
  # Compute RMSSE if historical data available
  if (!is.null(dx)) {
    tspdx <- tsp(dx)
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale2 <- mean((nd)^2, na.rm = TRUE)
    } else { # not time series
      scale2 <- mean((dx - mean(dx, na.rm = TRUE))^2, na.rm = TRUE)
    }
    rmsse <- mean((error^2 / scale2), na.rm = TRUE)
    out <- c(out, rmsse)
    names(out)[length(out)] <- "RMSSE"
  }
  
  # Additional time series measures
  
  if (!is.null(tsp(x)) && n > 1) {
    fpe <- (c(ff[2:n]) / c(xx[1:(n - 1)]) - 1)[test - 1]
    ape <- (c(xx[2:n]) / c(xx[1:(n - 1)]) - 1)[test - 1]
    theil <- sqrt(sum((fpe - ape) ^ 2, na.rm = TRUE) / sum(ape ^ 2, na.rm = TRUE))
    if (length(error) > 1) {
      r1 <- acf(error, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]
    } else {
      r1 <- NA
    }
    nj <- length(out)
    out <- c(out, r1, theil)
    names(out)[nj + (1:2)] <- c("ACF1", "Theil's U")
  }
  #### Ovdje sada dodati kod koji racuna dodatne greske
  
  if(!is.null(benchmark)){
    #Za MAE
    if (is.list(benchmark)) {
      if (is.element("mean", names(benchmark))) {
        benchmark <- benchmark$mean
      } else {
        stop("Benchmark must me forecast object class with the same tsp details as the initial forecast")
      }
    }
    if(all(tsp(xx)==tsp(benchmark))){
      RelMAE<-mae/mean(abs((xx - benchmark[1:n])[test]),na.rm = TRUE)
      RelRMSE<-sqrt(mse)/sqrt(mean((xx - benchmark[1:n])[test] ^ 2, na.rm = TRUE))
    }
    out <- c(out, RelMAE,RelRMSE)
    names(out)[c(length(out)-1,length(out))] <- c("RelMAE","RelRMSE")
  }
  
  if(!is.null(predictionintervals)){
    #dodati da se alpha mjenja
    
    #Ova prva tri su dodati jer se nekad nisu poklapali trenutak pocetka vremenske serije predikcionih intervala i test skupa
    xxx<-as.vector(xx)
    lower<-as.vector(lower)
    upper<-as.vector(upper)
    
    h <- length(xx)
    MISValue <- sum(upper - lower) + 2/0.05 * (sum((lower - xx) * (xx < lower)) + 2/0.05* sum((xx - upper) * (xx > upper)))
    
    MISValue[] <- MISValue/h
    MSIS<-MISValue/scale
    
    out <- c(out,MISValue,MSIS)
    names(out)[c(length(out)-1,length(out))]<-c("MIS","MSIS")
    
  }else{
    message("Model must be from forecast package or provide prediction intervals")
  }
  
  #Cumulated forecsating error kao iz: A study of different Croston-like forecasting methods
  
  CFE<-sum(error)
  
  # Period od Stock
  PIS<-sum(cumsum(-error))
  
  #PIS<-sum(cumsum((ff[1:n]-xx)[test])) # Racuna se obrnuto kao prognoze - stvarna potraznja
  
  if (!is.null(dx)){
    sCFE<-CFE/scale
    out <- c(out,CFE,sCFE,PIS)
    names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("CFE","sCFE","PIS")
  }else{out <- c(out,CFE,PIS)
  names(out)[c(length(out)-1,length(out))]<-c("CFE","PIS")}
  
  
  #OUT of Stock matrics
  error<-error[!is.na(error)]
  if(any(-error<0)){
    a<-rep(0,length(error))
    a[which(c(-error)<0)]<-1
    OOS<-sum(a)#Number of times that out ofstock situation occures
    MaxOOS<-min(-error)#Max out of stock situation
    SL<-100*(1-OOS/length(error))#Service level
    out <- c(out,SL,OOS,MaxOOS)
    names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("SL(%)","OOS","MaxOOS")
  }else{#No stock out
    OOS<-0
    MaxOOS<-0
    SL<-100
    out <- c(out,SL,OOS,MaxOOS)
    names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("SL(%)","OOS","MaxOOS")
  }
  
  return(out)
}


trainingaccuracy <- function(f, test, d, D,benchmark) {
  # Make sure x is an element of f when f is a fitted model rather than a forecast
  # if(!is.list(f))
  #  stop("f must be a forecast object or a time series model object.")
  dx <- getResponse(f)
  if (is.element("splineforecast", class(f))) {
    fits <- f$onestepf
  } else {
    fits <- fitted(f)
  } # Don't use f$resid as this may contain multiplicative errors.
  
  res <- dx - fits
  n <- length(res)
  if (is.null(test)) {
    test <- 1:n
  }
  if (min(test) < 1 || max(test) > n) {
    warning("test elements must be within sample")
    test <- test[test >= 1 & test <= n]
  }
  
  tspdx <- tsp(dx)
  
  res <- res[test]
  dx <- dx[test]
  pe <- res / dx * 100 # Percentage error
  
  
  
  me <- mean(res, na.rm = TRUE)
  mse <- mean(res ^ 2, na.rm = TRUE)
  mae <- mean(abs(res), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  mpe <- mean(pe, na.rm = TRUE)
  out <- c(me, sqrt(mse), mae, mpe, mape)
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  # Compute MASE if historical data available
  if (!is.null(dx)) {
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale <- mean(abs(nd), na.rm = TRUE)
    } else { # not time series
      scale <- mean(abs(dx - mean(dx, na.rm = TRUE)), na.rm = TRUE)
    }
    mase <- mean(abs(res / scale), na.rm = TRUE)
    out <- c(out, mase)
    names(out)[length(out)] <- "MASE"
  }
  
  # Compute RMSSE if historical data available
  if (!is.null(dx)) {
    if (!is.null(tspdx)) {
      if (D > 0) { # seasonal differencing
        nsd <- diff(dx, lag = round(tspdx[3L]), differences = D)
      } else { # non seasonal differencing
        nsd <- dx
      }
      if (d > 0) {
        nd <- diff(nsd, differences = d)
      } else {
        nd <- nsd
      }
      scale2 <- mean((nd)^2, na.rm = TRUE)
    } else { # not time series
      scale2 <- mean((dx - mean(dx, na.rm = TRUE))^2, na.rm = TRUE)
    }
    rmsse <- sqrt(mean((res^2 / scale2), na.rm = TRUE))
    out <- c(out, rmsse)
    names(out)[length(out)] <- "RMSSE"
  }
  
  # Additional time series measures
  if (!is.null(tspdx)) {
    if (length(res) > 1) {
      r1 <- acf(res, plot = FALSE, lag.max = 2, na.action = na.pass)$acf[2, 1, 1]
    } else {
      r1 <- NA
    }
    nj <- length(out)
    out <- c(out, r1)
    names(out)[nj + 1] <- "ACF1"
  }
  #Predikcioni intervali se ne racunaju na trening skupu
  MIS<-NA;MSIS<-NA
  out <- c(out,MIS,MSIS)
  names(out)[c(length(out)-1,length(out))]<-c("MIS","MSIS")
  
  
  #Kumulativne supply chain metrics
  cfe<-sum(res, na.rm = TRUE)
  scfe<-cfe/scale
  
  PIS<-sum(cumsum(-res[!is.na(res)]))
  out <- c(out,cfe,scfe,PIS)
  names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("CFE","sCFE","PIS")
  
  #OUT of Stock matrics
  #browser()
  res<-res[!is.na(res)]
  if(any(-res<0)){
    a<-rep(0,length(res))
    a[which(c(-res)<0)]<-1
    OOS<-sum(a)#Number of times that out ofstock situation occures
    MaxOOS<-min(-res)#Max out of stock situation
    SL<-100*(1-OOS/length(res))#Service level
    out <- c(out,SL,OOS,MaxOOS)
    names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("SL(%)","OOS","MaxOOS")
  }else{#No stock out
    OOS<-0
    MaxOOS<-0
    SL<-100
    out <- c(out,SL,OOS,MaxOOS)
    names(out)[c(length(out)-2,length(out)-1,length(out))]<-c("SL(%)","OOS","MaxOOS")
  }
  
  return(out)
}



#' Accuracy measures for a forecast model
#'
#' Returns range of summary measures of the forecast accuracy. If \code{x} is
#' provided, the function measures test set forecast accuracy
#' based on \code{x-f}. If \code{x} is not provided, the function only produces
#' training set accuracy measures of the forecasts based on
#' \code{f["x"]-fitted(f)}. All measures are defined and discussed in Hyndman
#' and Koehler (2006).
#'
#' The measures calculated are:
#' \itemize{
#'   \item ME: Mean Error
#'   \item RMSE: Root Mean Squared Error
#'   \item MAE: Mean Absolute Error
#'   \item MPE: Mean Percentage Error
#'   \item MAPE: Mean Absolute Percentage Error
#'   \item MASE: Mean Absolute Scaled Error
#'   \item RMSSE: Root Mean Scaled Squared Error
#'   \item ACF1: Autocorrelation of errors at lag 1.
#'   \item MIS:  Mean Interval Score
#'   \item MSIS: Mean Scaled Interval Score
#'   \item CFE: Cumulated forecsating error
#'   \item sCFE: Scaled Cumulated forecsating error
#'   \item PIS: Period in Stock
#'   \item SL(%): Service level
#'   \item OOS: Out of the stock
#'   \item MaxOOS: Max Out of the stock
#' }
#' By default, the MASE calculation is scaled using MAE of training set naive
#' forecasts for non-seasonal time series, training set seasonal naive forecasts
#' for seasonal time series and training set mean forecasts for non-time series data.
#' If \code{f} is a numerical vector rather than a \code{forecast} object, the MASE
#' will not be returned as the training data will not be available.
#'
#' See Hyndman and Koehler (2006) and Hyndman and Athanasopoulos (2014, Section
#' 2.5) for further details.
#'
#' @param f An object of class \dQuote{\code{forecast}}, or a numerical vector
#' containing forecasts. It will also work with \code{Arima}, \code{ets} and
#' \code{lm} objects if \code{x} is omitted -- in which case training set accuracy
#' measures are returned.
#' @param x An optional numerical vector containing actual values of the same
#' length as object, or a time series overlapping with the times of \code{f}.
#' @param test Indicator of which elements of \code{x} and \code{f} to test. If
#' \code{test} is \code{NULL}, all elements are used. Otherwise test is a
#' numeric vector containing the indices of the elements to use in the test.
#' @param d An integer indicating the number of lag-1 differences to be used
#' for the denominator in MASE calculation. Default value is 1 for non-seasonal
#' series and 0 for seasonal series.
#' @param D An integer indicating the number of seasonal differences to be used
#' for the denominator in MASE calculation. Default value is 0 for non-seasonal
#' series and 1 for seasonal series.
#' @param ... Additional arguments depending on the specific method.
#' @return Matrix giving forecast accuracy measures.
#' @author Rob J Hyndman
#' @references Hyndman, R.J. and Koehler, A.B. (2006) "Another look at measures
#' of forecast accuracy". \emph{International Journal of Forecasting},
#' \bold{22}(4), 679-688. Hyndman, R.J. and Athanasopoulos, G. (2018)
#' "Forecasting: principles and practice", 2nd ed., OTexts, Melbourne, Australia.
#' Section 3.4 "Evaluating forecast accuracy".
#' \url{https://otexts.org/fpp2/accuracy.html}.
#' @keywords ts
#' @examples
#'
#' fit1 <- rwf(EuStockMarkets[1:200,1],h=100)
#' fit2 <- meanf(EuStockMarkets[1:200,1],h=100)
#' extended_accuracy(fit1)
#' extended_accuracy(fit2)
#' extended_accuracy(fit1,EuStockMarkets[201:300,1])
#' extended_accuracy(fit2,EuStockMarkets[201:300,1])
#' plot(fit1)
#' lines(EuStockMarkets[1:300,1])
#' @export
extended_accuracy <- function(f, ...) {
  UseMethod("extended_accuracy")
}

#' @rdname extended_accuracy
#' @method extended_accuracy default
#' @export
extended_accuracy.default <- function(f, x, test=NULL, d=NULL, D=NULL,benchmark=NULL,trainingset=NULL,...) {
  #browser()
  if (!any(is.element(class(f), c(
    "mforecast", "forecast", "ts", "integer", "numeric",
    "Arima", "ets", "lm", "bats", "tbats", "nnetar", "stlm", "baggedModel"
  )))) {
    stop("First argument should be a forecast object or a time series.")
  }
  if (is.element("mforecast", class(f))) {
    return(accuracy.mforecast(f, x, test, d, D))
  }
  
  #Za RelMAE
  if(!is.null(benchmark)){
    if (!any(is.element(class(benchmark), c(
      "mforecast", "forecast", "ts", "integer", "numeric",
      "Arima", "ets", "lm", "bats", "tbats", "nnetar", "stlm", "baggedModel"
    )))) {
      stop("Benchmark argument should be a forecast object or a time series.")
    }
  }
  
  
  
  trainset <- (is.list(f))
  testset <- (!missing(x))
  if (testset && !is.null(test)) {
    trainset <- FALSE
  }
  if (!trainset && !testset) {
    stop("Unable to compute forecast accuracy measures")
  }
  
  #browser()
  
  trainingdata<-!is.null(trainingset)#Ovo je sve dodato zbog MASE na ne forecast podacima
  if(trainingdata){
    if (!any(is.element(class(trainingset),c("ts","tsaggregates")))){
      trainingset<-ts(trainingset)#ZA trening podaci moraju biti dati kao ts ne kao vektor jer ne bude MASE dobra
    }
  }
  
  # Find d and D
  if (is.null(D) && is.null(d)) {
    if (testset) {
      d <- as.numeric(frequency(x) == 1)
      D <- as.numeric(frequency(x) > 1)
    }
    else if (trainset | trainingdata) { #dodao zbog MASE i za ne forecast modele ovo OR traingset
      if (!is.null(f$mean)) {
        d <- as.numeric(frequency(f$mean) == 1)
        D <- as.numeric(frequency(f$mean) > 1)
      }
      else {
        d <- as.numeric(frequency(f$x) == 1)
        D <- as.numeric(frequency(f$x) > 1)
      }
    }
    else {
      d <- as.numeric(frequency(f) == 1)
      D <- as.numeric(frequency(f) > 1)
    }
    
    
    
    
    
  }
  
  
  if (trainset) {
    trainout <- trainingaccuracy(f, test, d, D,benchmark)
    trainnames <- names(trainout)
  }
  else {
    trainnames <- NULL
  }
  if (testset) {
    testout <- testaccuracy(f, x, test, d, D,benchmark,trainingset)
    testnames <- names(testout)
  }
  else {
    testnames <- NULL
  }
  
  
  outnames <- unique(c(trainnames, testnames))
  
  out <- matrix(NA, nrow = 2, ncol = length(outnames))
  colnames(out) <- outnames
  rownames(out) <- c("Training set", "Test set")
  if (trainset) {
    out[1, names(trainout)] <- trainout
  }
  if (testset) {
    out[2, names(testout)] <- testout
  }
  
  if (!testset) {
    out <- out[1, , drop = FALSE]
  }
  if (!trainset) {
    out <- out[2, , drop = FALSE]
  }
  return(out)
}

# Compute accuracy for an mforecast object
#' @export
accuracy.mforecast <- function(f, x, test=NULL, d, D, ...) {
  object <- f
  out <- NULL
  nox <- missing(x)
  i <- 1
  for (fcast in object$forecast)
  {
    if (nox) {
      out1 <- accuracy(fcast, test = test, d = d, D = D)
    } else {
      out1 <- accuracy(fcast, x[, i], test, d, D)
    }
    rownames(out1) <- paste(fcast$series, rownames(out1))
    out <- rbind(out, out1)
    i <- i + 1
  }
  return(out)
}
