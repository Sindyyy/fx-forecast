# install.packages("rugarch")
library(rugarch)
# mydf = read.csv("TWDUSD.csv")
setwd("/fap02/Home/strategy.intern.6/Desktop/FX Forcast")
mydf = read.csv("USDTWD_21factors_6tech..xlsx")
price = mydf$USDTWD.REGN.Curncy

preprocess <- function(mydf, lookback){
  p = mydf$USDTWD.REGN.Curncy
  xReg = mydf[,c(2:(ncol(mydf)))]
  ret = log(p[(lookback+1):nrow(mydf)]/p[1:nrow(mydf)-lookback]) 
  fac = xReg[1:nrow(mydf)-lookback, ]
  return (cbind(fac, ret))

}
dat = preprocess(mydf, 1)[5500:5900,] # try first 500 days
fac = dat[,1:(ncol(dat)-1)]
returns = dat[,ncol(dat)]


window.length <- 390
act.ret = returns[(window.length+1):(length(returns))]
forecasts.length <- length(returns) - window.length
forecasts <- vector(mode="numeric", length=forecasts.length) 
directions <- vector(mode="numeric", length=forecasts.length) 
p.val <- vector(mode="numeric", length=forecasts.length) 

# loop through every trading day, estimate optimal model parameters from rolling window
# and predict next day's return
for (i in 0:(forecasts.length-1)) {
  roll.returns <- returns[(1+i):(window.length + i)] # create rolling window
  reg.train =  fac[(1+i):(window.length + i),]
  reg.test = fac[window.length+i+1,]
  final.aic <- Inf
  final.order <- c(0,0,0)
  # estimate optimal ARIMA model order
  for (p in 0:5) for (q in 0:5) { # limit possible order to p,q <= 5
    if (p == 0 && q == 0) next # p and q can't both be zero
    arimaFit <- tryCatch( arima(roll.returns, order = c(p,0,q)), xreg = reg.train,
                          error = function( err ) FALSE,
                          warning = function( err ) FALSE )
    if (!is.logical( arimaFit)) {
      current.aic <- AIC(arimaFit)
      if (current.aic < final.aic) { # retain order if AIC is reduced
        final.aic <- current.aic
        final.order <- c(p,0,q)
        final.arima <- arima(roll.returns, order = final.order)
      }
    }
    else next 
  }
  print("done arima")
  # specify and fit the GARCH model
  
  final.aic <- Inf
    for (a in 1:2) for (b in 1:2) { # limit possible order to p,q <= 5
      print("a is :")
      print(a)
      spec = ugarchspec(variance.model <- list(garchOrder=c(a,b)),
                        mean.model = list(armaOrder = final.order[1], final.order[3], include.mean = TRUE，
                             external.regressors = reg.train),
                       # mean.model <- list(
                        #  armaOrder <- c(final.order[1], final.order[3]), include.mean = T),
                        distribution.model = "norm")
      print("here")
      fit = tryCatch(ugarchfit(spec, roll.returns, solver = 'hybrid'), error = function(e) e, warning = function(w) w)
    
    if (!is.logical( fit)) {
      current.aic <- AIC(fit)
      if (current.aic < final.aic) { # retain order if AIC is reduced
        final.aic <- current.aic
        final.fit <- fit
      }
    }
    else next 
    }
    fit = final.fit
  # calculate next day prediction from fitted mode
  # model does not always converge - assign value of 0 to prediction and p.val in this case
  if (is(fit, "warning")) {
    forecasts[i+1] <- 0 
    print(0)
    p.val[i+1] <- 0
  }
  else {
    next.day.fore = ugarchforecast(fit, n.ahead = 1, external.forecastslist(mregfor = reg.test, vregfor = NULL))
    x = next.day.fore@forecast$seriesFor
    directions[i+1] <- ifelse(x[1] > 0, 1, -1) # directional prediction only
    forecasts[i+1] <- x[1] # actual value of forecast
    print(i+1)
    print(forecasts[i+1])
    # analysis of residuals
    # resid <- as.numeric(residuals(fit, standardize = TRUE))
    # ljung.box <- Box.test(resid, lag = 20, type = "Ljung-Box", fitdf = 0)
    # p.val[i+1] <- ljung.box$p.value
  }
}
correct.rate = mean(sign(forecasts)==sign(act.ret), na.rm = TRUE)

dates <- mydf[, 1] 
forecasts.ts <- xts(forecasts, dates[(window.length):length(returns)])
# create lagged series of forecasts and sign of forecast
ag.forecasts <- Lag(forecasts.ts, 1)
ag.direction <- ifelse(ag.forecasts > 0, 1, ifelse(ag.forecasts < 0, -1, 0))

# Create the ARIMA/GARCH returns for the directional system
ag.direction.returns <- ag.direction * returns[(window.length):length(returns)]
ag.direction.returns[1] <- 0 # remove NA

