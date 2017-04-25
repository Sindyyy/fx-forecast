library(forecast)
library(rugarch)
#library(fGarch)
arima.garch.predict<- function(RET = ret, REG=NULL, STEP = 100, TEST.SIZE = 30, TRAIN.SIZE = 150, ROLL = 5) {
  hit.rates = matrix(0, nrow = 10, ncol = 1)
  rets = matrix(0, nrow = 10, ncol = 1)
  count = 0
  istart = 1
  for (i in 1:ROLL) {
    if (TRUE) {

      reg = REG[istart:(istart+TRAIN.SIZE+TEST.SIZE-1), ]
      ret = RET[istart:(istart+TRAIN.SIZE+TEST.SIZE-1) ]
      reg.train = head( reg, TRAIN.SIZE)
      reg.predict = tail( reg, TEST.SIZE) 
      train = head( ret, TRAIN.SIZE )
      test = tail( ret, TEST.SIZE )
      
      # Fit the ARIMA model
      final.aic <- Inf
      final.order <- c(0,0,0)
      for (p in 0:3) for (q in 0:3) {
        if ( p == 0 && q == 0) {
          next
        }

        arimaFit =  arima(train, order=c(p, 0, q),xreg = reg.train)
        if( !is.logical( arimaFit ) ) {
          current.aic <- AIC(arimaFit)
          if (current.aic < final.aic) {
            final.aic <- current.aic
            final.order <- c(p, 0, q)
            final.arima <- arima(train, order=final.order, xreg = reg.train)
          }
        } else {
          next
        }
      }
      
      # Specify and fit the GARCH model
      final.llh = -3000
      for (p in 1:1) for ( q in 1:1 ){
        spec = ugarchspec(
          variance.model=list(garchOrder=c(p,q)),
          mean.model=list(armaOrder=c(final.order[1], final.order[3]), external.regressors =  reg.train, include.mean=T),
          distribution.model="sged"
        )
        fit =  ugarchfit(spec, train, solver = 'hybrid')
        str(fit)
        print("LLH")
        llh = fit@fit$LLH
        print(  llh )
        if (llh) and (final.llh < llh) {
          final.llh = llh
          final.fit = fit
        }
      }

     
      print("now predict")
      fore = ugarchforecast(final.fit, n.ahead=TEST.SIZE, external.forecasts = list(mregfor = reg.predict))
      predict.ret = fore@forecast$seriesFor
      print(predict.ret)

      #########################################################################################

      hit.rates[i] = mean(sign(predict.ret) == sign(test))
      rets[i] = (sign(predict.ret)*test)*365/TEST.SIZE
      print(rets[i])
      istart = istart +STEP
    }
    
  }
  result = data.frame(cbind(hit.rates, rets)) 
  colnames(result) = c("hit.rates","returns")
  return(result)
}

mydf = read.table("lookback1.txt", header = T, sep = " ")
mydf = mydf[mydf$ret!=0,]
mydf.pca = prcomp(mydf[,4:ncol(mydf)], center = TRUE, scale. = TRUE)
reg.pca = mydf.pca$x[,1:10]
rates.with.reg = arima.garch.predict(RET=mydf$ret, REG = reg.pca, TRAIN.SIZE =320, ROLL = 10)

# 320 +30
show.data <- function(price, period, train.size=320, test.size = 30, step =100 ){

  start = (period-1)*step+1
  end = start+train.size+test.size
  dat = price[start:end]
  return(dat)
  
}
price = mydf$price
dat2 = show.data(price=price, period = 2)
dat9 = show.data(price=price, period = 9)
dat10 = show.data(price=price, period = 10)


dat1= show.data(price=price, period = 1)
dat7 = show.data(price=price, period = 7)

lapply(list(dat2,dat9,dat10), var)
lapply(list(dat1,dat7), var)
par(mfrow=c(2,3))
plot(dat2)
plot(dat9)
plot(dat10)
plot(show.data(price=price, period = 1))
plot(show.data(price=price, period = 7))



hit.rates = rates.with.reg[,1]
returns = rates.with.reg[,2]
hit.mean = mean(hit.rates)
return.mean = mean(returns[1:5])

hit.mean
hit.rates


