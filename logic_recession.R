library(Rblpapi)
library(quantmod)
# Get recession data
getSymbols('USREC',src='FRED') 
y = USREC["1953-04-01::2017-03-01"]

# Get SPX data
conn <- blpConnect()
opt = c("periodicitySelection"="MONTHLY")
spx <- bdh(con = conn, "SPX Index", "PX_LAST", start.date = date(ISOdate(1953,2,1)), end.date = date(ISOdate(2017,3,1)), options = opt )
p = spx$PX_LAST
spx.ret = log(p[2:length(p)]) - log(p[1:(length(p)-1)])
# Read term spread -- TO BE CHANGED
rates = read.csv(file = "//fap02/Home/strategy.intern.6/Desktop/Recession Forecast.csv")
term.spread = rates$X10.Year - rates$X3.Month
dat = data.frame( date = index(y), recession = y, spx.ret, term.spread)



logit.recession <- function(spx.lag = 6, term.spread.leg = 6, train.rate = 0.6, dat = dat){
  newdat = data.frame( date = dat$date[max(spx.lag+1, term.spread.leg+1):nrow(dat)], recession = dat$USREC[max(spx.lag+1, term.spread.leg+1):nrow(dat)],
                       spx.ret = dat$spx.ret[(max(spx.lag+1, term.spread.leg+1) - spx.lag):(nrow(dat)-spx.lag)], 
                       term.spread = dat$term.spread[(max(spx.lag+1, term.spread.leg+1) - term.spread.leg):(nrow(dat)-term.spread.leg)]) 
  train.size = ceiling(nrow(newdat)*0.6)
  train = newdat[1:train.size,]
  test = newdat [(train.size+1):nrow(newdat),]
  train$spx.ret = scale(train$spx.ret)
  train$term.spread = scale(train$term.spread)
  fit <- glm(formula = recession ~  spx.ret + term.spread , family="binomial", data=train)
  summary(fit)
  predicted = (predict(fit, type="response")>0.5) # predicted values
  
  test$spx.ret = scale(test$spx.ret)
  test$term.spread = scale(test$term.spread)
  fitted.results = predict(fit, newdata = subset(test, select = c("spx.ret","term.spread")), type="response") #"spx.ret","term.spread")))
  fitted.dir = fitted.results >0.5
  insample.rates = mean(predicted == train$recession)
  outsample.rates = mean( fitted.dir == test$recession )
  return(outsample.rates)
}

logit.recession(spx.lag = 1, term.spread.leg = 1, train.rate = 0.6, dat = dat) # training on first 461 data, testing on next

for (i in 1:7) for (j in 1:7){
  cat(paste0("spx.lag: ", i,"\n"))

  cat(paste0("term.spread.leg: ", j,"\n"))

  outsample.hit.rate = logit.recession(spx.lag = i, term.spread.leg = j, train.rate = 0.6, dat = dat)
  cat(paste0("Out-sample hit rate: ", outsample.hit.rate,"\n"))
  print("######################## New Test ##############################")

}



