library(caret)
library(doParallel) # parallel processing
library(dplyr) # Used by caret
library(pROC) # plot the ROC curve

### Use the segmentationData from caret
# Load the data and construct indices to divided it into training and test data sets.
set.seed(10)
# preprocess to filter out non-trading days
setwd("/fap02/Home/strategy.intern.6/Desktop/FX Forcast")
mydf = read.table("USDTWD.txt", header = 1)
price = mydf$NTN.1M.BGN.Curncy
ret = log(price[2:length(price)]/price[1:(length(price)-1)])
day = c(FALSE,ret==0)
mydf= mydf[day,]

USDTWD = mydf$NTN.1M.BGN.Curncy
ts.plot(USDTWD)
par(mfrow=c(1,2))
acf(USDTWD)
pacf(USDTWD)
price = USDTWD
return.rate = log(price[2:length(price)]/price[1:(length(price)-1)])
ts.plot(return.rate)
par(mfrow=c(1,1))
acf(return.rate)
pacf(return.rate)

preprocess <- function(mydf, lookback){
  #calculate returns during the lookback period
  
  p = mydf$NTN.1M.BGN.Curncy
  date = mydf$date[1:(nrow(mydf)-lookback)]
  length(date)
  xReg = mydf[,c(2:(ncol(mydf)))]
  ret = log(p[(lookback+1):nrow(mydf)]/p[1:(nrow(mydf)-lookback)]) 
  length(ret)
  fac = xReg[1:(nrow(mydf)-lookback), ]
  return(cbind(date,ret,fac)) 
}

segmentationData = preprocess(mydf, 1)


# Create technical tickers
ma5 = matrix(1,length(segmentationData$ret),0)
ma10 = matrix(1,length(segmentationData$ret),0)
momentum = matrix(1,length(segmentationData$ret),0)
roc = matrix(1,length(segmentationData$ret),0)
disparity5 = matrix(1,length(segmentationData$ret),0)
disparity10 =matrix(1,length(segmentationData$ret),0)
oscp = matrix(1,length(segmentationData$ret),0)
p = segmentationData$NTN.1M.BGN.Curncy
stoch.oscillator = matrix(1,length(segmentationData$ret),0)
ma.k = matrix(1,length(segmentationData$ret),0)
h10= matrix(1,length(segmentationData$ret),0)
l10 = matrix(1,length(segmentationData$ret),0)
for (i in c(10:nrow(segmentationData))) {
  #calculate technical statistics
  ma5[i] = mean(p[(i-4):i])
  ma10[i] = mean(p[(i-9):i])
  momentum[i] = p[i]-p[i-4]
  roc[i] = p[i]/p[i-10]
  disparity5[i] = p[i]/ma5[i]
  disparity10[i] = p[i]/ma10[i]
  oscp[i] =( ma5[i]-ma10[i])/ma5[i]
  h10[i] = max(p[(i-9):i])
  l10[i] = min(p[(i-9):i])
  stoch.oscillator[i] = (p[i]-l10[i])/(h10[i] -l10[i] )
  ma.k[i] = mean(stoch.oscillator[(i-2):i])
}
tech = cbind(ma5,ma10,momentum,roc,disparity5,disparity10, oscp,stoch.oscillator,ma.k)
final.table = cbind(segmentationData[12:nrow(tech),],tech[12:nrow(tech),])
#write.table(final.table,file = "lookback5.txt",row.names = FALSE)
write.table(final.table,file = "lookback1.txt",row.names = FALSE)
# train
trainsize <- 300
trainData <- segmentationData[1:300,]
testData <- segmentationData[300:600,]
#
trainX <-trainData[,-1] # Create training feature data frame
testX <- testData[,-1] # Create test feature data frame 
y=sign(trainData$ret) # Target variable for training


registerDoParallel(1) # Registrer a parallel backend for train
getDoParWorkers() # check that there are 4 workers

ga_ctrl <- gafsControl(functions = rfGA, # Assess fitness with RF
                       method = "cv",    # 10 fold cross validation
                       genParallel=TRUE, # Use parallel programming
                       allowParallel = TRUE)
## 
lev <- c("PS","WS")     # Set the levels

system.time(rf_ga3 <- gafs(x = trainX, y = y,
                           iters = 30, # 100 generations of algorithm
                           popSize = 25, # population size for each generation
                           levels = lev,
                           gafsControl = ga_ctrl))


final <- rf_ga3$ga$final # Get features selected by GA

