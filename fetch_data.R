library(quantmod)
library(lubridate)
library(Rblpapi)


fetchData <- function( tickers,fields="PX_LAST"){
  # startdate = ISOdate( year(Sys.Date())-10, 1, 1 ) # start from 6 years ago, because if the month is not before current month we will not include current year
  enddate = date(ISOdate(2017,4,2))
  conn <- blpConnect()
  opt = c("periodicitySelection"="DAILY", "nonTradingDayFillOption" = "ALL_CALENDAR_DAYS")
  rawdat <- bdh(con = conn, c(tickers), fields, start.date = date(ISOdate(2004,1,1)), end.date = date(ISOdate(2017,4,2)), options = opt )
  
  #na.omit(rawdat)
  return(rawdat)
}

# merge the data according to date
mergeData <- function(frame.list){
  cols =  c("date",names(frame.list))
  rows = frame.list[[1]]$date
  mergedDat = data.frame(matrix( ncol = length(cols), nrow = length(rows) ))
  colnames(mergedDat) =  cols
  rownames(mergedDat) = rows
  mergedDat$date = rows
  for (t in names(frame.list)){
    dat = factorData[[t]]
    if (any(is.na(dat))){
      cat(t, "is null")
    }
    
    mergedDat[[t]] = factorData[[t]]$PX_LAST
  }
  
  return (mergedDat)
}

currency = "NTN+1M BGN Curncy"

factors = c("CPI YOY Index","TWBKREP Index","SPX Index",
            "TWSE Index","XAU BGN Curncy","TWLFADJ Index","TBEXTWAN Index","TWEDUSA Index",
            "TWMSAM2Y Index","TWTREXPY Index","TWINDPIY Index", "TWIRFE Index","TWBCR Index",
            "TAREDSC  Index","TWTRBAL  Index","GDP CQOQ Index","TWBOPCUR Index","JPMVXYGL Index",
            "FDTR Index",  "TWGDCONY Index","TWWPIYOY Index",
            "CL1 COMB Comdty", "USDJPY BGN Curncy") # "PMITWMA Index",
tech.factors = c("RSI_3D","RSI_9D","RSI_14D","RSI_30D",
                 "PX_HIGH","PX_LOW")
factorNames = c("US CPI (RHS)","TW repo rate","SPX Index",
                "TW Stock Exchange Index"," Gold (RHS)","TW Unemployment Rate","US Exports to TW","TW Export to US", 
                "TW M2","TW Export YoY"," TW Industrial Production YoY", "TW Foreign Reserves","TW Bounced Check Ratio",
                "CBC Discount Rate","TW Merchandise Trade Balance"," US GDP QoQ (RHS) ","TW Current Account Balance","FX VIX",
                "US FEd Fund Rate","TW GDP","TW WPI",
                "WTI Oil","USDJPY" ) # 
usdtwd =fetchData(currency) 
factorData = fetchData(factors)
mergedFactorData = mergeData(factorData)
conn <- blpConnect()
opt = c("periodicitySelection"="DAILY", "nonTradingDayFillOption" = "ALL_CALENDAR_DAYS")
techData <- bdh(con = conn, c(currency), tech.factors, start.date = date(ISOdate(2004,1,1)), end.date=date(ISOdate(2017,4,2)), options = opt )
dat = cbind( usdtwd, mergedFactorData[,-1], techData[,-1])
names(dat) = c("date", currency, factorNames, tech.factors)

write.table(dat,"USDTWD.txt", col.names = T, row.names = F )

mydf = dat
names(mydf) = c("date", currency, factorNames, tech.factors)
# Compute technical indicators
preprocess <- function(mydf, lookback){
  
  p = mydf$`NTN+1M BGN Curncy`
  date = mydf$date[(1+lookback):(nrow(mydf))]
  ret = log(p[(lookback+1):nrow(mydf)]/p[1:(nrow(mydf)-lookback)]) # col-1: ret, col-2:price
  fac = mydf[1:(nrow(mydf)-lookback),c(2:(ncol(mydf)))]
  price = p[(lookback+1):nrow(mydf)]
  return(cbind(date,ret,price,fac)) 
}
lookback = 1
segmentationData = preprocess(dat, lookback)
#segmentationData = segmentationData[sign(segmentationData$ret)!=0,] # eliminate non-trading days

ma5 = matrix(1,length(segmentationData$ret),0)
ma10 = matrix(1,length(segmentationData$ret),0)
momentum = matrix(1,length(segmentationData$ret),0)
roc = matrix(1,length(segmentationData$ret),0)
disparity5 = matrix(1,length(segmentationData$ret),0)
disparity10 =matrix(1,length(segmentationData$ret),0)
oscp = matrix(1,length(segmentationData$ret),0)
p = segmentationData$price
h10 = matrix(1,length(segmentationData$ret),0)
l10 = matrix(1,length(segmentationData$ret),0)
stoch.oscillator = matrix(1,length(segmentationData$ret),0)
ma.k = matrix(1,length(segmentationData$ret),0)
for (i in c(11:nrow(segmentationData))) {
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

tech = cbind(ma5,ma10,momentum,roc,disparity5,disparity10,oscp,stoch.oscillator,h10,l10,ma.k)
final.table = cbind(segmentationData,tech)[(12+lookback):nrow(tech),]
write.table(final.table,file = "lookback1.txt",row.names = FALSE)

