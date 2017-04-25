install.packages("e1071")
install.packages("RSNNS")
install.packages("monmlp")
library(e1071)
library(RSNNS)
library(monmlp)
mydf = read.table("lookback1.txt", header = T, sep = " ")
#features= c("ret","USDJPY","US.CPI..RHS.","FX.VIX","TW.M2","TW.Current.Account.Balance","TW.repo.rate","X.TW.Industrial.Production.YoY", "US.FEd.Fund.Rate","TW.Foreign.Reserves","TW.Bounced.Check.Ratio",
 #          "TW.WPI","RSI_3D", "X.Gold..RHS.","CBC.Discount.Rate", "US.Exports.to.TW","X.US.GDP.QoQ..RHS..","TW.Export.to.US",'USDJPY','NTN.1M.BGN.Curncy')
#mydf = mydf[,features]

mydf = mydf[mydf$ret!=0,]
reg = mydf[,4:ncol(mydf)]
reg = mydf[, 2:ncol(mydf)]
mydf.pca = prcomp(reg, center = TRUE, scale. = TRUE)
vols=cumsum((mydf.pca$sdev)^2) / sum(mydf.pca$sdev^2)
plot(vols)
reg.pca = mydf.pca$x[ , 1:10 ]

res = logistic.train(mydf$ret, reg)
res.pca = logistic.train(mydf$ret, reg.pca)
print (res.pca)
print(res)
svm.pca.res = res.pca
svm.res = res
logistic.pca.res = res.pca
logistic.res = res
logistic.pca10.res = res.pca
svm.pca10.res = res.pca
arma.res = cbind(hit.mean,return.mean, sharpe)
table = data.frame( rbind(arma.res,logistic.res,logistic.pca.res, svm.res, svm.pca.res))
rownames(table) = c("ARMA+Garch","Logistic Reg", "Logistic Reg + PCA","SVM", "SVM+PCA")
write.table(table, "svm_arma_log.txt", col.names = T, row.names = T )

logistic.train <- function(dat.y, dat.x, train.size = 320, test.size = 150, times = 10, step =100){
  istart = 1
  hit.rates = matrix(0, nrow = times, ncol = 1)
  rets = matrix(0, nrow = times, ncol = 1)
  count = 0
  for ( i in 1:times) {
    x = dat.x[istart:(istart+train.size+test.size-1), ]
    y = dat.y[istart:(istart+train.size+test.size-1)]
    istart = istart+step
    train.x = head( x, train.size)
    test.x = tail( x, test.size ) 
    train.y = head( y, train.size )
    test.y = tail( y, test.size ) 
    train.dat = data.frame( cbind(train.x, y=(sign(train.y)+1)/2))
    test.dat = data.frame(cbind(test.x,y=(sign(test.y)+1)/2 ) )
    
    svm <- svm(y ~ ., data = train.dat)
    fitted.prob = predict(svm, newdata = data.frame(test.dat ))
    logistic <- glm(formula = y ~ ., data =  train.dat , family="binomial")
    
    fitted.prob = predict(logistic, newdata = test.dat)
   # mlp <- mlp(train.x,(sign(train.y)+1)/2, size=10,  
      #         maxit=50, inputsTest=test.x, targetsTest=(sign(test.y)+1)/2  )
    
    #model = rbfDDA(train.x,(sign(train.y)+1)/2)
    #fitted.prob = predict(model, newdata = test.x)
    # Fit the model and compute the predictions
   # r <- monmlp.fit(x = train.x, y = train.y, hidden1=3, n.ensemble=15, monotone=1, bag=TRUE)
   # z <- monmlp.predict(x = test.x, weights = r)
    predict.dir = (fitted.prob>0.5)
    hit.rates[i] = mean( predict.dir == (test.y>0) )
    rets[i] = sum(predict.dir*test.y)*365/test.size
    
  }
  mean.hit.rates = mean(hit.rates)
  mean.rets = mean(rets)
  sharpe = mean.rets/sqrt(var(rets))
  results = cbind(mean.hit.rates, mean.rets, sharpe)
  return(results)
}

train.dat = data.frame( cbind(train.x, y=train.y))
model <- svm(y ~ ., data = train.dat)
test.dat = data.frame(cbind(test.x,y=test.y) )
fitted.prob = predict(model, newdata = data.frame(test.dat ))
predict.dir = (fitted.prob>0.5)
hit.rates[i] = mean( predict.dir == (test.y>0) )
rets[i] = sum(predict.dir*test.y)*365/test.size
logit. mean(res.pca[,1])
