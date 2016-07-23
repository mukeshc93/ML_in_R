library(forecast)
library(TTR)
myts=ts(rev(Sensex[(1:4503),7]),start=c(1997,7,1),end=c(2015,9,14), frequency=250)
plot.ts(myts)
myts_sma=SMA(myts,n=20)
plot.ts(myts_sma)
myts_decomp=decompose(myts)
plot(myts_decomp)
components=myts_decomp$seasonal
myts_2=window(myts)

stockforecast=HoltWinters(myts,gamma=F)
fitted=stockforecast$fitted
futureforecast=forecast.HoltWinters(stockforecast,h=50)
summary(futureforecast)
plot.forecast(futureforecast)
acf(futureforecast$residuals, lagmax=50)
Box.test(futureforecast$residuals, lag=50, type="Ljung-Box")
plot.ts(futureforecast$residuals)
myts_diff=diff(myts, differences = 1)
plot.ts(myts_diff)

###zoo n dynlm
zoo_data=zoo(Sensex[,7])
ml=lapply(list(zoo_data), function(zoo_data) {dynlm(zoo_data~lag(zoo_data,-1))})
pred_lm=predict.dyn(ml,Sensex[1:40,-1])

####
sen=ts.intersect(Sensex[,8], sen6=lag(Sensex,-6), dframe = T)