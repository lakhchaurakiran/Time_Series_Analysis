# Read data into an R variable
suv = read.table("fancy.txt")$V1

#plot the time series
plot(suv,type="l")

# the data shows an increasing trend, seasonality and the variance also seems to be increasing with time => we need to transform the data to log return
# plotting the data, log transform, differenced log transform
par(mfrow=c(2,2))
plot(suv,type="l",main="Monthly sales for a souvenir shop",col="purple")
plot(log(suv),type="l",main="Log Transform of the data",col="red")
plot(diff(log(suv)),type="l",main="Differenced Log Transform of the data",col="maroon")
plot(diff(diff(log(suv)),12),type="l",main="Log Transform without seasonality and trend",col="green")

par(mfrow=c(2,1))
acf(diff(diff(log(suv)),12),main='ACF of log-transform without seasonality')
pacf(diff(diff(log(suv)),12),main='PACF of log-transform without seasonality')
# the ACF shows just one significant correlation close to zero i.e. lag=1 => q=0,1. It also shows a few other significant values later at around lag=22 and 32, so there may be 1, 2 or 3 seasonal moving average terms too (Q=0,1,2,3). The PACF shows one significant correlation close to zero i.e. at lag=1 => p=0,1 and may be just one other significant correlations later, so P=0,1.

d=1; DD=1; per=12

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(suv), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# minimum AIC found for the model p,d,q,P,D,Q,s values of 1,1,0,0,1,1,12 
# and it also has a large enough p-value indicating that the residuals don't have any significant correlations

sarima(log(suv),1,1,0,0,1,1,12)

# coefficients for the 1 AR term = -0.5017 and for the one seasonal MA term = -0.5107 and the variance of the innovation term 0.0311
# note that this model if for log(suv)
model<-arima(x=log(suv), order = c(1,1,0), seasonal = list(order=c(0,1,1), period=per))
plot(forecast(model))

# the model 0,1,3,0,1,1 actually gives even better values
sarima(log(suv),0,1,3,0,1,1,12)
