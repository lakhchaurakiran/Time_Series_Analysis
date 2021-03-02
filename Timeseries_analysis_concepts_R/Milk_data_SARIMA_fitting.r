milk<-read.csv('monthly-milk-production-pounds.csv')
my.data=milk$Monthly_milk_production

# Plot the time series
par(mfrow=c(2,1))
plot(my.data,typ="l")
plot(my.data[1:24],typ="l")
# data shows clear seasonality but no variation in variance so differencing should be enough for taking care of the trend

acf(my.data,main='ACF')
pacf(my.data,main='PACF')
# ACF also shows seasonality

# Non-seasonal and seasonal differencing diff(diff(data),12)
plot(diff(diff(my.data),12),type="l")
# the plot shows almost no-trnds except for two large spikes at later times which may be a outliers => d=1, D=1 
#no need for transformation

par(mfrow=c(2,1))
acf(diff(diff(my.data),12),main='differnced data ACF',50)
pacf(diff(diff(my.data),12),main='differnced data PACF',50)
# both ACF and PACF don't show significant correlations in the initial part only for lag=1 => p=q=1
# there are siginificant correlations in the later part for values around the lag corresponding to the seasonality (lag=12 for ACF and lag=12,24,36 for PACF) => Q=0,1, P=0,1,2,3

# try for different values of p,q,P,Q and note down AIC, SSE and p-value (for Ljun-box-test). 
# We want high p-values and small AIC and SSE
# using parsimony priciple (simpler the better) while searching

d=1; DD=1; per=12

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:4){
      for(j in 1:3){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=my.data, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# lowest AIC of 1064.837 found for a 1,1,0,3,1,0 model which also has a large enough p-value

# find the coefficients
sarima(my.data,1,1,0,3,1,0,12)
