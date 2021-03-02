# time plot of data
plot(jj,main='Johnson & Johnson per share quaretely earning')

# 1. data shows an increasing trend => differencing required
# 2. indication of seasonality (we know this since it is quartely data)
# 3. variance seems to be increasing with time; log trandoformation to stabilize the variance
# => diff(log) -> log return transformation

plot(diff(log(jj)),main='Log Returns of Johnson & Johnson per share quaretely earning')
# still some variation in the variance but we are going to ignore that for now

par(mfrow=c(2,1))
acf(diff(log(jj)),main='Log Return ACF')
pacf(diff(log(jj)),main='Log Return PACF')

# ACF of log return data clearly shows seasonality = > seasonal differencing required diff(diff(log()),4)

# ACF of differenced data => q = 0,1, Q = 0,1, s=4
# PACF of differenced data => p = 0,1, P = 0,1

# SARIMA(p,1,q,P,1,Q) s=4 for log(data)
# arima(x=log(jj),order=c(p,1,q),seasonal=list(order=(P,1,Q),period=4))


# try for different values of p,q,P,Q and note down AIC, SSE and p-value (for Ljun-box-test). 
# We want high p-values and small AIC and SSE
# using parsimony priciple (simpler the better) while searching

d=1; DD=1; per=4

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:2){
      for(j in 1:2){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=log(jj), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}
# smallest AIC (and large p-value) found for p=0, q=1, P=1 and Q=0

# or use sarima directly to find the parameters
sarima(log(jj), 0,1,1,1,1,0,4)
