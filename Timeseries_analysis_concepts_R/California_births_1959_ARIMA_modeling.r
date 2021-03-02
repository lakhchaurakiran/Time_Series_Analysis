library(astsa)

# Read data to an R variable
birth.data<-read.csv('daily-total-female-births-CA.csv')

# Pull out births column
number_of_births<-birth.data$births

# use date format for dates
birth.data$date <- as.Date(birth.data$date,"%Y-%m-%d")

# plot the series
plot(number_of_births~birth.data$date,type="l",main='Daily total frmale births in California, 1959',ylab='Number of births')

# test for correlation Ljung-Box Q test
Box.test(number_of_births, lag=log(length(number_of_births)))

# Since the p-value is very very small there is defnite auto-correlation

# to remove the trend we plot the differenced time series
plot(diff(number_of_births)~birth.data$date[1:364],type="l",main='differenced series',xlab='Date',ylab='')

Box.test(diff(number_of_births), lag=log(length(number_of_births)))
# Again the p-value is very very small so there is defnite auto-correlation

acf(diff(number_of_births),main='ACF of differenced data',50)
pacf(diff(number_of_births),main='PACF of differenced data',50)
# ACF suggests significant auto-correlation at lag 1 (ignoring lag 21 as due to noise) (order of MA part =1) and PACF suggests significant correlations till lag 7

# Fit various ARIMA models
model1<-arima(number_of_births,order=c(0,1,1))
SSE1<-sum(model1$residuals^2)
model1.test<-Box.test(model1$residuals,lag=log(length(model1$residuals)))

model2<-arima(number_of_births,order=c(0,1,2))
SSE2<-sum(model2$residuals^2)
model2.test<-Box.test(model2$residuals,lag=log(length(model2$residuals)))

model3<-arima(number_of_births,order=c(7,1,1))
SSE3<-sum(model3$residuals^2)
model3.test<-Box.test(model3$residuals,lag=log(length(model3$residuals)))

model4<-arima(number_of_births,order=c(7,1,2))
SSE4<-sum(model4$residuals^2)
model4.test<-Box.test(model4$residuals,lag=log(length(model4$residuals)))

df<-data.frame(row.names=c('AIC','SSE','p-value'), c(model1$aic,SSE1,model1.test$p.value),
c(model2$aic,SSE2,model2.test$p.value),
c(model3$aic,SSE3,model3.test$p.value),
c(model4$aic,SSE4,model4.test$p.value))
colnames(df)<-c('ARIMA(0,1,1)','ARIMA(0,1,2)','ARIMA(7,1,1)','ARIMA(7,1,2)')
format(df, scientific=FALSE)

# we see that none of the model residuals have low p-values (we cannot reject the null hypothesis) therefore we can say that there is no auto-correlation in the residuals i.e. residuals are like white noise
# based on the minimum AIC value we choose the model ARIMA(0,1,2) even though ARIMA(7,1,2) has the lowest SSE value since we would like to prefer a simpler model.

# finally we use SARIMA model with the selected model to find the best-fit parameters
sarima(number_of_births,0,1,2,0,0,0)
# we get coefficients -0.8511 and -0.1113 for the two moving average terms, constant = 0.015, and sigma^2=49.08 as the variance of the innovations. Note that we also have differencing of degree one



