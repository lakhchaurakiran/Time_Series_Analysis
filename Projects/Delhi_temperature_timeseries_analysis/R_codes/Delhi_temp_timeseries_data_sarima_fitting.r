library(zoo)
library(tidyr)
library(dplyr)
library(lubridate)

# reading the data
DelhiTemp <- read.csv(file = 'Data/Delhi_temperature_1995_2020.csv')

# plotting the data
Data <- DelhiTemp$Temperature
plot(Data)

# creating a date column from Month, Day and Year columns
DelhiTemp$Date <- as.Date(with(DelhiTemp, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

# Temperature vs. Date plot
Temp <- DelhiTemp$Temperature
Date <- DelhiTemp$Date
plot(Temp~Date)

# identifying outliers and replacing with backfilled values
DelhiTemp$Temperature[DelhiTemp$Temperature < -50] <- NA
which(is.na(DelhiTemp$Temperature))
DelhiTemp$Temperature <- na.locf(DelhiTemp$Temperature, fromLast = TRUE)
which(is.na(DelhiTemp$Temperature))

# updated Temperature vs. Date plot
Temp <- DelhiTemp$Temperature
Date <- DelhiTemp$Date
plot(Temp~Date)

# binning monthly temperatures to their mean values
DelhiMonthlyTemp <- DelhiTemp %>% group_by(Year,Month) %>% summarize(Temperature = mean(Temperature))
head(DelhiMonthlyTemp)
DelhiMonthlyTemp$Date <- as.yearmon(paste(DelhiMonthlyTemp$Year, DelhiMonthlyTemp$Month), "%Y %m")
plot(DelhiMonthlyTemp$Temperature~DelhiMonthlyTemp$Date,type="l")

DelhiMonthlyTempTS <- ts(DelhiMonthlyTemp)
plot(DelhiMonthlyTempTS)
# data shows clear seasonality but no variation in variance so differencing should be enough for taking care of the trend

par(mfrow=c(2,1))
acf(DelhiMonthlyTemp$Temperature,main='ACF')
pacf(DelhiMonthlyTemp$Temperature,main='PACF')
# ACF also shows seasonality

# Non-seasonal and seasonal differencing diff(diff(data),12)
plot(diff(diff(DelhiMonthlyTemp$Temperature),12),type="l")
# the plot shows almost no-trends except for a few large peaks at the centre which may be outliers
# => d=1, D=1

par(mfrow=c(2,1))
acf(diff(diff(DelhiMonthlyTemp$Temperature),12),main='differnced data ACF',50)
pacf(diff(diff(DelhiMonthlyTemp$Temperature),12),main='differnced data PACF',50)
# the acf plot shows significant correlations at lag=1,11 and 12 while the 
# pacf shows significant correlation for lag=1,11 and 12
# The correlations at later parts may be due to seadonality
# => Q = 0,1 P=0,1,2,3,4,5

# trying for different values of p,q,P,Q and note down AIC, SSE and p-value (for Ljun-box-test). 
# We want high p-values and small AIC and SSE
# using parsimony priciple (simpler the better) while searching

d=1; DD=1; per=12

for(p in 1:2){
  for(q in 1:2){
    for(i in 1:6){
      for(j in 1:3){
        if(p+d+q+i+DD+j<=10){
          model<-arima(x=DelhiMonthlyTemp$Temperature, order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

# lowest AIC of 1221.998 found for a 1,1,1,0,1,1,12 model which also has a large enough p-value

library(astsa)
library(forecast)
sarima(DelhiMonthlyTemp$Temperature,1,1,1,0,1,1,12)

model<-arima(x=DelhiMonthlyTemp$Temperature, order = c(1,1,1), seasonal = list(order=c(0,1,1), period=per))
par(mfrow=c(1,1))
plot(forecast(model))