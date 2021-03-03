rm(list=ls(all=TRUE))
# London daily rainfall data in mm
rain.data <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1) 
rain.ts <- ts(rain.data, start=c(1813))

par( mfrow=c(1,2) )
hist(rain.data, main="Annual London Rainfall 1813-1912", xlab="rainfall in inches")
qqnorm(rain.data,main="Normal Plot of London Rainfall")
qqline(rain.data)
# no clear trend or seasonality

par( mfrow=c(2,1) )
plot.ts(rain.ts, main="Annual London Rainfall 1813-1912", xlab="year", ylab="rainfall in inches")
acf(rain.ts, main="ACF: London Rainfall")
# no obvious correlations

library(forecast)
auto.arima(rain.ts)
# even arima gives up

# Simple Exponential Smoothing (SES) method
# DIY code
alpha=.2   # some random value of alpha
#increase alpha for more rapid decay
forecast.values = NULL #establish array to store forecast values
n = length(rain.data) 
#naive first forecast
forecast.values [1] = rain.data[1] 
#loop to create all forecast values
for( i in 1:n ) { 
	forecast.values [i+1] = alpha*rain.data[i] + (1-alpha)* forecast.values [i] 
	}
paste("forecast for time",n+1," = ", forecast.values [n+1])


# choosing alpha
SSE=NULL
n = length(rain.data)
alpha.values = seq( .001, .999, by=0.001)
number.alphas = length(alpha.values)
for( k in 1:number.alphas ) { 
	forecast.values=NULL
	alpha = alpha.values[k]
	forecast.values[1] = rain.data[1] 
	for( i in 1:n ) { 
		forecast.values[i+1] = alpha*rain.data[i] + (1-alpha)*forecast.values[i] 
		} 
	SSE[k] = sum( (rain.data - forecast.values[1:n])^2 ) }
plot(SSE~alpha.values, main="Optimal alpha value Minimizes SSE")
plot(SSE[19:30]~alpha.values[19:30], main="Optimal alpha value Minimizes SSE")

index.of.smallest.SSE = which.min(SSE) #returns position 24
alpha.values[which.min(SSE)] #returns 0.024

alpha=0.024
#increase alpha for more rapid decay
forecast.values = NULL #establish array to store forecast values
n = length(rain.data) 
#naive first forecast
forecast.values [1] = rain.data[1] 
#loop to create all forecast values
for( i in 1:n ) { 
	forecast.values [i+1] = alpha*rain.data[i] + (1-alpha)* forecast.values [i] 
	}
paste("forecast for time",n+1," = ", forecast.values [n+1])

# using Holt-Winters for the same
HoltWinters(x = rain.ts, beta = FALSE, gamma = FALSE)