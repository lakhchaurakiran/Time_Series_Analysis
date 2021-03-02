phi1 = .5; phi2 = -.4; 

X.ts <- arima.sim(list(ar = c(phi1 , phi2)), n=1000) 

par(mfrow=c(2,1)) 

plot(X.ts,main=paste("AR(2) Time Series, phi1=",phi1, "phi2=", phi2)) 
X.acf = acf(X.ts, main="Autocorrelation of AR(2) Time Series")