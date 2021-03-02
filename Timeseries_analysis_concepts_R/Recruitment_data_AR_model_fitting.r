# Edit the following line for different datasets
my.data = rec

# Plot rec
plot(rec,main='Recruitment Time Series',col='blue', lwd=3)

# Subtract mean to get a time-series with mean zero
ar.process = my.data-mean(my.data)

# ACF and PACF of the process
par(mfrow=c(2,1))
acf(ar.process,main='Recruitment ACF',col='red',lwd=3)
pacf(ar.process,main='Recruitment PACF',col='green',lwd=3)

# PACF cuts off at lag=2 so we will use order=2 for the AR model
p =2

# Sample auto-correlation function r-
r=NULL
r[1:p] = acf(ar.process,plot=F)$acf[2:(p+1)]
cat('r-',r,'\n')

# matrix R-
R = matrix(1,p,p)

for(i in 1:p){
	for(j in 1:p){
		if(i!=j)
			R[i,j]=r[abs(i-j)]
	}
}
print(R)

# b column-vector on the right
b=NULL
b=matrix(r,p,1)
print(b)

# solve(R,b) solves Rx=b and gives x=R^(-1)b vector
phi.hat = solve(R,b)[,1]
print(phi.hat)

# variance estimator using Yule-Walker estimator
c0 = acf(ar.process, type='covariance',plot=F)$acf[1]
print(c0)
var.hat = c0*(1-sum(phi.hat*r))
print(var.hat)

# constant term in the model
phi0.hat=mean(my.data)*(1-sum(phi.hat))
print(phi0.hat)


cat("constant:", phi0.hat," coefficients:",phi.hat," and variance:",var.hat,"\n")