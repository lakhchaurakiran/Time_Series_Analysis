 # fitting AR model using Yule-Walker method for Johnson & Johnson earnings (dollars) per share for the years between 1960-1980

# Time plot for the data
plot(jj,main='Johnson&Johnson earnings per share',col='blue',lwd=3)

# the data shows clearly increasing trend with the variance also increasing with time, we will have to transform this data to make it weakly stationary


# Log-return of the data
jj.log.return = diff(log(jj))
jj.log.return.mean.zero = jj.log.return-mean(jj.log.return)

# Plots for log returns
par(mfrow=c(3,1))
plot(jj.log.return.mean.zero,main='Log-return (mean zero) Johnson&Johnson earnings per share',col='blue',lwd=3)
acf(jj.log.return.mean.zero,main='ACF')
pacf(jj.log.return.mean.zero,main='PACF')

# PACF plot shows significant values till lag=4 so we will use order=4 for the AR model
p = 4

# Sample auto-correlation function r-
r=NULL
r[1:p] = acf(jj.log.return,plot=F)$acf[2:(p+1)]
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
c0 = acf(jj.log.return, type='covariance',plot=F)$acf[1]
print(c0)
var.hat = c0*(1-sum(phi.hat*r))
print(var.hat)

# constant term in the model
phi0.hat=mean(jj.log.return)*(1-sum(phi.hat))
print(phi0.hat)

cat("constant:", phi0.hat," coefficients:",phi.hat," and variance:",var.hat,"\n")
