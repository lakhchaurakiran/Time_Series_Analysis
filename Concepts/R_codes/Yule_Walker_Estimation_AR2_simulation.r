# Set Seed
set.seed(2017)

# Model Parameters
sigma=4
phi=NULL
phi[1:2]=c(1/3,1/2)
print(phi)

# Number of data points
n=10000

# Simulate AR process
ar.process = arima.sim(n,model=list(ar=c(1/3,1/2)),sd=4)
print(ar.process[1:5])

# Find and name 2nd and 3rd sample auto-correlation
r=NULL
r[1:2]=acf(ar.process,plot=F)$acf[2:3]
print(r)

# Matrix R
R=matrix(1,2,2)
R[1,2]=r[1]
R[2,1]=r[1]
print(R)

# b column vector
b=matrix(r,nrow=2,ncol=1)
print(b)

# solve(R,b) solves Rx=b and gives x=R^(-1)b vector
phi.hat = solve(R,b)
print(phi.hat)

# Estimating vatiance of the innovation term in the AR process
c0 = acf(ar.process,type='covariance',plot=F)$acf[1]
var.hat = c0*(1-sum(phi.hat*r))
print(var.hat)

# Plot time series along with ACF, PACF
par(mfrow=c(3,1))
plot(ar.process,main='Simulated AR(2)')
acf(ar.process,main='ACF')
pacf(ar.process,main='PACF')