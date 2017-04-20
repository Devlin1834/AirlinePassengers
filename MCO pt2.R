setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)
library(dplyr)


################################################################################################
######################## MCO -- ORLANDO INTERNATIONAL AIRPORT -- Part 2 ########################
################################################################################################

# I have decided to do my project on International Passengers into MCO

mco <- read.csv("mco.csv")

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mco$INTERNATIONAL, x = (mco$Year + (mco$Month/12)),
     ylab = 'International Passengers into MCO', 
     xlab = 'Time',
     main = 'International Passengers into MCO over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = log(mco$INTERNATIONAL), x = (mco$Year + (mco$Month/12)),
     ylab = 'Log of Int. Passengers into MCO', 
     xlab = 'Time',
     main = 'Log Int. Passengers into MCO over Time',
     type = 'o')
# That scales it down a little bit

mco.int <- ts(mco$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
adf.test(mco.int, k = 12)
# Yeah thats non stationary 

mco.diff.int <- diff(mco.int)
adf.test(mco.diff.int, k = 12)
# The first difference seems to fix it

win.graph(width = 10, height = 6, pointsize = 12)
plot(mco.diff.int, type = 'o', main = 'First Difference International Passengers into MCO')
# looks like i might have an issue with non-constant variance here

# lets do a log transformations
l.mco.int <- log(mco.int)
win.graph(width = 10, height = 6, pointsize = 12)
plot(l.mco.int, type = 'o', main = 'Log International Passengers into MCO')
adf.test(l.mco.int, k = 12)
l.mco.diff.int <- diff(l.mco.int)
win.graph(width = 10, height = 6, pointsize = 12)
plot(l.mco.diff.int, type = 'o', main = 'First Difference Log International Passengers into MCO')
adf.test(l.mco.diff.int, k = 12)
# Log of Int. Passengers is non-stationlary, but the first difference is, and the variance seems constant

# l.mco.diff.int is too much to type, I'm renaming it LDIM
# LOG DIFFERENCED INTERNATIONAL MCO
LDIM <- l.mco.diff.int

# No lets start with some modeling
eacf(LDIM)
acf(LDIM, lag.max = 120)               # lag.max = 120 chosen to give 10 years of autocorrelations
x11()
aacf(LDIM, lag.max = 120)              
# the acf tails off and the pacf cuts off after lag 13

# I would like to try a seasonal difference
win.graph(height = 8, width = 16)
plot(y = mco.int, x = (mco$Year + (mco$Month/12)),
     ylab = 'International Passengers into MCO', 
     xlab = 'Time',
     main = 'International Passengers into MCO over Time',
     type = 'l')
points(y = mco.int, x = (mco$Year + (mco$Month/12)), pch = as.vector(season(mco.int)))
sub <- armasubsets(y = LDIM, nar = 12, nma = 12, y.name = 'int', ar.method = 'mle')
plot(sub)
# this indicates that only lag 12 and errors 1 and 12 are necessary for modeling this

LDIM.12 <- diff(LDIM, lag = 12)
plot(LDIM.12)
x11()
acf(LDIM.12, lag.max = 120, ci.type = 'ma')
x11()
pacf(LDIM.12, lag.max = 120, ci.type = 'ma')
eacf(LDIM.12, ar.max = 12, ma.max = 12)
# The EACF seems to suggest an MA(1) process
sub.12 <- armasubsets(y = LDIM.12, nar = 12, nma = 12, y.name = 'test', ar.method = 'mle')
plot(sub.12)
# this wants lag 10, errors 1 and 12

# Now lets find a decent model
# The differencing is done and the ACF and the PACF are stelling me a bunch of nonsense
# So i'm going to use for loops to brute force my way through it
# ARIMA(0,0,1:4)
for(i in 1:4){
	print(arima(LDIM.12, order = c(0,0,i), method = 'ML', include.mean = false))
}
# The intercepts are again, essentially zero
# ARIMA(1:4,0,0)
for(i in 1:4){
	print(arima(LDIM.12, order = c(i,0,0), method = 'ML', include.mean = FALSE))
}
# a lot of these models have insignifigant intercepts so I removed them
############################################################################################
###########################  MODEL BREAKDOWNS ##############################################
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#

### Lets take a closer look at the MA(1) ###
ma1 <- arima(LDIM.12, order = c(0,0,1), method = 'ML')
ma1
# the intercept is ridiculous
ma1.i <- arima(LDIM.12, order = c(0,0,1), method = 'ML', include.mean = FALSE)
ma1.i
t <- -.6128 / .0666
# That standard error is a bit nerve wracking
t

# Diagnostics
tsdiag(ma1.i)
# looks like a signifigant acf residual at 12
# The P-Values are pretty awful too
## REJECTED ##

### Lets Try and AR(1)
ar1 <- arima(LDIM.12, order = c(1,0,0), method = 'ML', include.mean = FALSE)
ar1

t <- -0.5094 / .0693
t
tsdiag(ar1)
## REJECTED ##

### Instead of working with the adjusted data, lets work with the base data and let ARIMA adjust it
int.ma1 <- arima(mco.int, order = c(0,1,1), 
		     seasonal = list(order = c(0,1,1), period = 12), 
		     method = 'ML')
int.ma1
t1 <- -0.6698 / 0.0631
t2 <- -0.4410 / 0.0739
t1
t2
x11()
tsdiag(int.ma1)
x11()
qqnorm(rstandard(int.ma1))
qqline(rstandard(int.ma1))
x11()
hist(residuals(int.ma1))
shapiro.test(residuals(int.ma1))
LB.test(int.ma1)
# this looks so much better!
# But the QQNorm is not so great
# and the shapiro test comes back  in favor of rejecting normality
# The histogram looks fine, though
# and it passes the LB test

int.ar1 <- arima(mco.int, order = c(1,1,0), 
		     seasonal = list(order = c(1,1,0), period = 12), 
		     method = 'ML')
int.ar1
tsdiag(int.ar1)
## REJECTED ##

int.arima1 <- arima(mco.int, order = c(1,1,1), 
		     seasonal = list(order = c(1,1,1), period = 12), 
		     method = 'ML')
int.arima1
tsdiag(int.arima1)
## REJECTED ##

int.ma2 <- arima(mco.int, order = c(0,1,2), 
		     seasonal = list(order = c(0,1,2), period = 12), 
		     method = 'ML')
int.ma2
tsdiag(int.ma2)
## The MA(2) coeficients are not signifigant

int.max <- arima(log(mco.int), order = c(2,1,1), 
		     seasonal = list(order = c(1,1,0), period = 12), 
		     method = 'ML')
int.max
x11()
tsdiag(int.max)
x11()
qqnorm(rstandard(int.max))
qqline(rstandard(int.max))
x11()
hist(residuals(int.max))
shapiro.test(residuals(int.max))
LB.test(int.max)
# I had high hopes for this one but it failed the LB test

####################################################################################################
############################ I have settled on an ARIMA(0,1,1)x(0,1,1)12 ###########################
# First I want to check to see just how bad the qqplot is
# I found the folowing function on Nate Millers Blog
# http://www.nate-miller.org/blog/how-normal-is-normal-a-q-q-plot-approach
# it is altered slightly to work with the arima function

qqfunc <- function(model, sigma){

N <- length(resid(model))
par(mfrow = c(3,3))
rnum <- sample(1:9, 1)
for(i in 1:(rnum-1)){
	x <- rnorm(N,0,sigma)
	qqnorm(x, main = i)
		qqline(x)
}
qqnorm(resid(model), main = rnum)
qqline(resid(model))
for(i in (rnum+1):9){
	x <- rnorm(N,0,sigma)
	qqnorm(x, main = i)
	qqline(x)
}
return(rnum)
}

qqfunc(int.ma1, sqrt(77480339))

# This compares our QQNorm to 8 other randomly generated plots with the same
# sample size and variance
# Mine is still fairly extreme, but I've seen worse
win.graph(height = 6, width = 12)
plot(int.ma1, n.ahead=24, n1 = c(2014, 1), type = 'b', 
     ylab = 'International Visitors', xlab = 'Year',
     main = 'Forecast of Interntaional Passengers')
abline(v=2016.58, lty = 'dashed', col = 'darkorange1')


### Finally, I saved this for the very end to check my work
### auto.arima seems a bit like cheating
library(forecast)
auto.arima(mco.int)

### I MIGHT BE A GENIUS I GOT IT RIGHT AND I AM SO EXCITED

### First, though, I want to try using log data to fix my residual issues
log.arima <- auto.arima(log(mco.int))
log.arima
x11()
tsdiag(log.arima)
x11()
qqnorm(rstandard(log.arima))
qqline(rstandard(log.arima))
x11()
hist(residuals(log.arima))
shapiro.test(residuals(log.arima))
LB.test(log.arima)
## This one fails box-peirce too
## and it has several insignifigant coeficients
## I am going to stick with the ARIMA(0,1,1)(0,1,1)[12]

## Since I am not log-transforming my data like I thought I would, 
## I need to reproduce my acf and pacf and eacf

## first difference, followed by a seasonal difference
diff.int <- diff(mco.int)
SDI <- diff(diff.int, lag = 12)
plot(SDI)
acf(SDI, lag.max = 120, main = 'Int. Passengers ACF', 
    sub = 'First Difference and a 12 Period Seasonal Difference')
x11()
pacf(SDI, lag.max = 120,  main = 'Int. Passengers PACF', 
     sub = 'First Difference and a 12 Period Seasonal Difference')
eacf(SDI)
