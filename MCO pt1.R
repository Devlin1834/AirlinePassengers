setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)

######################################################################################
######################## MCO -- ORLANDO INTERNATIONAL AIRPORT ########################
######################################################################################


mco <- read.csv("mco.csv")
head(mco)
tail(mco)

win.graph(width = 6, height = 6, pointsize = 10)
plot(y = mco$INTERNATIONAL, x = mco$DOMESTIC, 
     ylab = 'International Passengers', xlab = 'Domestic Passengers',
     main = 'Passengers Landing at MCO', type = 'p')

mco.fit <- lm(mco$INTERNATIONAL ~ mco$DOMESTIC, data = mco)
abline(mco.fit)
summary(mco.fit)
x11()
par(mfrow = c(2,2))
plot(mco.fit)

## This is pretty interesting, domestic passengers predicts international passengers
#  fairly well. However, the normal QQ plot is not staright enough for me

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mco$TOTAL, x = (mco$Year + (mco$Month/12)),
     ylab = 'Total Passengers into MCO', 
     xlab = 'Time',
     main = 'Passengers into MCO over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mco$DOMESTIC, x = (mco$Year + (mco$Month/12)),
     ylab = 'Domestic Passengers into MCO', 
     xlab = 'Time',
     main = 'Domestic Passengers into MCO over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mco$INTERNATIONAL, x = (mco$Year + (mco$Month/12)),
     ylab = 'International Passengers into MCO', 
     xlab = 'Time',
     main = 'International Passengers into MCO over Time',
     type = 'o')

## Notice how Domestic Passengers remain fairly consistent but International Passengers
#  has grown immensley

## This is a beautiful time series, lets look at a lag

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mco$TOTAL, x = zlag(mco$TOTAL),
     ylab = 'Total Passengers into MCO', 
     xlab = 'Passengers Last Period',
     main = 'Passengers into MCO and a Time Lag',
     type = 'p')

mco.l1.fit <- lm(mco$TOTAL ~ zlag(mco$TOTAL), data = mco)
abline(mco.l1.fit)
summary(mco.l1.fit)
x11()
par(mfrow = c(2,2))
plot(mco.l1.fit)

## Somehow to the normal QQ plot got worse
## Lets look at a histogram of mco$DOMESTIC
x11()
mco.d.hist <- hist(mco$DOMESTIC, breaks = 13, density = 10, col = "gray", 
     xlab = "Domestic Passenger Frequency", main = "Distribution of Domestic Passnegers")
xfit <- seq(min(mco$DOMESTIC), max(mco$DOMESTIC), length = 40)
yfit <- dnorm(xfit, mean = mean(mco$DOMESTIC), sd = sd(mco$DOMESTIC))
yfit <- yfit * diff(mco.d.hist$mids[1:2]) * length(mco$DOMESTIC)
lines(xfit, yfit, col = "black", lwd = 2) 

## Looks normal enough
x11()
mco.i.hist <- hist(mco$INTERNATIONAL, breaks = 10, density = 10, col = "gray", 
     xlab = "International Passenger Frequency", main = "Distribution with Normal Curve")
xfit <- seq(min(mco$INTERNATIONAL), max(mco$INTERNATIONAL), length = 40)
yfit <- dnorm(xfit, mean = mean(mco$INTERNATIONAL), sd = sd(mco$INTERNATIONAL))
yfit <- yfit * diff(mco.i.hist$mids[1:2]) * length(mco$INTERNATIONAL)
lines(xfit, yfit, col = "black", lwd = 2) 
x11()
mco.i.d.hist <- hist(mco$INTERNATIONAL, breaks = 12, col = "darkgray", prob = TRUE,
                 xlab = "International Passengers", ylab = "Prob",
                 main = "Distribution with Density Curve")
lines(density(mco$INTERNATIONAL), col = "blue")
lines(density(mco$INTERNATIONAL, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)

## This one looks more like an F-distribution
## Checking a Cullen and Frey Graph, it looks to be more of a Beta Distribution
descdist(mco$INTERNATIONAL, discrete = FALSE, boot = 1000)
## After reading some about the beta distribution, I doubt the data falls there
plotdist(mco$INTERNATIONAL, hist = TRUE, demp = TRUE)
## Surprisingly the log-normal distribution seems to fit fairly well
mco.fit.test <- fitdist(mco$INTERNATIONAL, "lnorm")
plot(mco.fit.test)

## This means we should be able to do this and see a normal dist
mco.i.log.hist <- hist(log(mco$INTERNATIONAL), breaks = 25, col = "darkgray", prob = TRUE,
                 xlab = "Log International Passengers", ylab = "Prob",
                 main = "Log Distribution with Density Curve")
lines(density(log(mco$INTERNATIONAL)), col = "blue")
lines(density(log(mco$INTERNATIONAL), adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
mco.fit.test2 <- fitdist(log(mco$INTERNATIONAL), "norm")
x11()
plot(mco.fit.test2)

######################################################################################
#################################### MCO FINDINGS ####################################
######################################################################################

## 1. Domestic passengers doesnt explain a lot of variation in international passengers
###### R-squared   .2361
###### F-statistic 50.99

## 2. Domestic Passengers are distributed approximately normal

## 3. International Passengers are distribute approximately log-normal
###### This means we take the log of the data-set to make it normal
###### HOWEVER, when we do this the data-set becomes BIMODAL
###### Also worth noting that the QQ plot is still curvy
