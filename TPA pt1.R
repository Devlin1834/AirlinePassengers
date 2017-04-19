setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)

######################################################################################
######################### TPA -- TAMPA INTERNATIONAL AIRPORT #########################
###################################################################################### 

tpa <- read.csv("tpa.csv")
head(tpa)
tail(tpa)

win.graph(width = 6, height = 6, pointsize = 10)
plot(y = tpa$INTERNATIONAL, x = tpa$DOMESTIC,
     ylab = 'International Passengers', xlab = 'Domestic Passengers',
     main = 'Passengers Landing at TPA', type = 'p')

tpa.fit <- lm(tpa$INTERNATIONAL ~ tpa$DOMESTIC, data = tpa)
abline(tpa.fit)
summary(tpa.fit)

x11()
par(mfrow = c(2,2))
plot(tpa.fit)

## This is the weakest r-sq of the airports I've seen so far and that normal QQ is way too curvy

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = tpa$TOTAL, x = (tpa$Year + (tpa$Month/12)),
     ylab = 'Total Passengers into TPA', 
     xlab = 'Time',
     main = 'Passengers into TPA over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = tpa$DOMESTIC, x = (tpa$Year + (tpa$Month/12)),
     ylab = 'Domestic Passengers into TPA', 
     xlab = 'Time',
     main = 'Domestic Passengers into TPA over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = tpa$INTERNATIONAL, x = (tpa$Year + (tpa$Month/12)),
     ylab = 'International Passengers into TPA', 
     xlab = 'Time',
     main = 'International Passengers into TPA over Time',
     type = 'o')

## The total over time graph has a delightful curvature to it
## this curvature seems to come from dometsic passengers, international passengers have been steadily rising
## There are also come crazy outliers on the international passengers graph

## Lets look and see the lag function

win.graph(width = 6, height = 6, pointsize = 12)
plot(y = tpa$TOTAL, x = zlag(tpa$TOTAL),
     ylab = 'Total Passengers into TPA', 
     xlab = 'Passengers Last Period',
     main = 'Passengers into TPA and a Time Lag',
     type = 'p')

## Its a weaker fit than the original, I wont bother regressing it.
## Lets tackle some shapes

x11()
tpa.d.hist <- hist(tpa$DOMESTIC, breaks = 10, density = 50, col = "gray", 
     xlab = "Domestic Passenger Frequency", main = "Distribution of Domestic Passnegers")
xfit <- seq(min(tpa$DOMESTIC), max(tpa$DOMESTIC), length = 40)
yfit <- dnorm(xfit, mean = mean(tpa$DOMESTIC), sd = sd(tpa$DOMESTIC))
yfit <- yfit * diff(tpa.d.hist$mids[1:2]) * length(tpa$DOMESTIC)
lines(xfit, yfit, col = "black", lwd = 2) 

## This is pretty close to a normal distribution, even thought it looks skewed to the right

x11()
tpa.i.hist <- hist(tpa$INTERNATIONAL, breaks = 10, density = 50, col = "gray", 
     xlab = "International Passenger Frequency", main = "Distribution of International Passnegers")
xfit <- seq(min(tpa$INTERNATIONAL), max(tpa$INTERNATIONAL), length = 40)
yfit <- dnorm(xfit, mean = mean(tpa$INTERNATIONAL), sd = sd(tpa$INTERNATIONAL))
yfit <- yfit * diff(tpa.i.hist$mids[1:2]) * length(tpa$INTERNATIONAL)
lines(xfit, yfit, col = "black", lwd = 2) 

## Well this is interesting, lets look at another histogram

x11()
tpa.d.d.hist <- hist(tpa$DOMESTIC, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "Domestic Passengers", ylab = "Prob",
                 main = "Distribution with Density Curve")
lines(density(tpa$DOMESTIC), col = "blue")
lines(density(tpa$DOMESTIC, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
## Any doubts I had about normality are relived now, the domestic side of the data is fine

x11()
tpa.i.d.hist <- hist(tpa$INTERNATIONAL, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "International Passengers", ylab = "Prob",
                 main = "Distribution with Density Curve")
lines(density(tpa$INTERNATIONAL), col = "blue")
lines(density(tpa$INTERNATIONAL, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)

## The international passengers still has a strong right skew, lets investigate that some more
x11()
descdist(tpa$INTERNATIONAL, discrete = FALSE, boot = 1000)
## This looks to be another log-normal distribution. lets check
tpa.fit.test1 <- fitdist(tpa$INTERNATIONAL, "lnorm")
x11()
plot(tpa.fit.test1)
## Yup, that looks to be it again
x11()
tpa.i.log.hist <- hist(log(tpa$INTERNATIONAL), breaks = 25, col = "darkgray", prob = TRUE,
                 xlab = "Log International Passengers", ylab = "Prob",
                 main = "Log Distribution with Density Curve")
lines(density(log(tpa$INTERNATIONAL)), col = "blue")
lines(density(log(tpa$INTERNATIONAL), adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
tpa.fit.test2 <- fitdist(log(tpa$INTERNATIONAL), "norm")
x11()
plot(tpa.fit.test2)

## YEAH BAYBEY YEAH
## https://www.youtube.com/watch?v=J2HBdRCroks
## That almost became perfectly normal

## Lets regress this again because the first time sucked - thats what she said

tpawlog.int <- tpa
tpawlog.int[,4] <- log(tpa[,4])

tpa.log.int <- tpawlog.int[,4]
tpa.dom <- tpa[,3]

win.graph(width = 6, height = 6, pointsize = 10)
plot(y = tpa.log.int, x = tpa.dom, ylab = 'log(International Passengers)',
     xlab = 'Domestic Passengers', main = 'Passengers landing at TPA', type = 'p')
tpa.log.fit <- lm(tpa.log.int ~ tpa.dom, data = tpa)
summary(tpa.log.fit)
abline(tpa.log.fit)

## This is a much slightly better fit than the one before

######################################################################################
#################################### TPA FINDINGS ####################################
######################################################################################

## 1. International passengers are not explained well by domestic passengers
##### R-squared   .2382
##### F-statistic 51.6

## 2. Domestic passengers are distributed approximately normal

## 3. International passengers are distributed approximately log-normal
##### When the data-set is log-transformed it becomes almost perfectly normal
