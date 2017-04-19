setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)

######################################################################################
######################### FLL -- TAMPA INTERNATIONAL AIRPORT #########################
###################################################################################### 

fll <- read.csv("fll.csv")
head(fll)
tail(fll)

win.graph(width = 6, height = 6, pointsize = 10)
plot(y = fll$INTERNATIONAL, x = fll$DOMESTIC,
     ylab = 'International Passengers', xlab = 'Domestic Passengers',
     main = 'Passengers Landing at FLL', type = 'p')

fll.fit <- lm(fll$INTERNATIONAL ~ fll$DOMESTIC, data = fll)
abline(fll.fit)
summary(fll.fit)

x11()
par(mfrow = c(2,2))
plot(fll.fit)

## This regression isnt as bad as some of the others but its still not great
## QQ plot looks fine though

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = fll$TOTAL, x = (fll$Year + (fll$Month/12)),
     ylab = 'Total Passengers into FLL', 
     xlab = 'Time',
     main = 'Passengers into FLL over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = fll$DOMESTIC, x = (fll$Year + (fll$Month/12)),
     ylab = 'Domestic Passengers into FLL', 
     xlab = 'Time',
     main = 'Domestic Passengers into FLL over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = fll$INTERNATIONAL, x = (fll$Year + (fll$Month/12)),
     ylab = 'International Passengers into FLL', 
     xlab = 'Time',
     main = 'International Passengers into FLL over Time',
     type = 'o')

## HISTOGRAMS

x11()
fll.d.hist <- hist(fll$DOMESTIC, breaks = 10, density = 50, col = "gray", 
     xlab = "Domestic Passenger Frequency", main = "Distribution of Domestic Passnegers")
xfit <- seq(min(fll$DOMESTIC), max(fll$DOMESTIC), length = 40)
yfit <- dnorm(xfit, mean = mean(fll$DOMESTIC), sd = sd(fll$DOMESTIC))
yfit <- yfit * diff(fll.d.hist$mids[1:2]) * length(fll$DOMESTIC)
lines(xfit, yfit, col = "black", lwd = 2) 

## Remarkable normal

x11()
fll.i.hist <- hist(fll$INTERNATIONAL, breaks = 10, density = 50, col = "gray", 
     xlab = "International Passenger Frequency", main = "Distribution of International Passnegers")
xfit <- seq(min(fll$INTERNATIONAL), max(fll$INTERNATIONAL), length = 40)
yfit <- dnorm(xfit, mean = mean(fll$INTERNATIONAL), sd = sd(fll$INTERNATIONAL))
yfit <- yfit * diff(fll.i.hist$mids[1:2]) * length(fll$INTERNATIONAL)
lines(xfit, yfit, col = "black", lwd = 2)

## International passengers seem kinda backloaded.

x11()
fll.i.d.hist <- hist(fll$INTERNATIONAL, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "International Passengers", ylab = "Prob",
                 main = "Distribution with Density Curve")
lines(density(fll$INTERNATIONAL), col = "blue")
lines(density(fll$INTERNATIONAL, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)

## Yeah, that aint normal

x11()
descdist(fll$INTERNATIONAL, discrete = FALSE, boot = 1000)
## It is surprisingly close to normal but I'm going to keep punching
## lets check the log normal
fll.fit.test1 <- fitdist(fll$INTERNATIONAL, "lnorm")
x11()
plot(fll.fit.test1)
## that actually doesnt look like it
fll.fit.test2 <- fitdist(fll$INTERNATIONAL, "norm")
x11()
plot(fll.fit.test2)
## I think this concludes that international passengers are distributed approximately normal
## The density curve still doesnt look normal to me, though.
## Lets log-transform it anyways and see what happens

fll.log.fit.test <- fitdist(log(fll$INTERNATIONAL), "norm")
x11()
plot(fll.log.fit.test)

## That made it sooooo much worse
## I might just have a weird normal distribution 

######################################################################################
#################################### FLL FINDINGS ####################################
######################################################################################

## 1. Domestic Passengers is a rather poor predictor of International Passengers
##### R-squared   .4446
##### F-statictic 132.1

## 2. However, both are approximately normal
##### International passengers is a pain to fit to a distribution, its barely normal


