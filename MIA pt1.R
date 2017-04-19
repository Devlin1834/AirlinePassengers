setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)

######################################################################################
######################### MIA -- MIAMI INTERNATIONAL AIRPORT #########################
###################################################################################### 

mia <- read.csv("mia.csv")
head(mia)
tail(mia)

win.graph(width = 6, height = 6, pointsize = 10)
plot(y = mia$INTERNATIONAL, x = mia$DOMESTIC,
     ylab = 'International Passengers', xlab = 'Domestic Passengers',
     main = 'Passengers Landing at MIA', type = 'p')

mia.fit <- lm(mia$INTERNATIONAL ~ mia$DOMESTIC - 1, data = mia)
# The intercept wasnt signifigant to the fit, it has been removed with '-1'

abline(mia.fit)
summary(mia.fit)

x11()
par(mfrow = c(2,2))
plot(mia.fit)

## Its a pretty strong relationship, unfortunately its not very interesting
## The intuition would be that miami appeals to domestic travelers for the same reason it appeals
#  to inteernational travelers, thus when those factors change positively or negatively it
#  affects both groups evenly.
## The QQ Plot is also a wee bit curvy

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mia$TOTAL, x = (mia$Year + (mia$Month/12)),
     ylab = 'Total Passengers into MIA', 
     xlab = 'Time',
     main = 'Passengers into MIA over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mia$DOMESTIC, x = (mia$Year + (mia$Month/12)),
     ylab = 'Domestic Passengers into MIA', 
     xlab = 'Time',
     main = 'Domestic Passengers into MIA over Time',
     type = 'o')
win.graph(width = 10, height = 6, pointsize = 12)
plot(y = mia$INTERNATIONAL, x = (mia$Year + (mia$Month/12)),
     ylab = 'International Passengers into MIA', 
     xlab = 'Time',
     main = 'International Passengers into MIA over Time',
     type = 'o')

## Just like MCO, these are beautiful time series

win.graph(width = 6, height = 6, pointsize = 12)
plot(y = mia$TOTAL, x = zlag(mia$TOTAL),
     ylab = 'Total Passengers into MIA', 
     xlab = 'Passengers Last Period',
     main = 'Passengers into MIA and a Time Lag',
     type = 'p')
## It looks the same! EVEN WITH LAG!
## I'm not going to bother regressing with the lag, I'm more interested in shape at this point

x11()
mia.d.hist <- hist(mia$DOMESTIC, breaks = 10, density = 50, col = "gray", 
     xlab = "Domestic Passenger Frequency", main = "Distribution of Domestic Passnegers")
xfit <- seq(min(mia$DOMESTIC), max(mia$DOMESTIC), length = 40)
yfit <- dnorm(xfit, mean = mean(mia$DOMESTIC), sd = sd(mia$DOMESTIC))
yfit <- yfit * diff(mia.d.hist$mids[1:2]) * length(mia$DOMESTIC)
lines(xfit, yfit, col = "black", lwd = 2) 

## Domestic passengers is normal enough, again

mia.i.hist <- hist(mia$INTERNATIONAL, breaks = 10, density = 50, col = "gray", 
     xlab = "International Passenger Frequency", main = "Distribution of International Passnegers")
xfit <- seq(min(mia$INTERNATIONAL), max(mia$INTERNATIONAL), length = 40)
yfit <- dnorm(xfit, mean = mean(mia$INTERNATIONAL), sd = sd(mia$INTERNATIONAL))
yfit <- yfit * diff(mia.i.hist$mids[1:2]) * length(mia$INTERNATIONAL)
lines(xfit, yfit, col = "black", lwd = 2) 

## That looks WAAAAY more normal than MCO's, but it still has a slight second peak, lets dive in

x11()
mia.i.d.hist <- hist(mia$INTERNATIONAL, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "International Passengers", ylab = "Prob",
                 main = "Distribution with Density Curve")
lines(density(mia$INTERNATIONAL), col = "blue")
lines(density(mia$INTERNATIONAL, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)

## After plotting the density curves and increasing the breaks, its still approx normal!
## Lets do some analysis to be certain

descdist(mia$INTERNATIONAL, discrete = FALSE, boot = 1000)
plotdist(mia$INTERNATIONAL, histp = TRUE, demp = TRUE)
mia.fit.test <- fitdist(mia$INTERNATIONAL, "norm")
plot(mia.fit.test)
## Yup normal

######################################################################################
#################################### MIA FINDINGS ####################################
######################################################################################

## 1. Domestic Passengers preditc International Passengers extremely well!
###### R-squared   .9886
###### F-statistic 14400
###### The intuition would be that miami appeals to domestic travelers for the same reason it appeals
####   to inteernational travelers, thus when those factors change positively or negatively it
####   affects both groups evenly.

## 2. Domestic Passengers are distributed approximately normal

## 3. International Passengers are distributed approximately normal
