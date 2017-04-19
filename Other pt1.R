setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)
library(dplyr)

######################################################################################
################################# OTHERD DATA SETS ###################################
################################ THE USD DOLLAR INDEX ################################
############################## FLORIDA COINCIDENCE INDEX #############################
############################# INTERNATIONAL AIRFARE RATES ############################
######################################################################################

usdx <- read.csv("usdx.csv")
coin <- read.csv("coinc.csv")
airf <- read.csv("airfares.csv") 

head(usdx)
tail(usdx)

head(coin)
tail(coin)

head(airf)
tail(airf)

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = usdx$R.USDX, x = (usdx$YEAR + (usdx$MONTH/12)),
     ylab = 'USD Strength Index', 
     xlab = 'Time',
     main = 'The Strength of the Dollar Over Time',
     type = 'o')

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = coin$FL.COINCIDENT, x = (coin$YEAR + (coin$MONTH/12)),
     ylab = 'Coincidence Index', 
     xlab = 'Time',
     main = 'The Florida Coincidence Index', sub = 'A measure of the states economic strength',
     type = 'o')

win.graph(width = 10, height = 6, pointsize = 12)
plot(y = airf$INT.AIRFARES, x = (airf$YEAR + (airf$MONTH/12)),
     ylab = 'Airfare in USD', 
     xlab = 'Time',
     main = 'Airfare for International Flights',
     type = 'o')

## Basic Histograms & Normal Curves

x11()
usdx.hist <- hist(usdx$R.USDX, breaks = 10, density = 50, col = "gray", 
     xlab = "USD Index Frequency", main = "USDX Distribution")
xfit <- seq(min(usdx$R.USDX), max(usdx$R.USDX), length = 40)
yfit <- dnorm(xfit, mean = mean(usdx$R.USDX), sd = sd(usdx$R.USDX))
yfit <- yfit * diff(usdx.hist$mids[1:2]) * length(usdx$R.USDX)
lines(xfit, yfit, col = "black", lwd = 2) 
## This doesn't look even remotely normal and the curve barely fits

x11()
coin.hist <- hist(coin$FL.COINCIDENT, breaks = 10, density = 50, col = "gray", 
     xlab = "Coincidence Index Levels", main = "Frequency of Coincidence Index Levels")
xfit <- seq(min(coin$FL.COINCIDENT), max(coin$FL.COINCIDENT), length = 100)
yfit <- dnorm(xfit, mean = mean(coin$FL.COINCIDENT), sd = sd(coin$FL.COINCIDENT))
yfit <- yfit * diff(coin.hist$mids[1:2]) * length(coin$FL.COINCIDENT)
lines(xfit, yfit, col = "black", lwd = 2) 
## the distribution looks more uniform than anything else

x11()
airf.hist <- hist(airf$INT.AIRFARES, breaks = 10, density = 50, col = "gray", 
     xlab = "Airfare Frequency", main = "Distribution of International Airfare")
xfit <- seq(min(airf$INT.AIRFARES), max(airf$INT.AIRFARES), length = 40)
yfit <- dnorm(xfit, mean = mean(airf$INT.AIRFARES), sd = sd(airf$INT.AIRFARES))
yfit <- yfit * diff(airf.hist$mids[1:2]) * length(airf$INT.AIRFARES)
lines(xfit, yfit, col = "black", lwd = 2) 
## This distribution is bimodal as fuck

## Lets look at probablility desnity functions

x11()
usdx.d.hist <- hist(usdx$R.USDX, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "USDX Levels", ylab = "Prob",
                 main = "Distribution of USDX Levels")
lines(density(usdx$R.USDX), col = "blue")
lines(density(usdx$R.USDX, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)

x11()
coin.d.hist <- hist(coin$FL.COINCIDENT, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "Coincidence Index Levels", ylab = "Prob",
                 main = "Distribution of Coincidence Index Levels")
lines(density(coin$FL.COINCIDENT), col = "blue")
lines(density(coin$FL.COINCIDENT, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
## Really leaning towards uniform

x11()
airf.d.hist <- hist(airf$INT.AIRFARES, breaks = 20, col = "darkgray", prob = TRUE,
                 xlab = "Airfare Levels", ylab = "Prob",
                 main = "Distribution of Airfare")
lines(density(airf$INT.AIRFARES), col = "blue")
lines(density(airf$INT.AIRFARES, adjust = 2), lty = "dotted", col = "darkgreen", lwd = 2)
## Deffinitely bimodal