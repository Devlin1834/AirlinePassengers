setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)
library(dplyr)

######################################################################################
############################ DOMESTIC PASSENGERS ANALYSIS ############################
###################################################################################### 

mco <- read.csv("mco.csv")
mia <- read.csv("mia.csv")
tpa <- read.csv("tpa.csv")
fll <- read.csv("fll.csv")

YEAR <- mco[,1]
MONTH <- mco[,2]
MCO.DOMESTIC <- mco[,3]
MIA.DOMESTIC <- mia[,3]
TPA.DOMESTIC <- tpa[,3]
FLL.DOMESTIC <- fll[,3]

DOM <- data.frame(YEAR, MONTH, MCO.DOMESTIC, MIA.DOMESTIC, TPA.DOMESTIC, FLL.DOMESTIC)
DOM$TOTAL <- rowSums(DOM[,3:6])
head(DOM)
tail(DOM)

win.graph(width = 12, height = 6)
plot(y = DOM$TOTAL, x = (DOM$YEAR + (DOM$MONTH/12)),
     ylab = 'Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

win.graph(width = 12, height = 6)
plot(y = DOM$MCO.DOMESTIC, x = (DOM$YEAR + (DOM$MONTH/12)),
     ylab = 'Domestic Passengers', xlab = 'Year',
     main = 'Domestic Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(min(DOM[,3:6]), max(DOM[,3:6])))
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = DOM$MIA.DOMESTIC, col = 'red')
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = DOM$TPA.DOMESTIC, col = 'green')
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = DOM$FLL.DOMESTIC, col = 'blue')

## So this is an interesting onbservation
## Orlando takes in the most Domestic Passengers, the other three take in roughly the same ammount

## Lets look at a log transformation
win.graph(width = 12, height = 6)
plot(y = log(DOM$MCO.DOMESTIC), x = (DOM$YEAR + (DOM$MONTH/12)),
     ylab = 'log(Domestic Passengers)', xlab = 'Year',
     main = 'Domestic Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(min(log(DOM[,3:6])), max(log(DOM[,3:6]))))
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = log(DOM$MIA.DOMESTIC), col = 'red')
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = log(DOM$TPA.DOMESTIC), col = 'green')
lines(x = (DOM$YEAR + (DOM$MONTH/12)), y = log(DOM$FLL.DOMESTIC), col = 'blue')

## Thats a downer, it looks pretty much the same

DOM.TS <- ts(DOM$TOTAL, start = c(2002,10), end = c(2016,8), frequency = 12)
win.graph(width = 12, height = 6)
plot(y = DOM.TS, x = (DOM$YEAR + (DOM$MONTH/12)),
     ylab = 'Total Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'l')
points(y = DOM.TS, x = (DOM$YEAR + (DOM$MONTH/12)), pch = as.vector(season(DOM.TS)))

## Now a lag

DOM.l1.fit <- lm(DOM$TOTAL ~ zlag(DOM$TOTAL), data = DOM)
win.graph(width = 10, height = 10)
plot(y = DOM$TOTAL, x = zlag(DOM$TOTAL), xlab = 'Last Periods Total Dom. Passengers',
	ylab = 'Total Dom Passengers')
abline(DOM.l1.fit)
summary(DOM.l1.fit)

#### This is a log of DOM TOTAL
win.graph(width = 12, height = 6)
plot(y = log(DOM$TOTAL), x = (DOM$YEAR + (DOM$MONTH/12)),
     ylab = 'Log of Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

#### First Difference of DOM TOTAL
DOM.diff <- diff(DOM$TOTAL)
DOM.f.diff <- DOM[-1,]
win.graph(width = 12, height = 6)
plot(y = DOM.diff, x = (DOM.f.diff$YEAR + (DOM.f.diff$MONTH/12)),
     ylab = 'First Difference of Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

#### Log of First Difference
win.graph(width = 12, height = 6)
plot(y = diff(log(DOM$TOTAL)), x = (DOM.f.diff$YEAR + (DOM.f.diff$MONTH/12)),
     ylab = 'First Difference of Log Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

### The first difference seems satisfactory enough

x11()
acf(DOM$TOTAL, lag.max = 120)
x11()
acf(DOM.diff, lag.max = 120)
x11()
pacf(DOM$TOTAL, lag.max = 120)
x11()
pacf(DOM.diff, lag.max = 120)

plot(DOM.TS)
dec.DOM <- decompose(DOM.TS)
plot(dec.DOM)

adf.test(DOM$TOTAL, k = 12)
adf.test(DOM.diff, k = 12)
## Neither of these test as non-stationary

DOM.diff2 <- diff(DOM$TOTAL, differences = 2)
MY <- DOM[,1:2]
MY <- head(MY, -2)

adf.test(DOM.diff2, k = 12)
## Two differences removes the stationarity

win.graph(width = 12, height = 6)
plot(y = DOM.diff2, x = (MY$YEAR + (MY$MONTH/12)),
     ylab = 'Second Difference of Domestic Passengers', xlab = 'Year',
     main = 'Total Domestic Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

eacf(DOM.diff2)

x11()
acf(DOM.diff, lag.max = 120)
x11()
pacf(DOM.diff, lag.max = 120)

## The ACF tails off and the PACF cuts off indicating an AR model of order 12
## Gonna rename DOM.diff2 SDD for second difference deomestic

SDD <- DOM.diff2

SDD.ar <- arima(SDD, order = c(12, 0, 0), method = 'ML')
SDD.ar

## All the coeficients are signifigant

x11()
plot(rstandard(SDD.ar))
x11()
hist(rstandard(SDD.ar), breaks = 30)
x11()
qqnorm(residuals(SDD.ar))
qqline(residuals(SDD.ar))

## Thats a beautiful QQ Norm plot

x11()
acf(residuals(SDD.ar))
x11()
pacf(residuals(SDD.ar))

win.graph(width = 10, height = 10)
plot(y = diff(DOM$TOTAL, differences = 2), x = zlag(diff(DOM$TOTAL, differences = 2)), xlab = 'Last Periods Total Dom. Passengers',
	ylab = 'Total Dom. Passengers')
abline(SDD.ar)

## Not a bad fit at all boys

###############################################################################################################
#                                                                                                             #
#                 ########  #########  #     #  ####   #########  #     #    ####     ####                    #
#                 #             #      ##    #  #   #      #      ##    #   #    #   #    #                   #
#                 #             #      # #   #  #    #     #      # #   #  #      #  #                        #
#                 ########      #      #  #  #  #    #     #      #  #  #  #          ####                    #
#                 #             #      #   # #  #    #     #      #   # #  #    ###       #                   #
#                 #             #      #    ##  #   #      #      #    ##   #    #   #    #                   #
#                 #         #########  #     #  ####   #########  #     #    ####     ####                    #
#                                                                                                             #
###############################################################################################################

#    Orlando takes in more Domestic Passengers than the other three, with the other three being about tied

#    Domestic Passsengers is modeled fairly well with an ARI(12, 2) model

