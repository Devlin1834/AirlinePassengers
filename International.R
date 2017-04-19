setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)
library(dplyr)

######################################################################################
######################### INTERNATIONAL PASSENGERS ANALYSIS ##########################
###################################################################################### 

mco <- read.csv("mco.csv")
mia <- read.csv("mia.csv")
tpa <- read.csv("tpa.csv")
fll <- read.csv("fll.csv")

YEAR <- mco[,1]
MONTH <- mco[,2]
MCO.INTERNATIONAL <- mco[,4]
MIA.INTERNATIONAL <- mia[,4]
TPA.INTERNATIONAL <- tpa[,4]
FLL.INTERNATIONAL <- fll[,4]

INT <- data.frame(YEAR, MONTH, MCO.INTERNATIONAL, MIA.INTERNATIONAL, TPA.INTERNATIONAL, FLL.INTERNATIONAL)
INT$TOTAL <- rowSums(INT[,3:6])
head(INT)
tail(INT)

win.graph(width = 12, height = 6)
plot(y = INT$TOTAL, x = (INT$YEAR + (INT$MONTH/12)),
     ylab = 'International Passengers', xlab = 'Year',
     main = 'Total International Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

win.graph(width = 12, height = 6)
plot(y = INT$MCO.INTERNATIONAL, x = (INT$YEAR + (INT$MONTH/12)),
     ylab = 'International Passengers', xlab = 'Year',
     main = 'International Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(min(INT[,3:6]), max(INT[,3:6])))
lines(x = (INT$YEAR + (INT$MONTH/12)), y = INT$MIA.INTERNATIONAL, col = 'red')
lines(x = (INT$YEAR + (INT$MONTH/12)), y = INT$TPA.INTERNATIONAL, col = 'green')
lines(x = (INT$YEAR + (INT$MONTH/12)), y = INT$FLL.INTERNATIONAL, col = 'blue')

## Insights: MCO and FLL have roughly the same number of international passengers
############ MIA is prone to violent swings up and down
############ FLL is more volitile than MCO
############ TPA remains relatively constant

pre.pct <- INT[,3:6]
pre.pct <- pre.pct[-1,]
old.pct <- INT[,3:6]
old.pct <- head(old.pct, -1)
pct <- (pre.pct - old.pct) / old.pct

pct.INT <- INT[-1,]
pct.INT[,3:6] <- pct

win.graph(width = 12, height = 6)
plot(y = pct.INT$MCO.INTERNATIONAL, x = (pct.INT$YEAR + (pct.INT$MONTH/12)),
     ylab = 'Percent Change', xlab = 'Year',
     main = 'PCT Change in International Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l')
lines(x = (pct.INT$YEAR + (pct.INT$MONTH/12)), y = pct.INT$MIA.INTERNATIONAL, col = 'red')
lines(x = (pct.INT$YEAR + (pct.INT$MONTH/12)), y = pct.INT$TPA.INTERNATIONAL, col = 'green')
lines(x = (pct.INT$YEAR + (pct.INT$MONTH/12)), y = pct.INT$FLL.INTERNATIONAL, col = 'blue')

## Well this graph is a nightmare to look at
## It defies the observation of our previous graph, TPA has the largest relative fluctuations in international 
#  passengers

## Maybe a log transformations??

win.graph(width = 12, height = 6)
plot(y = log(INT$MCO.INTERNATIONAL), x = (INT$YEAR + (INT$MONTH/12)),
     ylab = 'log(International Passengers)', xlab = 'Year',
     main = 'International Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(min(log(INT[,3:6])), max(log(INT[,3:6]))))
lines(x = (INT$YEAR + (INT$MONTH/12)), y = log(INT$MIA.INTERNATIONAL), col = 'red')
lines(x = (INT$YEAR + (INT$MONTH/12)), y = log(INT$TPA.INTERNATIONAL), col = 'green')
lines(x = (INT$YEAR + (INT$MONTH/12)), y = log(INT$FLL.INTERNATIONAL), col = 'blue')

## OK this graph is waaaaay easier to interpret
## We see the variability in TPA, the similarities in MCO and FLL and the superiority of MIA
## We can also see that variability has decreased over time

INT.TS <- ts(INT$TOTAL, start = c(2002,10), end = c(2016,8), frequency = 12)
win.graph(width = 12, height = 6)
plot(y = INT.TS, x = (INT$YEAR + (INT$MONTH/12)),
     ylab = 'Total International Passengers', xlab = 'Year',
     main = 'Total International Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'l')
points(y = INT.TS, x = (INT$YEAR + (INT$MONTH/12)), pch = as.vector(season(INT.TS)))

## Notice how the peak is in July? This is different from Domestic Passengers who peak in March
## The trough is in september, thats in line with Domestic Passengers

## Now lets look at a lag

INT.l1.fit <- lm(INT$TOTAL ~ zlag(INT$TOTAL), data = INT)
win.graph(width = 10, height = 10)
plot(y = INT$TOTAL, x = zlag(INT$TOTAL), xlab = 'Last Periods Total Int. Passengers',
	ylab = 'Total Int Passengers')
abline(INT.l1.fit)
summary(INT.l1.fit)

## Lets work with some differencing

#### This is a log of INT TOTAL
win.graph(width = 12, height = 6)
plot(y = log(INT$TOTAL), x = (INT$YEAR + (INT$MONTH/12)),
     ylab = 'Log of International Passengers', xlab = 'Year',
     main = 'Total International Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

#### First Difference of INT TOTAL
INT.diff <- diff(INT$TOTAL)
INT.f.diff <- INT[-1,]
win.graph(width = 12, height = 6)
plot(y = INT.diff, x = (INT.f.diff$YEAR + (INT.f.diff$MONTH/12)),
     ylab = 'First Difference of International Passengers', xlab = 'Year',
     main = 'Total International Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

#### Log of First Difference
win.graph(width = 12, height = 6)
plot(y = diff(log(INT$TOTAL)), x = (INT.f.diff$YEAR + (INT.f.diff$MONTH/12)),
     ylab = 'First Difference of Log International Passengers', xlab = 'Year',
     main = 'Total International Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')
x11()
acf(INT$TOTAL, lag.max = 120)
x11()
acf(diff(INT$TOTAL), lag.max = 120)
x11()
acf(diff(log(INT$TOTAL)), lag.max = 120)
x11()
pacf(INT$TOTAL, lag.max = 120)
x11()
pacf(diff(INT$TOTAL), lag.max = 120)
x11()
pacf(diff(log(INT$TOTAL)), lag.max = 120)

## The takeaway here is that the data is correlated whe the lag is intervals of 12, i.e. one year

plot(INT.TS)
dec.INT <- decompose(INT.TS)
plot(dec.INT)

######################### This portion of code has the Seasonality removed  #######################
INT.SA <- INT$TOTAL - dec.INT$seasonal
win.graph(width = 12, height = 6, pointsize = 10)
plot(INT.SA, type = 'o', main = 'Seasonally Adjusted Int Passengers')
win.graph(width = 12, height = 6, pointsize = 10)
plot(diff(INT.SA), type = 'o', main = 'First difference of Seasoally Adjusted Int Passengers')

x11()
acf(INT.SA, lag.max = 120)
x11()
pacf(INT.SA, lag.max = 120)
## Here we see the ACF tailing off and the PACF cutting off indicating a AR(p) model!
## These ACF and PACF plots look nice, lets check to see if INT.SA is stationary

adf.test(INT.SA, k = 12)
## No, not stationary

DIFF.INT.SA <- diff(INT.SA)
adf.test(DIFF.INT.SA, k = 12)
## This is stationary
x11()
acf(DIFF.INT.SA, ci.type = 'ma', lag.max = 120)
x11()
pacf(DIFF.INT.SA, lag.max = 120)
## These are harder to interpret but suggests an MA(1) process

## Lets examine potential ARMA models

eacf(DIFF.INT.SA)
## This indicates that an ARMA Model might not be as useful as an MA(1) model

############################ No Seasonality makes for a bad model, it turns out ###################

adf.test(INT$TOTAL, k = 12)
## So back to my original data, this is a highly stationary process

adf.test(INT.diff, k = 12)
## So one difference removes the stationarity

## Naming DTT for Differnced Total Time Series
DTT <- ts(INT.diff, start = c(2002,10), end = c(2016,8), frequency = 12)
win.graph(width = 12, height = 6, pointsize = 10)
plot(DTT, type = 'o', main = 'First difference of Int Passengers')

## Just a diagnostic Decomposition
x11()
dec.DTT <- decompose(DTT)
plot(dec.DTT)

x11()
acf(DTT, lag.max = 120)
x11()
pacf(DTT, lag.max = 120)
## The ACF tails off and the PACF cuts off after lag 8, suggesting an ARI(8, 1) model

eacf(DTT)
## This is a jumble of x's and o's

## order 8 for the AR model, 0 because it has already been differenced, and 0 for lack of an MA process
## adding a seasonal argument prevents calling method = 'ML'
## forced using method = 'CSS'
## The CSS model is shit and the ML model is amazing
DTT.ar <- arima(DTT, order = c(8, 0, 0), method = 'ML')
DTT.ar
x11()
plot(rstandard(DTT.ar))
x11()
hist(rstandard(DTT.ar))
x11()
qqnorm(residuals(DTT.ar))
qqline(residuals(DTT.ar))

x11()
acf(residuals(DTT.ar))
x11()
pacf(residuals(DTT.ar))

win.graph(width = 10, height = 10)
plot(y = diff(INT$TOTAL), x = zlag(diff(INT$TOTAL)), xlab = 'Last Periods Total Int. Passengers',
	ylab = 'Total Int Passengers')
abline(DTT.ar)


## All this suggests a good fit, though the autocorrelated residuals might suggest a missing variable
## also the scatter plot is crap

################################################
################ Just for Kicks ################
####________________________________________####

INT.ratio <- INT

INT.ratio[3] <- INT[3] / INT[7]
INT.ratio[4] <- INT[4] / INT[7]
INT.ratio[5] <- INT[5] / INT[7]
INT.ratio[6] <- INT[6] / INT[7]
INT.ratio[7] <- INT[7] / INT[7]
tail(INT.ratio)

win.graph(width = 12, height = 6)
plot(y = INT.ratio$MCO.INTERNATIONAL, x = (INT.ratio$YEAR + (INT.ratio$MONTH/12)),
     ylab = 'Percent Change', xlab = 'Year',
     main = 'PCT Change in International Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(0, 1))
lines(x = (INT.ratio$YEAR + (INT.ratio$MONTH/12)), y = INT.ratio$MIA.INTERNATIONAL, col = 'red')
lines(x = (INT.ratio$YEAR + (INT.ratio$MONTH/12)), y = INT.ratio$TPA.INTERNATIONAL, col = 'green')
lines(x = (INT.ratio$YEAR + (INT.ratio$MONTH/12)), y = INT.ratio$FLL.INTERNATIONAL, col = 'blue')

sd.MCO.pct <- sd(INT.ratio[,3])
sd.MIA.pct <- sd(INT.ratio[,4])
sd.TPA.pct <- sd(INT.ratio[,5])
sd.FLL.pct <- sd(INT.ratio[,6])
sd.TOT.pct <- sd(INT.ratio[,7])

sd.MCO <- sd(INT[,3])
sd.MIA <- sd(INT[,4])
sd.TPA <- sd(INT[,5])
sd.FLL <- sd(INT[,6])
sd.TOT <- sd(INT[,7])

sd.df <- data.frame(Airport = c('MCO', 'MIA', 'TPA', 'FLL', 'Total'), 
			  sd = c(sd.MCO, sd.MIA, sd.TPA, sd.FLL, sd.TOT), 
			  sd.pct = c(sd.MCO.pct, sd.MIA.pct, sd.TPA.pct, sd.FLL.pct, sd.TOT.pct))

## From here we can see that Miami has the most overall variation and that Tampa is the most consistent


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

#    Established that International tourists into the four major hubs can be modeled by a ARI(8, 1) process

#    Explored removing seasonality, but just makes for a worse model

#    Saw that Miami takes in the most International Tourists, and has the most variability in its numbers
##   By comparison, Tampa is the smallest and sees the least variance


