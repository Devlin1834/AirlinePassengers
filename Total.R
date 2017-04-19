setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(fitdistrplus)
library(car)
library(dplyr)

######################################################################################
############################# TOTAL PASSENGERS ANALYSIS ##############################
###################################################################################### 

mco <- read.csv("mco.csv")
mia <- read.csv("mia.csv")
tpa <- read.csv("tpa.csv")
fll <- read.csv("fll.csv")

YEAR <- mco[,1]
MONTH <- mco[,2]
MCO.TOTAL <- mco[,5]
MIA.TOTAL <- mia[,5]
TPA.TOTAL <- tpa[,5]
FLL.TOTAL <- fll[,5]

TOT <- data.frame(YEAR, MONTH, MCO.TOTAL, MIA.TOTAL, TPA.TOTAL, FLL.TOTAL)
TOT$TOTAL <- rowSums(TOT[,3:6])
head(TOT)
tail(TOT)

win.graph(width = 12, height = 6)
plot(y = TOT$TOTAL, x = (TOT$YEAR + (TOT$MONTH/12)),
     ylab = 'Total Passengers', xlab = 'Year',
     main = 'Total Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

win.graph(width = 12, height = 6)
plot(y = TOT$MCO.TOTAL, x = (TOT$YEAR + (TOT$MONTH/12)),
     ylab = 'Total Passengers', xlab = 'Year',
     main = 'Total Passengers Landing in Florida by Airport', 
     sub = 'Only Major Hubs: MCO(black), MIA(red), TPA(green), FLL(blue)',
     type = 'l', ylim = c(min(TOT[,3:6]), max(TOT[,3:6])))
lines(x = (TOT$YEAR + (TOT$MONTH/12)), y = TOT$MIA.TOTAL, col = 'red')
lines(x = (TOT$YEAR + (TOT$MONTH/12)), y = TOT$TPA.TOTAL, col = 'green')
lines(x = (TOT$YEAR + (TOT$MONTH/12)), y = TOT$FLL.TOTAL, col = 'blue')


TOT.TS <- ts(TOT$TOTAL, start = c(2002,10), end = c(2016,8), frequency = 12)
win.graph(width = 12, height = 6)
plot(y = TOT.TS, x = (TOT$YEAR + (TOT$MONTH/12)),
     ylab = 'Total Passengers', xlab = 'Year',
     main = 'Total Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'l')
points(y = TOT.TS, x = (TOT$YEAR + (TOT$MONTH/12)), pch = as.vector(season(TOT.TS)))

## Now this is fascinating. MIA and MCO take in the same number of passengers but MIA takes in
#### more international passengers while MCO takes in more domestic

## Also of note, september seems to be the low point of the year while march is the high point
#### SPRING BREAK

## Lets look at a lag

TOT.l1.fit <- lm(TOT$TOTAL ~ zlag(TOT$TOTAL), data = TOT)
win.graph(width = 10, height = 10)
plot(y = TOT$TOTAL, x = zlag(TOT$TOTAL), xlab = 'Last Periods Total Tot. Passengers',
	ylab = 'Total Tot. Passengers')
abline(TOT.l1.fit)
summary(TOT.l1.fit)
## This isn't too bad of a fit. 

## Now lets move onto differencing

TOT.diff <- diff(TOT$TOTAL)
TOT.f.diff <- TOT[-1,]
win.graph(width = 12, height = 6)
plot(y = TOT.diff, x = (TOT.f.diff$YEAR + (TOT.f.diff$MONTH/12)),
     ylab = 'First Difference of Total Passengers', xlab = 'Year',
     main = 'Total Total Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

x11()
acf(TOT$TOTAL, lag.max = 120)
x11()
acf(TOT.diff, lag.max = 120)
x11()
pacf(TOT$TOTAL, lag.max = 120)
x11()
pacf(TOT.diff, lag.max = 120)

dec.TOT <- decompose(TOT.TS)
x11()
plot(dec.TOT)

adf.test(TOT$TOTAL, k = 12)
adf.test(TOT.diff, k = 12)
## Neither of these test as nonstationary

## The diff or log(TOT)

DTL <- diff(log(TOT$TOTAL))
adf.test(DTL, k = 12)
## This is still non-stationary as well

##Lets do the second difference of total, SDT
SDT <- diff(TOT$TOTAL, differences = 2)
adf.test(SDT, k = 12)
## Finally, this is Stationary

MY <- TOT[,1:2]
MY <- head(MY, -2)

win.graph(width = 12, height = 6)
plot(y = SDT, x = (MY$YEAR + (MY$MONTH/12)),
     ylab = 'Second Difference of Total Passengers', xlab = 'Year',
     main = 'Total Total Passengers Landing in Florida', 
     sub = 'Only Major Hubs: FLL, MCO, MIA, TPA',
     type = 'o')

eacf(SDT)
x11()
acf(SDT, lag.max = 120)
x11()
pacf(SDT, lag.max = 120)

## The ACF tails off and the PACF cuts off after 13 lags, suggesting an ARI(13, 2) model

SDT.ar <- arima(SDT, order = c(13, 0, 0), method = 'ML')
SDT.ar
## All the coefs are signifigant

x11()
plot(rstandard(SDT.ar))
x11()
hist(rstandard(SDT.ar), breaks = 30)
x11()
qqnorm(residuals(SDT.ar))
qqline(residuals(SDT.ar))

## There are two strong outliers that should probably be addressed but otherwise, its a strong model

win.graph(width = 10, height = 10)
plot(y = diff(TOT$TOTAL, differences = 2), x = zlag(diff(TOT$TOTAL, differences = 2)), xlab = 'Last Periods Total Passengers',
	ylab = 'Total Passengers')
abline(SDD.ar)

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

#    Orlando and Miami are the two largest airports with Tampa and Fort Lauderdale being about the same size
##   FLL has been growing faster than TPA however

#    Total Passengers is represented well by an AR(13, 2) model.  

#    There are two outliers that should be dealt with
