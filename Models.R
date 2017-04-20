setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(forecast)

################################################################################################
################################### ARIMA Models Collections  ##################################
################################################################################################
### MCO ###
mco <- read.csv("mco.csv")

mco.int <- ts(mco$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
mco.dom <- ts(mco$DOMESTIC, start = c(2002, 10), end = c(2016, 8), frequency = 12)
mco.tot <- ts(mco$TOTAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)

auto.arima(mco.int)
auto.arima(mco.dom)
auto.arima(mco.tot)

## MCO INTERNATIONAL -> ARIMA(0,1,1)(0,1,1)[12]
## MCO DOMESTIC      -> ARIMA(0,1,1)(0,0,2)[12] 
## MCO TOTAL         -> ARIMA(0,1,1)(0,0,2)[12]  

###############################################################################################
### MIA ###
mia <- read.csv("mia.csv")

mia.int <- ts(mia$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
mia.dom <- ts(mia$DOMESTIC, start = c(2002, 10), end = c(2016, 8), frequency = 12)
mia.tot <- ts(mia$TOTAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)

auto.arima(mia.int)
auto.arima(mia.dom)
auto.arima(mia.tot)

## MIA INTERNATIONAL -> ARIMA(3,0,0)(2,1,0)[12] with drift 
## MIA DOMESTIC      -> ARIMA(1,1,1)(0,0,2)[12] 
## MIA TOTAL         -> ARIMA(1,0,2)(0,1,2)[12] with drift 

###############################################################################################
### FLL ###
fll <- read.csv("fll.csv")

fll.int <- ts(fll$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
fll.dom <- ts(fll$DOMESTIC, start = c(2002, 10), end = c(2016, 8), frequency = 12)
fll.tot <- ts(fll$TOTAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)

auto.arima(fll.int)
auto.arima(fll.dom)
auto.arima(fll.tot)

## FLL INTERNATIONAL -> ARIMA(0,1,1)(1,1,1)[12]  
## FLL DOMESTIC      -> ARIMA(1,0,2)(0,1,1)[12] with drift 
## FLL TOTAL         -> ARIMA(1,0,2)(0,1,1)[12] with drift 

###############################################################################################
### TPA ###
tpa <- read.csv("tpa.csv")

tpa.int <- ts(tpa$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
tpa.dom <- ts(tpa$DOMESTIC, start = c(2002, 10), end = c(2016, 8), frequency = 12)
tpa.tot <- ts(tpa$TOTAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)

auto.arima(tpa.int)
auto.arima(tpa.dom)
auto.arima(tpa.tot)

## TPA INTERNATIONAL -> ARIMA(0,1,1)(0,1,1)[12]  
## TPA DOMESTIC      -> ARIMA(1,1,0)(2,1,0)[12]   
## TPA TOTAL         -> ARIMA(2,1,1)(2,1,2)[12] 

###############################################################################################