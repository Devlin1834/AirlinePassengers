setwd("D:/Files/Programing and Data/R Directory/Time Series/MCO Project")

library(TSA)
library(ggplot2)
library(reshape2)

################################################################################################
######################## MCO -- ORLANDO INTERNATIONAL AIRPORT -- Part 3 ########################
################################################################################################
# Dataframe Definition
################################################################################################
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
MCO.TOTAL <- mco[,5]
MIA.TOTAL <- mia[,5]
TPA.TOTAL <- tpa[,5]
FLL.TOTAL <- fll[,5]

INT <- data.frame(YEAR, MONTH, MCO.INTERNATIONAL, MIA.INTERNATIONAL, TPA.INTERNATIONAL, FLL.INTERNATIONAL)
INT$TOTAL <- rowSums(INT[,3:6])

TOT <- data.frame(YEAR, MONTH, MCO.TOTAL, MIA.TOTAL, TPA.TOTAL, FLL.TOTAL)
TOT$TOTAL <- rowSums(TOT[,3:6])

################################################################################################
# Themes
################################################################################################

mco_theme <- theme(plot.title = element_text(face = 'bold.italic',
							   size = '16', color = 'black'),
			 axis.title = element_text(face = 'bold', size = '12', color = 'black'),
			 axis.text = element_text(face = 'italic', size = '10', color = 'darkorange1'),
			 axis.ticks = element_blank(),
			 panel.background = element_rect(fill = 'grey99', color = 'white'),
			 panel.grid.major.x = element_line(color = 'darkorange1', linetype = 2))

florida_theme <- theme(plot.title = element_text(face = 'bold.italic',
							   size = '16', color = 'red4'),
			 axis.title = element_text(face = 'bold', size = '12', color = 'red4'),
			 axis.text = element_text(face = 'italic', size = '10', color = 'goldenrod4'),
			 axis.ticks = element_blank(),
			 panel.background = element_rect(fill = 'white', color = 'white'),
			 panel.grid.major.x = element_line(color = 'goldenrod4', linetype = 2),
			 panel.grid.major.y = element_line(color = 'goldenrod4', linetype = 2),
			 strip.background = element_rect(color = 'white', fill = 'goldenrod4'),
			 strip.text = element_text(size = 10, face = 'bold', color = 'white'))

################################################################################################
# Graphs
################################################################################################

### Graph1: MCO International Over Time ###
ggplot(mco, aes(y = INTERNATIONAL, x = (Year + (Month/12)))) + geom_line() + 
	labs(x = 'Year', y = 'International Passengers', title = 'International Passengers landing at MCO') +
      xlim(2002, 2016) + mco_theme

### Graph 2: MCO Domestic and International Over Time ###
ggplot(mco, aes(x = (Year + (Month/12)))) + 
	 geom_line(aes(y = INTERNATIONAL, color = 'International')) + 
	 geom_line(aes(y = DOMESTIC, color = 'Domestic')) +  
	 scale_color_manual(values = c('firebrick', 'mediumblue')) +
	 labs(x = 'Year', y = 'Passengers into MCO', title = 'Passengers Landing at MCO', color = 'Origin') + 
	 xlim(2002, 2016) +
	 mco_theme

### Graph 3: International Passengers Comparisons ###
ggplot(INT, aes(x = (YEAR + (MONTH/12)))) + 
	 geom_line(aes(y = MCO.INTERNATIONAL, color = 'Orlando'), size = 1) + 
	 geom_line(aes(y = MIA.INTERNATIONAL, color = 'Miami'), size = 1) + 
	 geom_line(aes(y = TPA.INTERNATIONAL, color = 'Tampa'), size = 1) + 
	 geom_line(aes(y = FLL.INTERNATIONAL, color = 'Fort Lauderdale'), size = 1) + 
	 scale_color_manual(values = c('goldenrod1', 'olivedrab3', 'dodgerblue3', 'plum')) + 
	 labs(x = 'Year', y = 'International Passengers', 
		title = 'International Passengers into Florida\'s Major Hubs', color = 'Destination') + 
	 florida_theme

### Graph 4: Total Passengers Comparisons ###
ggplot(TOT, aes(x = (YEAR + (MONTH/12)))) + 
	 geom_line(aes(y = MCO.TOTAL, color = 'Orlando'), size = 1) + 
	 geom_line(aes(y = MIA.TOTAL, color = 'Miami'), size = 1) + 
	 geom_line(aes(y = TPA.TOTAL, color = 'Tampa'), size = 1) + 
	 geom_line(aes(y = FLL.TOTAL, color = 'Fort Lauderdale'), size = 1) + 
	 scale_color_manual(values = c('goldenrod1', 'olivedrab3', 'dodgerblue3', 'plum')) + 
	 labs(x = 'Year', y = 'Total Passengers', 
		title = 'Total Passengers into Florida\'s Major Hubs', color = 'Destination') + 
	 florida_theme

### Graph 5: Faceted Histogram of International Tourists ###
v.INT <- INT[3:6]
colnames(v.INT) <- c('Orlando', 'Miami', 'Tampa', 'Fort Lauderdale')
v.INT$id <- rownames(v.INT)
v.INT <- melt(v.INT)
colnames(v.INT) <- c('id', 'Airport', 'Passengers')
ggplot(v.INT, aes(x = Passengers)) + 
	 geom_histogram(bins = 35) +  
	 facet_wrap(~Airport, nrow = 4) +
	 xlim(0, max(INT[4])) + 
	 florida_theme

### Graph 6: A Violin Plot of Total Passengers ###
f.TOT <- TOT[3:6]
colnames(f.TOT) <- c('Orlando', 'Miami', 'Tampa', 'Fort Lauderdale')
f.TOT$id <- rownames(f.TOT)
f.TOT <- melt(f.TOT)
colnames(f.TOT) <- c('id', 'Airport', 'Passengers')
ggplot(f.TOT, aes(x = Airport, y = Passengers)) + 
	 geom_violin(fill = 'lightblue') + 
	 labs(x = 'Destination Airport', y = 'Total Passeengers', 
		title = 'Total Passengers Distributions') + florida_theme

### Graph 7: Histogram of int.ma1 Residuals ###
mco.int <- ts(mco$INTERNATIONAL, start = c(2002, 10), end = c(2016, 8), frequency = 12)
int.ma1 <- arima(lmco.int, order = c(0,1,1), 
		     seasonal = list(order = c(0,1,1), period = 12), 
		     method = 'ML')
resid <- int.ma1$residuals
ggplot(resid, aes(x = resid)) + geom_histogram(bins = 35) + 
	 labs(x = 'Standardized Residuals', y = 'Count', 
		title = 'Histogram of Standardized Residuals') + mco_theme

### Graph 8: Y[t] vs Y[t-1] ###
mco.lag <- as.data.frame(mco$INTERNATIONAL)
mco.lag[2] <- zlag(mco$INTERNATIONAL)
mco.lag <- mco.lag[-1,]
colnames(mco.lag) <- c('INT', 'LAG')
ggplot(mco.lag, aes(x = LAG, y = INT)) + 
	 labs(x = 'Last Period\'s International Passengers', y = 'This Period\'s International Passengers',
		title = 'International Passengers with a Lag') + 
	 geom_point() + 
	 mco_theme + theme(panel.grid.major.y = element_line(color = 'darkorange1', linetype = 2))

### Graph 9: First Difference ###
diff.mco <- mco
for(i in 1:12){
	diff.mco <- diff.mco[-1,]
}
diff.mco <- diff.mco[-1,]
diff1 <- diff(mco$INTERNATIONAL)
sdiff <- diff(diff1, lag = 12)
diff.mco[6] <- as.data.frame(sdiff)
colnames(diff.mco)[6] <- c('S.Difference')
ggplot(diff.mco, aes(y = S.Difference, x = (Year + (Month/12)))) + geom_line() + 
	labs(x = 'Year', y = 'International Passengers', title = 'International Passengers landing at MCO',
	subtitle = 'First Difference with a 12 Period Seasonal Difference') +
      xlim(2004, 2016) + mco_theme

