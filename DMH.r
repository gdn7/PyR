 
##################libraries#####################
library(c(corrplot,leaps))
library(leaps)
library(bestglm)
library(MASS)
library(psych)
library(xts)
library(data.table)

library(deSolve)
library(rootSolve)
library(coda)
library(descr)

library(sandwich)
library(urca)
library(lmtest)
library(astsa)
library(MTS)
library(vars)
library(FME)
library(tseries)
library(forecast)
library(ggplot2)
library(reshape)

##################reading the data###############

getwd()
setwd("E:\\DMH")
DMH=read.table(file="data1.csv",sep=',',header=TRUE)## data being used isDMH Data Insights - TRACK 1 (Client Only Data)
colnames(DMH)
DMH_1=subset(DMH,select=-c(Macaddr,Ipaddr,Block,Serialnbr,ROOMGROUP))## not considering Macaddr,Ipaddr,Block,Serialnbr,ROOMGROUP 
dir()
View(DMH_1)

#################summary and descrption of data#############
str(DMH_1)
#################  time series analysis (sales in different months)##################

typeof(DMH_1$Regdate)
as.Date(DMH_1$Regdate,"%m/%d/%Y")
DMH_mailaddr=subset(DMH,select = c(mailaddr))
typeof(DMH_mailaddr)
head(DMH_item)
sales=ts(DMH$mailaddr, start = c(2014,7), end=c(2016,9),frequency = 12)
ts.plot(sales)


d_sales=decompose(sales)
plot(d_sales)
t_sales=diff(sales)
acf_sales=acf2(t_sales)
plot(acf_sales)
plot(t_sales)
auto.arima(sales)
adf.test(sales,alternative = "stationary",k=12)
fit=Arima(t_sales,order=c(0,0,1))
summary(fit)
###############Forecast analysis########################

sales_time=sales - d_sales$seasonal
exps = HoltWinters(sales_time, beta = FALSE, gamma = FALSE)
plot(exps)
exps_forecast = forecast.HoltWinters(exps, h =31)

plot(exps_forecast)
plot(exps_forecast$residuals)
lines(c(0,1869), c(0, 0), col = 'red')
plotForecastErrors = function(forecasterrors,forecasttitle) {
  forecasterrors = na.omit(forecasterrors)
  mybinsize = IQR(forecasterrors) / 4
  mysd = sd(forecasterrors)
  mymin = min(forecasterrors) - mysd * 5
  mymax = max(forecasterrors) + mysd * 3
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main=forecasttitle)
  myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
  points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}
plotForecastErrors(exps_forecast$residuals,'Assessing Normal Distribution')
texps = HoltWinters(sales_time, gamma = FALSE,
                    l.start = 70,
                    b.start = 10.05)

texps 
plot(texps)
texps_fore = forecast.HoltWinters(texps, h = 31)
plot(texps_fore)

plot(texps_fore$residuals)
lines(c(0,1870), c(0, 0), col = 'red')

plotForecastErrors(texps_fore$residuals, 'Assessing Normal Distribution')
hwa= HoltWinters(sales)
plot(hwa)

hwa_fore = forecast.HoltWinters(hwa, h = 30)
plot(hwa_fore, main='Forecast for 30 Periods')
plot(hwa_fore$residuals)

lines(c(0,1870), c(0, 0), col = 'red')
plotForecastErrors(hwa_fore$residuals, 'Assessing Normal Distribution')

boxplot(sales ~ cycle(sales))

sdts=sd(sales)
sdts
sdtse=sd(sales_time)
sdtse


############## crosstab analysis#########################
cross_DMH=crosstab(DMH$Itemname,DMH$Continent,dnn=c("item","continent"))

############## 

########################Holt intwr prediction Using R################################
###Function:###
HWplot<-function(sales,  n.ahead=6,  CI=.95,  error.ribbon='green', line.size=1){
  
  hw_object<-HoltWinters(sales)
  
  forecast<-predict(hw_object,  n.ahead=n.ahead,  prediction.interval=T,  level=CI)
  
  
  for_values<-data.frame(time=round(time(forecast),  3),  value_forecast=as.data.frame(forecast)$fit,  dev=as.data.frame(forecast)$upr-as.data.frame(forecast)$fit)
  
  fitted_values<-data.frame(time=round(time(hw_object$fitted),  3),  value_fitted=as.data.frame(hw_object$fitted)$xhat)
  
  actual_values<-data.frame(time=round(time(hw_object$x),  3),  Actual=c(hw_object$x))
  
  
  graphset<-merge(actual_values,  fitted_values,  by='time',  all=TRUE)
  graphset<-merge(graphset,  for_values,  all=TRUE,  by='time')
  graphset[is.na(graphset$dev),  ]$dev<-0
  
  graphset$Fitted<-c(rep(NA,  NROW(graphset)-(NROW(for_values) + NROW(fitted_values))),  fitted_values$value_fitted,  for_values$value_forecast)
  
  
  graphset.melt<-melt(graphset[, c('time', 'Actual', 'Fitted')], id='time')
  
  p<-ggplot(graphset.melt,  aes(x=time,y=value)) + geom_ribbon(data=graphset, aes(x=time, y=Fitted, ymin=Fitted-dev,  ymax=Fitted + dev),  alpha=.2,  fill=error.ribbon) + geom_line(aes(colour=variable ), size=line.size , xintercept=1) + geom_vline(x=max(actual_values$time), xintercept = 2014, lty=2) + xlab('Time') + ylab('sales') + theme(legend.position='bottom') + scale_colour_hue('')
  return(p)
  
}

HWplot(sales, n.ahead=6, CI=.95, error.ribbon='green',line.size=1)

