# PyR
#DMH competition , top coder.
 
###libraries  	

library(corrplot)
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

###reading the data

getwd()
setwd("E:\\DMH") ### setting the loation of dataset
DMH=read.table(file="data1.csv",sep=',',header=TRUE) ### client only data csv is read
colnames(DMH)
DMH_1=subset(DMH,select=-c(Macaddr,Ipaddr,Block,Serialnbr,ROOMGROUP))
dir()
View(DMH_1)

###	summary and descrption of data
str(DMH_1)

###time series analysis (sales in different months)

typeof(DMH_1$Regdate)
as.Date(DMH_1$Regdate,"%m/%d/%Y")
DMH_mailaddr=subset(DMH,select = c(mailaddr))
typeof(DMH_mailaddr)

sales=ts(DMH$mailaddr, start = c(2014,7), end=c(2016,9),frequency = 12)
ts.plot(sales)

####decomposing time series ( trend , seasonal , random )

d_sales=decompose(sales)
plot(d_sales)
t_sales=diff(sales)
acf_sales=acf2(t_sales)


auto.arima(sales) ## checking the time series is non stationary

### Holt winter prediction Using R
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

##########################################################################################################################



