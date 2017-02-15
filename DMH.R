 
##################libraries#####################
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
##################reading the data###############

getwd()
setwd("E:\\DMH")
DMH=read.table(file="data1.csv",sep=',',header=TRUE)
colnames(DMH)
DMH_1=subset(DMH,select=-c(Macaddr,Ipaddr,Block,Serialnbr,ROOMGROUP))
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

################# crosstab analysis#########################
cross_DMH=crosstab(DMH$Itemname,DMH$Continent,dnn=c("item","continent")
                   
