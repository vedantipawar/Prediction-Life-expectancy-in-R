install.packages("forecast")
library(forecast)
library(ggplot2)
data1<- read.csv("C:/Users/vedan/Downloads/PredictionDataset.csv")
tsdata1<- ts(data1$LifeExpectancy,frequency=1,start=c(1950) )
plot(tsdata1)
autoarima1<-auto.arima(tsdata1)
forecast1<-forecast(autoarima1,h=20)
forecast1
plot(forecast1)
plot(forecast1$residuals)
qqnorm(forecast1$residuals)
acf(forecast1$residuals)
pacf(forecast1$residuals)
summary(autoarima1)
accuracy(autoarima1)
plot(forecast1)

#Prediction of USA dataset lifeexpectency
data2<- read.csv("C:/Users/vedan/Downloads/PredictionUSA.csv")
tsdata2<- ts(data2$LifeExpectancy,frequency=1,start=c(1950) )
plot(tsdata2)
autoarima2<-auto.arima(tsdata2)
forecast2<-forecast(autoarima2,h=20)
forecast2
plot(forecast2)
plot(forecast2$residuals)
qqnorm(forecast2$residuals)
acf(forecast2$residuals)
pacf(forecast2$residuals)
summary(autoarima2)
accuracy(autoarima2)
plot(forecast2)

#both grpahs in one plot
plot(forecast1, type="l",col="green")
lines(forecast2,col="red")
plot(forecast2$Years,forecast2$LifeExpectancy,col="red")

