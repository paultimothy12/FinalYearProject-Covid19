library(covid19.analytics)
library(dplyr)
library(lubridate)
library(prophet)
library(ggplot2)
#Imporitng Dataset
covid <-covid19.data(case = "ts-confirmed")
covid
view(covid)
#Extract only India Dataset
covidin <- filter(covid,Country.Region == "India")
covidin
view(covidin)
covidin<-data.frame(t(covidin))
view(covidin)
#Data Preprocessing
covidin <- cbind(rownames(covidin),data.frame(covidin,row.names =
NULL))
view(covidin)
colnames(covidin) <- c('Date', 'Confirmed')
view(covidin)
covidin <- covidin[-c(1:4),]
view(covidin)
str(covidin)
covidin$Date <-ymd(covidin$Date)
covidin$Confirmed <- as.numeric(covidin$Confirmed)
str(covidin)
print(covidin)
#Visualizing current trend
qplot(Date,Confirmed, data = covidin,
main = 'COVID Confirmed Cases in India on 29-3-2021')
ds<- covidin$Date
y<-covidin$Confirmed
df <- data.frame(ds,y)
df
#Forecasting using prophet
model <-prophet(df)
#Creating future data frame
covidf <-make_future_dataframe(model,periods = 31)
print(covidf)
forecast <- predict(model,covidf)
#print(forecast)
tail(forecast[c('ds','yhat')],n=31)
#Visualize the forecasting values
plot(model,forecast, main = "Predicted COVID Confirmed cases in India till
29/04/2021", xlab = "Date",ylab = "Confirmed")
dyplot.prophet(model,forecast)
#Forecast components
prophet_plot_components(model,forecast)
