library(covid19.analytics)
library(dplyr)
library(lubridate)
library(prophet)
library(ggplot2)
covid <-covid19.data(case = "ts-deaths")
covid
View(covid)
covidin <- filter(covid,Country.Region == "India")
covidin
View(covidin)
covidin<-data.frame(t(covidin))
View(covidin)
covidin <- cbind(rownames(covidin),data.frame(covidin,row.names =
NULL))
View(covidin)
colnames(covidin) <- c('Date', 'deaths')
View(covidin)
covidin <- covidin[-c(1:4),]
view(covidin)
str(covidin)
covidin$Date <-ymd(covidin$Date)
covidin$deaths <- as.numeric(covidin$deaths)
str(covidin)
view(covidin)
qplot(Date,deaths, data = covidin,
main = 'COVID Death Cases in India on 29-3-2021')
ds<- covidin$Date
y<-covidin$deaths
df <- data.frame(ds,y)
df
#Forecasting
model <-prophet(df)
#prediction
covidf <-make_future_dataframe(model,periods = 31)
print(covidf)
forecast <- predict(model,covidf)
print(forecast)
tail(forecast[c('ds','yhat')],n=31)
#plot
plot(model,forecast, main = "Predicted COVID Death cases in India till
29/04/2021", xlab = "Date",ylab = "deaths")
dyplot.prophet(model,forecast)
#Forecast components
prophet_plot_components(model,forecast)
