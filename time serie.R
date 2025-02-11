data <- read.csv2("data/bilan-electrique-jour.csv")
head(data)

library(ggplot2)

data$Jour <- as.Date(data$Jour)
data$Puissance.moyenne.journalière.de.la.consommation.totale..W. <- as.numeric(data$Puissance.moyenne.journalière.de.la.consommation.totale..W.)

ggplot(data = data)+
  aes(x = Jour, y=Puissance.moyenne.journalière.de.la.consommation.totale..W.)+
  geom_point()

library(forecast)
library(astsa)
library(tseries)


datast <- ts(data = data$Puissance.moyenne.journalière.de.la.consommation.totale..W.,
             start = c(2019,11,23),
             frequency = 365)
datast %>% 
  plot(main = "Évolution de la puissance moyenne journalière de la consomation totale",
       xlab = "temps", ylab = "Consommation en W")

datast %>%
  decompose() %>%
  plot()

adf.test(datast)
# La série est donc stationnaire

datast %>%
  acf2()

model <- sarima(datast,2,0,130,0,1,0,365)

model_auto <- auto.arima(datast, seasonal = TRUE)
model_auto %>% summary()
sarima.for(datast,n.ahead = 10,2,0,130,0,1,1,7)

library(forecast)

model_auto <- auto.arima(datast, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
summary(model_auto)

sarima.for(datast,n.ahead = 10, 0,0,5)

model_sarima <- Arima(datast, order = c(1,1,1), seasonal = c(0,1,1), period = 365)
forecast_arima <- forecast(model_auto, h = 365)
plot(forecast_arima)

## tbats

model_tbats <- tbats(datast)
forecast_tbats <- forecast(model_tbats, h = 365)
plot(forecast_tbats)



## Ok ici on va essayer de moyeniser chaque mois pour n'avoir qu'un seul 