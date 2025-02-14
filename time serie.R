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
library(dplyr)


data_simplifie <- data %>% select(Jour,Puissance.moyenne.journalière.de.la.consommation.totale..W.)
write.csv(data_simplifie,file="Data.csv")


  
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

data_mois

# Transformer la date en format Année-Mois et faire la moyenne
df_mois <- data_simplifie %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%  # Extraire Année-Mois
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Puissance.moyenne.journalière.de.la.consommation.totale..W., na.rm = TRUE))

# Afficher le résultat
print(df_mois)

datast <- ts(data = df_mois$Moyenne_Valeur, start = c(2019,11), frequency = 12)

plot(datast)


adf.test(datast)
# La série est donc stationnaire

datast %>%
  acf2()

model <- auto.arima(datast, max.order = 10)
summary(model)

sarima.for(datast, n.ahead = 12, 0,1,1,1,1,1,12)

input <- c(debut, fin)

Variables <- c(data$proffess)

data$Puissance.moyenne.journalière.de.la.consommation.profilée.BT.INF.36.Professionnelle..W. <- as.numeric(data$Puissance.moyenne.journalière.de.la.consommation.profilée.BT.INF.36.Professionnelle..W.)

data_clean <- data %>%
  select(c("Puissance moyenne journalière de la consommation totale HTA (W)","Puissance moyenne journalière de la consommation télérelevée BT SUP 36 (W)","Puissance moyenne journalière de la consommation télérelevée BT INF 36 Professionelle (W)","Puissance moyenne journalière de la consommation télérelevée BT INF 36 Résidentielle (W)","Puissance moyenne journalière de la consommation totale.W"))


data_clean <- data %>%
  select(
    Jour,
    Puissance.moyenne.journalière.de.la.consommation.totale.HTA..W.,
    Puissance.moyenne.journalière.de.la.consommation.télérelevée.BT.SUP.36..W.,
    Puissance.moyenne.journalière.de.la.consommation.télérelevée.BT.INF.36.Professionelle..W.,
    Puissance.moyenne.journalière.de.la.consommation.télérelevée.BT.INF.36.Résidentielle..W.,
    Puissance.moyenne.journalière.de.la.consommation.totale..W.
  )

colnames(data_clean) <- c("Jour","Entreprises","PME/PMI","Professionnels","Résidentiels","Total")

write.csv(data_clean, "data.csv", row.names = FALSE)
write.csv(data_clean, "Application/data.csv", row.names = FALSE)

ggplot(data = data_clean)+
  aes(x = Jour, y = Total) +
  geom_point()+
  geom_smooth(method = "loess", color = "red", fill = "grey", linetype = "dashed", span=0.1)+
  theme_minimal()

data_avant_covid <- data_clean %>%
  filter(Jour < "2020-01-03")

data_apres_covid <- data_clean %>%
  filter(Jour > "2021-12-31")

data_nocovid <- bind_rows(data_avant_covid, data_apres_covid)

data_train_set <- data_nocovid %>%
  filter(Jour < "2022-10-01")

data_test_set <- data_nocovid %>%
  filter(Jour > "2022-10-01")

df_mois <- data_clean %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%  # Extraire Année-Mois
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))

data_apres_covid <- data_apres_covid %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%  # Extraire Année-Mois
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))

data_apres_covid_ts <- ts(data_apres_covid$Moyenne_Valeur,
                        frequency = 12,
                        start = c(2019,11))

resultat <- sarima.for(data_apres_covid_ts, n.ahead = 12, 0,1,1,0,1,0,12)

model <- auto.arima(data_apres_covid_ts)
data_apres_covid_ts %>% plot()

vrai_valeur <- data_clean %>%
  filter(Jour>"2022-10-01")

vrai_valeur <- vrai_valeur %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%  # Extraire Année-Mois
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))

vrai_valeur_ts <- ts(vrai_valeur$Moyenne_Valeur, start = c(2022,10), frequency = 12)

parametre <- model$arma

plot.ts(data_apres_covid_ts, type = "o", col = "black", 
        xlim=c(start(data_apres_covid_ts)[1],end(vrai_valeur_ts)[1]),
        ylim = range(c(data_apres_covid_ts, resultat$pred, vrai_valeur_ts)))
lines(resultat$pred, type = "o", col = "red")
lines(vrai_valeur_ts, vrai_valeur_ts, type = "o", col = "blue")
legend("topleft",
       legend = c("Données avant COVID", "Prédiction", "Vraies données"),
       col = c("black", "red", "blue"),
       lty = 0.1, pch = 1)


borne <- c(as.Date("2022-01-01"),as.Date("2023-10-05"))

data_train <- data_clean %>% filter(Jour < borne[1] & Jour >= "2019-12-01")
vrai_valeur <- data_clean %>% filter(Jour > borne[1] & Jour < borne[2])

vrai_valeur <- vrai_valeur %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))

vrai_valeur_ts <- ts(vrai_valeur$Moyenne_Valeur,
                     frequency = 12,
                     start=c(format(borne[1], format = "%Y"),format(borne[1], format = "%m")))

data_train <- data_train %>%
  mutate(Mois = format(Jour, "%Y-%m")) %>%
  group_by(Mois) %>%
  summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))

data_train_ts <- ts(data_train$Moyenne_Valeur, start = c(2019,12), frequency = 12)

model <- auto.arima(data_train_ts)

parametre <- model$arma

resultat <- sarima.for(data_train_ts, n.ahead = 100, parametre[1],parametre[2],parametre[3],parametre[4],parametre[7],parametre[6],parametre[5])
library(zoo)
plot.ts(data_train_ts, type = "o", col = "black",
        xlim=c(start(vrai_valeur_ts)[1],as.yearmon(end(vrai_valeur_ts)[1])),
        ylim = range(c(data_train_ts, resultat$pred, vrai_valeur_ts)))
lines(resultat$pred, type = "o", col = "red")
lines(vrai_valeur_ts, vrai_valeur_ts, type = "o", col = "blue")
legend("topleft",
       legend = c("Données avant COVID", "Prédiction", "Vraies données"),
       col = c("black", "red", "blue"),
       lty = 0.1, pch = 1)












