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

### Ici, on nettoie la table de données pour prendre uniquement les variables qui nous intéresse

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

data_clean <- read.csv("Application/data.csv")
ggplot(data = data_clean)+
  aes(x = Jour, y = Total) +
  geom_point()+
  geom_smooth(method = "loess", color = "red", fill = "grey", linetype = "dashed", span=0.1)+
  theme_minimal()

## Ici, on teste nos prédictions de données avec une méthode SARIMA

library(dplyr)
data_clean$Jour <- as.Date(data_clean$Jour)
borne <- c(as.Date("2022-01-01"),as.Date("2023-01-01"))

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
        xlim=c(start(vrai_valeur_ts)[1] + (start(vrai_valeur_ts)[2]-1)/12,end(vrai_valeur_ts)[1] + (end(vrai_valeur_ts)[2]-1)/12),
        ylim = range(c(data_train_ts, resultat$pred, vrai_valeur_ts)))
lines(resultat$pred, type = "o", col = "red")
lines(vrai_valeur_ts, vrai_valeur_ts, type = "o", col = "blue")
legend("topleft",
       legend = c("Données avant COVID", "Prédiction", "Vraies données"),
       col = c("black", "red", "blue"),
       lty = 0.1, pch = 1)
