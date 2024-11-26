
#GRAPHIQUE

library(lubridate)
library(dplyr)
library(ggplot2)
library(plotly)

#Creation de nos données
data_octobre <- read.csv2("data/bilan-electrique-jour-octobre.csv")
data_novembre <- read.csv2("data/bilan-electrique-jour-novembre.csv")
data_decembre <- read.csv2("data/bilan-electrique-jour-decembre.csv")
data_janvier <- read.csv2("data/bilan-electrique-jour-janvier.csv")
data_fevrier <- read.csv2("data/bilan-electrique-jour-fevrier.csv")
data_mars <- read.csv2("data/bilan-electrique-jour-mars.csv")

data <- rbind(data_octobre,data_novembre,data_decembre,data_janvier,data_fevrier,data_mars)

#On nettoie nos données
data$production_totale <- as.double(data$production_totale)
data$consommation_totale <- as.double(data$consommation_totale)
data$jour <- as.Date(data$jour)



depart <- ymd("2024-10-01") + days(15)
nouvelle_data <- data %>% filter(data$jour > depart)

nouvelle_data$somme_cumulee <- cumsum(nouvelle_data$consommation_totale)

ggplotly(ggplot(data = data) + 
  aes(x = jour, y = consommation_totale) +
  geom_point())


data <- read.csv2("Projet/bilan-electrique-jour.csv")

as.double(data$production_totale)

ggplot(data, aes(x = jour)) +
  geom_line(aes(y = production_totale, color = "Production Totale"), size = 1) +
  geom_line(aes(y = consommation_totale, color = "Consommation Totale"), size = 1) +
  labs(
    title = "Production vs Consommation d'Énergie (GWh)",
    x = "Jour",
    y = "Énergie (GWh)",
    color = "Légende"
  )


class(data$production_totale)

ggplot(data = data) + 
  aes(x = as.Date(jour), y = production_totale) + 
  geom_line()
