#borne <- c(input$dateRange[1], input$dateRange[2])
fct <- function(date1,date2){
  data_train <- data_clean %>% filter(Jour < date1 & Jour >= "2019-12-01")
  vrai_valeur <- data_clean %>% filter(Jour > date1 & Jour < date2)
  
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
  plot.ts(data_train_ts, type = "o", col = "black",
          xlim=c(start(vrai_valeur_ts)[1],as.yearmon(end(vrai_valeur_ts)[1])),
          ylim = range(c(data_train_ts, resultat$pred, vrai_valeur_ts)))
  lines(resultat$pred, type = "o", col = "red")
  lines(vrai_valeur_ts, vrai_valeur_ts, type = "o", col = "blue")
  legend("topleft",
         legend = c("Données avant COVID", "Prédiction", "Vraies données"),
         col = c("black", "red", "blue"),
         lty = 0.1, pch = 1)
}


fct(as.Date("2023-01-01"), as.Date("2024-01-01"))
