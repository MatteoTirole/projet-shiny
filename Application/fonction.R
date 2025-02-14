data_clean <- read.csv("Data.csv")
data_clean$Jour <- as.Date(data_clean$Jour)

## Fonction de graphique 
fct <- function(date1,date2, var){
  data_train <- data_clean %>% filter(Jour < date1 & Jour >= "2019-12-01")
  vrai_valeur <- data_clean %>% filter(Jour > date1 & Jour < date2)
  
  vrai_valeur <- vrai_valeur %>%
    mutate(Mois = format(Jour, "%Y-%m")) %>%
    group_by(Mois) %>%
    summarise(Moyenne_Valeur = mean(.data[[var]], na.rm = TRUE))
  
  vrai_valeur_ts <- ts(vrai_valeur$Moyenne_Valeur,
                       frequency = 12,
                       start=c(format(date1, format = "%Y"),format(date1, format = "%m")))
  
  data_train <- data_train %>%
    mutate(Mois = format(Jour, "%Y-%m")) %>%
    group_by(Mois) %>%
    summarise(Moyenne_Valeur = mean(.data[[var]], na.rm = TRUE))
  
  data_train_ts <- ts(data_train$Moyenne_Valeur, start = c(2019,12), frequency = 12)
  
  model <- auto.arima(data_train_ts)
  
  parametre <- model$arma
  
  resultat <- sarima.for(data_train_ts, n.ahead = 100, parametre[1],parametre[2],parametre[3],parametre[4],parametre[7],parametre[6],parametre[5])
  plot.ts(data_train_ts, type = "o", col = "black",
          xlim=c(start(vrai_valeur_ts)[1] + (start(vrai_valeur_ts)[2]-1)/12,end(vrai_valeur_ts)[1] + (end(vrai_valeur_ts)[2]-1)/12),
          ylim = range(c(data_train_ts, resultat$pred, vrai_valeur_ts)))
  lines(resultat$pred, type = "o", col = "red")
  lines(vrai_valeur_ts, vrai_valeur_ts, type = "o", col = "blue")
  lines(resultat$pred + 1.96 * resultat$se, col = "red", lty = 2)
  lines(resultat$pred - 1.96 * resultat$se, col = "red", lty = 2) 
  legend("topleft",
         legend = c("Données avant COVID", "Prédiction", "Vraies données"),
         col = c("black", "red", "blue"),
         lty = 0.1, pch = 1)
}

fct(as.Date("2022-01-01"), as.Date("2023-01-01"),"Total")

## Fonction qui renvoie la somme
somme_tot <- function(){
  data_fct <- data_clean %>% filter(Jour >= "2023-01-01" & Jour < "2024-01-01")
  resultat <- sum(data_fct$Total)
  resultat
}

## Fonction qui renvoie une prédiction de la somme
somme_predit <- function(){
  data_train <- data_clean %>% filter(Jour < "2023-01-01" & Jour >= "2019-12-01")
  data_train <- data_train %>%
    mutate(Mois = format(Jour, "%Y-%m")) %>%
    group_by(Mois) %>%
    summarise(Moyenne_Valeur = mean(Total, na.rm = TRUE))
  
  data_train_ts <- ts(data_train$Moyenne_Valeur, start = c(2019,12), frequency = 12)
  
  model <- auto.arima(data_train_ts)
  
  parametre <- model$arma
  
  resultat_model <- sarima.for(data_train_ts, n.ahead = 100, parametre[1],parametre[2],parametre[3],parametre[4],parametre[7],parametre[6],parametre[5])
  
  resultat <- sum(resultat_model$pred)
  resultat
}

## Fonction qui retourne la data en fonction du filtre de date et de variable
creation_data <- function(date1,date2,var){
  data_train <- data_clean %>% filter(Jour < date1 & Jour >= "2019-12-01")
  vrai_valeur <- data_clean %>% filter(Jour > date1 & Jour < date2)
  
  vrai_valeur <- vrai_valeur %>%
    mutate(Mois = format(Jour, "%Y-%m")) %>%
    group_by(Mois) %>%
    summarise(Moyenne_Valeur = mean(.data[[var]], na.rm = TRUE))
  
  vrai_valeur_ts <- ts(vrai_valeur$Moyenne_Valeur,
                       frequency = 12,
                       start=c(format(date1, format = "%Y"),format(date1, format = "%m")))
  
  data_train <- data_train %>%
    mutate(Mois = format(Jour, "%Y-%m")) %>%
    group_by(Mois) %>%
    summarise(Moyenne_Valeur = mean(.data[[var]], na.rm = TRUE))
  
  data_train_ts <- ts(data_train$Moyenne_Valeur, start = c(2019,12), frequency = 12)
  
  model <- auto.arima(data_train_ts)
  
  parametre <- model$arma
  
  resultat <- sarima.for(data_train_ts, n.ahead = length(vrai_valeur), parametre[1],parametre[2],parametre[3],parametre[4],parametre[7],parametre[6],parametre[5])
  
  data_resultat <- data.frame(date = vrai_valeur$Mois,
                              valeur = vrai_valeur$Moyenne_Valeur,
                              prediction = as.numeric(resultat$pred),
                              ecart = abs(vrai_valeur$Moyenne_Valeur - as.numeric(resultat$pred)))
  data_resultat
}
