
#GRAPHIQUE

library(lubridate)
depart <- ymd("2024-10-01") + days(15)
nouvelle_data <- data %>% filter(data$jour > depart)
ggplot(data = nouvelle_data) + 
  aes(x = ymd(jour), y = consommation_totale) +
  geom_point()


ymd(data$jour)

data <- read.csv2("data/bilan-electrique-jour.csv")
