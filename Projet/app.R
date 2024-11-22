library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)


data <- read.csv2("bilan-electrique-jour.csv", header=TRUE)

ui <- fluidPage(
  titlePanel("Application avec des onglets"),
  
  # Menu à onglets
  tabsetPanel(
    tabPanel(
      "Graphique",
      sidebarLayout(
        sidebarPanel(
          sliderInput("jour", "Nombre de jours :", 
                      min = 2, max = 31, value = 10)
        ),
        mainPanel(
          plotOutput("plotGraphique")
        )
      )
    ),
    tabPanel(
      "Valeurs clés",
      sidebarLayout(
        sidebarPanel(
          numericInput("val1", "Entrez une valeur :", value = 10),
          numericInput("val2", "Entrez une autre valeur :", value = 20)
        ),
        mainPanel(
          fluidRow(
            column(4, textOutput("valueBox1")),
            column(6, textOutput("valueBox2")),
            column(12, textOutput("valueBoxSum"))
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # Onglet 1 : Graphique interactif
  output$plotGraphique <- renderPlot({
    depart <- ymd("2024-11-01") - days(input$jour) - days(1)
    nouvelle_data <- data %>% filter(data$jour > depart)
    
    ggplot(data = nouvelle_data) + 
      aes(x = ymd(jour), y = consommation_totale) +
      geom_point()
  })
  
  # Onglet 2 : Valeurs clés
  output$valueBox1 <- renderText({
    paste("Valeur 1 :", input$val1)
  })
  
  output$valueBox2 <- renderText({
    paste("Valeur 2 :", input$val2)
  })
  
  output$valueBoxSum <- renderText({
    paste("Somme :", input$val1 + input$val2)
  })
}

shinyApp(ui = ui, server = server)
