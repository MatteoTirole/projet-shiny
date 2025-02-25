library(shiny)
library(ggplot2)
library(DT)
library(zoo)
library(dplyr)
library(forecast)
library(astsa)
library(tseries)


source("fonction.R")

# Charger les données
df <- read.csv("Data.csv")
df$Jour <- as.Date(df$Jour)  
data_clean <- read.csv("Data.csv")
library(zoo)


# UI
ui <- navbarPage("Shiny App",
                 
                 #Graphique
                 tabPanel("Graphiques",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var", "Choisir une catégorie :", 
                                          choices = c("Entreprises", "PME_PMI", "Professionnels", "Résidentiels", "Total")),
                              dateRangeInput("dateRange", "Période :", 
                                             start = as.Date("2022-10-01"), end = max(df$Jour),
                                             min = as.Date("2022-10-01"), max = max(df$Jour))
                            ),
                            mainPanel(
                              plotOutput("graph")
                            )
                          )),
                 
                 # ValueBox
                 tabPanel("Valeurs",
                          fluidRow(
                            column(4, verbatimTextOutput("total_reel")),
                            column(4, verbatimTextOutput("total_estimation")),
                            column(4, verbatimTextOutput("total_ecart"))
                          ),
                 fluidRow(
                   column(12, textOutput("explication_valeurs"))
                 )),
                 
                 
                 #Table
                 tabPanel("Table des Données",
                          DTOutput("table")),
                 
                 # Bouton de téléchargement
                 tabPanel("Téléchargement",
                          downloadButton("downloadData", "Télécharger CSV"))
)

server <- function(input, output) {
  
  # Filtrer les données
  data_filtered <- reactive({
    df[df$Jour >= input$dateRange[1] & df$Jour <= input$dateRange[2], ]
  })
  
  # Graphique
  output$graph <- renderPlot({
    fct(input$dateRange[1],input$dateRange[2], input$var)
  })
  
  # Valeurs totales
  output$total_reel <- renderText({
    paste("Somme total de l'année 2024 :", somme_tot())
  })
  
  output$total_estimation <- renderText({
    paste("Sommme prédite de l'année 2024 :", somme_predit())
  })
  
  output$total_ecart <- renderText({
    paste("Impact de la sobriété énerétique", abs(somme_tot() - somme_predit()))
  })
  
  output$explication_valeurs <- renderText({
    "Ces valeurs représentent la conssomation totale en France en GWh."
  })
  
  # Table des données
  output$table <- renderDT({
    datatable(creation_data(input$dateRange[1],input$dateRange[2], input$var))
  })
  
  # Téléchargement CSV
  output$downloadData <- downloadHandler(
    filename = function() { "données.csv" },
    content = function(file) {
      write.csv(creation_data(input$dateRange[1],input$dateRange[2], input$var), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
