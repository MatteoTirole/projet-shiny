library(shiny)
library(ggplot2)
library(DT)

# Charger les données
df <- read.csv("Data.csv")
df$Jour <- as.Date(df$Jour)  

# UI
ui <- navbarPage("Shiny App",
                 
                 #Graphique
                 tabPanel("Graphiques",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("var", "Choisir une catégorie :", 
                                          choices = c("Entreprises", "PME.PMI", "Professionnels", "Résidentiels", "Total")),
                              dateRangeInput("dateRange", "Période :", 
                                             start = min(df$Jour), end = max(df$Jour))
                            ),
                            mainPanel(
                              plotOutput("graph")
                            )
                          )),
                 
                 # ValueBox
                 tabPanel("Valeurs",
                          fluidRow(
                            column(4, verbatimTextOutput("total_entreprises")),
                            column(4, verbatimTextOutput("total_pme")),
                            column(4, verbatimTextOutput("total_total"))
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
    ggplot(data_filtered(), aes(x = Jour, y = .data[[input$var]])) +
      geom_point(color = "blue") +
      labs(title = paste("Évolution de", input$var),
           x = "Date", y = input$var) +
      theme_minimal()
  })
  
  # Valeurs totales
  output$total_entreprises <- renderText({
    paste("Total Entreprises :", sum(df$Entreprises, na.rm = TRUE))
  })
  
  output$total_pme <- renderText({
    paste("Total PME/PMI :", sum(df$PME.PMI, na.rm = TRUE))
  })
  
  output$total_total <- renderText({
    paste("Total Global :", sum(df$Total, na.rm = TRUE))
  })
  
  # Table des données
  output$table <- renderDT({
    datatable(data_filtered())
  })
  
  # Téléchargement CSV
  output$downloadData <- downloadHandler(
    filename = function() { "data_filtre.csv" },
    content = function(file) {
      write.csv(data_filtered(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
