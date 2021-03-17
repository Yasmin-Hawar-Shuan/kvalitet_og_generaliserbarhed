library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)

deselect <- c("turngl", "TurtypeHelrejse", "StartStopPointNr", "SlutStopPointNr", "RejseAar", "RejseMd", "KortNr")

df <- read.csv("2020/Helrejser_Rejsekortet_januar_2020.csv", sep = ";")[1:10000, ] %>% 
    select(-deselect)
   
ui <- fluidPage(
  tags$div(
    class = "wrapper",
    tags$style(type = "text/css", ".wrapper {padding: 30px}"),
    textInput(inputId = "inputTitle", label = "Plot title:", value = "Fynbus Visualiseringer"),
    varSelectInput(inputId = "dropdownX", label = "X-axis", data = df),
    varSelectInput(inputId = "dropdownY", label = "Y-axis", data = df),
    varSelectInput(inputId = "dropdownC", label = "Colour", data = df),
    plotlyOutput(outputId = "chart")
  )
)

server <- function(input, output) {
  output$chart <- renderPlotly({
    col_x <- sym(input$dropdownX)
    col_y <- sym(input$dropdownY)
    col_c <- sym(input$dropdownC)
    
    p <- ggplot(df, aes(x = !!col_x, y = !!col_y, color = !!col_c)) +
      geom_point() +
      labs(title = input$inputTitle)
    ggplotly(p)
  })
}

shinyApp(ui, server)
